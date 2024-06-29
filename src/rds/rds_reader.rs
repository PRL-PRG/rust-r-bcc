use std::cell::UnsafeCell;

use std::io::Read;

use crate::rds::RDSHeader;
use crate::sexp::bc::Bc;
use crate::sexp::bc::ConstPoolItem;
use crate::sexp::sexp::data;
use crate::sexp::sexp::lang;
use crate::sexp::sexp::Sexp;
use crate::sexp::sexp::SexpKind;
use crate::sexp::sexp_alloc::Alloc;

use super::sexptype;
use super::Flag;
use super::RDSResult;
use super::RefsTableReader;

#[derive(Debug)]
pub enum RDSReaderError {
    DataError(String),
    //WrongFlag(i32),
    IO(std::io::Error),
}

impl From<std::io::Error> for RDSReaderError {
    fn from(value: std::io::Error) -> Self {
        RDSReaderError::IO(value)
    }
}

impl From<std::string::FromUtf8Error> for RDSReaderError {
    fn from(value: std::string::FromUtf8Error) -> Self {
        RDSReaderError::DataError(format!("UTF-8 error {value:?}"))
    }
}

pub struct RDSReader<'a, T>
where
    T: Read + Sized,
{
    reader: UnsafeCell<T>,
    arena: &'a Alloc<'a>,
}

impl<'a, T> RDSReader<'a, T>
where
    T: Read + Sized,
{
    pub fn new(reader: UnsafeCell<T>, arena: &'a Alloc<'a>) -> Self {
        Self { reader, arena }
    }

    pub fn restore(self) -> T {
        self.reader.into_inner()
    }

    pub fn read_rds(&self) -> Result<RDSResult<'a>, RDSReaderError> {
        let header = self.read_header()?;
        let refs = RefsTableReader::new(self.arena);
        let item = self.read_item(&refs)?;

        Ok(RDSResult::new(header, item))
    }

    fn read_byte(&self) -> Result<u8, RDSReaderError> {
        let reader = unsafe { &mut *self.reader.get() };
        let mut buf: [u8; 1] = [0];
        reader.read(&mut buf)?;
        Ok(buf[0])
    }

    fn read_int(&self) -> Result<i32, RDSReaderError> {
        let reader = unsafe { &mut *self.reader.get() };
        let mut buf: [u8; 4] = [0; 4];
        if reader.read_exact(&mut buf).is_err() {
            return Err(RDSReaderError::DataError("Cannot read int".to_string()));
        }
        Ok(i32::from_be_bytes(buf))
    }

    fn read_complex(&self) -> Result<data::Complex, RDSReaderError> {
        let real = self.read_double()?.into();
        let imaginary = self.read_double()?.into();
        Ok(data::Complex { real, imaginary })
    }

    fn read_double(&self) -> Result<f64, RDSReaderError> {
        let reader = unsafe { &mut *self.reader.get() };
        let mut buf: [u8; 8] = [0; 8];
        if reader.read_exact(&mut buf).is_err() {
            return Err(RDSReaderError::DataError("Cannot read double".to_string()));
        }
        Ok(f64::from_be_bytes(buf))
    }

    fn read_string_len(&self, len: i32) -> Result<&'a str, RDSReaderError> {
        let reader = unsafe { &mut *self.reader.get() };
        let len = len as usize;
        let mut buf: Vec<u8> = vec![];
        buf.resize(len, 0);
        let read_len = reader.read(buf.as_mut_slice())?;
        if len != read_len {
            return Err(RDSReaderError::DataError(format!(
                "Cannot read string of len {len}"
            )));
        }
        let tmp = String::from_utf8(buf)?;
        let data = self.arena.alloc_str(tmp.as_str());
        Ok(data)
    }

    fn read_header(&self) -> Result<RDSHeader, RDSReaderError> {
        let rds_type = self.read_byte().expect("cannot open file");
        assert_eq!(rds_type, b'X');
        assert_eq!(self.read_byte()?, b'\n');
        let format_version = self.read_int()?;
        let writer_version = self.read_int()?;
        let min_reader_version = self.read_int()?;

        let res = RDSHeader {
            rds_type,
            format_version,
            writer_version,
            min_reader_version,
        };

        match format_version {
            2 => Ok(res),
            3 => {
                let len = self.read_int()?;
                self.read_string_len(len)?;
                Ok(res)
            }
            _ => Err(RDSReaderError::DataError(format!(
                "Wrong format : {format_version}"
            ))),
        }
    }

    fn read_item(&self, refs: &RefsTableReader<'a>) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let flag = self.read_flags()?;
        self.read_item_flags(refs, flag)
    }

    fn read_item_flags(
        &self,
        refs: &RefsTableReader<'a>,
        flag: Flag,
    ) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let sexp: &'a Sexp = match flag.sexp_type {
            sexptype::NILVALUE_SXP | sexptype::NILSXP => self.arena.nil,
            sexptype::REALSXP => self.read_realsxp()?,
            sexptype::INTSXP => self.read_intsxp()?,
            sexptype::LISTSXP => self
                .arena
                .alloc(SexpKind::List(self.read_listsxp(refs, flag)?).into()),
            sexptype::VECSXP => {
                let res = self.read_vecsxp(refs)?;
                self.arena.alloc(SexpKind::Vec(res).into())
            }
            sexptype::SYMSXP => {
                let data = self.read_symsxp(refs)?;
                let data = self.arena.alloc(SexpKind::Sym(data).into());
                refs.add_ref(data);
                data
            }
            sexptype::STRSXP => self.read_strsxp(refs)?,
            sexptype::CHARSXP => self.read_charsxp()?,
            sexptype::LANGSXP => {
                let data = self.read_langsxp(refs, flag)?;
                self.arena.alloc(SexpKind::Lang(data).into())
            }
            sexptype::CLOSXP => self.read_closxp(refs, flag)?,
            sexptype::ENVSXP => self.read_envsxp(refs)?,
            sexptype::GLOBALENV_SXP => self.arena.global_env,
            sexptype::EMPTYENV_SXP => self.arena.empty_env,
            sexptype::BASEENV_SXP | sexptype::BASENAMESPACE_SXP => self.arena.base_env,
            sexptype::MISSINGARG_SXP => self.arena.missing,
            sexptype::REFSXP => self.read_refsxp(refs, flag)?,
            sexptype::BCODESXP => self.read_bc(refs)?,
            sexptype::LGLSXP => self.read_lglsxp()?,
            sexptype::BUILTINSXP | sexptype::SPECIALSXP => {
                let len = self.read_int()?;
                let name = self.read_string_len(len)?;
                self.arena
                    .alloc(SexpKind::Buildin(lang::Sym::new(name)).into())
            }
            sexptype::ALTREP_SXP => {
                let info = self.read_item(refs)?;
                let _state = self.read_item(refs)?;
                let _attr = self.read_item(refs)?;
                let info = match info.kind {
                    SexpKind::List(list) if list.len() == 3 => list,
                    _ => {
                        return Err(RDSReaderError::DataError(
                            "Wrong info in ALTREP sexp".into(),
                        ))
                    }
                };
                let _class_sym = &info[0];
                let _package_sym = &info[1];
                let SexpKind::Int(class_type) = &info[2].data.kind else {
                    return Err(RDSReaderError::DataError(
                        "Wrong class type in info for ALTREP".into(),
                    ));
                };
                let _class_type = if class_type.len() == 1 {
                    class_type[0]
                } else {
                    return Err(RDSReaderError::DataError(
                        "Wrong class type in info for ALTREP".into(),
                    ));
                };

                todo!()
            }
            sexptype::PROMSXP => self.read_promsxp(flag, refs)?,
            sexptype::CPLXSXP => self.read_cplsxp()?,
            sexptype::NAMESPACESXP => self.read_namespace(refs)?,
            sexptype::UNBOUNDVALUE_SXP => self.arena.unbound,
            //sexptype::EXTPTRSXP => self.chain
            //sexptype::BASENAMESPACE_SXP => SexpKind::BaseNamespace.into(),
            x => {
                println!("{x}");
                todo!()
            }
        };

        // the part of the flag used in has attribute is
        // also used in values for REFSXP
        // since ENVSXP has attr every time then we skip it here
        // CLOSXP has attribute in different place
        if flag.has_attributes
            && flag.sexp_type != sexptype::REFSXP
            && flag.sexp_type != sexptype::ENVSXP
            && flag.sexp_type != sexptype::CLOSXP
        {
            sexp.set_attr(self.read_item(refs)?);
        }

        Ok(sexp)
    }

    fn read_flags(&self) -> Result<Flag, RDSReaderError> {
        let flag = self.read_int()?;
        let sexp_type: u8 = (flag & 255) as u8;
        let level: i32 = flag >> 12;
        let has_attributes = (flag & (1 << 9)) != 0;
        let has_tag = (flag & (1 << 10)) != 0;
        let obj = (flag & (1 << 8)) != 0;

        Ok(Flag {
            sexp_type,
            level,
            has_attributes,
            has_tag,
            obj,
            orig: flag,
        })
    }

    fn read_len(&self) -> Result<usize, RDSReaderError> {
        let len = self.read_int()?;
        if len < -1 {
            Err(RDSReaderError::DataError(format!(
                "Negative vector len {len}"
            )))
        } else if len >= 0 {
            Ok(len as usize)
        } else {
            // if len == -1
            let upper = self.read_int()?;
            let lower = self.read_int()?;
            // in orignal code descriped as sanity check
            if upper > 65536 {
                return Err(RDSReaderError::DataError(format!(
                    "Invalid upper part of vector len {upper}"
                )));
            }

            Ok((upper as usize) << 32 + lower as usize)
        }
    }

    fn read_realsxp(&self) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let len = self.read_len()?;

        let data = self.arena.alloc_slice_fill_default(len);

        for i in 0..len {
            data[i] = self.read_double()?.into();
        }

        Ok(self
            .arena
            .alloc(SexpKind::Real(data::RVec::new(data)).into()))
    }

    fn read_ints(&self) -> Result<&'a [i32], RDSReaderError> {
        let len = self.read_len()?;

        let data = self.arena.alloc_slice_fill_default(len);

        for i in 0..len {
            data[i] = self.read_int()?
        }
        Ok(data)
    }

    fn read_intsxp(&self) -> Result<&'a Sexp<'a>, RDSReaderError> {
        Ok(self
            .arena
            .alloc(SexpKind::Int(data::RVec::new(self.read_ints()?)).into()))
    }

    fn read_lglsxp(&self) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let len = self.read_len()?;

        let data = self.arena.alloc_slice_fill_copy(len, data::Logic::True);

        for i in 0..len {
            data[i] = self.read_int()?.into();
        }

        Ok(self.arena.alloc(SexpKind::Logic(data).into()))
    }

    fn read_cplsxp(&self) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let len = self.read_len()?;
        let data = self.arena.alloc_slice_fill_default(len);
        for i in 0..len {
            data[i] = self.read_complex()?;
        }
        Ok(self
            .arena
            .alloc(SexpKind::Complex(data::RVec::new(data)).into()))
    }

    fn read_vecsxp(
        &self,
        refs: &RefsTableReader<'a>,
    ) -> Result<&'a mut [&'a Sexp<'a>], RDSReaderError> {
        let len = self.read_len()?;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            // must be demoted to immutable because the mutable
            // reference cannot be copied, which is done
            // in alloc_slice_copy
            data.push(self.read_item(refs)? as &'a Sexp)
        }

        let data = self.arena.alloc_slice_copy(data.as_slice());

        Ok(data)
    }

    fn read_listsxp(
        &self,
        refs: &RefsTableReader<'a>,
        flags: Flag,
    ) -> Result<data::List<'a>, RDSReaderError> {
        // read in order attrib tag and value
        // only value is mandatory

        let mut flags = flags;
        let mut data = vec![];

        loop {
            let attr = if flags.has_attributes {
                Some(self.read_item(refs)?)
            } else {
                None
            };

            let tag = if flags.has_tag {
                Some(self.read_item(refs)?)
            } else {
                None
            };

            let value = self.read_item(refs)?;
            if let Some(attr) = attr {
                value.set_attr(attr);
            }

            if let Some(Sexp {
                kind: SexpKind::Sym(tag),
                ..
            }) = tag
            {
                data.push(data::TaggedSexp::new_with_tag(value, tag));
            } else {
                data.push(value.into());
            }

            flags = self.read_flags()?;
            if flags.sexp_type != sexptype::LISTSXP {
                let last = self.read_item_flags(refs, flags)?;
                match &last.kind {
                    SexpKind::Nil => (),
                    _ => data.push(last.into()),
                }
                let data = self.arena.alloc_slice_clone(data.as_slice());
                return Ok(data::List { data });
            }
        }
    }

    // this reads the CHARSXP to str
    fn read_string(&self) -> Result<Option<&'a str>, RDSReaderError> {
        let len = self.read_int()?;
        if len == -1 {
            return Ok(None);
        }
        let len = len as usize;
        let mut data: Vec<u8> = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_byte()?)
        }

        let data = String::from_utf8(data)?;
        Ok(Some(self.arena.alloc_str(data.as_str())))
    }

    fn read_symsxp(&self, _refs: &RefsTableReader<'a>) -> Result<lang::Sym<'a>, RDSReaderError> {
        let _ = self.read_flags()?;

        let data = match self.read_string()? {
            Some(data) => data,
            None => return Err(RDSReaderError::DataError("Symbol cannot be NA".into())),
        };

        Ok(lang::Sym::new(data))
    }

    fn read_charsxp(&self) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let len = self.read_int()?;
        if len == i32::MAX {
            return Ok(self.arena.alloc(SexpKind::NAString.into()));
        }
        let len = len as usize;

        let data = self.arena.alloc_slice_fill_copy(len, '\0');

        for i in 0..len {
            data[i] = self.read_byte()? as char;
        }

        Ok(self.arena.alloc(SexpKind::Char(data).into()))
    }

    fn read_strsxp(&self, _refs: &RefsTableReader<'a>) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let len = self.read_len()?;
        let data = self.arena.alloc_slice_fill_copy(len, self.arena.na_string);

        for i in 0..len {
            if self.read_flags()?.sexp_type != sexptype::CHARSXP {
                return Err(RDSReaderError::DataError(
                    "String must have charsxp expressions".into(),
                ));
            }

            let string = match self.read_string()? {
                Some(string) => string,
                None => continue,
            };

            data[i] = string;
        }

        Ok(self
            .arena
            .alloc(SexpKind::Str(data::RVec::new(data)).into()))
    }

    fn read_langsxp(
        &self,
        refs: &RefsTableReader<'a>,
        flag: Flag,
    ) -> Result<lang::Lang<'a>, RDSReaderError> {
        let attr = if flag.has_attributes {
            Some(self.read_item(refs)?)
        } else {
            None
        };

        if flag.has_tag {
            todo!()
        }

        let target = {
            let flag = self.read_flags()?;
            match flag.sexp_type {
                sexptype::REFSXP => {
                    let SexpKind::Sym(sym) = &self.read_refsxp(refs, flag)?.kind else {
                        return Err(RDSReaderError::DataError(format!(
                            "Target needs to be either symbol or lang in ref"
                        )));
                    };
                    lang::Target::Sym(sym.clone())
                }
                sexptype::SYMSXP => {
                    let sym = self.read_symsxp(refs)?;
                    refs.add_ref(self.arena.alloc(SexpKind::Sym(sym.clone()).into()));
                    lang::Target::Sym(sym)
                }
                sexptype::LANGSXP => {
                    lang::Target::Lang(self.arena.alloc(self.read_langsxp(refs, flag)?))
                }
                x => {
                    return Err(RDSReaderError::DataError(format!(
                        "Target needs to be either symbol or lang got {x}"
                    )))
                }
            }
        };

        let args = {
            let flags = self.read_flags()?;
            match flags.sexp_type {
                sexptype::NILSXP | sexptype::NILVALUE_SXP => self.arena.nil_list,
                sexptype::LISTSXP => self.read_listsxp(refs, flags)?,
                x => {
                    return Err(RDSReaderError::DataError(format!(
                        "Args need to be list got {x:?}"
                    )))
                }
            }
        };

        let res = lang::Lang::new(target, args);
        if let Some(_) = attr {
            todo!();
            //res.set_attr(attr);
        }
        Ok(res)
    }

    fn read_formals(
        &self,
        refs: &RefsTableReader<'a>,
    ) -> Result<&'a [lang::Formal<'a>], RDSReaderError> {
        let mut flags = self.read_flags()?;
        if flags.sexp_type == sexptype::NILSXP || flags.sexp_type == sexptype::NILVALUE_SXP {
            // panic because it should not run
            return Ok(self.arena.alloc_slice_fill_with(0, |_| panic!()));
        }
        let mut res: Vec<lang::Formal<'a>> = vec![];
        loop {
            if flags.has_attributes {
                return Err(RDSReaderError::DataError(
                    "Formal cannot have attribute".into(),
                ));
            }

            let tag = if flags.has_tag {
                Some(self.read_item(refs)?)
            } else {
                return Err(RDSReaderError::DataError("Formal must have tag".into()));
            };

            let value = self.read_item(refs)?;

            let Some(Sexp {
                kind: SexpKind::Sym(tag),
                ..
            }) = tag
            else {
                unreachable!()
            };
            res.push(lang::Formal::new(tag.clone(), value));

            flags = self.read_flags()?;
            if flags.sexp_type != sexptype::LISTSXP {
                match flags.sexp_type {
                    sexptype::NILSXP | sexptype::NILVALUE_SXP => (),
                    _ => {
                        return Err(RDSReaderError::DataError(
                            "formal must be ended with NIL".into(),
                        ))
                    }
                }
                return Ok(self.arena.alloc_slice_clone(res.as_slice()));
            }
        }
    }

    fn read_closxp(
        &self,
        refs: &RefsTableReader<'a>,
        flags: Flag,
    ) -> Result<&'a Sexp<'a>, RDSReaderError> {
        // order of the values : [attr], enviroment, formals, body
        let attr = if flags.has_attributes {
            Some(self.read_item(refs)?)
            //None
        } else {
            None
        };
        let environment = {
            let flag = self.read_flags()?;
            self.read_enviroment(refs, flag)?
        };
        let formals = self.read_formals(refs)?;
        let body = self.read_item(refs)?;

        let res: &mut Sexp = self
            .arena
            .alloc(SexpKind::Closure(lang::Closure::new(formals, body, environment)).into());

        if let Some(attr) = attr {
            res.set_attr(attr)
        }

        Ok(res)
    }

    fn read_namespace(&self, refs: &RefsTableReader<'a>) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let _ = self.read_int()?;
        let len = self.read_int()?;
        let mut res = vec![];
        for _ in 0..len {
            res.push(self.read_item(refs)? as &'a Sexp);
        }
        let res = lang::Environment::Namespace(self.arena.alloc_slice_copy(res.as_slice()));
        let res = self.arena.alloc(SexpKind::Environment(res).into());
        refs.add_ref(res);
        Ok(res)
    }

    fn read_enviroment(
        &self,
        refs: &RefsTableReader<'a>,
        flags: Flag,
    ) -> Result<&'a lang::Environment<'a>, RDSReaderError> {
        let res = match flags.sexp_type {
            sexptype::ENVSXP => {
                // here I need to get only env from Sexp that
                // for sure contains env. This could create unecessery
                // data in the self.arena however the env must be checked
                // into the refs so it still needs to be created
                let SexpKind::Environment(env) = &self.read_envsxp(refs)?.kind else {
                    unreachable!()
                };
                env
            }
            sexptype::NAMESPACESXP => {
                let SexpKind::Environment(env) = &self.read_namespace(refs)?.kind else {
                    unreachable!()
                };
                env
            }
            sexptype::REFSXP => {
                let SexpKind::Environment(env) = &self.read_refsxp(refs, flags)?.kind else {
                    unreachable!()
                };
                env
            }
            sexptype::GLOBALENV_SXP => self.arena.get_global(),
            sexptype::EMPTYENV_SXP | sexptype::NILSXP | sexptype::NILVALUE_SXP => {
                self.arena.get_empty()
            }
            sexptype::BASEENV_SXP | sexptype::BASENAMESPACE_SXP => self.arena.get_base(),
            _ => {
                return Err(RDSReaderError::DataError(format!(
                    "Parent of environment must be an environment got {flags:?}"
                )));
            }
        };

        Ok(res)
    }

    // This returs Sexp only because I need to check it in
    // the refs table however I should always be NormalEnv
    fn read_envsxp(&self, refs: &RefsTableReader<'a>) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let locked = self.read_int()?;

        // just register ref so my indexes are correct
        let index = refs.add_placeholder();

        let parent = {
            let flag = self.read_flags()?;
            self.read_enviroment(refs, flag)?
        };
        let frame = {
            let flag = self.read_flags()?;
            match flag.sexp_type {
                sexptype::LISTSXP => self.read_listsxp(refs, flag)?,
                sexptype::NILSXP | sexptype::NILVALUE_SXP => self.arena.nil_list,
                _ => return Err(RDSReaderError::DataError("List frame must be list".into())),
            }
        };
        let hashtab = {
            let flag = self.read_flags()?;
            match flag.sexp_type {
                sexptype::VECSXP => self.read_vecsxp(refs)?,
                sexptype::NILSXP | sexptype::NILVALUE_SXP => self.arena.nil_vec,
                _ => {
                    return Err(RDSReaderError::DataError(
                        "Hash frame must be vector".into(),
                    ))
                }
            }
        };
        let attr = self.read_item(refs)?;

        let env = self.arena.alloc(lang::NormalEnv::new(
            parent,
            locked == 1,
            lang::ListFrame::new(frame, self.arena),
            lang::HashFrame::new(hashtab, self.arena),
        ));
        let env: lang::Environment = lang::Environment::Normal(env);
        let env: SexpKind = env.into();
        let env: Sexp = env.into();
        let env = self.arena.alloc(env);
        env.set_attr(attr);
        if !refs.update_ref(index, env) {
            return Err(RDSReaderError::DataError(format!(
                "Wrong ref index {index}"
            )));
        }
        Ok(env)
    }

    fn read_refsxp(
        &self,
        refs: &RefsTableReader<'a>,
        flags: Flag,
    ) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let index = flags.orig >> 8;
        let index = if index == 0 { self.read_int()? } else { index } - 1;

        refs.get_ref(index).map_or(
            Err(RDSReaderError::DataError(format!(
                "Wrong ref index {index}"
            ))),
            |x| Ok(x),
        )
    }

    fn read_bc(&self, refs: &RefsTableReader<'a>) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let reps = self.read_int()?;
        // reps vec is not part of the
        // rds data it is the temp value
        // so I dont need to use self.arena
        let mut reps_vec = vec![];
        reps_vec.resize(reps as usize, self.arena.nil as &'a Sexp);
        self.read_bc_inner(refs, &mut reps_vec)
    }

    fn read_bc_inner(
        &self,
        refs: &RefsTableReader<'a>,
        reps: &mut Vec<&'a Sexp<'a>>,
    ) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let code = {
            let flag = self.read_flags()?;
            match flag.sexp_type {
                sexptype::INTSXP => self.read_ints()?,
                _ => todo!(),
            }
        };
        let consts = self.read_bcconsts(refs, reps)?;
        let bc = Bc::new(code, consts);
        Ok(self.arena.alloc(SexpKind::Bc(bc).into()))
    }

    fn read_bcconsts(
        &self,
        refs: &RefsTableReader<'a>,
        reps: &mut Vec<&'a Sexp<'a>>,
    ) -> Result<&'a [ConstPoolItem<'a>], RDSReaderError> {
        let n = self.read_int()?;
        let res = self
            .arena
            .alloc_slice_fill_copy(n as usize, ConstPoolItem::Sexp(self.arena.nil as &'a Sexp));
        for i in 0..n {
            let type_val = self.read_int()?;
            if type_val > 0xff || type_val < 0 {
                return Err(RDSReaderError::DataError(
                    "Wrong type in byte code constants".into(),
                ));
            }
            let type_val = type_val as u8;
            let tmp = match type_val {
                sexptype::BCODESXP => self.read_bc_inner(refs, reps)?,
                sexptype::LANGSXP
                | sexptype::LISTSXP
                | sexptype::BCREPDEF
                | sexptype::BCREPREF
                | sexptype::ATTRLANGSXP
                | sexptype::ATTRLISTSXP => self.read_bclang(type_val, refs, reps)?,
                _ => self.read_item(refs)?,
            };
            res[i as usize] = tmp.into();
        }
        Ok(res)
    }

    fn read_bclang(
        &self,
        type_val: u8,
        refs: &RefsTableReader<'a>,
        reps: &mut Vec<&'a Sexp<'a>>,
    ) -> Result<&'a Sexp<'a>, RDSReaderError> {
        match type_val {
            sexptype::BCREPREF => Ok(reps[self.read_int()? as usize]),
            sexptype::BCREPDEF => {
                let pos = self.read_int()?;
                let type_val = self.read_int()?;
                let res = self.read_bclang_inner(type_val as u8, refs, reps)?;

                if pos >= 0 {
                    reps[pos as usize] = res;
                }

                Ok(res)
            }
            sexptype::LANGSXP
            | sexptype::ATTRLANGSXP
            | sexptype::LISTSXP
            | sexptype::ATTRLISTSXP => self.read_bclang_inner(type_val, refs, reps),
            _ => self.read_item(refs).map(|x| x as &'a Sexp),
        }
    }

    fn read_bclang_langsxp(
        &self,
        type_val: u8,
        refs: &RefsTableReader<'a>,
        reps: &mut Vec<&'a Sexp<'a>>,
    ) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let (_, has_attr) = match type_val {
            sexptype::ATTRLANGSXP => (sexptype::LANGSXP, true),
            sexptype::LANGSXP => (type_val, false),
            // here should be panic since this is only for
            // this and should not be called otherwise
            _ => panic!("Unexpected type_orig in read_bclang_langsxp"),
        };

        let _attr = if has_attr {
            Some(self.read_item(refs)?)
        } else {
            None
        };
        let tag = self.read_item(refs)?;

        let _ = match &tag.kind {
            SexpKind::Sym(sym) => Ok(Some(sym.data)),
            SexpKind::Nil => Ok(None),
            _ => Err(RDSReaderError::DataError("Tag must be sym".into())),
        }?;

        let type_val = self.read_int()? as u8;
        let car = self.read_bclang(type_val, refs, reps)?;

        let type_val = self.read_int()? as u8;
        let cdr = self.read_bclang(type_val, refs, reps)?;

        let cdr = match cdr.kind {
            SexpKind::Nil => Ok(self.arena.nil_list),
            SexpKind::List(cdr) => Ok(cdr),
            _ => Err(RDSReaderError::DataError(format!(
                "Wrong cdr type in bc lang read {type_val}"
            ))),
        }?;

        let target: lang::Target = match &car.kind {
            SexpKind::Sym(sym) => Ok(lang::Target::Sym(sym.clone())),
            SexpKind::Lang(lang) => Ok(lang::Target::Lang(lang)),
            _ => Err(RDSReaderError::DataError("Wrong lang target".into())),
        }?;

        Ok(self
            .arena
            .alloc(SexpKind::Lang(lang::Lang::new(target, cdr)).into()))
    }

    fn read_bclang_list(
        &self,
        type_val: u8,
        refs: &RefsTableReader<'a>,
        reps: &mut Vec<&'a Sexp<'a>>,
    ) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let mut res = vec![];
        loop {
            let (_, has_attr) = match type_val {
                sexptype::ATTRLISTSXP => (sexptype::LISTSXP, true),
                sexptype::LISTSXP => (type_val, false),
                // same as in the previous function
                // basicaly if this happens it is my fuckup
                _ => panic!("Unexpected type_orig in read_bclang_list"),
            };

            let _attr = if has_attr {
                Some(self.read_item(refs)?)
            } else {
                None
            };
            let tag = self.read_item(refs)?;

            let tag = match &tag.kind {
                SexpKind::Sym(sym) => Ok(Some(sym)),
                SexpKind::Nil => Ok(None),
                _ => Err(RDSReaderError::DataError("Tag must be sym".into())),
            }?;

            let type_val = self.read_int()? as u8;
            let car = self.read_bclang(type_val, refs, reps)?;
            let car: data::TaggedSexp = if let Some(tag) = tag {
                data::TaggedSexp::new_with_tag(car, tag)
            } else {
                data::TaggedSexp::new(car)
            };

            res.push(car);
            let type_val = self.read_int()? as u8;
            match type_val {
                sexptype::LISTSXP | sexptype::ATTRLISTSXP => continue,
                sexptype::BCREPREF => {
                    let _ = reps[self.read_int()? as usize];
                    todo!()
                }
                _ => {
                    self.read_bclang(type_val, refs, reps)?;
                    break;
                }
            }
        }
        let data = self.arena.alloc_slice_clone(res.as_slice());
        Ok(self.arena.alloc(SexpKind::List(data::List { data }).into()))
    }

    fn read_bclang_inner(
        &self,
        type_val: u8,
        refs: &RefsTableReader<'a>,
        reps: &mut Vec<&'a Sexp<'a>>,
    ) -> Result<&'a Sexp<'a>, RDSReaderError> {
        match type_val {
            sexptype::LANGSXP | sexptype::ATTRLANGSXP => {
                self.read_bclang_langsxp(type_val, refs, reps)
            }
            sexptype::LISTSXP | sexptype::ATTRLISTSXP => {
                self.read_bclang_list(type_val, refs, reps)
            }
            _ => panic!("Expected either lang or list"),
        }
    }

    fn read_promsxp(
        &self,
        flags: Flag,
        refs: &RefsTableReader<'a>,
    ) -> Result<&'a Sexp<'a>, RDSReaderError> {
        let _attr = if flags.has_attributes {
            Some(self.read_item(refs)?)
        } else {
            None
        };
        let environment = if flags.has_tag {
            let flag = self.read_flags()?;
            if !matches!(
                flag.sexp_type,
                sexptype::EMPTYENV_SXP
                    | sexptype::ENVSXP
                    | sexptype::BASEENV_SXP
                    | sexptype::BASENAMESPACE_SXP
                    | sexptype::GLOBALENV_SXP
            ) {
                return Err(RDSReaderError::DataError("Expected environment".into()));
            }
            let env = self.read_enviroment(refs, flag)?;
            env
        } else {
            self.arena.get_empty()
        };

        let value = self.read_item(refs)?;
        let expr = self.read_item(refs)?;
        let result = SexpKind::Promise {
            environment,
            value,
            expr,
        };
        Ok(self.arena.alloc(result.into()))
    }
}

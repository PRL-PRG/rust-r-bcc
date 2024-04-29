use std::io::BufReader;

use std::io::Read;

use crate::rds::RDSHeader;
use crate::sexp::bc::Bc;
use crate::sexp::sexp::data;
use crate::sexp::sexp::lang;
use crate::sexp::sexp::Sexp;
use crate::sexp::sexp::SexpKind;

use super::sexptype;
use super::Flag;
use super::RDSResult;
use super::RefsTable;

#[derive(Debug)]
pub enum RDSReaderError {
    DataError(String),
    WrongFlag(i32),
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

impl TryInto<lang::HashFrame> for Sexp {
    type Error = RDSReaderError;

    fn try_into(self) -> Result<lang::HashFrame, Self::Error> {
        match self.kind {
            SexpKind::Vec(vec) => Ok(lang::HashFrame::new(vec)),
            //SexpKind::List(list) => Ok(lang::HashFrame::new(vec![SexpKind::List(list).into()])),
            SexpKind::Nil => Ok(lang::HashFrame::default()),
            data => Err(RDSReaderError::DataError(format!(
                "Hash frame must be vector got {data}"
            ))),
        }
    }
}

impl TryInto<lang::ListFrame> for Sexp {
    type Error = RDSReaderError;

    fn try_into(self) -> Result<lang::ListFrame, Self::Error> {
        match self.kind {
            SexpKind::List(list) => Ok(lang::ListFrame::new(list)),
            SexpKind::Nil => Ok(lang::ListFrame::default()),
            _ => Err(RDSReaderError::DataError("List frame must be list".into())),
        }
    }
}

pub trait RDSReader: Read {
    fn read_byte(&mut self) -> Result<u8, RDSReaderError> {
        let mut buf: [u8; 1] = [0];
        self.read(&mut buf)?;
        Ok(buf[0])
    }

    fn read_int(&mut self) -> Result<i32, RDSReaderError> {
        let mut buf: [u8; 4] = [0; 4];
        if self.read_exact(&mut buf).is_err() {
            return Err(RDSReaderError::DataError("Cannot read int".to_string()));
        }
        Ok(i32::from_be_bytes(buf))
    }

    fn read_complex(&mut self) -> Result<data::Complex, RDSReaderError> {
        let real = self.read_double()?;
        let imaginary = self.read_double()?;
        Ok(data::Complex { real, imaginary })
    }

    fn read_double(&mut self) -> Result<f64, RDSReaderError> {
        let mut buf: [u8; 8] = [0; 8];
        if self.read_exact(&mut buf).is_err() {
            return Err(RDSReaderError::DataError("Cannot read double".to_string()));
        }
        Ok(f64::from_be_bytes(buf))
    }

    fn read_string_len(&mut self, len: i32) -> Result<String, RDSReaderError> {
        let len = len as usize;
        let mut buf: Vec<u8> = vec![];
        buf.resize(len, 0);
        let read_len = self.read(buf.as_mut_slice())?;
        if len != read_len {
            return Err(RDSReaderError::DataError(format!(
                "Cannot read string of len {len}"
            )));
        }
        Ok(String::from_utf8(buf)?)
    }

    fn read_rds(&mut self) -> Result<RDSResult, RDSReaderError> {
        let header = self.read_header()?;
        let mut refs = RefsTable::default();
        let item = self.read_item(&mut refs)?;

        Ok(RDSResult::new(header, item))
    }

    fn read_header(&mut self) -> Result<RDSHeader, RDSReaderError> {
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

    fn read_item(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let flag = self.read_flags()?;
        self.read_item_flags(refs, flag)
    }

    fn lookup_class(&self, class_sym: &str, package_sym: &str) -> Result<Sexp, RDSReaderError> {
        todo!()
    }

    fn read_item_flags(
        &mut self,
        refs: &mut RefsTable,
        flag: Flag,
    ) -> Result<Sexp, RDSReaderError> {
        let mut sexp: Sexp = match flag.sexp_type {
            sexptype::NILVALUE_SXP | sexptype::NILSXP => SexpKind::Nil.into(),
            sexptype::REALSXP => self.read_realsxp()?,
            sexptype::INTSXP => self.read_intsxp()?,
            sexptype::LISTSXP => self.read_listsxp(refs, flag)?,
            sexptype::VECSXP => self.read_vecsxp(refs)?,
            sexptype::SYMSXP => self.read_symsxp(refs)?,
            sexptype::STRSXP => self.read_strsxp(refs)?,
            sexptype::CHARSXP => self.read_charsxp()?,
            sexptype::LANGSXP => self.read_langsxp(refs, flag)?,
            sexptype::CLOSXP => self.read_closxp(refs, flag)?,
            sexptype::ENVSXP => self.read_envsxp(refs)?,
            sexptype::GLOBALENV_SXP => {
                let tmp: SexpKind = lang::Environment::Global.into();
                tmp.into()
            }
            sexptype::EMPTYENV_SXP => {
                let tmp: SexpKind = lang::Environment::Empty.into();
                tmp.into()
            }
            sexptype::BASEENV_SXP | sexptype::BASENAMESPACE_SXP => {
                let tmp: SexpKind = lang::Environment::Base.into();
                tmp.into()
            }
            sexptype::MISSINGARG_SXP => SexpKind::MissingArg.into(),
            sexptype::REFSXP => self.read_refsxp(refs, flag)?,
            sexptype::BCODESXP => self.read_bc(refs)?,
            sexptype::LGLSXP => self.read_lglsxp()?,
            sexptype::BUILTINSXP | sexptype::SPECIALSXP => {
                let len = self.read_int()?;
                let name = self.read_string_len(len)?;
                SexpKind::Buildin(name.as_str().into()).into()
            }
            sexptype::ALTREP_SXP => {
                let info = self.read_item(refs)?;
                let state = self.read_item(refs)?;
                let attr = self.read_item(refs)?;
                let info = match info.kind {
                    SexpKind::List(list) if list.len() == 3 => list,
                    _ => {
                        return Err(RDSReaderError::DataError(
                            "Wrong info in ALTREP sexp".into(),
                        ))
                    }
                };
                let class_sym = &info[0];
                let package_sym = &info[1];
                let SexpKind::Int(class_type) = &info[2].data.kind else {
                    return Err(RDSReaderError::DataError(
                        "Wrong class type in info for ALTREP".into(),
                    ));
                };
                let class_type = if class_type.len() == 1 {
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
            sexptype::NAMESPACESXP => {
                let _ = self.read_int()?;
                let len = self.read_int()?;
                let mut res = vec![];
                for _ in 0..len {
                    res.push(self.read_item(refs)?);
                }
                let res = lang::Environment::Namespace(res);
                let res = res.into();
                refs.add_ref(&res);
                res
            }
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

    fn read_flags(&mut self) -> Result<Flag, RDSReaderError> {
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

    fn read_len(&mut self) -> Result<usize, RDSReaderError> {
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

    fn read_realsxp(&mut self) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_double()?)
        }

        Ok(SexpKind::Real(data).into())
    }

    fn read_intsxp(&mut self) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_int()?)
        }

        Ok(SexpKind::Int(data).into())
    }

    fn read_lglsxp(&mut self) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_int()?.into())
        }

        Ok(SexpKind::Logic(data).into())
    }

    fn read_cplsxp(&mut self) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;
        let mut data = vec![];
        data.reserve(len);
        for _ in 0..len {
            data.push(self.read_complex()?);
        }
        Ok(SexpKind::Complex(data).into())
    }

    fn read_vecsxp(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_item(refs)?)
        }

        Ok(SexpKind::Vec(data).into())
    }

    fn read_listsxp(&mut self, refs: &mut RefsTable, flags: Flag) -> Result<Sexp, RDSReaderError> {
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

            let mut value = self.read_item(refs)?;
            if let Some(attr) = attr {
                value.set_attr(attr);
            }

            if let Some(Sexp {
                kind: SexpKind::Sym(tag),
                ..
            }) = tag
            {
                data.push(data::TaggedSexp::new_with_tag(value.into(), tag.data));
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
                return Ok(SexpKind::List(data).into());
            }
        }
    }

    fn read_symsxp(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let _ = self.read_flags()?;
        let print_name = self.read_charsxp()?;
        if let SexpKind::Char(chars) = print_name.kind {
            let res: Sexp = SexpKind::Sym(lang::Sym::new(String::from_utf8(
                chars.iter().map(|x| *x as u8).collect(),
            )?))
            .into();

            refs.add_ref(&res);
            return Ok(res);
        }
        Err(RDSReaderError::DataError(
            "Symsxp must be created from charsxp".into(),
        ))
    }

    fn read_charsxp(&mut self) -> Result<Sexp, RDSReaderError> {
        let len = self.read_int()? as usize;
        if len == usize::MAX {
            return Ok(SexpKind::NAString.into());
        }

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_byte()? as char);
        }

        Ok(SexpKind::Char(data).into())
    }

    fn read_strsxp(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;
        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            let item = self.read_item(refs)?;
            if let Sexp {
                kind: SexpKind::Char(chars),
                ..
            } = item
            {
                data.push(String::from_utf8(chars.iter().map(|x| *x as u8).collect())?);
            } else if item.kind == SexpKind::NAString {
                // TODO ask Filip
            } else {
                return Err(RDSReaderError::DataError(
                    "Strsxp must be created from charsxp".into(),
                ));
            }
        }

        Ok(SexpKind::Str(data).into())
    }

    fn read_langsxp(&mut self, refs: &mut RefsTable, flag: Flag) -> Result<Sexp, RDSReaderError> {
        let attr = if flag.has_attributes {
            Some(self.read_item(refs)?)
        } else {
            None
        };

        if flag.has_tag {
            todo!()
        }
        let target = self.read_item(refs)?;

        let args = self.read_item(refs)?;

        let args = match args.kind {
            SexpKind::List(list) => list,
            SexpKind::Nil => vec![],
            x => {
                return Err(RDSReaderError::DataError(format!(
                    "Args need to be list got {x:?}"
                )))
            }
        };

        let target = match target.kind {
            SexpKind::Sym(sym) => lang::Target::Sym(sym),
            SexpKind::Lang(lang) => lang::Target::Lang(Box::new(lang)),
            _ => {
                return Err(RDSReaderError::DataError(format!(
                    "Target needs to be either symbol or lang got {target}"
                )))
            }
        };
        let mut res: Sexp = SexpKind::Lang(lang::Lang::new(target, args)).into();
        if let Some(attr) = attr {
            res.set_attr(attr);
        }
        Ok(res)
    }

    fn read_closxp(&mut self, refs: &mut RefsTable, flags: Flag) -> Result<Sexp, RDSReaderError> {
        // order of the values : [attr], enviroment, formals, body
        let attr = if flags.has_attributes {
            Some(self.read_item(refs)?)
            //None
        } else {
            None
        };
        let environment = self.read_item(refs)?;
        let formals = self.read_item(refs)?;
        let body = self.read_item(refs)?;

        let environment = match environment.kind {
            SexpKind::Environment(environment) => environment,
            SexpKind::Nil => lang::Environment::Empty.into(),
            _ => unreachable!()
        };

        let mut res: Sexp = match formals.kind {
            SexpKind::List(formals) => {
                let formals: Result<Vec<_>, _> =
                    formals.into_iter().map(|x| x.try_into()).collect();
                Ok(SexpKind::Closure(lang::Closure::new(formals?, body, environment)).into())
            }
            SexpKind::Nil => {
                Ok(SexpKind::Closure(lang::Closure::new(vec![], body, environment)).into())
            }
            formals => Err(RDSReaderError::DataError(format!(
                "Wrong format of the closure : {environment}, {formals}"
            ))),
        }?;

        if let Some(attr) = attr {
            res.set_attr(attr)
        }

        Ok(res)
    }

    fn read_envsxp(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let locked = self.read_int()?;

        // just register ref so my indexes are correct
        let index = refs.add_placeholder();

        let parent = self.read_item(refs)?;
        let frame = self.read_item(refs)?;
        let hashtab = self.read_item(refs)?;
        let attr = self.read_item(refs)?;

        let res = match parent.kind {
            SexpKind::Environment(env) => {
                let env = lang::NormalEnv::new(
                    Box::new(env),
                    locked == 1,
                    frame.try_into()?,
                    hashtab.try_into()?,
                );
                let env: lang::Environment = env.into();
                let env: SexpKind = env.into();
                let mut env: Sexp = env.into();
                env.set_attr(attr);
                if !refs.update_ref(index, env.clone()) {
                    return Err(RDSReaderError::DataError(format!(
                        "Wrong ref index {index}"
                    )));
                }
                Ok(env)
            }
            SexpKind::Nil => {
                let env = lang::NormalEnv::new(
                    Box::new(lang::Environment::Empty),
                    locked == 1,
                    frame.try_into()?,
                    hashtab.try_into()?,
                );
                let env: lang::Environment = env.into();
                let env: SexpKind = env.into();
                let mut env: Sexp = env.into();
                env.set_attr(attr);
                if !refs.update_ref(index, env.clone()) {
                    return Err(RDSReaderError::DataError(format!(
                        "Wrong ref index {index}"
                    )));
                }
                Ok(env)
            }
            _ => Err(RDSReaderError::DataError(format!(
                "Parent of environment must be an environment got {parent}"
            ))),
        }?;
        Ok(res)
    }

    fn read_refsxp(&mut self, refs: &mut RefsTable, flags: Flag) -> Result<Sexp, RDSReaderError> {
        let index = flags.orig >> 8;
        let index = if index == 0 { self.read_int()? } else { index } - 1;

        refs.get_ref(index).map_or(
            Err(RDSReaderError::DataError(format!(
                "Wrong ref index {index}"
            ))),
            |x| Ok(x),
        )
    }

    fn read_bc(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let reps = self.read_int()?;
        let mut reps_vec = vec![];
        reps_vec.resize(reps as usize, SexpKind::Nil.into());
        self.read_bc_inner(refs, &mut reps_vec)
    }

    fn read_bc_inner(
        &mut self,
        refs: &mut RefsTable,
        reps: &mut Vec<Sexp>,
    ) -> Result<Sexp, RDSReaderError> {
        let code = self.read_item(refs)?;
        let consts = self.read_bcconsts(refs, reps)?;
        match code.kind {
            SexpKind::Int(ints) => {
                let bc = Bc::new_init(ints, consts);
                Ok(SexpKind::Bc(bc).into())
            }
            _ => todo!(),
        }
    }

    fn read_bcconsts(
        &mut self,
        refs: &mut RefsTable,
        reps: &mut Vec<Sexp>,
    ) -> Result<Vec<Sexp>, RDSReaderError> {
        let n = self.read_int()?;
        let mut res = vec![];
        for _ in 0..n {
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
            res.push(tmp)
        }
        Ok(res)
    }

    fn read_bclang(
        &mut self,
        type_val: u8,
        refs: &mut RefsTable,
        reps: &mut Vec<Sexp>,
    ) -> Result<Sexp, RDSReaderError> {
        match type_val {
            sexptype::BCREPREF => Ok(reps[self.read_int()? as usize].clone()),
            sexptype::BCREPDEF => {
                let pos = self.read_int()?;
                let type_val = self.read_int()?;
                let res = self.read_bclang_inner(type_val as u8, refs, reps)?;

                if pos >= 0 {
                    reps[pos as usize] = res.clone();
                }

                Ok(res)
            }
            sexptype::LANGSXP
            | sexptype::LISTSXP
            | sexptype::ATTRLANGSXP
            | sexptype::ATTRLISTSXP => self.read_bclang_inner(type_val, refs, reps),
            _ => self.read_item(refs),
        }
    }

    fn read_bclang_inner(
        &mut self,
        type_val: u8,
        refs: &mut RefsTable,
        reps: &mut Vec<Sexp>,
    ) -> Result<Sexp, RDSReaderError> {
        let (type_orig, has_attr) = match type_val {
            sexptype::ATTRLANGSXP => (sexptype::LANGSXP, true),
            sexptype::ATTRLISTSXP => (sexptype::LISTSXP, true),
            _ => (type_val, false),
        };

        let attr = if has_attr {
            Some(self.read_item(refs)?)
        } else {
            None
        };
        let tag = self.read_item(refs)?;

        let tag = match tag.kind {
            SexpKind::Sym(sym) => Ok(Some(sym.data)),
            SexpKind::Nil => Ok(None),
            _ => Err(RDSReaderError::DataError("Tag must be sym".into())),
        }?;

        let type_val = self.read_int()? as u8;
        let car = self.read_bclang(type_val, refs, reps)?;

        let type_val = self.read_int()? as u8;
        let cdr = self.read_bclang(type_val, refs, reps)?;

        let car: data::TaggedSexp = if let Some(tag) = tag {
            data::TaggedSexp::new_with_tag(car, tag)
        } else {
            car.into()
        };

        let mut cdr = match cdr.kind {
            SexpKind::Nil => Ok(vec![]),
            SexpKind::List(cdr) => Ok(cdr),
            _ => Err(RDSReaderError::DataError(format!(
                "Wrong cdr type in bc lang read {type_val}"
            ))),
        }?;

        match type_orig {
            sexptype::LANGSXP => {
                let target: lang::Target = match car.data.kind {
                    SexpKind::Sym(sym) => Ok(sym.into()),
                    SexpKind::Lang(lang) => Ok(lang.into()),
                    _ => Err(RDSReaderError::DataError("Wrong lang target".into())),
                }?;

                Ok(lang::Lang::new(target, cdr).into())
            }
            sexptype::LISTSXP => {
                cdr.insert(0, car);
                Ok(SexpKind::List(cdr).into())
            }
            _ => Err(RDSReaderError::DataError(format!(
                "Wrong sexp type in bc lang read {type_val}"
            ))),
        }
    }

    fn read_promsxp(&mut self, flags: Flag, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let attr = if flags.has_attributes {
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
            let super::SexpKind::Environment(env) = self.read_envsxp(refs)?.kind else {
                unreachable!()
            };
            env
        } else {
            lang::Environment::Empty
        };

        let value = self.read_item(refs)?;
        let expr = self.read_item(refs)?;
        let result = SexpKind::Promise {
            environment,
            value: Box::new(value),
            expr: Box::new(expr),
        };
        Ok(result.into())
    }
}

impl<T> RDSReader for BufReader<T> where T: Sized + std::io::Read {}

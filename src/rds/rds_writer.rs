use std::io::{BufWriter, Write};

use crate::sexp::{
    bc::Bc,
    sexp::{data, lang, MetaData, Sexp, SexpKind},
};

use super::{sexptype, Flag, RDSHeader, RefsTable};

#[derive(Debug)]
pub enum RDSWriterError {
    DataError(String),
    IO(std::io::Error),
}

impl From<std::io::Error> for RDSWriterError {
    fn from(value: std::io::Error) -> Self {
        RDSWriterError::IO(value)
    }
}

type Ret = Result<(), RDSWriterError>;

pub trait RDSWriter: Write {
    fn write_byte(&mut self, byte: u8) -> Ret {
        let len = self.write(&[byte])?;
        if len != 1 {
            return Err(RDSWriterError::DataError("Cannot write byte".into()));
        }
        Ok(())
    }

    fn write_int(&mut self, value: i32) -> Ret {
        let data = i32::to_be_bytes(value);
        let len = self.write(&data)?;
        if len != 4 {
            return Err(RDSWriterError::DataError("Cannot write int".into()));
        }
        Ok(())
    }

    fn write_double(&mut self, value: f64) -> Ret {
        let data = f64::to_be_bytes(value);
        let len = self.write(&data)?;
        if len != 8 {
            return Err(RDSWriterError::DataError("Cannot write double".into()));
        }
        Ok(())
    }

    fn write_str(&mut self, value: &str) -> Ret {
        self.write_int(value.len() as i32)?;
        for val in value.bytes() {
            self.write_byte(val)?;
        }
        Ok(())
    }

    fn write_header(&mut self, header: RDSHeader) -> Ret {
        self.write_byte(b'X')?;
        self.write_byte(b'\n')?;
        self.write_int(header.format_version)?;

        self.write_int(header.writer_version)?;

        self.write_int(header.min_reader_version)?;
        if header.format_version == 3 {
            self.write_str("UTF-8")?;
        }
        Ok(())
    }

    fn write_flags(&mut self, flags: Flag) -> Ret {
        let mut res: i32 = flags.sexp_type as i32;
        res |= flags.level << 12;
        if flags.has_attributes {
            res |= 1 << 9;
        }

        if flags.has_tag {
            res |= 1 << 10;
        }

        if flags.obj {
            res |= 1 << 8;
        }

        self.write_int(res)
    }

    fn write_len(&mut self, len: usize) -> Ret {
        // check if len is long vec length
        if len > 2147483647 {
            self.write_int(-1)?;
            self.write_int((len / 4294967296) as i32)?;
            self.write_int((len % 4294967296) as i32)
        } else {
            self.write_int(len as i32)
        }
    }

    fn write_intvec(&mut self, ints: &Vec<i32>) -> Ret {
        self.write_len(ints.len())?;

        for val in ints {
            self.write_int(*val)?;
        }
        Ok(())
    }

    fn write_rds(&mut self, header: RDSHeader, sexp: Sexp) -> Ret {
        self.write_header(header)?;
        self.write_item(&sexp, &mut RefsTable::default())?;
        Ok(())
    }

    fn write_item(&mut self, sexp: &Sexp, refs: &mut RefsTable) -> Ret {
        // when creating refs there are different rules for creating
        // flags so it must be done seperatelly
        if let Some(idx) = refs.find(sexp) {
            self.write_int(((idx + 1) << 8) | super::sexptype::REFSXP as i32)?;
            return Ok(());
        }

        let flag: Flag = sexp.into();
        if flag.sexp_type != super::sexptype::LISTSXP {
            self.write_flags(flag)?;
        }

        match &sexp.kind {
            SexpKind::Sym(sym) => {
                refs.add_ref(sexp);
                self.write_int(
                    super::string_format::ASCII << 12 | super::sexptype::CHARSXP as i32,
                )?;
                self.write_charsxp(sym.data.as_str())
            }
            SexpKind::List(taggedlist) => {
                self.write_listsxp(taggedlist, &sexp.metadata, flag, refs)
            }
            SexpKind::Nil => Ok(()),
            SexpKind::Closure(closure) => self.write_closxp(closure, &sexp.metadata, refs),
            SexpKind::Environment(lang::Environment::Normal(env)) => {
                refs.add_ref(sexp);
                self.write_envsxp(env, &sexp.metadata, refs)
            }
            SexpKind::Environment(_) => Ok(()),
            SexpKind::Promise => todo!(),
            SexpKind::Lang(lang) => self.write_langsxp(lang, &sexp.metadata, refs),
            SexpKind::Bc(bc) => self.write_bc(bc, refs),
            SexpKind::Char(chars) => {
                // lenght of the char vector is limited
                self.write_int(chars.len() as i32)?;

                for val in chars {
                    self.write_byte(*val as u8)?;
                }

                Ok(())
            }
            SexpKind::Logic(logics) => {
                self.write_len(logics.len())?;

                for val in logics {
                    self.write_int(if !val { 0 } else { 1 })?;
                }
                Ok(())
            }
            SexpKind::Real(reals) => {
                self.write_len(reals.len())?;

                for val in reals {
                    self.write_double(*val)?;
                }
                Ok(())
            }
            SexpKind::Int(ints) => self.write_intvec(ints),
            SexpKind::Complex(_) => todo!(),
            SexpKind::Str(strs) => {
                self.write_len(strs.len())?;
                for s in strs {
                    self.write_int(
                        super::string_format::ASCII << 12 | super::sexptype::CHARSXP as i32,
                    )?;
                    self.write_charsxp(s.as_str())?;
                }
                Ok(())
            }
            SexpKind::Vec(items) => self.write_vecsxp(items, refs),
            SexpKind::MissingArg => Ok(()),
        }?;
        if flag.has_attributes
            && flag.sexp_type != super::sexptype::REFSXP
            && flag.sexp_type != super::sexptype::ENVSXP
            && flag.sexp_type != super::sexptype::CLOSXP
        {
            let Some(attr) = sexp.metadata.attr.clone() else {
                unreachable!()
            };
            self.write_item(&attr, refs)?;
        }
        Ok(())
    }

    fn write_charsxp(&mut self, data: &str) -> Ret {
        self.write_int(data.len() as i32)?;
        for b in data.bytes() {
            self.write_byte(b)?;
        }
        Ok(())
    }

    fn write_bc(&mut self, bc: &Bc, refs: &mut RefsTable) -> Ret {
        let mut reps_vec = create_reps(bc, refs);
        let reps = reps_vec.len() as i32;
        let mut reps_visit = vec![];
        reps_visit.resize(reps as usize, false);

        self.write_int(reps)?;
        self.write_bc_inner(bc, refs, &mut reps_vec, &mut reps_visit)
    }

    fn write_bc_inner(
        &mut self,
        bc: &Bc,
        refs: &mut RefsTable,
        reps: &Vec<Sexp>,
        reps_visit: &mut Vec<bool>,
    ) -> Ret {
        self.write_int(super::sexptype::INTSXP as i32)?;
        self.write_intvec(&bc.instructions)?;
        self.write_bcconsts(&bc.constpool, refs, reps, reps_visit)
    }

    fn write_bcconsts(
        &mut self,
        consts: &Vec<Sexp>,
        refs: &mut RefsTable,
        reps: &Vec<Sexp>,
        reps_visit: &mut Vec<bool>,
    ) -> Ret {
        self.write_int(consts.len() as i32)?;
        for c in consts {
            let flags: Flag = c.into();
            match &c.kind {
                SexpKind::Nil => {
                    self.write_int(sexptype::NILSXP as i32)?;
                    self.write_item(c, refs)?;
                }
                SexpKind::Bc(bc) => {
                    self.write_int(sexptype::BCODESXP as i32)?;
                    self.write_bc_inner(bc, refs, reps, reps_visit)?
                }
                SexpKind::Lang(lang) => {
                    self.write_bclang(lang, &c.metadata, refs, reps, reps_visit)?
                }
                SexpKind::List(list) => {
                    self.write_bclist(list, &c.metadata, refs, reps, reps_visit)?
                }
                _ => {
                    self.write_int(flags.sexp_type as i32)?;
                    self.write_item(c, refs)?;
                }
            }
        }
        Ok(())
    }

    fn write_bclang(
        &mut self,
        lang: &lang::Lang,
        metadata: &MetaData,
        refs: &mut RefsTable,
        reps: &Vec<Sexp>,
        reps_visit: &mut Vec<bool>,
    ) -> Ret {
        if let Some(pos) = reps
            .iter()
            .position(|x| x == &SexpKind::Lang(lang.clone()).into())
        {
            if reps_visit[pos] {
                self.write_int(sexptype::BCREPREF as i32)?;
                self.write_int(pos as i32 - 1)?;
                return Ok(());
            }

            self.write_int(sexptype::BCREPDEF as i32)?;
            self.write_int(pos as i32 - 1)?;
            reps_visit[pos] = true;
        }

        let type_val = if metadata.attr.is_some() {
            sexptype::ATTRLANGSXP
        } else {
            sexptype::LANGSXP
        } as i32;

        self.write_int(type_val)?;

        if let Some(attr) = metadata.attr.as_ref() {
            self.write_item(&attr, refs)?;
        }

        // write tag as nil since it does not ever exist here
        self.write_int(sexptype::NILVALUE_SXP as i32)?;

        match &lang.target {
            lang::Target::Lang(lang) => {
                self.write_bclang(lang.as_ref(), &MetaData::default(), refs, reps, reps_visit)?;
            }
            lang::Target::Sym(sym) => {
                // padding dont ask why
                self.write_int(0)?;
                self.write_item(&sym.clone().into(), refs)?;
            }
        }

        self.write_bclist(&lang.args, &MetaData::default(), refs, reps, reps_visit)?;

        Ok(())
    }

    fn write_bclist(
        &mut self,
        list: &data::List,
        metadata: &MetaData,
        refs: &mut RefsTable,
        reps: &Vec<Sexp>,
        reps_visit: &mut Vec<bool>,
    ) -> Ret {
        // special case for empty list
        // it must behave as Nil
        if list.is_empty() {
            // padding
            self.write_int(0)?;
            self.write_int(sexptype::NILVALUE_SXP as i32)?;
            return Ok(());
        }

        if let Some(pos) = reps
            .iter()
            .position(|x| x == &SexpKind::List(list.clone()).into())
        {
            if reps_visit[pos] {
                self.write_int(sexptype::BCREPREF as i32)?;
                self.write_int(pos as i32)?;
                return Ok(());
            }

            self.write_int(sexptype::BCREPDEF as i32)?;
            self.write_int(pos as i32)?;
            reps_visit[pos] = true;
        }

        let type_val = if metadata.attr.is_some() {
            sexptype::ATTRLISTSXP
        } else {
            sexptype::LISTSXP
        } as i32;

        self.write_int(type_val)?;

        if let Some(attr) = metadata.attr.as_ref() {
            self.write_item(&attr, refs)?;
        }

        let mut first: bool = true;

        for item in list {
            if first {
                first = false;
            } else {
                self.write_int(sexptype::LISTSXP as i32)?;
            }
            if let Some(tag) = &item.tag {
                let tag: lang::Sym = tag.as_str().into();
                let tag: Sexp = tag.into();
                self.write_item(&tag, refs)?;
            } else {
                self.write_int(sexptype::NILVALUE_SXP as i32)?;
            }

            match &item.data.kind {
                SexpKind::Lang(lang) => {
                    self.write_bclang(lang, &item.data.metadata, refs, reps, reps_visit)?;
                }
                _ => {
                    self.write_int(0)?;
                    self.write_item(&item.data, refs)?;
                }
            }
        }

        // last element of list is nil with padding
        self.write_int(0)?;
        self.write_int(sexptype::NILVALUE_SXP as i32)
    }

    fn write_vecsxp(&mut self, items: &Vec<Sexp>, refs: &mut RefsTable) -> Ret {
        self.write_len(items.len())?;

        for item in items {
            self.write_item(item, refs)?;
        }
        Ok(())
    }

    fn write_listsxp(
        &mut self,
        list: &data::List,
        _metadata: &MetaData,
        flag: Flag,
        refs: &mut RefsTable,
    ) -> Ret {
        let mut flag = flag;
        if list.is_empty() {
            return self.write_int(super::sexptype::NILVALUE_SXP as i32);
        }

        for item in list {
            flag.has_tag = item.tag.is_some();
            self.write_flags(flag.clone())?;

            if let Some(tag) = &item.tag {
                let tag: lang::Sym = tag.as_str().into();
                let tag: Sexp = tag.into();
                self.write_item(&tag, refs)?;
            }

            flag = Flag {
                sexp_type: super::sexptype::LISTSXP,
                level: 0,
                has_attributes: false,
                has_tag: false,
                obj: false,
                orig: 0,
            };

            self.write_item(&item.data, refs)?;
        }
        self.write_int(super::sexptype::NILVALUE_SXP as i32)?;

        Ok(())
    }

    fn write_formals(&mut self, formals: &Vec<lang::Formal>, refs: &mut RefsTable) -> Ret {
        if formals.is_empty() {
            self.write_int(super::sexptype::NILVALUE_SXP as i32)
        } else {
            let vals: data::List = formals
                .into_iter()
                .map(|x| data::TaggedSexp::new_with_tag(*x.value.clone(), x.name.data.clone()))
                .collect();
            self.write_listsxp(
                &vals,
                &MetaData::default(),
                Flag {
                    sexp_type: super::sexptype::LISTSXP,
                    level: 0,
                    has_attributes: false,
                    has_tag: false,
                    obj: false,
                    orig: 0,
                },
                refs,
            )
        }
    }

    fn write_closxp(
        &mut self,
        closure: &lang::Closure,
        metadata: &MetaData,
        refs: &mut RefsTable,
    ) -> Ret {
        if let Some(attr) = &metadata.attr {
            self.write_item(&attr, refs)?;
        }
        self.write_item(&closure.environment.clone().into(), refs)?;
        self.write_formals(&closure.formals, refs)?;
        self.write_item(&closure.body, refs)?;

        Ok(())
    }

    fn write_langsxp(
        &mut self,
        lang: &lang::Lang,
        _metadata: &MetaData,
        refs: &mut RefsTable,
    ) -> Ret {
        let target: &lang::Target = &lang.target;
        self.write_item(&target.into(), refs)?;
        if lang.args.is_empty() {
            self.write_item(&SexpKind::Nil.into(), refs)
        } else {
            self.write_listsxp(
                &lang.args,
                &MetaData::default(),
                Flag {
                    sexp_type: super::sexptype::LISTSXP,
                    level: 0,
                    has_attributes: false,
                    has_tag: false,
                    obj: false,
                    orig: 0,
                },
                refs,
            )
        }
    }

    fn write_envsxp(
        &mut self,
        env: &lang::NormalEnv,
        metadata: &MetaData,
        refs: &mut RefsTable,
    ) -> Ret {
        self.write_int(if env.locked { 1 } else { 0 })?;

        match env.parent.as_ref() {
            lang::Environment::Global => self.write_int(super::sexptype::GLOBALENV_SXP as i32)?,
            lang::Environment::Base => self.write_int(super::sexptype::BASEENV_SXP as i32)?,
            lang::Environment::Empty => self.write_int(super::sexptype::EMPTYENV_SXP as i32)?,
            lang::Environment::Normal(env) => {
                self.write_item(&lang::Environment::Normal(env.clone()).into(), refs)?;
            }
        };

        let tmp_flag = Flag {
            sexp_type: super::sexptype::LISTSXP,
            level: 0,
            has_attributes: false,
            has_tag: false,
            obj: false,
            orig: 0,
        };

        match env.frame.data.as_ref() {
            Some(list) => self.write_listsxp(list, &MetaData::default(), tmp_flag, refs)?,
            None => self.write_int(super::sexptype::NILVALUE_SXP as i32)?,
        };

        match env.hash_frame.data.as_ref() {
            Some(v) => {
                self.write_int(super::sexptype::VECSXP as i32)?;
                self.write_vecsxp(v, refs)?;
            }
            None => self.write_int(super::sexptype::NILVALUE_SXP as i32)?,
        };

        match &metadata.attr {
            Some(attr) => self.write_item(attr.as_ref(), refs)?,
            None => self.write_int(super::sexptype::NILVALUE_SXP as i32)?,
        };

        Ok(())
    }
}

fn create_reps(bc: &Bc, refs: &mut RefsTable) -> Vec<Sexp> {
    let mut temp = vec![];
    let mut res = vec![SexpKind::Nil.into()];
    for c in &bc.constpool {
        create_reps_inner(c, refs, &mut res, &mut temp);
    }
    res
}

fn create_reps_inner(
    sexp: &Sexp,
    refs: &mut RefsTable,
    reps: &mut Vec<Sexp>,
    temp: &mut Vec<Sexp>,
) {
    if reps.iter().any(|x| x == sexp) {
        return;
    }

    match &sexp.kind {
        SexpKind::List(list) => {
            if temp.iter().any(|x| x == sexp) {
                reps.push(sexp.clone());
                return;
            }

            temp.push(sexp.clone());

            for item in list {
                create_reps_inner(&item.data, refs, reps, temp);
            }
        }
        SexpKind::Lang(lang) => {
            if temp.iter().any(|x| x == sexp) {
                reps.push(sexp.clone());
                return;
            }

            temp.push(sexp.clone());

            create_reps_inner(&lang.target.clone().into(), refs, reps, temp);
            create_reps_inner(&SexpKind::List(lang.args.clone()).into(), refs, reps, temp);
        }
        SexpKind::Bc(bc) => {
            for c in &bc.constpool {
                create_reps_inner(c, refs, reps, temp)
            }
        }
        _ => (),
    }
}

impl<W> RDSWriter for BufWriter<W> where W: std::io::Write {}

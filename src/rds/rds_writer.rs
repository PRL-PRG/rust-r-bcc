use std::io::{BufWriter, Write};

use crate::sexp::{
    bc::Bc,
    sexp::{data, lang, MetaData, Sexp, SexpKind},
    sexp_alloc::Alloc,
};

use super::{sexptype, Flag, RDSHeader, RefsTableWriter};

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

pub trait RDSWriter<'a>: Write {
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

    fn write_intvec(&mut self, ints: &[i32]) -> Ret {
        self.write_len(ints.len())?;

        for val in ints {
            self.write_int(*val)?;
        }
        Ok(())
    }

    fn write_rds(&mut self, header: RDSHeader, sexp: &'a Sexp<'a>, arena: &'a mut Alloc) -> Ret {
        self.write_header(header)?;
        self.write_item(sexp, &mut RefsTableWriter::new(arena))?;
        Ok(())
    }

    fn write_item(&mut self, sexp: &'a Sexp<'a>, refs: &mut RefsTableWriter) -> Ret {
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
                self.write_charsxp(sym.data)
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
            SexpKind::Promise {
                environment: _,
                expr: _,
                value: _,
            } => todo!(),
            SexpKind::Lang(lang) => self.write_langsxp(lang, &sexp.metadata, refs),
            SexpKind::Bc(bc) => self.write_bc(bc, refs),
            SexpKind::Char(chars) => {
                // lenght of the char vector is limited
                self.write_int(chars.len() as i32)?;

                for val in chars.iter() {
                    self.write_byte(*val as u8)?;
                }

                Ok(())
            }
            SexpKind::Logic(logics) => {
                self.write_len(logics.len())?;

                for val in logics.iter() {
                    self.write_int(val.into())?;
                }
                Ok(())
            }
            SexpKind::Real(reals) => {
                self.write_len(reals.len())?;

                for val in reals.iter() {
                    self.write_double(*val)?;
                }
                Ok(())
            }
            SexpKind::Int(ints) => self.write_intvec(ints),
            SexpKind::Complex(complexes) => {
                self.write_len(complexes.len())?;

                for val in complexes.iter() {
                    self.write_double(val.real)?;
                    self.write_double(val.imaginary)?;
                }
                Ok(())
            }
            SexpKind::Str(strs) => {
                self.write_len(strs.len())?;
                for s in strs.iter() {
                    self.write_int(
                        super::string_format::ASCII << 12 | super::sexptype::CHARSXP as i32,
                    )?;
                    self.write_charsxp(s)?;
                }
                Ok(())
            }
            SexpKind::Vec(items) => self.write_vecsxp(items, refs),
            SexpKind::MissingArg => Ok(()),
            SexpKind::BaseNamespace => Ok(()),
            SexpKind::Buildin(sym) => {
                self.write_int(
                    super::string_format::ASCII << 12 | super::sexptype::CHARSXP as i32,
                )?;
                self.write_charsxp(sym.data)
            }
            SexpKind::NAString => self.write_int(-1),
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

    fn write_sym_inner(&mut self, sym: &'a lang::Sym<'a>, refs: &mut RefsTableWriter) -> Ret {
        if let Some(idx) = refs.find_sym(sym) {
            self.write_int(((idx + 1) << 8) | super::sexptype::REFSXP as i32)?;
            return Ok(());
        }
        self.write_int(sexptype::SYMSXP as i32)?;
        refs.add_sym(sym);
        self.write_int(super::string_format::ASCII << 12 | super::sexptype::CHARSXP as i32)?;
        self.write_charsxp(sym.data)
    }

    fn write_bc(&mut self, bc: &Bc, refs: &mut RefsTableWriter) -> Ret {
        let mut reps_vec = create_reps(bc, refs);
        let reps = reps_vec.len() as i32;
        let mut reps_visit = vec![];
        reps_visit.resize(reps as usize, None);

        self.write_int(reps)?;
        let mut reps_count = 0;
        self.write_bc_inner(bc, refs, &mut reps_vec, &mut reps_count, &mut reps_visit)
    }

    fn write_bc_inner(
        &mut self,
        bc: &Bc,
        refs: &mut RefsTableWriter,
        reps: &Vec<RepsItem<'a>>,
        reps_count: &mut i32,
        reps_visit: &mut Vec<Option<i32>>,
    ) -> Ret {
        self.write_int(super::sexptype::INTSXP as i32)?;
        self.write_intvec(&bc.instructions)?;
        self.write_bcconsts(bc.constpool, refs, reps, reps_count, reps_visit)
    }

    fn write_bcconsts(
        &mut self,
        consts: &[&'a Sexp<'a>],
        refs: &mut RefsTableWriter,
        reps: &Vec<RepsItem<'a>>,
        reps_count: &mut i32,
        reps_visit: &mut Vec<Option<i32>>,
    ) -> Ret {
        self.write_int(consts.len() as i32)?;
        for c in consts {
            let flags: Flag = (*c).into();
            match &c.kind {
                SexpKind::Nil => {
                    self.write_int(sexptype::NILSXP as i32)?;
                    self.write_item(c, refs)?;
                }
                SexpKind::Bc(bc) => {
                    self.write_int(sexptype::BCODESXP as i32)?;
                    self.write_bc_inner(bc, refs, reps, reps_count, reps_visit)?
                }
                SexpKind::Lang(lang) => {
                    self.write_bclang(lang, &c.metadata, refs, reps, reps_count, reps_visit)?
                }
                SexpKind::List(list) => {
                    self.write_bclist(list, &c.metadata, refs, reps, reps_count, reps_visit)?
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
        refs: &mut RefsTableWriter,
        reps: &Vec<RepsItem<'a>>,
        reps_count: &mut i32,
        reps_visit: &mut Vec<Option<i32>>,
    ) -> Ret {
        if let Some(pos) = reps
            .iter()
            .position(|x| x == lang)
        {
            if let Some(index) = reps_visit[pos] {
                self.write_int(sexptype::BCREPREF as i32)?;
                self.write_int(index)?;
                return Ok(());
            }

            self.write_int(sexptype::BCREPDEF as i32)?;
            self.write_int(reps_count.clone())?;
            reps_visit[pos] = Some(reps_count.clone());
            *reps_count += 1;
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
                self.write_bclang(
                    lang,
                    &MetaData::default(),
                    refs,
                    reps,
                    reps_count,
                    reps_visit,
                )?;
            }
            lang::Target::Sym(sym) => {
                // padding dont ask why
                self.write_int(0)?;
                self.write_sym_inner(&sym, refs)?;
            }
        }

        self.write_bclist(
            &lang.args,
            &MetaData::default(),
            refs,
            reps,
            reps_count,
            reps_visit,
        )?;

        Ok(())
    }

    fn write_bclist(
        &mut self,
        list: &data::List,
        metadata: &MetaData,
        refs: &mut RefsTableWriter,
        reps: &Vec<RepsItem<'a>>,
        reps_count: &mut i32,
        reps_visit: &mut Vec<Option<i32>>,
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
            .position(|x| x == list)
        {
            if let Some(index) = reps_visit[pos] {
                self.write_int(sexptype::BCREPREF as i32)?;
                self.write_int(index)?;
                return Ok(());
            }

            self.write_int(sexptype::BCREPDEF as i32)?;
            self.write_int(reps_count.clone())?;
            reps_visit[pos] = Some(reps_count.clone());
            *reps_count += 1;
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

        for item in list.into_iter() {
            if first {
                first = false;
            } else {
                self.write_int(sexptype::LISTSXP as i32)?;
            }
            if let Some(tag) = &item.tag {
                self.write_sym_inner(tag, refs)?;
            } else {
                self.write_int(sexptype::NILVALUE_SXP as i32)?;
            }

            match &item.data.kind {
                SexpKind::Lang(lang) => {
                    self.write_bclang(
                        &lang,
                        &item.data.metadata,
                        refs,
                        reps,
                        reps_count,
                        reps_visit,
                    )?;
                }
                SexpKind::List(list) => {
                    self.write_bclist(
                        &list,
                        &item.data.metadata,
                        refs,
                        reps,
                        reps_count,
                        reps_visit,
                    )?;
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

    fn write_vecsxp(&mut self, items: &'a [&'a Sexp<'a>], refs: &mut RefsTableWriter) -> Ret {
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
        refs: &mut RefsTableWriter,
    ) -> Ret {
        let mut flag = flag;
        if list.is_empty() {
            return self.write_int(super::sexptype::NILVALUE_SXP as i32);
        }

        for item in list.into_iter() {
            flag.has_tag = item.tag.is_some();
            self.write_flags(flag.clone())?;

            if let Some(tag) = &item.tag {
                self.write_sym_inner(tag, refs)?;
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

    fn write_formals(&mut self, formals: &'a [lang::Formal], refs: &mut RefsTableWriter) -> Ret {
        if formals.is_empty() {
            return self.write_int(super::sexptype::NILVALUE_SXP as i32);
        }
        let flag = Flag {
            sexp_type: super::sexptype::LISTSXP,
            level: 0,
            has_attributes: false,
            has_tag: true,
            obj: false,
            orig: 0,
        };
        for lang::Formal { name, value } in formals {
            self.write_flags(flag.clone())?;

            self.write_sym_inner(name, refs)?;
            self.write_item(value, refs)?;
        }

        self.write_int(super::sexptype::NILVALUE_SXP as i32)?;
        Ok(())
    }

    fn write_closxp(
        &mut self,
        closure: &lang::Closure,
        metadata: &MetaData,
        refs: &mut RefsTableWriter,
    ) -> Ret {
        if let Some(attr) = &metadata.attr {
            self.write_item(&attr, refs)?;
        }
        self.write_env_inner(&closure.environment, refs)?;
        self.write_formals(&closure.formals, refs)?;
        self.write_item(&closure.body, refs)?;

        Ok(())
    }

    fn write_target(&mut self, target: &'a lang::Target<'a>, refs: &mut RefsTableWriter) -> Ret {
        match target {
            lang::Target::Lang(lang) => {
                self.write_int(sexptype::LANGSXP as i32)?;
                self.write_langsxp(lang, &MetaData::default(), refs)
            }
            lang::Target::Sym(sym) => {
                if let Some(idx) = refs.find_sym(sym) {
                    self.write_int(((idx + 1) << 8) | super::sexptype::REFSXP as i32)?;
                    return Ok(());
                }
                self.write_int(sexptype::SYMSXP as i32)?;
                refs.add_sym(sym);
                self.write_int(
                    super::string_format::ASCII << 12 | super::sexptype::CHARSXP as i32,
                )?;
                self.write_charsxp(sym.data)
            }
        }
    }

    fn write_langsxp(
        &mut self,
        lang: &lang::Lang,
        _metadata: &MetaData,
        refs: &mut RefsTableWriter,
    ) -> Ret {
        self.write_target(&lang.target, refs)?;
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

    fn write_env_inner(
        &mut self,
        env: &'a lang::Environment<'a>,
        refs: &mut RefsTableWriter,
    ) -> Ret {
        match env {
            lang::Environment::Global => self.write_int(sexptype::GLOBALENV_SXP as i32),
            lang::Environment::Base => self.write_int(sexptype::BASEENV_SXP as i32),
            lang::Environment::Empty => self.write_int(sexptype::EMPTYENV_SXP as i32),
            lang::Environment::Normal(normal) => self.write_envsxp_rec(normal, refs),
            lang::Environment::Namespace(_) => todo!(),
        }
    }

    fn write_envsxp_rec(
        &mut self,
        env: &'a lang::NormalEnv<'a>,
        refs: &mut RefsTableWriter,
    ) -> Ret {
        if let Some(idx) = refs.find_normal_env(env) {
            self.write_int(((idx + 1) << 8) | super::sexptype::REFSXP as i32)?;
            return Ok(());
        }

        refs.add_normal_env(env);

        self.write_int(sexptype::ENVSXP as i32)?;
        self.write_envsxp(env, &MetaData::default(), refs)
    }

    fn write_envsxp(
        &mut self,
        env: &'a lang::NormalEnv<'a>,
        metadata: &MetaData,
        refs: &mut RefsTableWriter,
    ) -> Ret {
        self.write_int(if env.locked { 1 } else { 0 })?;

        match env.parent {
            lang::Environment::Global => self.write_int(super::sexptype::GLOBALENV_SXP as i32)?,
            lang::Environment::Base => self.write_int(super::sexptype::BASEENV_SXP as i32)?,
            lang::Environment::Empty => self.write_int(super::sexptype::EMPTYENV_SXP as i32)?,
            lang::Environment::Namespace(names) => {
                self.write_int(super::sexptype::NAMESPACESXP as i32)?;
                self.write_int(names.len() as i32)?;
                for name in names.into_iter() {
                    self.write_item(name, refs)?
                }
            }
            lang::Environment::Normal(env) => {
                self.write_envsxp_rec(env, refs)?;
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
            Some(attr) => self.write_item(attr, refs)?,
            None => self.write_int(super::sexptype::NILVALUE_SXP as i32)?,
        };

        Ok(())
    }
}

enum RepsItem<'a> {
    Placeholder,
    Lang(&'a lang::Lang<'a>),
    List(&'a data::List<'a>),
}

impl<'a> PartialEq<Sexp<'a>> for RepsItem<'a> {
    fn eq(&self, other: &Sexp<'a>) -> bool {
        match &other.kind {
            SexpKind::Lang(lang) => self == lang,
            SexpKind::List(list) => self == list,
            _ => false,
        }
    }
}

impl<'a> PartialEq<lang::Lang<'a>> for RepsItem<'a> {
    fn eq(&self, other: &lang::Lang<'a>) -> bool {
        match self {
            RepsItem::Placeholder | RepsItem::List(_) => false,
            RepsItem::Lang(lang) => std::ptr::eq(*lang, other) || *lang == other,
        }
    }
}

impl<'a> PartialEq<data::List<'a>> for RepsItem<'a> {
    fn eq(&self, other: &data::List<'a>) -> bool {
        match self {
            RepsItem::Placeholder | RepsItem::Lang(_) => false,
            RepsItem::List(list) => std::ptr::eq(*list, other) || *list == other,
        }
    }
}

fn create_reps<'a>(bc: &'a Bc<'a>, refs: &mut RefsTableWriter) -> Vec<RepsItem<'a>> {
    let mut temp = vec![];
    let mut res = vec![RepsItem::Placeholder];
    for c in bc.constpool.into_iter() {
        create_reps_inner(c, refs, &mut res, &mut temp);
    }
    res
}

fn create_reps_inner<'a>(
    sexp: &'a Sexp<'a>,
    refs: &mut RefsTableWriter,
    reps: &mut Vec<RepsItem<'a>>,
    temp: &mut Vec<RepsItem<'a>>,
) {
    if reps.iter().any(|x| x == sexp) {
        return;
    }

    match &sexp.kind {
        SexpKind::List(list) => {
            create_reps_inner_list(list, refs, reps, temp);
        }
        SexpKind::Lang(lang) => {
            if temp.iter().any(|x| x == sexp) {
                reps.push(RepsItem::Lang(lang));
                return;
            }

            temp.push(RepsItem::Lang(lang));

            create_reps_inner_target(&lang.target, refs, reps, temp);
            create_reps_inner_list(&lang.args, refs, reps, temp);
        }
        SexpKind::Bc(bc) => {
            for c in bc.constpool {
                create_reps_inner(c, refs, reps, temp)
            }
        }
        _ => (),
    }
}

fn create_reps_inner_target<'a>(
    target: &'a lang::Target,
    refs: &mut RefsTableWriter,
    reps: &mut Vec<RepsItem<'a>>,
    temp: &mut Vec<RepsItem<'a>>,
) {
    match target {
        lang::Target::Sym(_) => (),
        lang::Target::Lang(lang) => {
            if reps.iter().any(|x| x == *lang) {
                return;
            }
            if temp.iter().any(|x| x == *lang) {
                reps.push(RepsItem::Lang(lang));
                return;
            }

            temp.push(RepsItem::Lang(lang));

            create_reps_inner_target(&lang.target, refs, reps, temp);
            create_reps_inner_list(&lang.args, refs, reps, temp);
        }
    }

}

fn create_reps_inner_list<'a>(
    list: &'a data::List<'a>,
    refs: &mut RefsTableWriter,
    reps: &mut Vec<RepsItem<'a>>,
    temp: &mut Vec<RepsItem<'a>>,
) {
    if reps.into_iter().any(|x| x == list) {
        return;
    }
    if temp.iter().any(|x| x == list) {
        reps.push(RepsItem::List(list));
        return;
    }

    temp.push(RepsItem::List(list));

    for item in list.into_iter() {
        create_reps_inner(&item.data, refs, reps, temp);
    }
}

impl<'a, W> RDSWriter<'a> for BufWriter<W> where W: std::io::Write {}

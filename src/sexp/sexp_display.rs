use std::fmt::Display;

use super::sexp::{data, lang, Sexp, SexpKind};

impl Display for Sexp<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(args) = self.metadata.get_attr() {
            write!(f, "(data : {}, args : {})", self.kind, args)?;
        } else {
            write!(f, "{}", self.kind)?;
        }
        Ok(())
    }
}

pub(super) fn join_string<T>(data: &[T], sep: &str) -> String
where
    T: Display,
{
    data.iter()
        .fold(("".to_string(), true), |(acc, first), x| {
            if first {
                (x.to_string(), false)
            } else {
                (acc.to_string() + sep + x.to_string().as_str(), false)
            }
        })
        .0
}

impl Display for data::Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} + {}*i", self.real, self.imaginary)
    }
}

impl Display for SexpKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SexpKind::Sym(sym) => write!(f, "{}", sym),
            SexpKind::List(data) => write!(f, "list: [{}]", join_string(data, ", ")),
            SexpKind::Nil => write!(f, "nil"),
            SexpKind::Closure(closure) => write!(f, "{closure}"),
            SexpKind::Environment(env) => write!(f, "env: ({env})"),
            SexpKind::Promise {
                environment,
                expr,
                value,
            } => {
                write!(f, "{{ {expr} ({value}, {environment}) }}")
            }
            SexpKind::Lang(lang) => write!(f, "{lang}"),
            SexpKind::Char(chars) => write!(f, "'{}'", join_string(chars, "")),
            SexpKind::Logic(logs) => write!(f, "[{}]", join_string(logs, ", ")),
            SexpKind::Real(reals) => write!(f, "[{}]", join_string(reals, ", ")),
            SexpKind::Int(ints) => write!(f, "[{}]", join_string(ints, ", ")),
            SexpKind::Complex(cmplx) => write!(f, "[{}]", join_string(cmplx, ", ")),
            SexpKind::Str(strs) => write!(f, "['{}']", join_string(strs, ", ")),
            SexpKind::Vec(data) => {
                if data.is_empty() {
                    write!(f, "[]")
                } else {
                    write!(f, "{}", data[0])?;
                    for item in &data[1..] {
                        write!(f, ", {}", item)?;
                    }
                    Ok(())
                }
            }
            SexpKind::MissingArg => write!(f, "missing"),
            SexpKind::Bc(bc) => write!(f, "{bc}"),
            SexpKind::BaseNamespace => write!(f, "base namespace"),
            SexpKind::Buildin(sym) => write!(f, "{sym}"),
            SexpKind::NAString => write!(f, "NASTRING"),
        }
    }
}

impl Display for lang::Lang<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.target, join_string(&self.args, ", "))
    }
}

impl Display for lang::Sym<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl Display for lang::Target<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            lang::Target::Lang(lang) => write!(f, "{lang}"),
            lang::Target::Sym(sym) => write!(f, "{}", sym),
        }
    }
}

impl Display for data::TaggedSexp<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.tag {
            Some(tag) => write!(f, "({tag}) {}", self.data),
            None => write!(f, "{}", self.data),
        }
    }
}

impl Display for lang::Environment<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            lang::Environment::Global => write!(f, "global env"),
            lang::Environment::Empty => write!(f, "empty env"),
            lang::Environment::Base => write!(f, "base env"),
            lang::Environment::Normal(env) => write!(f, "{env}"),
            lang::Environment::Namespace(names) => {
                write!(f, "namespace ({})", join_string(names, ", "))
            }
        }
    }
}

impl Display for lang::NormalEnv<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parent : {}", self.parent)?;
        if let Some(frame) = &self.frame.data {
            write!(f, ", frame : {}", join_string(frame, ", "))?;
        }

        if let Some(hash_frame) = &self.hash_frame.data {
            write!(f, ", hash frame : {}", join_string(hash_frame, ", "))?;
        }

        Ok(())
    }
}

impl Display for lang::Formal<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;

        match self.value.kind {
            SexpKind::MissingArg => (),
            _ => write!(f, " = {}", self.value)?,
        }

        Ok(())
    }
}

impl Display for lang::Closure<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function({})\n", join_string(&self.formals, ", "))?;
        write!(f, "{}\n", self.body)?;
        write!(f, "(env : {})", self.environment)
    }
}

impl Display for data::Logic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            data::Logic::True => write!(f, "true"),
            data::Logic::False => write!(f, "false"),
            data::Logic::NA => write!(f, "NA"),
        }
    }
}

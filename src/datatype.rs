use std::fmt;
use std::hash::{Hash, Hasher};
#[derive(Debug, Clone)]
pub enum Datatype {
    Integer,
    Decimal,
    Text,
    Boolean,
    Any,
    Function((Vec<(String, Datatype)>, Box<Datatype>)),
    Object(Box<Datatype>),
    Array(Box<Datatype>),
    Tuple(Box<Datatype>),
    _Multitype(Vec<Box<Datatype>>),
    _NOTYPE,
}

impl PartialEq for Datatype {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Datatype::Integer, Datatype::Integer)
            | (Datatype::Decimal, Datatype::Decimal)
            | (Datatype::Text, Datatype::Text)
            | (Datatype::Boolean, Datatype::Boolean)
            | (Datatype::Any, Datatype::Any)
            | (Datatype::_NOTYPE, Datatype::_NOTYPE) => true,

            (Datatype::Function((params1, rettype1)), Datatype::Function((params2, rettype2))) => {
                // Check if params and return type are equal
                params1 == params2 && rettype1 == rettype2
            }
            (Datatype::_Multitype(a), Datatype::_Multitype(b)) => a == b,

            (Datatype::Object(a), Datatype::Object(b)) => a == b,
            (Datatype::Array(a), Datatype::Array(b)) => a == b,
            (Datatype::Tuple(a), Datatype::Tuple(b)) => a == b,
            // Default case for inequality
            _ => false,
        }
    }
}

impl Eq for Datatype {}

impl fmt::Display for Datatype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Datatype::Function((params, rettype)) => {
                write!(f, "label:")?;
                if params.len() == 0 {
                    return write!(f, "{}", rettype);
                };
                write!(f, "(")?;
                let mut i = 1;
                for (name, typ) in params {
                    if i == params.len() {
                        write!(f, "{}: {}", name, typ)?;
                    } else {
                        write!(f, "{}: {}, ", name, typ)?;
                    }

                    i = i + 1;
                }
                write!(f, "):{}", rettype)
            }
            Datatype::_Multitype(types) => {
                write!(f, "[")?;
                let i = 1;
                for t in types {
                    if i == types.len() {
                        write!(f, "{}", t)?;
                    } else {
                        write!(f, "{} | ", t)?;
                    }
                }
                write!(f, "]")
            }
            Datatype::Array(ts) => {
                write!(f, "Array<{}>", ts)
            }
            Datatype::Tuple(ts) => {
                write!(f, "(")?;
                match *ts.clone() {
                    Datatype::_Multitype(ts2) => {
                        let mut i = 1;
                        for t in ts2.clone() {
                            if i == ts2.len() {
                                write!(f, "{}", t)?;
                            } else {
                                write!(f, "{}, ", t)?;
                            }
                            i = i + 1;
                        }
                    }
                    _ => {
                        write!(f, "{}", ts)?;
                    }
                };
                write!(f, ")")
            }
            Datatype::Object(ts) => {
                write!(f, "Object<{}>", ts)
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

//CRÉDITOS: ChatGPT modelo 4o
impl Hash for Datatype {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Datatype::Integer => state.write_u8(0),
            Datatype::Decimal => state.write_u8(1),
            Datatype::Text => state.write_u8(2),
            Datatype::Boolean => state.write_u8(3),
            Datatype::Any => state.write_u8(4),
            Datatype::Function((params, rettype)) => {
                state.write_u8(5);
                for (name, dt) in params {
                    name.hash(state);
                    dt.hash(state);
                }
                (*&rettype).hash(state);
            }
            Datatype::Object(dt) => {
                state.write_u8(6);
                dt.hash(state);
            }
            Datatype::Array(dt) => {
                state.write_u8(7);
                dt.hash(state);
            }
            Datatype::Tuple(dt) => {
                state.write_u8(8);
                dt.hash(state);
            }
            Datatype::_Multitype(types) => {
                state.write_u8(9);
                for dt in types {
                    dt.hash(state);
                }
            }
            Datatype::_NOTYPE => state.write_u8(10),
        }
    }
}

impl Datatype {
    pub fn cast(&self, other: &Datatype) -> Result<Datatype, String> {
        match (self, other) {
            (Datatype::Integer, Datatype::Decimal)
            | (Datatype::Boolean, Datatype::Decimal)
            | (Datatype::Decimal, Datatype::Decimal) => Ok(other.clone()),

            (Datatype::Text, Datatype::Array(boxed_inner))
            | (Datatype::Integer, Datatype::Array(boxed_inner))
                if **boxed_inner == Datatype::Integer =>
            {
                Ok(other.clone())
            }

            (Datatype::Boolean, Datatype::Integer) => Ok(other.clone()),

            (Datatype::Integer, Datatype::Boolean)
            | (Datatype::Decimal, Datatype::Boolean)
            | (Datatype::Text, Datatype::Boolean)
            | (Datatype::Boolean, Datatype::Boolean) => Ok(other.clone()),

            (Datatype::Any, _) | (Datatype::_NOTYPE, _) => {
                Err(format!("Impossible to cast \"null values\" for any type."))
            }
            _ => Ok(self.clone()),
        }
    }
}

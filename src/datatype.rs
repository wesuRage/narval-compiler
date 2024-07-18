use std::hash::{Hash, Hasher};
#[derive(Debug, Clone)]
pub enum Datatype {
    Integer,
    Decimal,
    String,
    Boolean,
    Undefined,
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
            | (Datatype::String, Datatype::String)
            | (Datatype::Boolean, Datatype::Boolean)
            | (Datatype::Undefined, Datatype::Undefined)
            | (Datatype::_NOTYPE, Datatype::_NOTYPE) => true,

            (Datatype::Function((params1, rettype1)), Datatype::Function((params2, rettype2))) => {
                // Check if params and return type are equal
                params1 == params2 && rettype1 == rettype2
            }

            (Datatype::Object(_), Datatype::Object(_))
            | (Datatype::Array(_), Datatype::Array(_))
            | (Datatype::Tuple(_), Datatype::Tuple(_))
            | (Datatype::_Multitype(_), Datatype::_Multitype(_)) => true,
            // Default case for inequality
            _ => false,
        }
    }
}

impl Hash for Datatype {
    fn hash<H: Hasher>(&self, state: &mut H) {
        return {};
    }
}

// impl Datatype {
//     fn cast(&self, other: Datatype) -> Result<Datatype, String> {
//         match (self, other) {
//             (Datatype::Integer, Datatype::Decimal)
//             | (Datatype::Boolean, Datatype::Decimal)
//             | (Datatype::Decimal, Datatype::Decimal) => Ok(other),

//             // (Datatype::String, Datatype::Array(Box::new(Datatype::Integer)))
//             // | (Datatype::Integer, Datatype::Array(Box::new(Datatype::Integer))) => Ok(other),
//             (Datatype::Boolean, Datatype::Integer) => Ok(other),

//             (Datatype::Integer, Datatype::Boolean)
//             | (Datatype::Decimal, Datatype::Boolean)
//             | (Datatype::String, Datatype::Boolean)
//             | (Datatype::Boolean, Datatype::Boolean) => Ok(other),

//             (Datatype::Undefined, _) | (Datatype::_NOTYPE, _) => {
//                 Err(format!("Impossible to cast \"null values\" for any type."))
//             }
//             _ => Ok(self.clone()),
//         }
//     }
// }

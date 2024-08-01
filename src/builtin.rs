use crate::checker::Checker;
use crate::datatype::Datatype;

pub fn utilities(checker: &mut Checker) {
    checker.inject_value(
        "write".to_string(),
        Datatype::Function((
            vec![("argument".to_string(), Some(Datatype::Text))],
            ("word".to_string(), Box::new(Datatype::Void)),
        )),
    );

    checker.inject_value(
        "totxt".to_string(),
        Datatype::Function((
            vec![("value".to_string(), Some(Datatype::Integer))],
            ("word".to_string(), Box::new(Datatype::Text)),
        )),
    );

    checker.inject_value(
        "exit".to_string(),
        Datatype::Function((
            vec![("value".to_string(), Some(Datatype::Integer))],
            ("word".to_string(), Box::new(Datatype::Void)),
        )),
    );

    checker.inject_value(
        "clear".to_string(),
        Datatype::Function((
            vec![("value".to_string(), None)],
            ("byte".to_string(), Box::new(Datatype::Void)),
        )),
    );

    checker.inject_value(
        "nanosleep".to_string(),
        Datatype::Function((
            vec![(
                "value".to_string(),
                Some(Datatype::Tuple(Box::new(Datatype::_Multitype(vec![
                    Box::new(Datatype::Integer),
                    Box::new(Datatype::Integer),
                ])))),
            )],
            ("byte".to_string(), Box::new(Datatype::Void)),
        )),
    );
}

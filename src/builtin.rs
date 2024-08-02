use crate::checker::Checker;
use crate::datatype::Datatype;

pub fn utilities(checker: &mut Checker) {
    checker.inject_value(
        "write".to_string(),
        Datatype::Function((
            vec![("buffer".to_string(), Some(Datatype::Text))],
            ("word".to_string(), Box::new(Datatype::Void)),
        )),
    );

    checker.inject_value(
        "write_raw".to_string(),
        Datatype::Function((
            vec![
                ("fd".to_string(), Some(Datatype::Integer)),
                ("buffer".to_string(), Some(Datatype::Text)),
                ("size".to_string(), Some(Datatype::Integer)),
            ],
            ("word".to_string(), Box::new(Datatype::Void)),
        )),
    );

    checker.inject_value(
        "btotxt".to_string(),
        Datatype::Function((
            vec![
                ("input".to_string(), Some(Datatype::Integer)),
                ("len".to_string(), Some(Datatype::Integer)),
                ("out".to_string(), Some(Datatype::Text)),
            ],
            ("word".to_string(), Box::new(Datatype::Void)),
        )),
    );

    checker.inject_value(
        "totxt".to_string(),
        Datatype::Function((
            vec![("input".to_string(), Some(Datatype::Integer))],
            ("word".to_string(), Box::new(Datatype::Text)),
        )),
    );

    checker.inject_value(
        "toint".to_string(),
        Datatype::Function((
            vec![("input".to_string(), Some(Datatype::Text))],
            ("word".to_string(), Box::new(Datatype::Integer)),
        )),
    );

    checker.inject_value(
        "exit".to_string(),
        Datatype::Function((
            vec![("code".to_string(), Some(Datatype::Integer))],
            ("word".to_string(), Box::new(Datatype::Void)),
        )),
    );

    checker.inject_value(
        "clear".to_string(),
        Datatype::Function((
            vec![("none".to_string(), None)],
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

mod lexer;

use crate::lexer::Lexer;

fn main() {
    let code = r#"
    label maingun: integer {
       return 5;
    }
    "#;

    let lexer = Lexer::new("<inline string>");
    let tokens = lexer.tokenize(code);
    println!("{:#?}", tokens);
}

use crate::ast::*;
use crate::colors::{escape, printc};

pub struct Checker {
    pub program: Program,
}

impl Checker {
    pub fn new(tree: Program) -> Checker {
        Checker { program: tree }
    }

    pub fn check_tree(&mut self) {
        for stmt in &self.program.body {
            match stmt.kind {
                NodeType::Program => todo!(),
                NodeType::Stmt => todo!(),
                NodeType::ImportStmt => todo!(),
                NodeType::ExportStmt => todo!(),
                NodeType::IfStmt => todo!(),
                NodeType::ReturnStmt => todo!(),
                NodeType::AsmStmt => todo!(),
                NodeType::Identifier => todo!(),
                NodeType::NullLiteral => todo!(),
                NodeType::NumericLiteral => todo!(),
                NodeType::StringLiteral => todo!(),
                NodeType::Property => todo!(),
                NodeType::ObjectLiteral => todo!(),
                NodeType::VarDeclaration => todo!(),
                NodeType::FunctionDeclaration => todo!(),
                NodeType::AssignmentExpr => todo!(),
                NodeType::BinaryExpr => todo!(),
                NodeType::MemberExpr => todo!(),
                NodeType::CallExpr => todo!(),
                NodeType::TernaryExpr => todo!(),
                NodeType::BlockExpr => todo!(),
                NodeType::ArrayExpr => todo!(),
                NodeType::ArrayAccess => todo!(),
            }
        }
    }
}

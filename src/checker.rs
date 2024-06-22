use crate::ast::*;
use crate::colors::{escape, printc};
use crate::datatype::*;
use std::collections::HashMap;
use std::path::Path;

pub struct Namespace {
    parent: Option<Namespace>,
    names: HashMap<String, Expr>,
}

impl Namespace {
    pub fn new() {
        Namespace {
            names: HashMap::new(),
        }
    }

    pub fn newvar(&mut self, name: String, value: Expr) -> Result<i32, &str> {
        if self.existsvar(name) {
            return Err(format!("The name \"{}\" already has been declared.", name));
        }

        self.names.insert(name, value);
        Ok(0)
    }

    pub fn setvar(&mut self, name: String, value: Expr) -> Result<i32, &str> {
        if !self.existsvar(name) {
            if let Some(parent) = self.parent {
                return parent.setvar(name, value);
            } else {
                return Err(format!("The name \"{}\" hasn't been declared.", name));
            }
        }

        self.names.insert(name, value);
        Ok(0)
    }

    pub fn existsvar(self, name: String) -> bool {
        return self.names.contains_key(name);
    }

    pub fn getvar(self, name: String) -> Result<Expr, &str> {
        if !self.existsvar(name) {
            if let Some(parent) = self.parent {
                return parent.getvar(name);
            } else {
                return Err(format!("The name \"{}\" hasn't been declared.", name));
            }
        }

        Ok(self.names.get(name))
    }
}
pub struct Checker {
    pub program: Program,
    pub namespaces: Vector<Namespace>,
}

type Dt = Option<Datatype>;

impl Checker {
    pub fn new(tree: Program) -> Checker {
        Checker { program: tree }
    }

    pub fn check(&mut self, node: Stmt) -> Dt {
        let mut stmt = node.expr;
        match Some(stmt).kind {
            NodeType::ImportStmt => self.check_import(stmt),
            NodeType::ExportStmt => self.check_export(stmt),
            NodeType::IfStmt => self.check_if(stmt),
            NodeType::ReturnStmt => self.check_return(stmt),
            NodeType::AsmStmt => self.check_asmpiece(stmt),
            NodeType::Identifier => self.check_id(stmt),
            NodeType::NullLiteral => Some(()),
            NodeType::NumericLiteral => Some(()),
            NodeType::StringLiteral => Some(()),
            NodeType::Property => self.check_prop(stmt),
            NodeType::ObjectLiteral => self.check_object(stmt),
            NodeType::VarDeclaration => self.check_newvar(stmt),
            NodeType::FunctionDeclaration => self.check_newfunc(stmt),
            NodeType::AssignmentExpr => self.check_setvar(stmt),
            NodeType::BinaryExpr => self.check_binop(stmt),
            NodeType::MemberExpr => self.check_memberexpr(stmt),
            NodeType::CallExpr => self.check_call(stmt),
            NodeType::TernaryExpr => self.check_ternaryop(stmt),
            NodeType::BlockExpr => self.check_blockexpr(stmt),
            NodeType::ArrayExpr => self.check_newarr(stmt),
            NodeType::ArrayAccess => self.check_getarr(stmt),
            _ => Some(()),
        }
    }

    fn check_tree(&mut self) {
        for stmt in &self.program.body {
            self.check(stmt);
        }
    }

    fn check_import(&mut self, stmt: ImportStmt) -> dt {
        let mut i = 0;
        for (path, alias) in stmt.paths.iter_mut() {
            if path.starts_with("nv:") {
                *path = path.replace("nv:", "/root/rust/narval/tests/libs/nv/");
            }
            let does_it_exists = Path::new(path).exists();
            if !does_it_exists {
                self.error(stmt)
            }
        }
    }
}

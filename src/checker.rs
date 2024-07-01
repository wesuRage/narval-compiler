use crate::ast::*;
use crate::colors::{escape, printc};
use crate::datatype::*;
use std::collections::HashMap;
use std::path::Path;

pub struct Namespace {
    parent: Option<Namespace>,
    names: HashMap<String, Datatype>,
}

impl Namespace {
    pub fn new() {
        Namespace {
            names: HashMap::new(),
        }
    }

    pub fn newvar(&mut self, name: String, dt: Datatype) -> Result<i32, &str> {
        if self.existsvar(name) {
            return Err(format!("The name \"{}\" already has been declared.", name));
        }

        self.names.insert(name, dt);
        Ok(0)
    }

    pub fn setvar(&mut self, name: String, dt: Datatype) -> Result<i32, &str> {
        if !self.existsvar(name) {
            if let Some(parent) = self.parent {
                return parent.setvar(name, dt);
            } else {
                return Err(format!("The name \"{}\" hasn't been declared.", name));
            }
        }

        self.names.insert(name, dt);
        Ok(0)
    }

    pub fn existsvar(self, name: String) -> bool {
        return self.names.contains_key(name);
    }

    pub fn getvar(self, name: String) -> Result<Datatype, &str> {
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
    pub namespaces: Vec<Namespace>,
    pub namespace: Namespace,
}

type Dt = Option<Datatype>;

impl Checker {
    pub fn new(tree: Program) -> Checker {
        let nss: Vec<Namespace> = Vec::new();
        nss.push(Namespace::new());
        Checker {
            program: tree,
            namespaces: nss,
            namespace: nss[0],
        }
    }

    fn check(&mut self, node: Stmt) -> Dt {
        let mut stmt = node.expr.unwrap();
        match stmt.kind() {
            NodeType::ImportStmt => self.check_import(stmt),
            NodeType::ExportStmt => self.check_export(stmt),
            NodeType::IfStmt => self.check_if(stmt),
            NodeType::ReturnStmt => self.check_return(stmt),
            NodeType::AsmStmt => self.check_asmpiece(stmt),
            NodeType::Identifier => self.check_id(stmt),
            NodeType::NullLiteral => Some(Datatype::Null),
            NodeType::NumericLiteral => Some(Datatype),
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
            NodeType::MovStmt => self.check_move(stmt),
            NodeType::LoopStmt => self.check_loop(stmt),
            NodeType::ForStmt => self.check_forloop(stmt),
            NodeType::UndefinedLiteral => self.check_undefined(stmt), //ajeitou aeee
            NodeType::TrueLiteral => self.check_ifistrue(stmt),
            NodeType::FalseLiteral => self.check_ifisfalse(stmt),
            NodeType::UnitDeclaration => self.check_unit(stmt),
            NodeType::UnitVarDeclaration => self.check_attr(stmt),
            NodeType::UnitFunctionDeclaration => self.check_method(stmt),
            NodeType::BreakExpr => self.check_break(stmt),
            NodeType::PreIncrementExpr => self.check_plusplusN(stmt),
            NodeType::PreDecrementExpr => self.check_subsubN(stmt),
            NodeType::PostIncrementExpr => self.check_Nplusplus(stmt),
            NodeType::PostDecrementExpr => self.check_Nsubsub(stmt),
            NodeType::LogicalNotExpr => self.check_not(stmt),
            NodeType::UnaryMinusExpr => self.check_neg(stmt),
            _ => Some(()),
        }
    }

    pub fn check_tree(&mut self) {
        for stmt in &self.program.body {
            self.check(stmt);
        }
    }

    fn check_import(&mut self, stmt: ImportStmt) -> Dt {
        let mut i = 0;
        for (path, alias) in stmt.paths.iter_mut() {
            if path.starts_with("nv:") {
                *path = path.replace("nv:", "/root/rust/narval/tests/libs/nv/");
            }
            let does_it_exists = Path::new(path).exists();
            if !does_it_exists {
                self.error("Bad Path.")
            }
        }
        None
    }

    fn error(&self, message: &str) {
        println!(
            "Oh não! vamos ver o seu erro, se vale a pena relatar: {}",
            message
        );
    }

    fn check_export(&self, stmt: ExportStmt) -> Dt {
        for i in stmt.identifiers {
            self.check_id(i);
        }

        None
    }

    fn check_id(&self, expr: Identifier) -> Dt {
        return match self.namespace.getvar(expr.symbol) {
            Ok(value) => Some(value),
            Err(error) => {
                self.error(error);
                None
            }
        };
    }

    fn check_if(&self, stmt: Box<IfStmt>) -> Dt {
        let test = (*stmt).test;
        self.check(Stmt { kind: (*test) });
        for s in (*stmt).consequent {}
    }
}

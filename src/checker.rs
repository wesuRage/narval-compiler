use crate::ast::*;
use crate::colors::{escape, printc};
use crate::datatype::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Namespace {
    parent: Option<Rc<RefCell<Namespace>>>,
    names: HashMap<String, (bool, Datatype)>,
}

impl Namespace {
    pub fn new_inheriting(parent: Option<Rc<RefCell<Namespace>>>) -> Namespace {
        Namespace {
            names: HashMap::new(),
            parent,
        }
    }

    pub fn new() -> Namespace {
        Namespace::new_inheriting(None)
    }

    pub fn newvar(&mut self, name: String, dt: Datatype, isconst: bool) -> Result<i32, String> {
        if self.existsvar(name.clone()) {
            return Err(format!("The name \"{}\" has already been declared.", name));
        }

        self.names.insert(name.clone(), (isconst, dt));
        Ok(0)
    }

    pub fn setvar(&mut self, name: String, dt: Datatype) -> Result<i32, String> {
        if !self.existsvar(name.clone()) {
            if let Some(parent) = &mut self.parent {
                return parent.borrow_mut().setvar(name, dt);
            } else {
                return Err(format!("The name \"{}\" hasn't been declared.", name));
            }
        }

        if self.names.get(name.as_str()).unwrap().0 {
            return Err(format!(
                "The name \"{}\" has been declared as a constant.",
                name
            ));
        }
        self.names.insert(name, (false, dt));
        Ok(0)
    }

    pub fn existsvar(&mut self, name: String) -> bool {
        return self.names.contains_key(name.as_str());
    }

    pub fn getvar(&mut self, name: String) -> Result<Option<Datatype>, String> {
        if !self.existsvar(name.clone()) {
            if let Some(parent) = &self.parent {
                return parent.borrow_mut().getvar(name);
            } else {
                return Err(format!("The name \"{}\" hasn't been declared.", name));
            }
        }

        Ok(Some(self.names.get(&name).unwrap().1.clone()))
    }
}

pub struct Checker<'a> {
    pub program: Program,
    pub filename: &'a String,
    pub func_id: i32,
    pub bodies: Vec<(i32, Dt, Vec<Stmt>)>,
    pub current_body: (i32, Dt, Vec<Stmt>),
    pub namespaces: Vec<Namespace>,
    pub namespace: Namespace,
    pub lines: Option<Vec<String>>,
}

type Dt = Option<Datatype>;

impl<'a> Checker<'a> {
    pub fn new(tree: Program, source_code: &str, filename: &'a &String) -> Checker<'a> {
        let mut nss: Vec<Namespace> = Vec::new();
        nss.push(Namespace::new());
        let mut bodies: Vec<(i32, Dt, Vec<Stmt>)> = Vec::new();
        bodies.push((0, None, tree.body.clone()));

        Checker {
            program: tree,
            func_id: 0,
            filename,
            bodies: bodies.clone(),
            current_body: bodies[0].clone(),
            namespaces: nss.clone(),
            namespace: nss[0].clone(),
            lines: Some(source_code.split('\n').map(String::from).collect()),
        }
    }

    pub fn check(&mut self, stmt: Stmt) -> Dt {
        match stmt.expr {
            Some(Expr::Identifier(id)) => self.check_id(id),
            Some(Expr::NullLiteral(_)) => Some(Datatype::Undefined),
            Some(Expr::NumericLiteral(_)) => Some(Datatype::Integer),
            Some(Expr::StringLiteral(_)) => Some(Datatype::String),
            Some(Expr::ObjectLiteral(object)) => self.check_object(object),
            Some(Expr::VarDeclaration(decl)) => self.check_newvar(decl),
            Some(Expr::AssignmentExpr(expr)) => self.check_setvar(expr),
            _ => Some(Datatype::Undefined),
        }
    }

    pub fn check_body(&mut self) {
        for stmt in self.current_body.2.clone() {
            self.check(stmt);
        }
    }

    fn error(&mut self, node: Expr, message: &str) {
        let nd: &Expr = &node; // Obtém o token atual
        let filename: &String = &self.filename; // Obtém o nome do arquivo do token
        let data: ((usize, usize), (usize, usize), usize) = nd.local();
        let column: (usize, usize) = data.1; // Obtém a posição da coluna do token
        let lineno: usize = data.2; // Obtém o número da linha do token
        let line: &String = &self.lines.as_ref().unwrap()[lineno - 1]; // Obtém a linha do código fonte
        let column_repr: String = format!(
            "{}{}",
            " ".repeat(column.0 - 1),
            "^".repeat(data.1 .1 - data.1 .0)
        );
        let formatted_message: String = format!(
            "%%r{} %%b{}%%!:%%y{}%%!:%%y{}%%!:\n\t{}\n\t%%r{}%%!\n%%y{}%%!",
            "ERROR:",
            filename,
            lineno,
            column.0 - 1,
            escape(line),
            column_repr,
            message
        );
        printc(&formatted_message);
    }

    fn check_id(&mut self, expr: Identifier) -> Dt {
        return match self.namespace.getvar(expr.clone().symbol) {
            Ok(value) => value.clone(),
            Err(error) => {
                self.error(Expr::Identifier(expr.clone()), &error);
                Some(Datatype::Undefined)
            }
        };
    }

    fn check_brute_id(&mut self, expr: String) -> Dt {
        return match self.namespace.getvar(expr.clone()) {
            Ok(Some(value)) => Some(value),
            Ok(None) => None,
            Err(error) => {
                self.error(
                    Expr::StringLiteral(StringLiteral {
                        kind: NodeType::StringLiteral,
                        value: expr.clone(),
                        column: (0, 0),
                        position: (0, 0),
                        lineno: 0,
                    }),
                    &error,
                );
                Some(Datatype::Undefined)
            }
        };
    }

    fn expr2stmt(&self, expr: Expr) -> Stmt {
        let data: ((usize, usize), (usize, usize), usize) = expr.local();
        Stmt {
            kind: expr.kind(),
            expr: Some(expr),
            return_stmt: None,
            position: data.0,
            column: data.1,
            lineno: data.2,
        }
    }

    fn check_object(&mut self, expr: ObjectLiteral) -> Dt {
        let mut types: Vec<Datatype> = Vec::new();

        for prop in expr.properties {
            let key: String = prop.key;
            let value: Option<Box<Expr>> = prop.value;
            if let Some(v) = value {
                let stmt: Stmt = self.expr2stmt(*v);
                let dt: Datatype = self.check(stmt.clone()).unwrap();
                if !(types.contains(&dt)) {
                    types.push(dt);
                }
            } else {
                let dt: Option<Datatype> = self.check_brute_id(key);
                if !(types.contains(&dt.clone().unwrap())) {
                    types.push(dt.clone().unwrap());
                }
            }
        }

        return if types.len() == 1 {
            Some(Datatype::Object(Box::new(types[0].clone())))
        } else {
            Some(Datatype::Undefined)
        };
    }

    fn newvar(&mut self, name: String, dt: Dt, constant: bool) {
        self.namespace
            .newvar(name, dt.unwrap(), constant)
            .expect("Error while creating new variable");
    }
    fn check_newvar(&mut self, decl: VarDeclaration) -> Dt {
        let dt = self.check(self.expr2stmt(*(decl.value)));
        self.newvar(decl.identifier.unwrap(), dt, false);
        None
    }

    fn check_setvar(&mut self, expr: AssignmentExpr) -> Dt {
        let assigne: Expr = *(expr.assigne);
        let value: Expr = *(expr.value);

        if assigne.kind() != NodeType::Identifier {
            self.error(
                assigne.clone(),
                format!("Expected Identifier here, but found {:?}.", assigne.kind()).as_str(),
            );
            return None;
        }

        let dt = self.check(self.expr2stmt(value));

        if let Expr::Identifier(id) = assigne.clone() {
            if self.namespace.existsvar(id.clone().symbol) {
                let sucess = self.namespace.setvar(id.symbol, dt.unwrap());
                match sucess {
                    Ok(value) => (),
                    Err(error) => self.error(assigne, error.as_str()),
                }
                return None;
            } else {
                self.newvar(id.symbol, dt, true);
            }
        }
        None
    }
}

use crate::ast::*;
use crate::colors::{escape, printc};
use crate::datatype::Datatype;
use crate::datatype::Datatype::*;
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
    pub fn new(tree: Program, source_code: &str, filename: &'a String) -> Checker<'a> {
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
        if let Some(ret) = stmt.return_stmt {
            if self.current_body.0 == 0 {
                self.error_wdata(
                    (ret.position, ret.column, ret.lineno),
                    "Can not have return statments at global scope.",
                );
                return Some(Any);
            }
            if let Some(value) = ret.argument {
                let dt = self.check(self.expr2stmt(value.clone()));

                if let (Some(fdt), Some(rdt)) = (self.current_body.1.clone(), dt) {
                    if rdt != fdt {
                        self.error(
                            value,
                            format!("Expected a value of type {} here, but found {}", fdt, rdt)
                                .as_str(),
                        );
                    }
                }
            } else {
                println!("AQUIIII");
                println!("FUNCIONA");
                if let Some(fdt) = self.current_body.1.clone() {
                    if fdt != Any {
                        self.error_wdata(
                            (ret.position, ret.column, ret.lineno),
                            "PUT SOMETHING HERE, THE FUNCTION REQUIRES SOME VALUE, RIGHT?",
                        );
                        return Some(Any);
                    }
                }
            }

            return Some(Any);
        }
        match stmt.expr {
            Some(Expr::Identifier(id)) => self.check_id(id),
            Some(Expr::NullLiteral(_)) => Some(Any),
            Some(Expr::NumericLiteral(_)) => Some(Integer),
            Some(Expr::StringLiteral(_)) => Some(Text),
            Some(Expr::TrueLiteral(_)) | Some(Expr::FalseLiteral(_)) => Some(Boolean),
            Some(Expr::ObjectLiteral(object)) => self.check_object(object),
            Some(Expr::VarDeclaration(decl)) => self.check_newvar(decl),
            Some(Expr::AssignmentExpr(expr)) => self.check_setvar(expr),
            Some(Expr::BinaryExpr(expr)) => self.check_binop(expr),
            Some(Expr::ArrayExpr(expr)) => self.check_array(expr),
            Some(Expr::FunctionDeclaration(decl)) => self.check_label(decl),
            Some(Expr::_ENDFUNCTION(..)) => self.finalize_check_label(),
            _ => Some(Any),
        }
    }

    fn error_wdata(&mut self, data: ((usize, usize), (usize, usize), usize), message: &str) {
        let filename: &String = &self.filename;
        let column: (usize, usize) = data.1;
        let lineno: usize = data.2;
        let line: &String = &self.lines.as_ref().unwrap()[lineno - 1];
        let column_repr: String = format!(
            "{}{}",
            " ".repeat(column.0 - 1),
            "^".repeat(data.1 .1 - data.1 .0)
        );
        let formatted_message: String = format!(
            "%%b{}%%!:%%y{}%%!:%%y{}%%!:\n%%r{} %%y{}%%!\n\t{}\n\t%%r{}%%!",
            filename,
            lineno,
            column.0,
            "ERROR:",
            message,
            escape(line),
            column_repr,
        );
        printc(&formatted_message);
    }
    fn error(&mut self, node: Expr, message: &str) {
        self.error_wdata(node.local(), message);
    }

    fn check_id(&mut self, expr: Identifier) -> Dt {
        return match self.namespace.getvar(expr.clone().symbol) {
            Ok(value) => value.clone(),
            Err(error) => {
                self.error(Expr::Identifier(expr.clone()), &error);
                Some(Any)
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
                        typ: Some(Text),
                        column: (0, 0),
                        position: (0, 0),
                        lineno: 0,
                    }),
                    &error,
                );
                Some(Any)
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
            let r = Some(Object(Box::new(types[0].clone())));

            return r;
        } else {
            Some(Any)
        };
    }

    fn newvar(&mut self, name: String, dt: Dt, constant: bool) {
        self.namespace
            .newvar(name, dt.unwrap(), constant)
            .expect("Error while creating new variable");
    }
    fn check_newvar(&mut self, decl: VarDeclaration) -> Dt {
        let dt = self.check(self.expr2stmt(*(decl.clone().value)));
        if decl.data_type != dt.clone().unwrap() {
            self.error(
                *(decl.clone().value),
                format!(
                    "Type mismatch: expected type {} for value, but found {}",
                    decl.data_type,
                    dt.clone().unwrap()
                )
                .as_str(),
            )
        }
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
                    Ok(_) => (),
                    Err(error) => self.error(assigne, error.as_str()),
                }
                return None;
            } else {
                self.newvar(id.symbol, dt, true);
            }
        }
        None
    }

    fn check_binop(&mut self, expr: BinaryExpr) -> Dt {
        let left: Option<Datatype> = self.check(self.expr2stmt(*expr.left.clone()));
        let right: Option<Datatype> = self.check(self.expr2stmt(*expr.right.clone()));

        if let (Some(l), Some(r)) = (left, right) {
            //TODO: checar operadores com graça, elegância, extravagância e tesão
            let op = expr.operator.as_str();
            match op {
                "+" | "-" | "\\" | "/" | "**" => {
                    let isntnum_l = !(l == Integer || l == Decimal);
                    let isntnum_r = !(r == Integer || r == Decimal);

                    if isntnum_l && !isntnum_r {
                        self.error(
                            *expr.left.clone(),
                            format!(
                                "Attempting to use non-numerical types in \"{}\" operation.",
                                op
                            )
                            .as_str(),
                        );
                    } else if !isntnum_l && isntnum_r {
                        self.error(
                            *expr.right.clone(),
                            format!(
                                "Attempting to use non-numerical types in \"{}\" operation.",
                                op
                            )
                            .as_str(),
                        );
                    } else if isntnum_l && isntnum_r {
                        self.error(
                            Expr::BinaryExpr(expr.clone()),
                            format!(
                                "Attempting to use non-numerical types in \"{}\" operation.",
                                op
                            )
                            .as_str(),
                        )
                    }
                    let casted = l.cast(&r);

                    match casted {
                        Ok(dt) => Some(dt),
                        Err(err) => {
                            self.error(Expr::BinaryExpr(expr.clone()), err.as_str());
                            Some(Any)
                        }
                    }
                }
                "*" => {
                    if l == Text {
                        if r != Integer {
                            self.error(
                                Expr::BinaryExpr(expr.clone()),
                                "Can't repeat a string using non-integer values.",
                            );
                        }
                        return Some(Text);
                    } else {
                        let isntnum_l = !(l == Integer || l == Decimal);
                        let isntnum_r = !(r == Integer || r == Decimal);

                        if isntnum_l && !isntnum_r {
                            self.error(
                                *expr.left.clone(),
                                format!(
                                    "Attempting to use non-numerical types in \"{}\" operation.",
                                    op
                                )
                                .as_str(),
                            );
                        } else if !isntnum_l && isntnum_r {
                            self.error(
                                *expr.right.clone(),
                                format!(
                                    "Attempting to use non-numerical types in \"{}\" operation.",
                                    op
                                )
                                .as_str(),
                            );
                        } else if isntnum_l && isntnum_r {
                            self.error(
                                Expr::BinaryExpr(expr.clone()),
                                format!(
                                    "Attempting to use non-numerical types in \"{}\" operation.",
                                    op
                                )
                                .as_str(),
                            )
                        }
                        let casted = l.cast(&r);

                        match casted {
                            Ok(dt) => Some(dt),
                            Err(err) => {
                                self.error(Expr::BinaryExpr(expr.clone()), err.as_str());
                                Some(Any)
                            }
                        }
                    }
                }
                _ => Some(Any),
            }
        } else {
            return Some(Any);
        }
    }

    fn check_array(&mut self, expr: ArrayExpr) -> Dt {
        let mut types: Vec<Datatype> = Vec::new();

        for e in &expr.elements {
            let dt: Option<Datatype> = self.check(self.expr2stmt(e.clone())).clone();
            if !(types.contains(&dt.clone()?)) {
                types.push(dt.clone()?);
            }
        }

        if types.len() == 1 {
            return Some(Array(Box::new(types[0].clone())));
        } else {
            self.error(Expr::ArrayExpr(expr), "Arrays can't be multitype!");
            Some(Any)
        }
    }

    fn pushbody(
        &mut self,
        body: &mut Vec<Stmt>,
        params: &Vec<(String, Datatype)>,
        rettype: Datatype,
    ) {
        self.func_id += 1;
        body.push(self.expr2stmt(Expr::_ENDFUNCTION(params.to_vec(), rettype.clone())));
        self.bodies
            .push((self.func_id, Some(rettype), body.to_vec()));
        self.current_body = self.bodies[self.bodies.len() - 1].clone();
    }

    fn popbody(&mut self) {
        self.func_id -= 1;
        self.bodies.remove(self.bodies.len() - 1);
        self.current_body = self.bodies[self.bodies.len() - 1].clone();
    }

    fn pushns(&mut self) {
        self.namespaces
            .push(Namespace::new_inheriting(Some(Rc::new(RefCell::new(
                self.namespaces[self.namespaces.len() - 1].clone(),
            )))));
        self.namespace = self.namespaces[self.namespaces.len() - 1].clone();
    }

    fn popns(&mut self) {
        self.namespaces.remove(self.namespaces.len() - 1);
        self.namespace = self.namespaces[self.namespaces.len() - 1].clone();
    }

    fn check_label(&mut self, declt: Box<FunctionDeclaration>) -> Dt {
        let decl = *declt;
        let mut params: Vec<(String, Datatype)> = Vec::new();
        let mut body = decl.body;
        for (_size, t, name) in decl.parameters {
            params.push((name, t));
        }
        let ft = Some(Function((
            params.clone(),
            Box::new(decl.return_type.clone()),
        )));
        self.newvar(decl.name, ft.clone(), true);
        self.pushbody(&mut body, &params, decl.return_type.clone());
        self.pushns();

        for (name, t) in params.clone() {
            self.namespace.newvar(name, t, false);
        }

        return ft.clone();
    }

    fn finalize_check_label(&mut self) -> Dt {
        self.popbody();
        self.popns();
        Some(Any)
    }
}

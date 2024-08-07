use crate::ast;
use crate::ast::*;
use crate::colors::printc;
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

    pub fn getvar(&mut self, name: &String) -> Result<Option<Datatype>, String> {
        if !self.existsvar(name.clone()) {
            if let Some(parent) = &self.parent {
                return parent.borrow_mut().getvar(name);
            } else {
                return Err(format!("The name \"{}\" hasn't been declared.", name));
            }
        }

        Ok(Some(self.names.get(name).unwrap().1.clone()))
    }
}

pub struct Checker<'a> {
    pub program: &'a Program,
    pub filename: &'a String,
    pub func_id: i32,
    pub funcnames: Vec<String>,
    pub bodies: Vec<(i32, Dt, Vec<Stmt>)>,
    pub current_body: (i32, Dt, Vec<Stmt>),
    pub namespaces: Vec<Namespace>,
    pub namespace: &'a mut Namespace,
    pub current_namespace_index: usize,
    pub lines: Option<Vec<String>>,
    pub in_loop: bool

}

type Dt = Option<Datatype>;

impl<'a> Checker<'a> {
    pub fn new(
        tree: &'a Program,
        namespacearr: &'a mut Vec<Namespace>,
        source_code: &str,
        filename: &'a String,
    ) -> Checker<'a> {
        namespacearr.push(Namespace::new());
        let mut bodies: Vec<(i32, Dt, Vec<Stmt>)> = Vec::new();
        bodies.push((0, None, tree.body.clone()));

        Checker {
            program: tree,
            func_id: 0,
            filename,
            funcnames: vec!["(global scope)".to_string()],
            bodies: bodies.clone(),
            current_body: bodies[0].clone(),
            namespaces: namespacearr.to_vec(),
            namespace: &mut namespacearr[0],
            current_namespace_index: 0,
            lines: Some(source_code.split('\n').map(String::from).collect()),
            in_loop: false
        }
    }

    pub fn inject_value(&mut self, name: String, tdef: Datatype) {
        self.newvar(name, Some(tdef), true);
    }
    pub fn check(&mut self, stmt: &Stmt) -> Dt {
        if let Some(ret) = &stmt.return_stmt {
            if self.current_body.0 == 0 {
                self.error_wdata(
                    (ret.position, ret.column, ret.lineno),
                    "Can not have return statments at global scope.",
                );
                return Some(Void);
            }
            if let Some(value) = &ret.argument {
                let dt = self.check(&self.expr2stmt(value.to_owned()));

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
                if let Some(fdt) = self.current_body.1.clone() {
                    if fdt != Void {
                        self.error_wdata(
                            //voltei
                            (ret.position, ret.column, ret.lineno),
                            "Expected no value here, but found one value.",
                        );
                        return Some(Void);
                    }
                }
            }

            return Some(Void);
        }

        unsafe {
            match &stmt.expr {
                Some(Expr::Identifier(ref id)) => self.check_id(id),
                Some(Expr::VoidLiteral(_)) => Some(Void),
                Some(Expr::NumericLiteral(_)) => Some(Integer),
                Some(Expr::StringLiteral(_)) => Some(Text),
                Some(Expr::BooleanLiteral(_)) => Some(Boolean),
                Some(Expr::ObjectLiteral(ref object)) => self.check_object(object),
                Some(Expr::VarDeclaration(ref decl)) => self.check_newvar(decl),
                Some(Expr::AssignmentExpr(ref expr)) => self.check_setvar(expr),
                Some(Expr::BinaryExpr(ref expr)) => self.check_binop(expr),
                Some(Expr::ArrayExpr(ref expr)) => self.check_array(expr),
                Some(Expr::FunctionDeclaration(ref decl)) => self.check_label(decl),
                Some(Expr::_EOL(_)) => self.finalize_label(),
                Some(Expr::IfStmt(ref stmt)) => self.check_if(stmt),
                Some(Expr::WhileStmt(ref stmt)) => self.check_while(stmt),
                Some(Expr::CallExpr(ref expr)) => self.check_call(expr),
                Some(Expr::Enum(ref enm)) => self.check_enum(enm),
                Some(Expr::AsmStmt(ref stmt)) => self.check_asmpiece(stmt),
                Some(Expr::BreakExpr(ref b)) => {
                    if !self.in_loop {
                        self.error(&Expr::BreakExpr(b.clone()), "Attempting to use break in a non-loop context.");
                    }
                    return Some(Void);
                }
                _ => Some(Void),
            }
        }
    }

    fn error_wdata(&mut self, data: ((usize, usize), (usize, usize), usize), message: &str) {
        let filename: &String = &self.filename;
        let column: (usize, usize) = data.1;
        let index = self.funcnames.len() - 1;
        let name: &String = &self.funcnames[index];
        let lineno: usize = data.2;
        let lineshift = " ".repeat(3 + (lineno.to_string().len()));
        let line: &String = &self.lines.as_ref().unwrap()[lineno - 1];
        let column_repr: String = format!(
            "{}{}",
            " ".repeat(column.0 - 1),
            "^".repeat(data.1 .1 - data.1 .0)
        );
        let formatted_message: String = format!(
            "%%b{}%%!:%%y{}%%!:%%y{}%%!:\n%%rERROR %%gin {}%%!: %%y{}%%!\n\t{} | {}\n\t{}%%r{}%%!",
            filename, lineno, column.0, name, message, lineno, line, lineshift, column_repr,
        );
        printc(&formatted_message);
    }
    fn error(&mut self, node: &Expr, message: &str) {
        self.error_wdata(node.local(), message);
    }

    fn check_id(&mut self, expr: &Identifier) -> Dt {
        return match self.namespace.getvar(&expr.symbol) {
            Ok(value) => value.clone(),
            Err(error) => {
                self.error(&Expr::Identifier(expr.clone()), &error);
                Some(Void)
            }
        };
    }

    fn check_brute_id(&mut self, expr: &String) -> Dt {
        return match self.namespace.getvar(&expr) {
            Ok(Some(value)) => Some(value),
            Ok(None) => None,
            Err(error) => {
                self.error(
                    &Expr::StringLiteral(StringLiteral {
                        kind: NodeType::StringLiteral,
                        value: expr.clone(),
                        typ: Some(Text),
                        column: (0, 0),
                        position: (0, 0),
                        lineno: 0,
                    }),
                    &error,
                );
                Some(Void)
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

    unsafe fn check_object(&mut self, expr: &ObjectLiteral) -> Dt {
        let mut types: Vec<Datatype> = Vec::new();
        for prop in &expr.properties {
            let key: &String = &prop.key;
            let value: &Option<Box<Expr>> = &prop.value;
            if let Some(v) = value {
                let stmt: Stmt = self.expr2stmt(v.as_ref().to_owned());
                let dt: &Datatype = &self.check(&stmt).unwrap();
                if !(types.contains(&dt)) {
                    types.push(dt.clone());
                }
            } else {
                let dt: Option<Datatype> = self.check_brute_id(key);
                if !(types.contains(&dt.clone().unwrap())) {
                    types.push(dt.clone().unwrap());
                }
            }
        }

        return if types.len() == 1 {
            if Some(types[0].clone()).is_none() {
                return Some(Void);
            }
            let r: Option<Datatype> = Some(Object(Box::new(types[0].clone())));
            *expr.typ.borrow_mut() = r.clone();
            return r;
        } else {
            Some(Void)
        };
    }

    fn newvar(&mut self, name: String, dt: Dt, constant: bool) {
        if let Some(t) = dt {
            self.namespace
                .newvar(name, t, constant)
                .expect("Error while creating new variable");
        }
    }
    unsafe fn check_newvar(&mut self, decl: &VarDeclaration) -> Dt {
        let dt: Option<Datatype> = self.check(&self.expr2stmt(*decl.clone().value));
        if let Some(datatyp) = dt.clone() {
            if !decl.inferred && decl.data_type != datatyp {
                self.error(
                    &*(decl.value),
                    format!(
                        "Type mismatch: expected type {} for value, but found {}",
                        decl.data_type, datatyp
                    )
                    .as_str(),
                )
            }
            self.newvar(decl.identifier.clone().unwrap(), dt, false);
            None
        } else {
            None
        }
    }

    fn check_setvar(&mut self, expr: &AssignmentExpr) -> Dt {
        let assigne: &Expr = &*expr.assigne;
        let value: &Expr = &*expr.value;

        if assigne.kind() != NodeType::Identifier {
            self.error(
                &assigne,
                format!("Expected Identifier here, but found {:?}.", assigne.kind()).as_str(),
            );
            return None;
        }

        let dt: Option<Datatype> = self.check(&self.expr2stmt(value.to_owned()));

        if let Expr::Identifier(id) = assigne.clone() {
            if self.namespace.existsvar(id.clone().symbol) {
                let sucess = self.namespace.setvar(id.symbol, dt.unwrap());
                match sucess {
                    Ok(_) => (),
                    Err(error) => self.error(&assigne, error.as_str()),
                }
                return None;
            } else {
                self.newvar(id.symbol, dt, true);
            }
        }
        None
    }

    fn check_binop(&mut self, expr: &BinaryExpr) -> Dt {
        let left: Option<Datatype> = self.check(&self.expr2stmt(*expr.left.clone()));
        let right: Option<Datatype> = self.check(&self.expr2stmt(*expr.right.clone()));

        if let (Some(l), Some(r)) = (left, right) {
            let op = expr.operator.as_str();
            match op {
                "+" | "-" | "\\" | "/" | "**" | "%" => {
                    let isntnum_l = !(l == Integer || l == Decimal);
                    let isntnum_r = !(r == Integer || r == Decimal);

                    if isntnum_l && !isntnum_r {
                        self.error(
                            &*expr.left,
                            format!(
                                "Attempting to use non-numerical types in \"{}\" operation.",
                                op
                            )
                            .as_str(),
                        );
                    } else if !isntnum_l && isntnum_r {
                        self.error(
                            &*expr.right,
                            format!(
                                "Attempting to use non-numerical types in \"{}\" operation.",
                                op
                            )
                            .as_str(),
                        );
                    } else if isntnum_l && isntnum_r {
                        self.error(
                            &Expr::BinaryExpr(expr.clone()),
                            format!(
                                "Attempting to use non-numerical types in \"{}\" operation.",
                                op
                            )
                            .as_str(),
                        )
                    }
                    let casted = l.cast(&r);

                    match casted {
                        Ok(dt) => {
                            *expr.typ.borrow_mut() = Some(dt.clone());
                            Some(dt)
                        }
                        Err(err) => {
                            self.error(&Expr::BinaryExpr(expr.clone()), err.as_str());
                            Some(Void)
                        }
                    }
                }
                "*" => {
                    if l == Text {
                        if r != Integer {
                            self.error(
                                &Expr::BinaryExpr(expr.clone()),
                                "Can't repeat a string using non-integer values.",
                            );
                        }
                        let st = Some(Text);
                        *expr.typ.borrow_mut() = st.clone();
                        return st;
                    } else {
                        let isntnum_l = !(l == Integer || l == Decimal);
                        let isntnum_r = !(r == Integer || r == Decimal);

                        if isntnum_l && !isntnum_r {
                            self.error(
                                &*expr.left,
                                format!(
                                    "Attempting to use non-numerical types in \"{}\" operation.",
                                    op
                                )
                                .as_str(),
                            );
                        } else if !isntnum_l && isntnum_r {
                            self.error(
                                &*expr.right,
                                format!(
                                    "Attempting to use non-numerical types in \"{}\" operation.",
                                    op
                                )
                                .as_str(),
                            );
                        } else if isntnum_l && isntnum_r {
                            self.error(
                                &Expr::BinaryExpr(expr.clone()),
                                format!(
                                    "Attempting to use non-numerical types in \"{}\" operation.",
                                    op
                                )
                                .as_str(),
                            )
                        }
                        let casted = l.cast(&r);

                        match casted {
                            Ok(dt) => {
                                *expr.typ.borrow_mut() = Some(dt.clone());
                                Some(dt)
                            }
                            Err(err) => {
                                self.error(&Expr::BinaryExpr(expr.clone()), err.as_str());
                                Some(Void)
                            }
                        }
                    }
                }
                "==" | "!=" => {
                    let t = Some(Boolean);
                    *expr.typ.borrow_mut() = t.clone();
                    return t;
                }
                ">" | "<" | ">=" | "<=" => {
                    let isntnum_l = !(l == Integer || l == Decimal);
                    let isntnum_r = !(r == Integer || r == Decimal);

                    if isntnum_l && !isntnum_r {
                        self.error(
                            &*expr.left,
                            format!(
                                "Attempting to use non-numerical types in \"{}\" comparison operation.",
                                op
                            )
                            .as_str(),
                        );
                    } else if !isntnum_l && isntnum_r {
                        self.error(
                            &*expr.right,
                            format!(
                                "Attempting to use non-numerical types in \"{}\" comparison operation.",
                                op
                            )
                            .as_str(),
                        );
                    } else if isntnum_l && isntnum_r {
                        self.error(
                            &Expr::BinaryExpr(expr.clone()),
                            format!(
                                "Attempting to use non-numerical types in \"{}\" comparison operation.",
                                op
                            )
                            .as_str(),
                        )
                    }
                    let casted = l.cast(&r);

                    match casted {
                        Ok(_) => {
                            let t: Option<Datatype> = Some(Boolean);
                            *expr.typ.borrow_mut() = t.clone();
                            return t;
                        }
                        Err(err) => {
                            self.error(&Expr::BinaryExpr(expr.clone()), err.as_str());
                            Some(Void)
                        }
                    }
                }
                "<<" | ">>" | "&" | "|" | "!" | "^" => {
                    let side1 = l.cast(&Integer);
                    let side2 = r.cast(&Integer);

                    match (side1, side2) {
                        (Ok(a), Ok(b)) => {
                            if a != Integer {
                                self.error(
                                    &*expr.left,
                                    format!("Expected an Integer here, not {}.", a).as_str(),
                                );
                            }

                            if a != b {
                                self.error(
                                    &*expr.right,
                                    format!("Expected an Integer here, not {}.", b).as_str(),
                                );
                            }
                        }
                        (Err(err), _) => {
                            self.error(&*expr.left, err.as_str());
                        }
                        (Ok(_), Err(err)) => {
                            self.error(&*expr.right, err.as_str());
                        }
                    }
                    let t = Some(Integer);
                    *expr.typ.borrow_mut() = t.clone();
                    return t;
                }
                _ => Some(Boolean),
            }
        } else {
            return Some(Void);
        }
    }

    fn check_array(&mut self, expr: &ArrayExpr) -> Dt {
        let mut types: Vec<Datatype> = Vec::new();

        for e in &expr.elements {
            let dt: Option<Datatype> = self.check(&self.expr2stmt(e.clone())).clone();
            if !(types.contains(&dt.clone()?)) {
                types.push(dt.clone()?);
            }
        }

        if types.len() == 1 {
            return Some(Array(Box::new(types[0].clone())));
        } else {
            self.error(
                &Expr::ArrayExpr(expr.to_owned()),
                "Arrays can't be multitype!",
            );
            Some(Void)
        }
    }

    fn pushbody(&mut self, body: &mut Vec<Stmt>, name: String, rettype: Datatype) {
        self.func_id += 1;
        self.bodies
            .push((self.func_id, Some(rettype), body.to_vec()));
        self.funcnames.push(name);
        self.current_body = self.bodies[self.bodies.len() - 1].clone();
    }

    fn popbody(&mut self) {
        self.func_id -= 1;
        self.bodies.pop();
        self.funcnames.pop();
        self.current_body = self.bodies[self.bodies.len() - 1].clone();
    }

    pub fn pushns(&mut self) {
        let new_ns: Namespace = Namespace::new_inheriting(Some(Rc::new(RefCell::new(
            self.namespaces[self.current_namespace_index].clone(),
        ))));
        self.namespaces.push(new_ns);
        self.current_namespace_index = self.namespaces.len() - 1;
    }

    pub fn popns(&mut self) {
        if self.namespaces.len() > 1 {
            self.namespaces.pop();
            self.current_namespace_index = self.namespaces.len() - 1;
        }
    }

    pub fn current_namespace(&self) -> &Namespace {
        &self.namespaces[self.current_namespace_index]
    }

    pub fn current_namespace_mut(&mut self) -> &mut Namespace {
        &mut self.namespaces[self.current_namespace_index]
    }

    fn check_label(&mut self, declt: &Box<FunctionDeclaration>) -> Dt {
        let decl: FunctionDeclaration = *declt.clone();
        let mut params: Vec<(String, Option<Datatype>)> = Vec::new();
        let mut body: Vec<Stmt> = decl.body;
        for (_size, name, t) in decl.parameters {
            params.push((name, Some(t)));
        }
        let ft: Option<Datatype> = Some(Function((
            params.clone(),
            (decl.return_size, Box::new(decl.return_type.clone())),
        )));

        self.newvar(decl.name.clone(), ft.clone(), true);

        self.pushbody(&mut body, decl.name.clone(), decl.return_type.clone());
        self.pushns();

        for (name, t) in params.clone() {
            self.namespace.newvar(name, t.unwrap(), false).ok();
        }

        return ft.clone();
    }

    fn finalize_label(&mut self) -> Dt {
        let lastid = self.func_id;
        self.popns();
        self.popbody();
        if lastid == self.func_id {
            panic!("For some reason, the scope wasn't popped");
        }

        None
    }

    fn check_if(&mut self, stat: &Box<IfStmt>) -> Dt {
        let stmt: IfStmt = *stat.clone();

        let dt: Datatype = self.check(&self.expr2stmt(*stmt.test.clone())).unwrap();

        match dt.cast(&Boolean) {
            Ok(casted) => {
                if casted != Boolean {
                    self.error(
                        &*stmt.test,
                        "If's test expressions must to be boolean expressions.",
                    );
                }
            }
            Err(error) => {
                self.error(&*stmt.test, &error.as_str());
            }
        }

        self.pushns();
        for s in stmt.consequent {
            self.check(&s);
        }
        self.popns();
        if let Some(alt) = stmt.alternate {
            self.pushns();
            for s in alt {
                self.check(&s);
            }
            self.popns();
        }

        Some(Void)
    }

    fn check_call(&mut self, call: &CallExpr) -> Dt {
        let caller_t = self.check(&self.expr2stmt(*call.clone().caller));
        let mut rt: Dt = None;
        if let Some(ct) = caller_t {
            if let Function((params, rettype)) = ct {
                let fplen = params.len();
                if &call.clone().args.len() != &fplen {
                    self.error(
                        &Expr::CallExpr(call.clone()),
                        format!(
                            "Expected {} arguments here, but found {}.",
                            fplen,
                            call.clone().args.len()
                        )
                        .as_str(),
                    );
                }

                for (i, arg) in call.clone().args.into_iter().enumerate() {
                    let at: Datatype = self.check(&self.expr2stmt(*arg.clone())).unwrap_or(Void);

                    let p: &Datatype = params[i].1.as_ref().unwrap();

                    if !(at == *p || at.cast(&p).unwrap_or(_NOTYPE) == *p) {
                        self.error(
                            &*arg,
                            format!(
                                "Expected type {} here for argument \"{}\", but found {}.",
                                p, params[i].0, at
                            )
                            .as_str(),
                        )
                    }
                }

                rt = Some(*rettype.1);
            }
        }

        rt
    }

    fn check_enum(&mut self, enm: &ast::Enum) -> Dt {
        let et: Option<Datatype> = Some(Datatype::Enum(enm.name.clone(), enm.items.to_owned()));
        self.newvar(enm.name.to_owned(), et.clone(), true);
        return et;
    }

    fn check_asmpiece(&mut self, stmt: &AsmStmt) -> Dt {
        for l in &stmt.code {
            let dt: Option<Datatype> = self.check(&self.expr2stmt(l.clone()));
            if let Some(t) = dt {
                if t != Text {
                    self.error(&l, format!("Expected Text in assembly statments, but found {}.... %%g(please God forgive this developer, he's a imperfect human being that don't know write assembly).%%!", t).as_str());
                }
            }
        }

        Some(Void)
    }

     fn check_while(&mut self, stat: &WhileStmt) -> Dt {
        let dt = self.check(&self.expr2stmt(stat.condition.clone()));
        let isboolean: bool = dt.clone().is_some() && (dt.clone().unwrap() == Boolean);
        if !isboolean {
            self.error(&stat.condition, format!("Expected boolean expression here, but found a expression of type \"{}\"", dt.unwrap()).as_str());
        }

        self.in_loop = true;

        for s in &stat.body {
            self.check(&s);
        }

        self.in_loop = false;

        Some(Void)
    }
}

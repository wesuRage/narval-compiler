use crate::datatype::Datatype;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeType {
    Program,
    Stmt,
    ImportStmt,
    ExportStmt,
    IfStmt,
    ReturnStmt,
    AsmStmt,
    MovStmt,
    LoopStmt,
    ForStmt,
    WhileStmt,

    Identifier,
    VoidLiteral,
    NumericLiteral,
    StringLiteral,
    Property,
    BooleanLiteral,
    ObjectLiteral,
    TupleLiteral,
    Enum,

    VarDeclaration,
    FunctionDeclaration,
    ClassDeclaration,
    ClassVarDeclaration,
    ClassFunctionDeclaration,

    AssignmentExpr,
    BinaryExpr,
    MemberExpr,
    CallExpr,
    TernaryExpr,
    BlockExpr,
    BreakExpr,
    ContinueExpr,
    ArrayExpr,
    ArrayAccess,
    PreIncrementExpr,
    PreDecrementExpr,
    PostIncrementExpr,
    PostDecrementExpr,
    LogicalNotExpr,
    UnaryMinusExpr,
    UnaryBitwiseNotExpr,
    RangeExpr,
    _EOL,
}

#[derive(Debug, Clone)]
pub enum Expr {
    ImportStmt(ImportStmt),
    ExportStmt(ExportStmt),
    IfStmt(Box<IfStmt>),
    AsmStmt(AsmStmt),
    MovStmt(MovStmt),
    LoopStmt(LoopStmt),
    ForStmt(ForStmt),
    WhileStmt(Box<WhileStmt>),

    Identifier(Identifier),
    VoidLiteral(VoidLiteral),
    NumericLiteral(NumericLiteral),
    StringLiteral(StringLiteral),
    ObjectLiteral(ObjectLiteral),
    TupleLiteral(TupleLiteral),
    BooleanLiteral(BooleanLiteral),
    Enum(Enum),

    VarDeclaration(VarDeclaration),
    FunctionDeclaration(Box<FunctionDeclaration>),
    ClassDeclaration(Box<ClassDeclaration>),
    ClassVarDeclaration(Box<ClassVarDeclaration>),
    ClassFunctionDeclaration(Box<ClassFunctionDeclaration>),

    AssignmentExpr(AssignmentExpr),
    BinaryExpr(BinaryExpr),
    MemberExpr(MemberExpr),
    CallExpr(CallExpr),
    TernaryExpr(TernaryExpr),
    BlockExpr(Box<BlockExpr>),
    BreakExpr(BreakExpr),
    ContinueExpr(ContinueExpr),
    ArrayExpr(ArrayExpr),
    ArrayAccess(ArrayAccess),
    LogicalNotExpr(LogicalNotExpr),
    UnaryMinusExpr(UnaryMinusExpr),
    PreIncrementExpr(PreIncrementExpr),
    PreDecrementExpr(PreDecrementExpr),
    PostIncrementExpr(PostIncrementExpr),
    PostDecrementExpr(PostDecrementExpr),
    RangeExpr(Box<RangeExpr>),
    UnaryBitwiseNotExpr(UnaryBitwiseNotExpr),
    _EOL(_EOL),
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::BinaryExpr(b1), Expr::BinaryExpr(b2)) => {
                if b1.left == b2.left && b1.operator == b2.operator && b1.right == b2.right {
                    true
                } else {
                    false
                }
            }
            (Expr::BooleanLiteral(b1), Expr::BooleanLiteral(b2)) => {
                if b1.value == b2.value {
                    true
                } else {
                    false
                }
            }
            (Expr::Identifier(i1), Expr::Identifier(i2)) => {
                if i1.symbol == i2.symbol {
                    true
                } else {
                    false
                }
            }
            (Expr::NumericLiteral(n1), Expr::NumericLiteral(n2)) => {
                if n1.value == n2.value {
                    true
                } else {
                    false
                }
            }
            (Expr::StringLiteral(s1), Expr::StringLiteral(s2)) => {
                if s1.value == s2.value {
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

impl Expr {
    pub fn kind(&self) -> NodeType {
        match self {
            Expr::ArrayAccess(_) => NodeType::ArrayAccess,
            Expr::ArrayExpr(_) => NodeType::ArrayExpr,
            Expr::AsmStmt(_) => NodeType::AsmStmt,
            Expr::AssignmentExpr(_) => NodeType::AssignmentExpr,
            Expr::BinaryExpr(_) => NodeType::BinaryExpr,
            Expr::BlockExpr(_) => NodeType::BlockExpr,
            Expr::BooleanLiteral(_) => NodeType::BooleanLiteral,
            Expr::BreakExpr(_) => NodeType::BreakExpr,
            Expr::CallExpr(_) => NodeType::CallExpr,
            Expr::ContinueExpr(_) => NodeType::ContinueExpr,
            Expr::Enum(_) => NodeType::Enum,
            Expr::ExportStmt(_) => NodeType::ExportStmt,
            Expr::ForStmt(_) => NodeType::ForStmt,
            Expr::FunctionDeclaration(_) => NodeType::FunctionDeclaration,
            Expr::Identifier(_) => NodeType::Identifier,
            Expr::IfStmt(_) => NodeType::IfStmt,
            Expr::ImportStmt(_) => NodeType::ImportStmt,
            Expr::LogicalNotExpr(_) => NodeType::LogicalNotExpr,
            Expr::LoopStmt(_) => NodeType::LoopStmt,
            Expr::MemberExpr(_) => NodeType::MemberExpr,
            Expr::MovStmt(_) => NodeType::MovStmt,
            Expr::VoidLiteral(_) => NodeType::VoidLiteral,
            Expr::NumericLiteral(_) => NodeType::NumericLiteral,
            Expr::ObjectLiteral(_) => NodeType::ObjectLiteral,
            Expr::PostDecrementExpr(_) => NodeType::PostDecrementExpr,
            Expr::PostIncrementExpr(_) => NodeType::PostIncrementExpr,
            Expr::PreDecrementExpr(_) => NodeType::PreDecrementExpr,
            Expr::PreIncrementExpr(_) => NodeType::PreIncrementExpr,
            Expr::RangeExpr(_) => NodeType::RangeExpr,
            Expr::StringLiteral(_) => NodeType::StringLiteral,
            Expr::TernaryExpr(_) => NodeType::TernaryExpr,
            Expr::TupleLiteral(_) => NodeType::TupleLiteral,
            Expr::UnaryBitwiseNotExpr(_) => NodeType::UnaryBitwiseNotExpr,
            Expr::UnaryMinusExpr(_) => NodeType::UnaryMinusExpr,
            Expr::ClassDeclaration(_) => NodeType::ClassDeclaration,
            Expr::ClassFunctionDeclaration(_) => NodeType::ClassFunctionDeclaration,
            Expr::ClassVarDeclaration(_) => NodeType::ClassVarDeclaration,
            Expr::VarDeclaration(_) => NodeType::VarDeclaration,
            Expr::WhileStmt(_) => NodeType::WhileStmt,
            Expr::_EOL(_) => NodeType::_EOL,
        }
    }

    pub fn local(&self) -> ((usize, usize), (usize, usize), usize) {
        match self {
            Expr::ArrayAccess(e) => (e.position, e.column, e.lineno),
            Expr::ArrayExpr(e) => (e.position, e.column, e.lineno),
            Expr::AsmStmt(e) => (e.position, e.column, e.lineno),
            Expr::AssignmentExpr(e) => (e.position, e.column, e.lineno),
            Expr::BinaryExpr(e) => (e.position, e.column, e.lineno),
            Expr::BlockExpr(e) => (e.position, e.column, e.lineno),
            Expr::BooleanLiteral(e) => (e.position, e.column, e.lineno),
            Expr::BreakExpr(e) => (e.position, e.column, e.lineno),
            Expr::CallExpr(e) => (e.position, e.column, e.lineno),
            Expr::ContinueExpr(e) => (e.position, e.column, e.lineno),
            Expr::Enum(e) => (e.position, e.column, e.lineno),
            Expr::ExportStmt(e) => (e.position, e.column, e.lineno),
            Expr::ForStmt(e) => (e.position, e.column, e.lineno),
            Expr::FunctionDeclaration(e) => (e.position, e.column, e.lineno),
            Expr::Identifier(e) => (e.position, e.column, e.lineno),
            Expr::IfStmt(e) => (e.position, e.column, e.lineno),
            Expr::ImportStmt(e) => (e.position, e.column, e.lineno),
            Expr::LogicalNotExpr(e) => (e.position, e.column, e.lineno),
            Expr::LoopStmt(e) => (e.position, e.column, e.lineno),
            Expr::MemberExpr(e) => (e.position, e.column, e.lineno),
            Expr::MovStmt(e) => (e.position, e.column, e.lineno),
            Expr::VoidLiteral(e) => (e.position, e.column, e.lineno),
            Expr::NumericLiteral(e) => (e.position, e.column, e.lineno),
            Expr::ObjectLiteral(e) => (e.position, e.column, e.lineno),
            Expr::PostDecrementExpr(e) => (e.position, e.column, e.lineno),
            Expr::PostIncrementExpr(e) => (e.position, e.column, e.lineno),
            Expr::PreDecrementExpr(e) => (e.position, e.column, e.lineno),
            Expr::PreIncrementExpr(e) => (e.position, e.column, e.lineno),
            Expr::RangeExpr(e) => (e.position, e.column, e.lineno),
            Expr::StringLiteral(e) => (e.position, e.column, e.lineno),
            Expr::TernaryExpr(e) => (e.position, e.column, e.lineno),
            Expr::TupleLiteral(e) => (e.position, e.column, e.lineno),
            Expr::UnaryBitwiseNotExpr(e) => (e.position, e.column, e.lineno),
            Expr::UnaryMinusExpr(e) => (e.position, e.column, e.lineno),
            Expr::ClassDeclaration(e) => (e.position, e.column, e.lineno),
            Expr::ClassFunctionDeclaration(e) => (e.position, e.column, e.lineno),
            Expr::ClassVarDeclaration(e) => (e.position, e.column, e.lineno),
            Expr::VarDeclaration(e) => (e.position, e.column, e.lineno),
            Expr::WhileStmt(e) => (e.position, e.column, e.lineno),
            Expr::_EOL(_) => ((0, 0), (0, 0), 0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub kind: NodeType,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct _EOL {
    pub kind: NodeType,
}

#[derive(Debug, Clone)]
pub struct UnaryBitwiseNotExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
    pub typ: Option<Datatype>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub kind: NodeType,
    pub start: Expr,
    pub range: String,
    pub end: Expr,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ClassDeclaration {
    pub kind: NodeType,
    pub name: String,
    pub super_class: Option<String>,
    pub body: Vec<Stmt>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ClassFunctionDeclaration {
    pub kind: NodeType,
    pub access_modifier: String,
    pub function: Expr,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ClassVarDeclaration {
    pub kind: NodeType,
    pub access_modifier: String,
    pub var: Stmt,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub kind: NodeType,
    pub name: String,
    pub items: Vec<(String, i32)>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct MovStmt {
    pub kind: NodeType,
    pub values: Vec<(String, String)>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub kind: NodeType,
    pub value: String,
    pub typ: Option<Datatype>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct PreIncrementExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct PreDecrementExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct PostIncrementExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct PostDecrementExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct UnaryMinusExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct LogicalNotExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
    pub typ: Option<Datatype>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub kind: NodeType,
    pub items: Vec<String>,
    pub sequence: Box<Expr>,
    pub body: Vec<Stmt>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct LoopStmt {
    pub kind: NodeType,
    pub body: Vec<Stmt>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct BreakExpr {
    pub kind: NodeType,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ContinueExpr {
    pub kind: NodeType,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: NodeType,
    pub expr: Option<Expr>,
    pub return_stmt: Option<ReturnStmt>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub kind: NodeType,
    pub argument: Option<Expr>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub kind: NodeType,
    pub symbol: String,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
            && self.column == other.column
            && self.position == other.position
            && self.lineno == other.lineno
    }
}

impl Eq for Identifier {}

impl Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
        self.column.hash(state);
        self.position.hash(state);
        self.lineno.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub kind: NodeType,
    pub value: String,
    pub typ: Option<Datatype>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

impl PartialEq for StringLiteral {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
            && self.column == other.column
            && self.position == other.position
            && self.lineno == other.lineno
    }
}

impl Eq for StringLiteral {}

impl Hash for StringLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
        self.column.hash(state);
        self.position.hash(state);
        self.lineno.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct VoidLiteral {
    pub kind: NodeType,
    pub value: &'static str,
    pub typ: Option<Datatype>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct NumericLiteral {
    pub kind: NodeType,
    pub value: String,
    pub typ: Option<Datatype>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct TupleLiteral {
    pub kind: NodeType,
    pub value: Vec<Expr>,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct Property {
    pub kind: NodeType,
    pub key: String,
    pub value: Option<Box<Expr>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ObjectLiteral {
    pub kind: NodeType,
    pub properties: Vec<Property>,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub kind: NodeType,
    pub condition: Expr,
    pub body: Vec<Stmt>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub kind: NodeType,
    pub constant: bool,
    pub data_size: String,
    pub data_type: Datatype,
    pub identifier: Option<String>,
    pub inferred: bool,
    pub value: Box<Expr>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub kind: NodeType,
    pub return_size: String,
    pub return_type: Datatype,
    pub name: String,
    pub parameters: Vec<(String, String, Datatype)>,
    pub body: Vec<Stmt>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub kind: NodeType,
    pub assigne: Box<Expr>,
    pub value: Box<Expr>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub kind: NodeType,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: String,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct MemberExpr {
    pub kind: NodeType,
    pub object: Box<Expr>,
    pub property: Box<Expr>,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub kind: NodeType,
    pub args: Vec<Box<Expr>>,
    pub caller: Box<Expr>,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub kind: NodeType,
    pub paths: HashMap<StringLiteral, Option<Identifier>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ExportStmt {
    pub kind: NodeType,
    pub statement: Box<Stmt>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub kind: NodeType,
    pub test: Box<Expr>,
    pub consequent: Vec<Stmt>,
    pub alternate: Option<Vec<Stmt>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct TernaryExpr {
    pub kind: NodeType,
    pub condition: Box<Expr>,
    pub consequent: Box<Expr>,
    pub alternate: Box<Expr>,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub kind: NodeType,
    pub statements: Vec<Stmt>,
    pub position: (usize, usize),
    pub column: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct AsmStmt {
    pub kind: NodeType,
    pub code: Vec<Expr>,
    pub position: (usize, usize),
    pub column: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ArrayExpr {
    pub kind: NodeType,
    pub elements: Vec<Expr>,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct ArrayAccess {
    pub kind: NodeType,
    pub array: Box<Expr>,
    pub index: Box<Expr>,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

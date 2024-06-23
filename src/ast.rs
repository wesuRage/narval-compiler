use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Program,
    Stmt,
    ImportStmt,
    ExportStmt,
    IfStmt,
    ReturnStmt,
    AsmStmt,
    LoopStmt,
    ForStmt,

    Identifier,
    NullLiteral,
    UndefinedLiteral,
    NumericLiteral,
    StringLiteral,
    Property,
    ObjectLiteral,
    TrueLiteral,
    FalseLiteral,

    VarDeclaration,
    FunctionDeclaration,
    UnitDeclaration,
    UnitVarDeclaration,
    UnitFunctionDeclaration,

    AssignmentExpr,
    BinaryExpr,
    MemberExpr,
    CallExpr,
    TernaryExpr,
    BlockExpr,
    BreakExpr,
    ArrayExpr,
    ArrayAccess,
    PreIncrementExpr,
    PreDecrementExpr,
    PostIncrementExpr,
    PostDecrementExpr,
    LogicalNotExpr,
    UnaryMinusExpr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    ImportStmt(ImportStmt),
    ExportStmt(ExportStmt),
    IfStmt(Box<IfStmt>),
    AsmStmt(AsmStmt),
    LoopStmt(LoopStmt),
    ForStmt(ForStmt),

    Identifier(Identifier),
    NullLiteral(NullLiteral),
    UndefinedLiteral(UndefinedLiteral),
    NumericLiteral(NumericLiteral),
    StringLiteral(StringLiteral),
    ObjectLiteral(ObjectLiteral),
    TrueLiteral(TrueLiteral),
    FalseLiteral(FalseLiteral),

    VarDeclaration(VarDeclaration),
    FunctionDeclaration(Box<FunctionDeclaration>),
    UnitDeclaration(Box<UnitDeclaration>),
    UnitVarDeclaration(Box<UnitVarDeclaration>),
    UnitFunctionDeclaration(Box<UnitFunctionDeclaration>),

    AssignmentExpr(AssignmentExpr),
    BinaryExpr(BinaryExpr),
    MemberExpr(MemberExpr),
    CallExpr(CallExpr),
    TernaryExpr(TernaryExpr),
    BlockExpr(Box<BlockExpr>),
    BreakExpr(BreakExpr),
    ArrayExpr(ArrayExpr),
    ArrayAccess(ArrayAccess),
    LogicalNotExpr(LogicalNotExpr),
    UnaryMinusExpr(UnaryMinusExpr),
    PreIncrementExpr(PreIncrementExpr),
    PreDecrementExpr(PreDecrementExpr),
    PostIncrementExpr(PostIncrementExpr),
    PostDecrementExpr(PostDecrementExpr),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub kind: NodeType,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct UnitDeclaration {
    pub kind: NodeType,
    pub name: String,
    pub super_units: Option<Vec<String>>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct UnitFunctionDeclaration {
    pub kind: NodeType,
    pub access_modifier: String,
    pub return_size: String,
    pub return_type: String,
    pub name: String,
    pub parameters: Vec<(String, String, String)>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct UnitVarDeclaration {
    pub kind: NodeType,
    pub access_modifier: String,
    pub constant: bool,
    pub data_size: String,
    pub data_type: String,
    pub identifier: String,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct TrueLiteral {
    pub kind: NodeType,
}

#[derive(Debug, Clone)]
pub struct FalseLiteral {
    pub kind: NodeType,
}

#[derive(Debug, Clone)]
pub struct PreIncrementExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct PreDecrementExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct PostIncrementExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct PostDecrementExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnaryMinusExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct LogicalNotExpr {
    pub kind: NodeType,
    pub operand: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub kind: NodeType,
    pub items: Vec<String>,
    pub sequence: Vec<Expr>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct LoopStmt {
    pub kind: NodeType,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct BreakExpr {
    pub kind: NodeType,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: NodeType,
    pub expr: Option<Expr>,
    pub return_stmt: Option<ReturnStmt>,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub kind: NodeType,
    pub argument: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub kind: NodeType,
    pub symbol: String,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub kind: NodeType,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct NullLiteral {
    pub kind: NodeType,
    pub value: &'static str,
}

#[derive(Debug, Clone)]
pub struct UndefinedLiteral {
    pub kind: NodeType,
    pub value: &'static str,
}

#[derive(Debug, Clone)]
pub struct NumericLiteral {
    pub kind: NodeType,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Property {
    pub kind: NodeType,
    pub key: String,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct ObjectLiteral {
    pub kind: NodeType,
    pub properties: Vec<Property>,
}

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub kind: NodeType,
    pub constant: bool,
    pub data_size: String,
    pub data_type: String,
    pub identifier: String,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub kind: NodeType,
    pub return_size: String,
    pub return_type: String,
    pub name: String,
    pub parameters: Vec<(String, String, String)>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub kind: NodeType,
    pub assigne: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub kind: NodeType,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: String,
}

#[derive(Debug, Clone)]
pub struct MemberExpr {
    pub kind: NodeType,
    pub object: Box<Expr>,
    pub property: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub kind: NodeType,
    pub args: Vec<Box<Expr>>,
    pub caller: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub kind: NodeType,
    pub paths: HashMap<String, Option<String>>,
}

#[derive(Debug, Clone)]
pub struct ExportStmt {
    pub kind: NodeType,
    pub identifiers: Vec<Identifier>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub kind: NodeType,
    pub test: Box<Expr>,
    pub consequent: Vec<Stmt>,
    pub alternate: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct TernaryExpr {
    pub kind: NodeType,
    pub condition: Box<Expr>,
    pub consequent: Box<Expr>,
    pub alternate: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub kind: NodeType,
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct AsmStmt {
    pub kind: NodeType,
    pub code: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct ArrayExpr {
    pub kind: NodeType,
    pub elements: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct ArrayAccess {
    pub kind: NodeType,
    pub array: Box<Expr>,
    pub index: Box<Expr>,
}

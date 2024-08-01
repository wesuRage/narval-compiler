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
    RangeExpr(Box<RangeExpr>),
    UnaryBitwiseNotExpr(UnaryBitwiseNotExpr),
    _EOL(_EOL),
}

// Implementação de métodos para a enumeração Expr
impl Expr {
    // Método que retorna o tipo do nó da árvore de sintaxe abstrata (AST)
    pub fn kind(&self) -> NodeType {
        // O match é usado para verificar o tipo de expressão e retornar o tipo correspondente
        match self {
            Expr::ArrayAccess(_) => NodeType::ArrayAccess, // Se for um array access
            Expr::ArrayExpr(_) => NodeType::ArrayExpr,     // Se for um array
            Expr::AsmStmt(_) => NodeType::AsmStmt,         // Se for um statement de assembly
            Expr::AssignmentExpr(_) => NodeType::AssignmentExpr, // Se for uma expressão de atribuição
            Expr::BinaryExpr(_) => NodeType::BinaryExpr,         // Se for uma expressão binária
            Expr::BlockExpr(_) => NodeType::BlockExpr,           // Se for uma expressão de bloco
            Expr::BooleanLiteral(_) => NodeType::BooleanLiteral, // Se for uma expressão booleana
            Expr::BreakExpr(_) => NodeType::BreakExpr,           // Se for um break
            Expr::CallExpr(_) => NodeType::CallExpr,             // Se for uma chamada de função
            Expr::Enum(_) => NodeType::Enum,                     // Se for um enum
            Expr::ExportStmt(_) => NodeType::ExportStmt, // Se for uma declaração de exportação
            Expr::ForStmt(_) => NodeType::ForStmt,       // Se for um for statement
            Expr::FunctionDeclaration(_) => NodeType::FunctionDeclaration, // Se for uma declaração de função
            Expr::Identifier(_) => NodeType::Identifier, // Se for um identificador
            Expr::IfStmt(_) => NodeType::IfStmt, // Se for uma declaração de condicional 'if'
            Expr::ImportStmt(_) => NodeType::ImportStmt, // Se for uma declaração de importação
            Expr::LogicalNotExpr(_) => NodeType::LogicalNotExpr, // Se for uma expressão como !x
            Expr::LoopStmt(_) => NodeType::LoopStmt, // Se for um loop statement
            Expr::MemberExpr(_) => NodeType::MemberExpr, // Se for uma expressão de membro
            Expr::MovStmt(_) => NodeType::MovStmt, // Se for um statement de mov em assembly
            Expr::VoidLiteral(_) => NodeType::VoidLiteral, // Se for um literal nulo
            Expr::NumericLiteral(_) => NodeType::NumericLiteral, // Se for um literal numérico
            Expr::ObjectLiteral(_) => NodeType::ObjectLiteral, // Se for um literal de objeto
            Expr::PostDecrementExpr(_) => NodeType::PostDecrementExpr, // Se for uma expressão como x--
            Expr::PostIncrementExpr(_) => NodeType::PostIncrementExpr, // Se for uma expressão como x++
            Expr::PreDecrementExpr(_) => NodeType::PreDecrementExpr, // Se for uma expressão como --x
            Expr::PreIncrementExpr(_) => NodeType::PreIncrementExpr, // Se for uma expressão como ++x
            Expr::RangeExpr(_) => NodeType::RangeExpr, // Se for uma expressão de intervalo ou intervalo incluso
            Expr::StringLiteral(_) => NodeType::StringLiteral, // Se for um literal de string
            Expr::TernaryExpr(_) => NodeType::TernaryExpr, // Se for uma expressão ternária
            Expr::TupleLiteral(_) => NodeType::TupleLiteral, // Se for uma tupla
            Expr::UnaryBitwiseNotExpr(_) => NodeType::UnaryBitwiseNotExpr, // Se for uma expressão de bitwise not
            Expr::UnaryMinusExpr(_) => NodeType::UnaryMinusExpr, // Se for uma expressão como -x
            Expr::UnitDeclaration(_) => NodeType::UnitDeclaration, // Se for uma declaração de unit
            Expr::UnitFunctionDeclaration(_) => NodeType::UnitFunctionDeclaration, // Se for uma declaração de função dentro de uma unit
            Expr::UnitVarDeclaration(_) => NodeType::UnitVarDeclaration, // Se for uma declaração de variável dentro de uma unit
            Expr::VarDeclaration(_) => NodeType::VarDeclaration, // Se for uma declaração de variável
            Expr::WhileStmt(_) => NodeType::WhileStmt,           // Se for um while statement
            Expr::_EOL(_) => NodeType::_EOL,
        }
    }

    pub fn local(&self) -> ((usize, usize), (usize, usize), usize) {
        match self {
            Expr::ArrayAccess(e) => (e.position, e.column, e.lineno), // Se for um array access
            Expr::ArrayExpr(e) => (e.position, e.column, e.lineno),   // Se for um array
            Expr::AsmStmt(e) => (e.position, e.column, e.lineno), // Se for um statement de assembly
            Expr::AssignmentExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão de atribuição
            Expr::BinaryExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão binária
            Expr::BlockExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão de bloco
            Expr::BooleanLiteral(e) => (e.position, e.column, e.lineno), // Se for uma expressão booleana
            Expr::BreakExpr(e) => (e.position, e.column, e.lineno),      // Se for um break
            Expr::CallExpr(e) => (e.position, e.column, e.lineno), // Se for uma chamada de função
            Expr::Enum(e) => (e.position, e.column, e.lineno),     // Se for um enum
            Expr::ExportStmt(e) => (e.position, e.column, e.lineno), // Se for uma declaração de exportação
            Expr::ForStmt(e) => (e.position, e.column, e.lineno),    // Se for um for statement
            Expr::FunctionDeclaration(e) => (e.position, e.column, e.lineno), // Se for uma declaração de função
            Expr::Identifier(e) => (e.position, e.column, e.lineno), // Se for um identificador
            Expr::IfStmt(e) => (e.position, e.column, e.lineno), // Se for uma declaração de condicional 'if'
            Expr::ImportStmt(e) => (e.position, e.column, e.lineno), // Se for uma declaração de importação
            Expr::LogicalNotExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão como !x
            Expr::LoopStmt(e) => (e.position, e.column, e.lineno),       // Se for um loop statement
            Expr::MemberExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão de membro
            Expr::MovStmt(e) => (e.position, e.column, e.lineno), // Se for um statement de mov em assembly
            Expr::VoidLiteral(e) => (e.position, e.column, e.lineno), // Se for um literal nulo
            Expr::NumericLiteral(e) => (e.position, e.column, e.lineno), // Se for um literal numérico
            Expr::ObjectLiteral(e) => (e.position, e.column, e.lineno), // Se for um literal de objeto
            Expr::PostDecrementExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão como x--
            Expr::PostIncrementExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão como x++
            Expr::PreDecrementExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão como --x
            Expr::PreIncrementExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão como ++x
            Expr::RangeExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão de intervalo ou intervalo incluso
            Expr::StringLiteral(e) => (e.position, e.column, e.lineno), // Se for um literal de string
            Expr::TernaryExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão ternária
            Expr::TupleLiteral(e) => (e.position, e.column, e.lineno), // Se for uma tupla
            Expr::UnaryBitwiseNotExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão de bitwise not
            Expr::UnaryMinusExpr(e) => (e.position, e.column, e.lineno), // Se for uma expressão como -x
            Expr::UnitDeclaration(e) => (e.position, e.column, e.lineno), // Se for uma declaração de unit
            Expr::UnitFunctionDeclaration(e) => (e.position, e.column, e.lineno), // Se for uma declaração de função dentro de uma unit
            Expr::UnitVarDeclaration(e) => (e.position, e.column, e.lineno), // Se for uma declaração de variável dentro de uma unit
            Expr::VarDeclaration(e) => (e.position, e.column, e.lineno), // Se for uma declaração de variável
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
pub struct UnitDeclaration {
    pub kind: NodeType,
    pub name: String,
    pub super_units: Option<Vec<String>>,
    pub body: Vec<Stmt>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct UnitFunctionDeclaration {
    pub kind: NodeType,
    pub access_modifier: String,
    pub function: Expr,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

#[derive(Debug, Clone)]
pub struct UnitVarDeclaration {
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
    pub array: Box<Expr>, // mano ce vai usar aquela função hash ainda? responde no zap
    pub index: Box<Expr>,
    pub typ: RefCell<Option<Datatype>>,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub lineno: usize,
}

//! Abstract Syntax Tree definitions for seq-actor
//!
//! The AST represents the parsed structure of a seq-actor program.

use std::fmt;

/// Source location for error reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line.min(other.line),
            column: if self.line <= other.line {
                self.column
            } else {
                other.column
            },
        }
    }
}

/// A node with source location
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

/// Top-level program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

/// Top-level items
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    /// Type definition: `type Name = ...`
    TypeDef(TypeDef),
    /// Function definition: `: name (params) -> body ;`
    FuncDef(FuncDef),
    /// Include directive: `include "path"` or `include std:name`
    Include(Include),
}

/// Type definition
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub variants: Vec<Variant>,
    pub span: Span,
}

/// Variant of an algebraic data type
#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Field>,
    pub span: Span,
}

/// Field in a variant
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Option<String>,
    pub typ: TypeExpr,
    pub span: Span,
}

/// Type expressions
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    /// Named type: `Int`, `String`, `Option[T]`
    Named {
        name: String,
        args: Vec<TypeExpr>,
        span: Span,
    },
    /// Actor type: `Actor[M]`
    Actor {
        message_type: Box<TypeExpr>,
        span: Span,
    },
    /// Tuple type: `(Int, String)`
    Tuple { elements: Vec<TypeExpr>, span: Span },
    /// Function type: `(Int) -> Int`
    Function {
        params: Vec<TypeExpr>,
        ret: Box<TypeExpr>,
        span: Span,
    },
    /// Unit type: `()`
    Unit { span: Span },
}

impl TypeExpr {
    pub fn span(&self) -> Span {
        match self {
            TypeExpr::Named { span, .. } => *span,
            TypeExpr::Actor { span, .. } => *span,
            TypeExpr::Tuple { span, .. } => *span,
            TypeExpr::Function { span, .. } => *span,
            TypeExpr::Unit { span } => *span,
        }
    }
}

/// Function definition
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub clauses: Vec<FuncClause>,
    pub span: Span,
}

/// A single clause of a function (pattern -> body)
#[derive(Debug, Clone, PartialEq)]
pub struct FuncClause {
    pub params: Vec<Pattern>,
    pub guard: Option<Expr>,
    pub body: Expr,
    pub span: Span,
}

/// Patterns for matching
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard: `_`
    Wildcard { span: Span },
    /// Variable binding: `x`
    Var { name: String, span: Span },
    /// Literal: `42`, `"hello"`, `true`
    Literal { value: Literal, span: Span },
    /// Tuple pattern: `(a, b, c)`
    Tuple { elements: Vec<Pattern>, span: Span },
    /// Tagged pattern: `{tag, x, y}`
    Tagged {
        tag: String,
        elements: Vec<Pattern>,
        span: Span,
    },
    /// Constructor pattern: `Some(x)`, `None`
    Constructor {
        name: String,
        args: Vec<Pattern>,
        span: Span,
    },
    /// List pattern: `[h | t]` or `[a, b, c]`
    List {
        elements: Vec<Pattern>,
        tail: Option<Box<Pattern>>,
        span: Span,
    },
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard { span } => *span,
            Pattern::Var { span, .. } => *span,
            Pattern::Literal { span, .. } => *span,
            Pattern::Tuple { span, .. } => *span,
            Pattern::Tagged { span, .. } => *span,
            Pattern::Constructor { span, .. } => *span,
            Pattern::List { span, .. } => *span,
        }
    }
}

/// Literal values
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Unit,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(n) => write!(f, "{}", n),
            Literal::Float(n) => write!(f, "{}", n),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Unit => write!(f, "()"),
        }
    }
}

/// Supervision strategies
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SupervisionStrategy {
    #[default]
    OneForOne,
    OneForAll,
    RestForOne,
}

/// Expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literal value
    Literal { value: Literal, span: Span },

    /// Variable reference
    Var { name: String, span: Span },

    /// Binary operation
    BinOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },

    /// Unary operation
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr>,
        span: Span,
    },

    /// Function call
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },

    /// Let binding (affine)
    Let {
        pattern: Pattern,
        value: Box<Expr>,
        body: Box<Expr>,
        span: Span,
    },

    /// If expression
    If {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
        span: Span,
    },

    /// Match expression
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },

    /// Tuple construction
    Tuple { elements: Vec<Expr>, span: Span },

    /// Tagged tuple construction: `{tag, x, y}`
    Tagged {
        tag: String,
        elements: Vec<Expr>,
        span: Span,
    },

    /// Constructor call: `Some(x)`, `None`
    Constructor {
        name: String,
        args: Vec<Expr>,
        span: Span,
    },

    /// List construction: `[1, 2, 3]` or `[h | t]`
    List {
        elements: Vec<Expr>,
        tail: Option<Box<Expr>>,
        span: Span,
    },

    /// Lambda/anonymous function
    Lambda {
        params: Vec<Pattern>,
        body: Box<Expr>,
        span: Span,
    },

    /// Block of expressions (last is return value)
    Block { exprs: Vec<Expr>, span: Span },

    /// Receive block (actor)
    Receive {
        arms: Vec<ReceiveArm>,
        timeout: Option<ReceiveTimeout>,
        span: Span,
    },

    /// Become (tail-recursive state transition)
    Become {
        func: String,
        args: Vec<Expr>,
        span: Span,
    },

    /// Spawn actor
    Spawn {
        func: String,
        args: Vec<Expr>,
        span: Span,
    },

    /// Send message
    Send {
        target: Box<Expr>,
        message: Box<Expr>,
        span: Span,
    },

    /// Self reference (current actor)
    SelfRef { span: Span },

    /// Module-qualified call: `io:println("hello")`, `str:concat(a, b)`
    ModuleCall {
        module: String,
        func: String,
        args: Vec<Expr>,
        span: Span,
    },

    /// Supervised block: `supervised [strategy] ... end`
    Supervised {
        strategy: SupervisionStrategy,
        body: Vec<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal { span, .. } => *span,
            Expr::Var { span, .. } => *span,
            Expr::BinOp { span, .. } => *span,
            Expr::UnaryOp { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Let { span, .. } => *span,
            Expr::If { span, .. } => *span,
            Expr::Match { span, .. } => *span,
            Expr::Tuple { span, .. } => *span,
            Expr::Tagged { span, .. } => *span,
            Expr::Constructor { span, .. } => *span,
            Expr::List { span, .. } => *span,
            Expr::Lambda { span, .. } => *span,
            Expr::Block { span, .. } => *span,
            Expr::Receive { span, .. } => *span,
            Expr::Become { span, .. } => *span,
            Expr::Spawn { span, .. } => *span,
            Expr::Send { span, .. } => *span,
            Expr::SelfRef { span } => *span,
            Expr::ModuleCall { span, .. } => *span,
            Expr::Supervised { span, .. } => *span,
        }
    }
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Boolean
    And,
    Or,
    // List
    Cons, // |
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "and",
            BinOp::Or => "or",
            BinOp::Cons => "|",
        };
        write!(f, "{}", s)
    }
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "not",
        };
        write!(f, "{}", s)
    }
}

/// Match arm
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
    pub span: Span,
}

/// Receive arm
#[derive(Debug, Clone, PartialEq)]
pub struct ReceiveArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
    pub span: Span,
}

/// Receive timeout clause
#[derive(Debug, Clone, PartialEq)]
pub struct ReceiveTimeout {
    pub duration: Box<Expr>,
    pub body: Box<Expr>,
    pub span: Span,
}

/// Include directive
#[derive(Debug, Clone, PartialEq)]
pub struct Include {
    pub kind: IncludeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IncludeKind {
    /// Local file: `include "path/file.act"`
    File(String),
    /// Standard library: `include std:lists`
    Std(String),
    /// FFI: `include ffi:libname`
    Ffi(String),
}

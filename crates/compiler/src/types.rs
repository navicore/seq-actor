//! Type system for seq-actor
//!
//! Defines types and their relationships for type checking.

use std::collections::HashMap;
use std::fmt;

/// Type identifier (unique within a context)
pub type TypeId = usize;

/// Types in seq-actor
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Primitive types
    Int,
    Float,
    Bool,
    String,
    Unit,

    /// Type variable (for polymorphism)
    Var(TypeVar),

    /// Function type
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
    },

    /// Tuple type
    Tuple(Vec<Type>),

    /// List type
    List(Box<Type>),

    /// Map type
    Map {
        key: Box<Type>,
        value: Box<Type>,
    },

    /// Actor type
    Actor(Box<Type>), // Message type

    /// Channel type
    Channel(Box<Type>),

    /// Tagged tuple type (like Erlang atoms + tuples)
    Tagged {
        tag: String,
        fields: Vec<Type>,
    },

    /// Algebraic data type (sum type)
    Adt {
        name: String,
        type_args: Vec<Type>,
    },

    /// Type constructor (uninstantiated ADT)
    TypeConstructor {
        name: String,
        params: Vec<String>,
    },

    /// Unknown type (for inference)
    Unknown(TypeId),

    /// Error type (for recovery)
    Error,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, Type::Int | Type::Float | Type::Bool | Type::String | Type::Unit)
    }

    /// Check if this type contains the given type variable
    pub fn contains_var(&self, var: &TypeVar) -> bool {
        match self {
            Type::Var(v) => v == var,
            Type::Function { params, ret } => {
                params.iter().any(|p| p.contains_var(var)) || ret.contains_var(var)
            }
            Type::Tuple(elems) => elems.iter().any(|e| e.contains_var(var)),
            Type::List(elem) => elem.contains_var(var),
            Type::Map { key, value } => key.contains_var(var) || value.contains_var(var),
            Type::Actor(msg) => msg.contains_var(var),
            Type::Channel(elem) => elem.contains_var(var),
            Type::Tagged { fields, .. } => fields.iter().any(|f| f.contains_var(var)),
            Type::Adt { type_args, .. } => type_args.iter().any(|a| a.contains_var(var)),
            Type::Unknown(_) => false,
            _ => false,
        }
    }

    /// Substitute a type variable with a type
    pub fn substitute(&self, var: &TypeVar, replacement: &Type) -> Type {
        match self {
            Type::Var(v) if v == var => replacement.clone(),
            Type::Function { params, ret } => Type::Function {
                params: params.iter().map(|p| p.substitute(var, replacement)).collect(),
                ret: Box::new(ret.substitute(var, replacement)),
            },
            Type::Tuple(elems) => {
                Type::Tuple(elems.iter().map(|e| e.substitute(var, replacement)).collect())
            }
            Type::List(elem) => Type::List(Box::new(elem.substitute(var, replacement))),
            Type::Map { key, value } => Type::Map {
                key: Box::new(key.substitute(var, replacement)),
                value: Box::new(value.substitute(var, replacement)),
            },
            Type::Actor(msg) => Type::Actor(Box::new(msg.substitute(var, replacement))),
            Type::Channel(elem) => Type::Channel(Box::new(elem.substitute(var, replacement))),
            Type::Tagged { tag, fields } => Type::Tagged {
                tag: tag.clone(),
                fields: fields.iter().map(|f| f.substitute(var, replacement)).collect(),
            },
            Type::Adt { name, type_args } => Type::Adt {
                name: name.clone(),
                type_args: type_args
                    .iter()
                    .map(|a| a.substitute(var, replacement))
                    .collect(),
            },
            other => other.clone(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::Unit => write!(f, "()"),
            Type::Var(v) => write!(f, "{}", v),
            Type::Function { params, ret } => {
                write!(f, "(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ") -> {}", ret)
            }
            Type::Tuple(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Type::List(elem) => write!(f, "List[{}]", elem),
            Type::Map { key, value } => write!(f, "Map[{}, {}]", key, value),
            Type::Actor(msg) => write!(f, "Actor[{}]", msg),
            Type::Channel(elem) => write!(f, "Channel[{}]", elem),
            Type::Tagged { tag, fields } => {
                write!(f, "{{{}", tag)?;
                for field in fields {
                    write!(f, ", {}", field)?;
                }
                write!(f, "}}")
            }
            Type::Adt { name, type_args } => {
                write!(f, "{}", name)?;
                if !type_args.is_empty() {
                    write!(f, "[")?;
                    for (i, a) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", a)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Type::TypeConstructor { name, params } => {
                write!(f, "{}", name)?;
                if !params.is_empty() {
                    write!(f, "[")?;
                    for (i, p) in params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", p)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Type::Unknown(id) => write!(f, "?{}", id),
            Type::Error => write!(f, "<error>"),
        }
    }
}

/// Type variable for polymorphism
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub name: String,
    pub id: usize,
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// Type scheme (polymorphic type)
#[derive(Debug, Clone)]
pub struct TypeScheme {
    pub type_vars: Vec<TypeVar>,
    pub typ: Type,
}

impl TypeScheme {
    pub fn monomorphic(typ: Type) -> Self {
        Self {
            type_vars: Vec::new(),
            typ,
        }
    }

    pub fn polymorphic(type_vars: Vec<TypeVar>, typ: Type) -> Self {
        Self { type_vars, typ }
    }
}

/// Variant definition in an ADT
#[derive(Debug, Clone)]
pub struct VariantDef {
    pub name: String,
    pub fields: Vec<FieldDef>,
}

/// Field definition in a variant
#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: Option<String>,
    pub typ: Type,
}

/// ADT definition
#[derive(Debug, Clone)]
pub struct AdtDef {
    pub name: String,
    pub type_params: Vec<String>,
    pub variants: Vec<VariantDef>,
}

/// Type environment for looking up types
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    /// Variable types
    variables: HashMap<String, TypeScheme>,
    /// ADT definitions
    adts: HashMap<String, AdtDef>,
    /// Type aliases
    aliases: HashMap<String, Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_builtins() -> Self {
        let mut env = Self::new();

        // Add builtin functions
        env.insert_var(
            "print".to_string(),
            TypeScheme::monomorphic(Type::Function {
                params: vec![Type::String],
                ret: Box::new(Type::Unit),
            }),
        );

        env.insert_var(
            "println".to_string(),
            TypeScheme::monomorphic(Type::Function {
                params: vec![Type::String],
                ret: Box::new(Type::Unit),
            }),
        );

        env.insert_var(
            "int_to_string".to_string(),
            TypeScheme::monomorphic(Type::Function {
                params: vec![Type::Int],
                ret: Box::new(Type::String),
            }),
        );

        env.insert_var(
            "float_to_string".to_string(),
            TypeScheme::monomorphic(Type::Function {
                params: vec![Type::Float],
                ret: Box::new(Type::String),
            }),
        );

        env.insert_var(
            "concat".to_string(),
            TypeScheme::monomorphic(Type::Function {
                params: vec![Type::String, Type::String],
                ret: Box::new(Type::String),
            }),
        );

        env.insert_var(
            "length".to_string(),
            TypeScheme::monomorphic(Type::Function {
                params: vec![Type::String],
                ret: Box::new(Type::Int),
            }),
        );

        // copy returns a tuple of the same type
        let copy_var = TypeVar {
            name: "T".to_string(),
            id: 0,
        };
        env.insert_var(
            "copy".to_string(),
            TypeScheme::polymorphic(
                vec![copy_var.clone()],
                Type::Function {
                    params: vec![Type::Var(copy_var.clone())],
                    ret: Box::new(Type::Tuple(vec![
                        Type::Var(copy_var.clone()),
                        Type::Var(copy_var),
                    ])),
                },
            ),
        );

        env
    }

    pub fn insert_var(&mut self, name: String, scheme: TypeScheme) {
        self.variables.insert(name, scheme);
    }

    pub fn get_var(&self, name: &str) -> Option<&TypeScheme> {
        self.variables.get(name)
    }

    pub fn insert_adt(&mut self, def: AdtDef) {
        self.adts.insert(def.name.clone(), def);
    }

    pub fn get_adt(&self, name: &str) -> Option<&AdtDef> {
        self.adts.get(name)
    }

    pub fn adts(&self) -> impl Iterator<Item = &AdtDef> {
        self.adts.values()
    }

    pub fn extend(&self) -> TypeEnv {
        self.clone()
    }
}

/// Affine usage tracking
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Usage {
    /// Value has not been used
    Unused,
    /// Value has been used once
    Used,
    /// Value has been moved/consumed
    Consumed,
}

/// Variable binding with usage tracking
#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub typ: Type,
    pub usage: Usage,
    pub mutable: bool,
}

impl Binding {
    pub fn new(name: String, typ: Type) -> Self {
        Self {
            name,
            typ,
            usage: Usage::Unused,
            mutable: false,
        }
    }
}

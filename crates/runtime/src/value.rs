//! Value representation for seq-actor runtime
//!
//! Values are represented as 40-byte tagged unions, matching seq-core.

use std::fmt;

/// Tag values for the Value type
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueTag {
    Int = 0,
    Float = 1,
    Bool = 2,
    String = 3,
    Unit = 4,
    Tuple = 5,
    List = 6,
    Actor = 7,
    Tagged = 8,
    Closure = 9,
    Channel = 10,
}

/// Runtime value representation
#[repr(C)]
pub struct Value {
    /// Type tag
    pub tag: u8,
    /// Payload (39 bytes)
    pub payload: [u8; 39],
}

impl Value {
    /// Create an integer value
    pub fn int(n: i64) -> Self {
        let mut payload = [0u8; 39];
        payload[..8].copy_from_slice(&n.to_le_bytes());
        Self {
            tag: ValueTag::Int as u8,
            payload,
        }
    }

    /// Create a float value
    pub fn float(n: f64) -> Self {
        let mut payload = [0u8; 39];
        payload[..8].copy_from_slice(&n.to_le_bytes());
        Self {
            tag: ValueTag::Float as u8,
            payload,
        }
    }

    /// Create a boolean value
    pub fn bool(b: bool) -> Self {
        let mut payload = [0u8; 39];
        payload[0] = if b { 1 } else { 0 };
        Self {
            tag: ValueTag::Bool as u8,
            payload,
        }
    }

    /// Create a unit value
    pub fn unit() -> Self {
        Self {
            tag: ValueTag::Unit as u8,
            payload: [0u8; 39],
        }
    }

    /// Create a string value (stores pointer)
    pub fn string(ptr: *const u8) -> Self {
        let mut payload = [0u8; 39];
        let ptr_bytes = (ptr as usize).to_le_bytes();
        payload[..8].copy_from_slice(&ptr_bytes);
        Self {
            tag: ValueTag::String as u8,
            payload,
        }
    }

    /// Get the tag of a value
    pub fn get_tag(&self) -> ValueTag {
        match self.tag {
            0 => ValueTag::Int,
            1 => ValueTag::Float,
            2 => ValueTag::Bool,
            3 => ValueTag::String,
            4 => ValueTag::Unit,
            5 => ValueTag::Tuple,
            6 => ValueTag::List,
            7 => ValueTag::Actor,
            8 => ValueTag::Tagged,
            9 => ValueTag::Closure,
            10 => ValueTag::Channel,
            _ => ValueTag::Unit, // Unknown tag defaults to Unit
        }
    }

    /// Try to get as integer
    pub fn as_int(&self) -> Option<i64> {
        if self.tag == ValueTag::Int as u8 {
            Some(i64::from_le_bytes(self.payload[..8].try_into().unwrap()))
        } else {
            None
        }
    }

    /// Try to get as float
    pub fn as_float(&self) -> Option<f64> {
        if self.tag == ValueTag::Float as u8 {
            Some(f64::from_le_bytes(self.payload[..8].try_into().unwrap()))
        } else {
            None
        }
    }

    /// Try to get as boolean
    pub fn as_bool(&self) -> Option<bool> {
        if self.tag == ValueTag::Bool as u8 {
            Some(self.payload[0] != 0)
        } else {
            None
        }
    }

    /// Check if this is a unit value
    pub fn is_unit(&self) -> bool {
        self.tag == ValueTag::Unit as u8
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::unit()
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        Self {
            tag: self.tag,
            payload: self.payload,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.get_tag() {
            ValueTag::Int => write!(f, "Int({})", self.as_int().unwrap_or(0)),
            ValueTag::Float => write!(f, "Float({})", self.as_float().unwrap_or(0.0)),
            ValueTag::Bool => write!(f, "Bool({})", self.as_bool().unwrap_or(false)),
            ValueTag::String => write!(f, "String(...)"),
            ValueTag::Unit => write!(f, "()"),
            ValueTag::Tuple => write!(f, "Tuple(...)"),
            ValueTag::List => write!(f, "List(...)"),
            ValueTag::Actor => write!(f, "Actor(...)"),
            ValueTag::Tagged => write!(f, "Tagged(...)"),
            ValueTag::Closure => write!(f, "Closure(...)"),
            ValueTag::Channel => write!(f, "Channel(...)"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_value() {
        let v = Value::int(42);
        assert_eq!(v.get_tag(), ValueTag::Int);
        assert_eq!(v.as_int(), Some(42));
    }

    #[test]
    fn test_float_value() {
        let v = Value::float(3.14);
        assert_eq!(v.get_tag(), ValueTag::Float);
        let f = v.as_float().unwrap();
        assert!((f - 3.14).abs() < 0.001);
    }

    #[test]
    fn test_bool_value() {
        let t = Value::bool(true);
        let f = Value::bool(false);
        assert_eq!(t.as_bool(), Some(true));
        assert_eq!(f.as_bool(), Some(false));
    }

    #[test]
    fn test_unit_value() {
        let u = Value::unit();
        assert!(u.is_unit());
    }
}

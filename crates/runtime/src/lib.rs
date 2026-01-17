//! seq-actor Runtime Library
//!
//! Provides runtime support for compiled seq-actor programs.
//! This library is linked with compiled programs to provide:
//! - Memory allocation
//! - Built-in functions
//! - Actor primitives (Phase 2)
//! - Supervision (Phase 4)

pub mod actor;
pub mod builtins;

// Re-export seq-core types
pub use seq_core::{
    ChannelData, Value, WeaveChannelData, WeaveMessage, DISC_BOOL, DISC_CHANNEL, DISC_CLOSURE,
    DISC_FLOAT, DISC_INT, DISC_MAP, DISC_QUOTATION, DISC_STRING, DISC_SYMBOL, DISC_VARIANT,
    DISC_WEAVECTX,
};

use std::alloc::{alloc, dealloc, Layout};
use std::ffi::CStr;
use std::os::raw::c_char;

/// Allocate memory for runtime values
#[no_mangle]
pub extern "C" fn sa_alloc(size: i64) -> *mut u8 {
    if size <= 0 {
        return std::ptr::null_mut();
    }
    unsafe {
        let layout = Layout::from_size_align(size as usize, 8).unwrap();
        alloc(layout)
    }
}

/// Free allocated memory
///
/// # Safety
/// Caller must ensure `ptr` was allocated by `sa_alloc` with the same `size`.
#[no_mangle]
pub unsafe extern "C" fn sa_free(ptr: *mut u8, size: i64) {
    if ptr.is_null() || size <= 0 {
        return;
    }
    let layout = Layout::from_size_align(size as usize, 8).unwrap();
    dealloc(ptr, layout);
}

// Integer operations

#[no_mangle]
pub extern "C" fn sa_add_int(a: i64, b: i64) -> i64 {
    a.wrapping_add(b)
}

#[no_mangle]
pub extern "C" fn sa_sub_int(a: i64, b: i64) -> i64 {
    a.wrapping_sub(b)
}

#[no_mangle]
pub extern "C" fn sa_mul_int(a: i64, b: i64) -> i64 {
    a.wrapping_mul(b)
}

#[no_mangle]
pub extern "C" fn sa_div_int(a: i64, b: i64) -> i64 {
    if b == 0 {
        0 // Return 0 for division by zero (could panic or return Option)
    } else {
        a / b
    }
}

#[no_mangle]
pub extern "C" fn sa_mod_int(a: i64, b: i64) -> i64 {
    if b == 0 {
        0
    } else {
        a % b
    }
}

#[no_mangle]
pub extern "C" fn sa_neg_int(a: i64) -> i64 {
    -a
}

// Integer comparisons

#[no_mangle]
pub extern "C" fn sa_eq_int(a: i64, b: i64) -> bool {
    a == b
}

#[no_mangle]
pub extern "C" fn sa_lt_int(a: i64, b: i64) -> bool {
    a < b
}

#[no_mangle]
pub extern "C" fn sa_le_int(a: i64, b: i64) -> bool {
    a <= b
}

#[no_mangle]
pub extern "C" fn sa_gt_int(a: i64, b: i64) -> bool {
    a > b
}

#[no_mangle]
pub extern "C" fn sa_ge_int(a: i64, b: i64) -> bool {
    a >= b
}

// Float operations

#[no_mangle]
pub extern "C" fn sa_add_float(a: f64, b: f64) -> f64 {
    a + b
}

#[no_mangle]
pub extern "C" fn sa_sub_float(a: f64, b: f64) -> f64 {
    a - b
}

#[no_mangle]
pub extern "C" fn sa_mul_float(a: f64, b: f64) -> f64 {
    a * b
}

#[no_mangle]
pub extern "C" fn sa_div_float(a: f64, b: f64) -> f64 {
    a / b
}

#[no_mangle]
pub extern "C" fn sa_neg_float(a: f64) -> f64 {
    -a
}

// String operations

/// Print a string without newline
///
/// # Safety
/// Caller must ensure `s` is a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn sa_print(s: *const c_char) {
    if s.is_null() {
        return;
    }
    let cstr = CStr::from_ptr(s);
    if let Ok(str) = cstr.to_str() {
        print!("{}", str);
    }
}

/// Print a string with newline
///
/// # Safety
/// Caller must ensure `s` is a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn sa_println(s: *const c_char) {
    if s.is_null() {
        println!();
        return;
    }
    let cstr = CStr::from_ptr(s);
    if let Ok(str) = cstr.to_str() {
        println!("{}", str);
    }
}

#[no_mangle]
pub extern "C" fn sa_int_to_string(n: i64) -> *mut c_char {
    let s = format!("{}\0", n);
    let ptr = sa_alloc(s.len() as i64);
    if !ptr.is_null() {
        unsafe {
            std::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len());
        }
    }
    ptr as *mut c_char
}

#[no_mangle]
pub extern "C" fn sa_float_to_string(n: f64) -> *mut c_char {
    let s = format!("{}\0", n);
    let ptr = sa_alloc(s.len() as i64);
    if !ptr.is_null() {
        unsafe {
            std::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len());
        }
    }
    ptr as *mut c_char
}

#[no_mangle]
pub extern "C" fn sa_bool_to_string(b: bool) -> *mut c_char {
    let s = if b { "true\0" } else { "false\0" };
    let ptr = sa_alloc(s.len() as i64);
    if !ptr.is_null() {
        unsafe {
            std::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len());
        }
    }
    ptr as *mut c_char
}

/// Concatenate two strings
///
/// # Safety
/// Caller must ensure `a` and `b` are valid null-terminated C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn sa_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    if a.is_null() && b.is_null() {
        return std::ptr::null_mut();
    }

    let s1 = if a.is_null() {
        String::new()
    } else {
        CStr::from_ptr(a).to_string_lossy().into_owned()
    };

    let s2 = if b.is_null() {
        String::new()
    } else {
        CStr::from_ptr(b).to_string_lossy().into_owned()
    };

    let result = format!("{}{}\0", s1, s2);
    let ptr = sa_alloc(result.len() as i64);
    if !ptr.is_null() {
        std::ptr::copy_nonoverlapping(result.as_ptr(), ptr, result.len());
    }
    ptr as *mut c_char
}

/// Get the length of a string
///
/// # Safety
/// Caller must ensure `s` is a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn sa_string_length(s: *const c_char) -> i64 {
    if s.is_null() {
        return 0;
    }
    CStr::from_ptr(s).to_bytes().len() as i64
}

// List operations

/// Get the head of a list (first element)
///
/// # Safety
/// Caller must ensure `list` is a valid list cell allocated by `sa_list_cons`.
#[no_mangle]
pub unsafe extern "C" fn sa_list_head(list: *const u8) -> i64 {
    if list.is_null() {
        return 0;
    }
    *(list as *const i64)
}

/// Get the tail of a list (rest of the list)
///
/// # Safety
/// Caller must ensure `list` is a valid list cell allocated by `sa_list_cons`.
#[no_mangle]
pub unsafe extern "C" fn sa_list_tail(list: *const u8) -> *const u8 {
    if list.is_null() {
        return std::ptr::null();
    }
    let tail_ptr = list.add(8) as *const *const u8;
    *tail_ptr
}

/// Check if a list is empty
#[no_mangle]
pub extern "C" fn sa_list_is_empty(list: *const u8) -> bool {
    list.is_null()
}

/// Create a new cons cell
#[no_mangle]
pub extern "C" fn sa_list_cons(head: i64, tail: *const u8) -> *mut u8 {
    let cell = sa_alloc(16); // 8 bytes for head + 8 bytes for tail pointer
    if !cell.is_null() {
        unsafe {
            *(cell as *mut i64) = head;
            *((cell as *mut i64).add(1)) = tail as i64;
        }
    }
    cell
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_int_ops() {
        assert_eq!(sa_add_int(1, 2), 3);
        assert_eq!(sa_sub_int(5, 3), 2);
        assert_eq!(sa_mul_int(4, 5), 20);
        assert_eq!(sa_div_int(10, 2), 5);
        assert_eq!(sa_mod_int(10, 3), 1);
        assert_eq!(sa_neg_int(5), -5);
    }

    #[test]
    fn test_int_cmp() {
        assert!(sa_eq_int(5, 5));
        assert!(!sa_eq_int(5, 6));
        assert!(sa_lt_int(3, 5));
        assert!(sa_le_int(5, 5));
        assert!(sa_gt_int(7, 5));
        assert!(sa_ge_int(5, 5));
    }

    #[test]
    fn test_float_ops() {
        assert!((sa_add_float(1.0, 2.0) - 3.0).abs() < 0.001);
        assert!((sa_sub_float(5.0, 3.0) - 2.0).abs() < 0.001);
        assert!((sa_mul_float(4.0, 5.0) - 20.0).abs() < 0.001);
        assert!((sa_div_float(10.0, 2.0) - 5.0).abs() < 0.001);
    }

    #[test]
    fn test_string_length() {
        let s = CString::new("hello").unwrap();
        // SAFETY: s is a valid CString we just created
        assert_eq!(unsafe { sa_string_length(s.as_ptr()) }, 5);
    }

    #[test]
    fn test_list_ops() {
        // Empty list
        assert!(sa_list_is_empty(std::ptr::null()));

        // Create [1, 2, 3]
        let list = sa_list_cons(3, std::ptr::null());
        let list = sa_list_cons(2, list);
        let list = sa_list_cons(1, list);

        assert!(!sa_list_is_empty(list));
        // SAFETY: list was just created by sa_list_cons
        assert_eq!(unsafe { sa_list_head(list) }, 1);

        // SAFETY: list was just created by sa_list_cons
        let tail = unsafe { sa_list_tail(list) };
        // SAFETY: tail is a valid list cell
        assert_eq!(unsafe { sa_list_head(tail) }, 2);

        // SAFETY: tail is a valid list cell
        let tail2 = unsafe { sa_list_tail(tail) };
        // SAFETY: tail2 is a valid list cell
        assert_eq!(unsafe { sa_list_head(tail2) }, 3);

        // SAFETY: tail2 is a valid list cell
        let tail3 = unsafe { sa_list_tail(tail2) };
        assert!(sa_list_is_empty(tail3));
    }
}

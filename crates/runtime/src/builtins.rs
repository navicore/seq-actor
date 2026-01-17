//! Built-in functions for seq-actor
//!
//! These are higher-level built-ins that work with Value types.

use crate::value::Value;

/// Map a function over a list
pub fn map(list: *const u8, f: fn(i64) -> i64) -> *mut u8 {
    if list.is_null() {
        return std::ptr::null_mut();
    }

    // Get head and tail
    let head = unsafe { *(list as *const i64) };
    let tail = unsafe { *((list as *const i64).add(1)) as *const u8 };

    // Apply function to head
    let new_head = f(head);

    // Recursively map over tail
    let new_tail = map(tail, f);

    // Cons new head onto mapped tail
    crate::sa_list_cons(new_head, new_tail)
}

/// Filter a list
pub fn filter(list: *const u8, predicate: fn(i64) -> bool) -> *mut u8 {
    if list.is_null() {
        return std::ptr::null_mut();
    }

    let head = unsafe { *(list as *const i64) };
    let tail = unsafe { *((list as *const i64).add(1)) as *const u8 };

    let filtered_tail = filter(tail, predicate);

    if predicate(head) {
        crate::sa_list_cons(head, filtered_tail)
    } else {
        filtered_tail
    }
}

/// Fold a list from the left
pub fn fold(list: *const u8, init: i64, f: fn(i64, i64) -> i64) -> i64 {
    if list.is_null() {
        return init;
    }

    let head = unsafe { *(list as *const i64) };
    let tail = unsafe { *((list as *const i64).add(1)) as *const u8 };

    let new_acc = f(init, head);
    fold(tail, new_acc, f)
}

fn add_one(acc: i64, _: i64) -> i64 {
    acc + 1
}

/// Get the length of a list
pub fn list_length(list: *const u8) -> i64 {
    fold(list, 0, add_one)
}

/// Reverse a list
pub fn list_reverse(list: *const u8) -> *mut u8 {
    fn go(list: *const u8, acc: *mut u8) -> *mut u8 {
        if list.is_null() {
            return acc;
        }
        let head = unsafe { *(list as *const i64) };
        let tail = unsafe { *((list as *const i64).add(1)) as *const u8 };
        let new_acc = crate::sa_list_cons(head, acc);
        go(tail, new_acc)
    }
    go(list, std::ptr::null_mut())
}

/// Copy a value (for affine semantics)
pub fn copy_value(value: &Value) -> (Value, Value) {
    (value.clone(), value.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sa_list_cons;

    fn make_list(items: &[i64]) -> *mut u8 {
        let mut list: *mut u8 = std::ptr::null_mut();
        for &item in items.iter().rev() {
            list = sa_list_cons(item, list);
        }
        list
    }

    fn list_to_vec(list: *const u8) -> Vec<i64> {
        let mut result = Vec::new();
        let mut current = list;
        while !current.is_null() {
            let head = unsafe { *(current as *const i64) };
            result.push(head);
            current = unsafe { *((current as *const i64).add(1)) as *const u8 };
        }
        result
    }

    fn double(x: i64) -> i64 { x * 2 }
    fn is_even(x: i64) -> bool { x % 2 == 0 }
    fn sum(acc: i64, x: i64) -> i64 { acc + x }

    #[test]
    fn test_map() {
        let list = make_list(&[1, 2, 3]);
        let mapped = map(list, double);
        assert_eq!(list_to_vec(mapped), vec![2, 4, 6]);
    }

    #[test]
    fn test_filter() {
        let list = make_list(&[1, 2, 3, 4, 5]);
        let filtered = filter(list, is_even);
        assert_eq!(list_to_vec(filtered), vec![2, 4]);
    }

    #[test]
    fn test_fold() {
        let list = make_list(&[1, 2, 3, 4, 5]);
        let total = fold(list, 0, sum);
        assert_eq!(total, 15);
    }

    #[test]
    fn test_list_length() {
        let list = make_list(&[1, 2, 3, 4, 5]);
        assert_eq!(list_length(list), 5);
    }

    #[test]
    fn test_list_reverse() {
        let list = make_list(&[1, 2, 3]);
        let reversed = list_reverse(list);
        assert_eq!(list_to_vec(reversed), vec![3, 2, 1]);
    }
}

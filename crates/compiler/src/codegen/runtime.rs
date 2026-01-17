//! Runtime function declarations for LLVM codegen
//!
//! This module contains constants for the runtime library interface.

/// Runtime function names
pub mod functions {
    pub const ALLOC: &str = "sa_alloc";
    pub const FREE: &str = "sa_free";

    // Integer operations
    pub const ADD_INT: &str = "sa_add_int";
    pub const SUB_INT: &str = "sa_sub_int";
    pub const MUL_INT: &str = "sa_mul_int";
    pub const DIV_INT: &str = "sa_div_int";
    pub const MOD_INT: &str = "sa_mod_int";
    pub const NEG_INT: &str = "sa_neg_int";

    // Float operations
    pub const ADD_FLOAT: &str = "sa_add_float";
    pub const SUB_FLOAT: &str = "sa_sub_float";
    pub const MUL_FLOAT: &str = "sa_mul_float";
    pub const DIV_FLOAT: &str = "sa_div_float";
    pub const NEG_FLOAT: &str = "sa_neg_float";

    // Comparison
    pub const EQ_INT: &str = "sa_eq_int";
    pub const LT_INT: &str = "sa_lt_int";
    pub const LE_INT: &str = "sa_le_int";
    pub const GT_INT: &str = "sa_gt_int";
    pub const GE_INT: &str = "sa_ge_int";

    // String operations
    pub const PRINT: &str = "sa_print";
    pub const PRINTLN: &str = "sa_println";
    pub const INT_TO_STRING: &str = "sa_int_to_string";
    pub const FLOAT_TO_STRING: &str = "sa_float_to_string";
    pub const BOOL_TO_STRING: &str = "sa_bool_to_string";
    pub const CONCAT: &str = "sa_concat";
    pub const STRING_LENGTH: &str = "sa_string_length";
    pub const STRING_SPLIT: &str = "sa_string_split";

    // List operations
    pub const LIST_HEAD: &str = "sa_list_head";
    pub const LIST_TAIL: &str = "sa_list_tail";
    pub const LIST_CONS: &str = "sa_list_cons";
    pub const LIST_IS_EMPTY: &str = "sa_list_is_empty";

    // Actor operations (Phase 2)
    pub const ACTOR_SPAWN: &str = "sa_actor_spawn";
    pub const ACTOR_SEND: &str = "sa_actor_send";
    pub const ACTOR_RECEIVE: &str = "sa_actor_receive";
    pub const ACTOR_SELF: &str = "sa_actor_self";
    pub const ACTOR_BECOME: &str = "sa_actor_become";

    // Supervision (Phase 4)
    pub const ACTOR_LINK: &str = "sa_actor_link";
    pub const ACTOR_MONITOR: &str = "sa_actor_monitor";
    pub const ACTOR_TRAP_EXITS: &str = "sa_actor_trap_exits";
}

/// LLVM IR type declarations for the runtime
pub const TYPE_DECLARATIONS: &str = r#"
; Value type - 40 byte tagged union
; Tag byte: 0=Int, 1=Float, 2=Bool, 3=String, 4=Unit, 5=Tuple, 6=List, 7=Actor, 8=Tagged
%Value = type { i8, [39 x i8] }

; Actor handle
%Actor = type { i64, i8* }  ; id, mailbox pointer

; Cons cell for lists
%Cons = type { %Value, %Cons* }

; Tagged tuple
%Tagged = type { i8, i32, %Value* }  ; tag hash, field count, fields pointer
"#;

/// LLVM IR function declarations for the runtime
pub const FUNCTION_DECLARATIONS: &str = r#"
; Memory allocation
declare i8* @sa_alloc(i64)
declare void @sa_free(i8*)

; Integer operations
declare i64 @sa_add_int(i64, i64)
declare i64 @sa_sub_int(i64, i64)
declare i64 @sa_mul_int(i64, i64)
declare i64 @sa_div_int(i64, i64)
declare i64 @sa_mod_int(i64, i64)
declare i64 @sa_neg_int(i64)

; Float operations
declare double @sa_add_float(double, double)
declare double @sa_sub_float(double, double)
declare double @sa_mul_float(double, double)
declare double @sa_div_float(double, double)
declare double @sa_neg_float(double)

; Comparisons
declare i1 @sa_eq_int(i64, i64)
declare i1 @sa_lt_int(i64, i64)
declare i1 @sa_le_int(i64, i64)
declare i1 @sa_gt_int(i64, i64)
declare i1 @sa_ge_int(i64, i64)

; String operations
declare void @sa_print(i8*)
declare void @sa_println(i8*)
declare i8* @sa_int_to_string(i64)
declare i8* @sa_float_to_string(double)
declare i8* @sa_bool_to_string(i1)
declare i8* @sa_concat(i8*, i8*)
declare i64 @sa_string_length(i8*)

; List operations
declare %Value @sa_list_head(%Cons*)
declare %Cons* @sa_list_tail(%Cons*)
declare %Cons* @sa_list_cons(%Value, %Cons*)
declare i1 @sa_list_is_empty(%Cons*)

; Actor operations (Phase 2)
declare %Actor* @sa_actor_spawn(i8*, i8*)
declare void @sa_actor_send(%Actor*, %Value)
declare %Value @sa_actor_receive()
declare %Actor* @sa_actor_self()

; Supervision (Phase 4)
declare void @sa_actor_link(%Actor*)
declare i64 @sa_actor_monitor(%Actor*)
declare void @sa_actor_trap_exits(i1)
"#;

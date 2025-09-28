(** Arrow column operations *)

(* The types Table.t and Wrapper.Table.t are the same, as are the column types.
   We define these functions to have the correct signatures for the interface. *)

let read_i32_ba table ~column =
  Wrapper.Column.read_i32_ba table ~column

let read_i64_ba table ~column =
  Wrapper.Column.read_i64_ba table ~column

let read_f64_ba table ~column =
  Wrapper.Column.read_f64_ba table ~column

let read_f32_ba table ~column =
  Wrapper.Column.read_f32_ba table ~column

let read_i32_ba_opt table ~column =
  Wrapper.Column.read_i32_ba_opt table ~column

let read_i64_ba_opt table ~column =
  Wrapper.Column.read_i64_ba_opt table ~column

let read_f64_ba_opt table ~column =
  Wrapper.Column.read_f64_ba_opt table ~column

let read_f32_ba_opt table ~column =
  Wrapper.Column.read_f32_ba_opt table ~column

let read_int table ~column = Wrapper.Column.read_int table ~column
let read_int32 table ~column = Wrapper.Column.read_int32 table ~column
let read_float table ~column = Wrapper.Column.read_float table ~column
let read_utf8 table ~column = Wrapper.Column.read_utf8 table ~column
let read_date table ~column = Wrapper.Column.read_date table ~column
let read_time_ns table ~column = Wrapper.Column.read_time_ns table ~column
let read_ofday_ns table ~column = Wrapper.Column.read_ofday_ns table ~column
let read_span_ns table ~column = Wrapper.Column.read_span_ns table ~column

let read_int_opt table ~column = Wrapper.Column.read_int_opt table ~column
let read_int32_opt table ~column = Wrapper.Column.read_int32_opt table ~column
let read_float_opt table ~column = Wrapper.Column.read_float_opt table ~column
let read_utf8_opt table ~column = Wrapper.Column.read_utf8_opt table ~column
let read_date_opt table ~column = Wrapper.Column.read_date_opt table ~column
let read_time_ns_opt table ~column = Wrapper.Column.read_time_ns_opt table ~column
let read_ofday_ns_opt table ~column = Wrapper.Column.read_ofday_ns_opt table ~column
let read_span_ns_opt table ~column = Wrapper.Column.read_span_ns_opt table ~column

let read_bitset table ~column = Wrapper.Column.read_bitset table ~column
let read_bitset_opt table ~column = Wrapper.Column.read_bitset_opt table ~column

type t = Wrapper.Column.t =
  | Unsupported_type
  | String of string array
  | String_option of string option array
  | Int64 of (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Int64_option of
      (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.ba
  | Double of (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Double_option of
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.ba

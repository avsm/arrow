(** Arrow column operations *)

type column = Wrapper.Column.column

let read_i32_ba = Wrapper.Column.read_i32_ba
let read_i64_ba = Wrapper.Column.read_i64_ba
let read_f64_ba = Wrapper.Column.read_f64_ba
let read_f32_ba = Wrapper.Column.read_f32_ba

let read_i32_ba_opt = Wrapper.Column.read_i32_ba_opt
let read_i64_ba_opt = Wrapper.Column.read_i64_ba_opt
let read_f64_ba_opt = Wrapper.Column.read_f64_ba_opt
let read_f32_ba_opt = Wrapper.Column.read_f32_ba_opt

let read_int = Wrapper.Column.read_int
let read_int32 = Wrapper.Column.read_int32
let read_float = Wrapper.Column.read_float
let read_utf8 = Wrapper.Column.read_utf8
let read_date = Wrapper.Column.read_date
let read_time_ns = Wrapper.Column.read_time_ns
let read_ofday_ns = Wrapper.Column.read_ofday_ns
let read_span_ns = Wrapper.Column.read_span_ns

let read_int_opt = Wrapper.Column.read_int_opt
let read_int32_opt = Wrapper.Column.read_int32_opt
let read_float_opt = Wrapper.Column.read_float_opt
let read_utf8_opt = Wrapper.Column.read_utf8_opt
let read_date_opt = Wrapper.Column.read_date_opt
let read_time_ns_opt = Wrapper.Column.read_time_ns_opt
let read_ofday_ns_opt = Wrapper.Column.read_ofday_ns_opt
let read_span_ns_opt = Wrapper.Column.read_span_ns_opt

let read_bitset = Wrapper.Column.read_bitset
let read_bitset_opt = Wrapper.Column.read_bitset_opt

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

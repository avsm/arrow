(** Arrow column operations *)

(* Abstract column type that wraps a chunked array *)
type t = C_wrapper.ChunkedArray.t

(* Column specifier: either by name or index *)
type column = [`Index of int | `Name of string]

(* Fast column reading result type *)
type fast_column = C_wrapper.Column.t =
  | Unsupported_type
  | String of string array
  | String_option of string option array
  | Int64 of (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Int64_option of
      (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.ba
  | Double of (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Double_option of
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.ba

(* Helper to convert Table.t to C_wrapper.Table.t *)
let to_wrapper_table (table : Table.t) : C_wrapper.Table.t =
  (* Since Table.t is defined as private C_wrapper.Table.t, we can cast directly *)
  (Obj.magic table : C_wrapper.Table.t)

(* Helper to convert column spec to appropriate form *)
let column_to_wrapper_spec = function
  | `Index i -> `Index i
  | `Name s -> `Name s

(* Column creation and access *)
let get_column (table : Table.t) (column : column) =
  let wrapper_table = to_wrapper_table table in
  match column with
  | `Name name -> C_wrapper.Table.get_column wrapper_table name
  | `Index _idx -> failwith "get_column by index not yet implemented" (* Would need C API extension *)

(* Reading columns as Bigarrays *)
let read_i32_ba (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_i32_ba wrapper_table ~column:(column_to_wrapper_spec column)

let read_i64_ba (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_i64_ba wrapper_table ~column:(column_to_wrapper_spec column)

let read_f64_ba (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_f64_ba wrapper_table ~column:(column_to_wrapper_spec column)

let read_f32_ba (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_f32_ba wrapper_table ~column:(column_to_wrapper_spec column)

let read_i32_ba_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_i32_ba_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_i64_ba_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_i64_ba_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_f64_ba_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_f64_ba_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_f32_ba_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_f32_ba_opt wrapper_table ~column:(column_to_wrapper_spec column)

(* Direct column reading from column values - placeholder implementations *)
(* These would need additional C API functions to read directly from chunked arrays *)
let read_i32_ba_from_column _col = failwith "read_i32_ba_from_column not yet implemented"
let read_i64_ba_from_column _col = failwith "read_i64_ba_from_column not yet implemented"
let read_f64_ba_from_column _col = failwith "read_f64_ba_from_column not yet implemented"
let read_f32_ba_from_column _col = failwith "read_f32_ba_from_column not yet implemented"
let read_i32_ba_opt_from_column _col = failwith "read_i32_ba_opt_from_column not yet implemented"
let read_i64_ba_opt_from_column _col = failwith "read_i64_ba_opt_from_column not yet implemented"
let read_f64_ba_opt_from_column _col = failwith "read_f64_ba_opt_from_column not yet implemented"
let read_f32_ba_opt_from_column _col = failwith "read_f32_ba_opt_from_column not yet implemented"

(* Reading columns as arrays *)
let read_int (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_int wrapper_table ~column:(column_to_wrapper_spec column)

let read_int32 (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_int32 wrapper_table ~column:(column_to_wrapper_spec column)

let read_float (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_float wrapper_table ~column:(column_to_wrapper_spec column)

let read_utf8 (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_utf8 wrapper_table ~column:(column_to_wrapper_spec column)

let read_date (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_date wrapper_table ~column:(column_to_wrapper_spec column)

let read_time_ns (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_time_ns wrapper_table ~column:(column_to_wrapper_spec column)

let read_ofday_ns (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_ofday_ns wrapper_table ~column:(column_to_wrapper_spec column)

let read_span_ns (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_span_ns wrapper_table ~column:(column_to_wrapper_spec column)

let read_int_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_int_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_int32_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_int32_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_float_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_float_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_utf8_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_utf8_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_date_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_date_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_time_ns_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_time_ns_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_ofday_ns_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_ofday_ns_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_span_ns_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_span_ns_opt wrapper_table ~column:(column_to_wrapper_spec column)

let read_bitset (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_bitset wrapper_table ~column:(column_to_wrapper_spec column)

let read_bitset_opt (table : Table.t) ~column =
  let wrapper_table = to_wrapper_table table in
  C_wrapper.Column.read_bitset_opt wrapper_table ~column:(column_to_wrapper_spec column)

(* Direct column reading from column values - placeholder implementations *)
let read_int_from_column _col = failwith "read_int_from_column not yet implemented"
let read_int32_from_column _col = failwith "read_int32_from_column not yet implemented"
let read_float_from_column _col = failwith "read_float_from_column not yet implemented"
let read_utf8_from_column _col = failwith "read_utf8_from_column not yet implemented"
let read_date_from_column _col = failwith "read_date_from_column not yet implemented"
let read_time_ns_from_column _col = failwith "read_time_ns_from_column not yet implemented"
let read_ofday_ns_from_column _col = failwith "read_ofday_ns_from_column not yet implemented"
let read_span_ns_from_column _col = failwith "read_span_ns_from_column not yet implemented"
let read_int_opt_from_column _col = failwith "read_int_opt_from_column not yet implemented"
let read_int32_opt_from_column _col = failwith "read_int32_opt_from_column not yet implemented"
let read_float_opt_from_column _col = failwith "read_float_opt_from_column not yet implemented"
let read_utf8_opt_from_column _col = failwith "read_utf8_opt_from_column not yet implemented"
let read_date_opt_from_column _col = failwith "read_date_opt_from_column not yet implemented"
let read_time_ns_opt_from_column _col = failwith "read_time_ns_opt_from_column not yet implemented"
let read_ofday_ns_opt_from_column _col = failwith "read_ofday_ns_opt_from_column not yet implemented"
let read_span_ns_opt_from_column _col = failwith "read_span_ns_opt_from_column not yet implemented"
let read_bitset_from_column _col = failwith "read_bitset_from_column not yet implemented"
let read_bitset_opt_from_column _col = failwith "read_bitset_opt_from_column not yet implemented"

(* Fast column reading *)
let read_fast (table : Table.t) ~column =
  let _wrapper_table = to_wrapper_table table in
  let _wrapper_column = column_to_wrapper_spec column in
  (* This would need to be implemented based on the existing C_wrapper.Column.t functionality *)
  (* For now, we'll provide a basic implementation that tries to infer the type *)
  failwith "read_fast not yet implemented"

let read_fast_from_column _col =
  failwith "read_fast_from_column not yet implemented"
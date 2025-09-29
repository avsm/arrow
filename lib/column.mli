(** Arrow column operations *)

(** Abstract column type that wraps a chunked array *)
type t

(** Column specifier: either by name or index *)
type column =
  [ `Index of int
  | `Name of string
  ]

(** {1 Column creation and access} *)

(** Get a column from a table by name or index *)
val get_column : Table.t -> column -> t

(** {1 Reading columns as Bigarrays} *)

val read_i32_ba
  :  Table.t
  -> column:column
  -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_i64_ba
  :  Table.t
  -> column:column
  -> (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_f64_ba
  :  Table.t
  -> column:column
  -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_f32_ba
  :  Table.t
  -> column:column
  -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_i32_ba_opt
  :  Table.t
  -> column:column
  -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

val read_i64_ba_opt
  :  Table.t
  -> column:column
  -> (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

val read_f64_ba_opt
  :  Table.t
  -> column:column
  -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

val read_f32_ba_opt
  :  Table.t
  -> column:column
  -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

(** {1 Direct column reading from column values} *)

val read_i32_ba_from_column
  : t -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_i64_ba_from_column
  : t -> (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_f64_ba_from_column
  : t -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_f32_ba_from_column
  : t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_i32_ba_opt_from_column
  : t -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

val read_i64_ba_opt_from_column
  : t -> (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

val read_f64_ba_opt_from_column
  : t -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

val read_f32_ba_opt_from_column
  : t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

(** {1 Reading columns as arrays} *)

val read_int : Table.t -> column:column -> int array
val read_int32 : Table.t -> column:column -> Int32.t array
val read_float : Table.t -> column:column -> float array
val read_utf8 : Table.t -> column:column -> string array
val read_date : Table.t -> column:column -> Time.Date.t array
val read_time_ns : Table.t -> column:column -> Time.Time_ns.t array
val read_ofday_ns : Table.t -> column:column -> Time.Time_ns.Ofday.t array
val read_span_ns : Table.t -> column:column -> Time.Time_ns.Span.t array

val read_int_opt : Table.t -> column:column -> int option array
val read_int32_opt : Table.t -> column:column -> Int32.t option array
val read_float_opt : Table.t -> column:column -> float option array
val read_utf8_opt : Table.t -> column:column -> string option array
val read_date_opt : Table.t -> column:column -> Time.Date.t option array
val read_time_ns_opt : Table.t -> column:column -> Time.Time_ns.t option array
val read_ofday_ns_opt : Table.t -> column:column -> Time.Time_ns.Ofday.t option array
val read_span_ns_opt : Table.t -> column:column -> Time.Time_ns.Span.t option array

val read_bitset : Table.t -> column:column -> Valid.t
val read_bitset_opt : Table.t -> column:column -> Valid.t * Valid.t

(** {1 Direct column reading from column values} *)

val read_int_from_column : t -> int array
val read_int32_from_column : t -> Int32.t array
val read_float_from_column : t -> float array
val read_utf8_from_column : t -> string array
val read_date_from_column : t -> Time.Date.t array
val read_time_ns_from_column : t -> Time.Time_ns.t array
val read_ofday_ns_from_column : t -> Time.Time_ns.Ofday.t array
val read_span_ns_from_column : t -> Time.Time_ns.Span.t array

val read_int_opt_from_column : t -> int option array
val read_int32_opt_from_column : t -> Int32.t option array
val read_float_opt_from_column : t -> float option array
val read_utf8_opt_from_column : t -> string option array
val read_date_opt_from_column : t -> Time.Date.t option array
val read_time_ns_opt_from_column : t -> Time.Time_ns.t option array
val read_ofday_ns_opt_from_column : t -> Time.Time_ns.Ofday.t option array
val read_span_ns_opt_from_column : t -> Time.Time_ns.Span.t option array

val read_bitset_from_column : t -> Valid.t
val read_bitset_opt_from_column : t -> Valid.t * Valid.t

(** {1 Fast column reading} *)

(** Fast column reading result type *)
type fast_column =
  | Unsupported_type
  | String of string array
  | String_option of string option array
  | Int64 of (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Int64_option of
      (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.ba
  | Double of (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Double_option of
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.ba

(** Read a column quickly into a specialized format *)
val read_fast : Table.t -> column:column -> fast_column
val read_fast_from_column : t -> fast_column

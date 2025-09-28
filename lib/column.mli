(** Arrow column operations *)

(** {1 Reading columns as Bigarrays} *)

val read_i32_ba
  :  Table.t
  -> column:Table.column
  -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_i64_ba
  :  Table.t
  -> column:Table.column
  -> (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_f64_ba
  :  Table.t
  -> column:Table.column
  -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_f32_ba
  :  Table.t
  -> column:Table.column
  -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

val read_i32_ba_opt
  :  Table.t
  -> column:Table.column
  -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

val read_i64_ba_opt
  :  Table.t
  -> column:Table.column
  -> (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

val read_f64_ba_opt
  :  Table.t
  -> column:Table.column
  -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

val read_f32_ba_opt
  :  Table.t
  -> column:Table.column
  -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.t

(** {1 Reading columns as arrays} *)

val read_int : Table.t -> column:Table.column -> int array
val read_int32 : Table.t -> column:Table.column -> Int32.t array
val read_float : Table.t -> column:Table.column -> float array
val read_utf8 : Table.t -> column:Table.column -> string array
val read_date : Table.t -> column:Table.column -> Time.Date.t array
val read_time_ns : Table.t -> column:Table.column -> Time.Time_ns.t array
val read_ofday_ns : Table.t -> column:Table.column -> Time.Time_ns.Ofday.t array
val read_span_ns : Table.t -> column:Table.column -> Time.Time_ns.Span.t array

val read_int_opt : Table.t -> column:Table.column -> int option array
val read_int32_opt : Table.t -> column:Table.column -> Int32.t option array
val read_float_opt : Table.t -> column:Table.column -> float option array
val read_utf8_opt : Table.t -> column:Table.column -> string option array
val read_date_opt : Table.t -> column:Table.column -> Time.Date.t option array
val read_time_ns_opt : Table.t -> column:Table.column -> Time.Time_ns.t option array
val read_ofday_ns_opt : Table.t -> column:Table.column -> Time.Time_ns.Ofday.t option array
val read_span_ns_opt : Table.t -> column:Table.column -> Time.Time_ns.Span.t option array

val read_bitset : Table.t -> column:Table.column -> Valid.t
val read_bitset_opt : Table.t -> column:Table.column -> Valid.t * Valid.t

(** {1 Fast column reading} *)

type t =
  | Unsupported_type
  | String of string array
  | String_option of string option array
  | Int64 of (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Int64_option of
      (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.ba
  | Double of (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Double_option of
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t * Valid.ba

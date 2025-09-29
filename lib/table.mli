(** Arrow Table - A collection of equal-length columns *)

(** Abstract table type *)
type t = private C_wrapper.Table.t
(** An Arrow table that abstracts over the underlying C implementation *)

val concatenate : t list -> t
val slice : t -> offset:int -> length:int -> t
val num_rows : t -> int
val schema : t -> Schema.t
val to_string_debug : t -> string
val add_column : t -> string -> C_wrapper.ChunkedArray.t -> t
val get_column : t -> string -> C_wrapper.ChunkedArray.t
val add_all_columns : t -> t -> t

type _ col_type =
  | Int : int col_type
  | Float : float col_type
  | Utf8 : string col_type
  | Date : Time.Date.t col_type
  | Time_ns : Time.Time_ns.t col_type
  | Span_ns : Time.Time_ns.Span.t col_type
  | Ofday_ns : Time.Time_ns.Ofday.t col_type
  | Bool : bool col_type

type packed_col =
  | P : 'a col_type * 'a array -> packed_col
  | O : 'a col_type * 'a option array -> packed_col

type writer_col

val create : writer_col list -> t
val named_col : packed_col -> string -> writer_col
val col : 'a array -> 'a col_type -> string -> writer_col
val col_opt : 'a option array -> 'a col_type -> string -> writer_col

type column = [`Index of int | `Name of string]
(** Column selector type *)

val read : t -> column:column -> 'a col_type -> 'a array
val read_opt : t -> column:column -> 'a col_type -> 'a option array
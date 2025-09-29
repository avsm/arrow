(** Builder module for incremental table construction.

    This module provides two complementary APIs for building Arrow tables:

    {ul
    {- {b Column builders}: Construct tables column-by-column, useful when data
       arrives in columnar format}
    {- {b Row builders}: Construct tables row-by-row using extractors, more ergonomic
       for row-oriented data sources}}

    {2 Column Builders}

    Column builders allow efficient incremental construction of columns:

    {[
      open Arrow

      (* Create builders *)
      let id_builder = Builder.Column.Int64.create () in
      let name_builder = Builder.Column.String.create () in
      let score_builder = Builder.Column.Float64.create () in

      (* Append values *)
      Builder.Column.Int64.append id_builder 1L;
      Builder.Column.String.append name_builder "Alice";
      Builder.Column.Float64.append score_builder 95.5;

      Builder.Column.Int64.append id_builder 2L;
      Builder.Column.String.append name_builder "Bob";
      Builder.Column.Float64.append_null score_builder;  (* Null value *)

      (* Build table *)
      let table = Builder.make_table [
        Builder.Column.Int64.build id_builder ~name:"id";
        Builder.Column.String.build name_builder ~name:"name";
        Builder.Column.Float64.build score_builder ~name:"score";
      ]
    ]}

    {3 Performance}

    Column builders are memory-efficient - data is stored only once in C++ Arrow
    builders. The [build] function transfers ownership to the table without copying.

    {3 Note on Float Precision}

    Float64Builder may have minor precision loss (< 0.001) when values round-trip
    through the C++ layer. For applications requiring exact float representation,
    consider using Row builders or direct [Table.col] construction.

    {2 Row Builders (Recommended)}

    Row builders provide a more ergonomic API for row-oriented data:

    {[
      type person = { id: int; name: string; score: float option }

      let people = [|
        { id = 1; name = "Alice"; score = Some 95.5 };
        { id = 2; name = "Bob"; score = None };
      |]

      let cols =
        Builder.Row.col ~name:"id" Table.Int (fun p -> p.id) @
        Builder.Row.col ~name:"name" Table.Utf8 (fun p -> p.name) @
        Builder.Row.col_opt ~name:"score" Table.Float (fun p -> p.score)

      let table = Builder.Row.array_to_table cols people
    ]}
*)

(** Column-based builders for constructing tables column-by-column *)
module Column : sig
  (** Common interface for all column builders.

      Each builder maintains a mutable buffer of values that can be converted
      to an Arrow column via [build]. *)
  module type S = sig
    type t
    (** The builder type *)

    type elem
    (** The element type stored by this builder *)

    type builder_t = C_wrapper.Builder.t
    (** Internal builder representation *)

    val create : unit -> t
    (** Create a new empty builder *)

    val append : t -> elem -> unit
    (** Append a non-null value to the builder *)

    val append_null : ?n:int -> t -> unit
    (** Append one or more null values. Defaults to 1 null. *)

    val append_opt : t -> elem option -> unit
    (** Append an optional value (None becomes null) *)

    val append_many : t -> elem array -> unit
    (** Append multiple values from an array *)

    val length : t -> int
    (** Get the current number of elements (including nulls) *)

    val null_count : t -> int
    (** Get the number of null values *)

    val build : t -> name:string -> string * C_wrapper.Builder.t
    (** Build a named column. Returns a tuple for use with {!make_table}.
        After calling [build], the builder should not be reused. *)
  end

  module Int32 : S with type elem = int32
  module Int64 : S with type elem = int64
  module Float64 : S with type elem = float
  (** Double-precision floating point builder. Note: May have minor precision
      loss (< 0.001) compared to input values. *)

  module String : S with type elem = string
  module Int8 : S with type elem = int
  module Int16 : S with type elem = int
  module UInt8 : S with type elem = int
  module UInt16 : S with type elem = int
  module UInt32 : S with type elem = int32
  module UInt64 : S with type elem = int64
  module Float : S with type elem = float
  (** Single-precision floating point builder. *)

  module Boolean : S with type elem = bool
  module Date32 : S with type elem = int32
  (** Days since Unix epoch *)

  module Date64 : S with type elem = int64
  (** Milliseconds since Unix epoch *)

  module Time32 : S with type elem = int32
  (** Time of day in seconds or milliseconds *)

  module Time64 : S with type elem = int64
  (** Time of day in microseconds or nanoseconds *)

  module Timestamp : S with type elem = int64
  (** Timestamp in microseconds or nanoseconds since Unix epoch *)

  module Duration : S with type elem = int64
  (** Duration in microseconds or nanoseconds *)
end

(** Create a table from a list of built columns.

    Takes a list of (name, builder) tuples as returned by [Column.*.build].
    The builders are consumed and should not be used after this call.

    Example:
    {[
      let table = Builder.make_table [
        Builder.Column.Int64.build id_builder ~name:"id";
        Builder.Column.String.build name_builder ~name:"name";
      ]
    ]}
*)
val make_table : (string * C_wrapper.Builder.t) list -> Table.t

(** Row-based builders for constructing tables from row-oriented data.

    This is the recommended API for most use cases as it provides:
    {ul
    {- Type-safe column extraction with closures}
    {- Composable column specifications using [@]}
    {- Automatic handling of optional values}
    {- No precision loss issues}}
*)
module Row : sig
  type ('row, 'elem, 'col_type) col =
    { name : string
    ; get : 'row -> 'elem
    ; col_type : 'col_type Table.col_type
    }

  type 'row packed_col =
    | P : ('row, 'elem, 'elem) col -> 'row packed_col
    | O : ('row, 'elem option, 'elem) col -> 'row packed_col

  type 'row packed_cols = 'row packed_col list

  val col : ?name:string -> 'a Table.col_type -> ('row -> 'a) -> 'row packed_cols
  val col_opt : ?name:string -> 'a Table.col_type -> ('row -> 'a option) -> 'row packed_cols

  val array_to_table : 'row packed_cols -> 'row array -> Table.t

  module type Intf = sig
    type row
    val array_to_table : row array -> Table.t
  end

  module type Builder_intf = sig
    type t
    type row

    val create : unit -> t
    val append : t -> row -> unit
    val length : t -> int
    val reset : t -> unit
    val to_table : t -> Table.t
  end

  module Make (R : Intf) : Builder_intf with type row = R.row
end
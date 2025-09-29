(** Unified Builder module with both Column and Row interfaces *)

(** Column-based builders (Arrow-style) *)
module Column : sig
  (** Common interface for all column builders *)
  module type S = sig
    type t
    type elem

    val create : unit -> t
    val append : t -> elem -> unit
    val append_null : ?n:int -> t -> unit
    val append_opt : t -> elem option -> unit
    val append_many : t -> elem array -> unit
    val length : t -> int
    val null_count : t -> int
    val build : t -> name:string -> C_wrapper.ChunkedArray.t
  end

  module Int32 : S with type elem = int32
  module Int64 : S with type elem = int64
  module Float64 : S with type elem = float
  module String : S with type elem = string
  module Int8 : S with type elem = int
  module Int16 : S with type elem = int
  module UInt8 : S with type elem = int
  module UInt16 : S with type elem = int
  module UInt32 : S with type elem = int32
  module UInt64 : S with type elem = int64
  module Float : S with type elem = float
  module Boolean : S with type elem = bool
  module Date32 : S with type elem = int32
  module Date64 : S with type elem = int64
  module Time32 : S with type elem = int32
  module Time64 : S with type elem = int64
  module Timestamp : S with type elem = int64
  module Duration : S with type elem = int64
end

(** Helper function for creating tables from column builders *)
val make_table : (string * C_wrapper.ChunkedArray.t) list -> Table.t

(** Row-based construction (existing ergonomic interface) *)
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
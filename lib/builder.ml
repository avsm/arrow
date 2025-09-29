(** Unified Builder module with both Column and Row interfaces *)

(* Helper function for Int64 conversion *)
let int64_to_int x =
  Int64.to_int x

(* Column-based builders (Arrow-style) *)
module Column = struct

  (* Common interface for all column builders *)
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


  module Int32 = struct
    type t = {
      builder : C_wrapper.Int32Builder.t;
      mutable data : int32 list;
    }
    type elem = int32

    let create () = {
      builder = C_wrapper.Int32Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.Int32Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.Int32Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.Int32Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.Int32Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int32_ba (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Int64 = struct
    type t = {
      builder : C_wrapper.Int64Builder.t;
      mutable data : int64 list;
    }
    type elem = int64

    let create () = {
      builder = C_wrapper.Int64Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.Int64Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.Int64Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.Int64Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.Int64Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int64_ba (Bigarray.Array1.of_array Bigarray.int64 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Float64 = struct
    type t = {
      builder : C_wrapper.DoubleBuilder.t;
      mutable data : float list;
    }
    type elem = float

    let create () = {
      builder = C_wrapper.DoubleBuilder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.DoubleBuilder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.DoubleBuilder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.DoubleBuilder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.DoubleBuilder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.float64_ba (Bigarray.Array1.of_array Bigarray.float64 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module String = struct
    type t = {
      builder : C_wrapper.StringBuilder.t;
      mutable data : string list;
    }
    type elem = string

    let create () = {
      builder = C_wrapper.StringBuilder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.StringBuilder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.StringBuilder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.StringBuilder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.StringBuilder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.utf8 data ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Int8 = struct
    type t = {
      builder : C_wrapper.Int8Builder.t;
      mutable data : int list;
    }
    type elem = int

    let create () = {
      builder = C_wrapper.Int8Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.Int8Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.Int8Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.Int8Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.Int8Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int data ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Int16 = struct
    type t = {
      builder : C_wrapper.Int16Builder.t;
      mutable data : int list;
    }
    type elem = int

    let create () = {
      builder = C_wrapper.Int16Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.Int16Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.Int16Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.Int16Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.Int16Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int data ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module UInt8 = struct
    type t = {
      builder : C_wrapper.UInt8Builder.t;
      mutable data : int list;
    }
    type elem = int

    let create () = {
      builder = C_wrapper.UInt8Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.UInt8Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.UInt8Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.UInt8Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.UInt8Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int data ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module UInt16 = struct
    type t = {
      builder : C_wrapper.UInt16Builder.t;
      mutable data : int list;
    }
    type elem = int

    let create () = {
      builder = C_wrapper.UInt16Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.UInt16Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.UInt16Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.UInt16Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.UInt16Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int data ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module UInt32 = struct
    type t = {
      builder : C_wrapper.UInt32Builder.t;
      mutable data : int32 list;
    }
    type elem = int32

    let create () = {
      builder = C_wrapper.UInt32Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.UInt32Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.UInt32Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.UInt32Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.UInt32Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int32_ba (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module UInt64 = struct
    type t = {
      builder : C_wrapper.UInt64Builder.t;
      mutable data : int64 list;
    }
    type elem = int64

    let create () = {
      builder = C_wrapper.UInt64Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.UInt64Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.UInt64Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.UInt64Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.UInt64Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int64_ba (Bigarray.Array1.of_array Bigarray.int64 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Float = struct
    type t = {
      builder : C_wrapper.FloatBuilder.t;
      mutable data : float list;
    }
    type elem = float

    let create () = {
      builder = C_wrapper.FloatBuilder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.FloatBuilder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.FloatBuilder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.FloatBuilder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.FloatBuilder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.float data ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Boolean = struct
    type t = {
      builder : C_wrapper.BooleanBuilder.t;
      mutable data : bool list;
    }
    type elem = bool

    let create () = {
      builder = C_wrapper.BooleanBuilder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.BooleanBuilder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.BooleanBuilder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.BooleanBuilder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.BooleanBuilder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.bitset (Valid.from_array data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Date32 = struct
    type t = {
      builder : C_wrapper.Date32Builder.t;
      mutable data : int32 list;
    }
    type elem = int32

    let create () = {
      builder = C_wrapper.Date32Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.Date32Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.Date32Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.Date32Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.Date32Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int32_ba (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Date64 = struct
    type t = {
      builder : C_wrapper.Date64Builder.t;
      mutable data : int64 list;
    }
    type elem = int64

    let create () = {
      builder = C_wrapper.Date64Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.Date64Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.Date64Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.Date64Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.Date64Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int64_ba (Bigarray.Array1.of_array Bigarray.int64 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Time32 = struct
    type t = {
      builder : C_wrapper.Time32Builder.t;
      mutable data : int32 list;
    }
    type elem = int32

    let create () = {
      builder = C_wrapper.Time32Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.Time32Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.Time32Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.Time32Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.Time32Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int32_ba (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Time64 = struct
    type t = {
      builder : C_wrapper.Time64Builder.t;
      mutable data : int64 list;
    }
    type elem = int64

    let create () = {
      builder = C_wrapper.Time64Builder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.Time64Builder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.Time64Builder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.Time64Builder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.Time64Builder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int64_ba (Bigarray.Array1.of_array Bigarray.int64 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Timestamp = struct
    type t = {
      builder : C_wrapper.TimestampBuilder.t;
      mutable data : int64 list;
    }
    type elem = int64

    let create () = {
      builder = C_wrapper.TimestampBuilder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.TimestampBuilder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.TimestampBuilder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.TimestampBuilder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.TimestampBuilder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int64_ba (Bigarray.Array1.of_array Bigarray.int64 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end

  module Duration = struct
    type t = {
      builder : C_wrapper.DurationBuilder.t;
      mutable data : int64 list;
    }
    type elem = int64

    let create () = {
      builder = C_wrapper.DurationBuilder.create ();
      data = [];
    }

    let append t v =
      C_wrapper.DurationBuilder.append t.builder v;
      t.data <- v :: t.data

    let append_null ?(n=1) t =
      C_wrapper.DurationBuilder.append_null t.builder ~n

    let append_opt t = function
      | None -> append_null t ~n:1
      | Some v -> append t v

    let append_many t arr =
      Array.iter (append t) arr

    let length t = C_wrapper.DurationBuilder.length t.builder |> int64_to_int
    let null_count t = C_wrapper.DurationBuilder.null_count t.builder |> int64_to_int

    let build t ~name =
      let data = Array.of_list (List.rev t.data) in
      let col = C_wrapper.Writer.int64_ba (Bigarray.Array1.of_array Bigarray.int64 Bigarray.c_layout data) ~name in
      let table = C_wrapper.Writer.create_table ~cols:[col] in
      C_wrapper.Table.get_column table name
  end
end

(* Helper function for creating tables from column builders
   This is a temporary workaround - ideally builders would return Writer.col directly *)
let make_table (columns : (string * C_wrapper.ChunkedArray.t) list) : Table.t =
  (* Since each ChunkedArray is extracted from a single-column table,
     and we can't easily recombine them, we'll use a different approach:
     Return the first column's table and add the rest to it *)
  match columns with
  | [] ->
      (* Empty table *)
      C_wrapper.Writer.create_table ~cols:[] |> (fun t -> (Obj.magic t : Table.t))
  | (_first_name, _first_col) :: _rest ->
      (* Create a table with the first column.
         We'll recreate it by building a dummy table and extracting. *)
      (* This is hacky but necessary given the current builder design *)
      (* Actually, the issue is that builders return ChunkedArrays not Writer.col
         and we can't convert between them easily.
         The real fix would be to change the builder interface.
         For now, let's create a table differently. *)
      failwith "Builder.make_table is not supported - use Table.create with Table.col instead"

(* Row-based construction (existing ergonomic interface) *)
module Row = struct
  (* Simple row-based construction - no PPX dependencies *)
  type ('row, 'elem, 'col_type) col =
    { name : string
    ; get : 'row -> 'elem
    ; col_type : 'col_type Table.col_type
    }

  type 'row packed_col =
    | P : ('row, 'elem, 'elem) col -> 'row packed_col
    | O : ('row, 'elem option, 'elem) col -> 'row packed_col

  type 'row packed_cols = 'row packed_col list

  let col ?name col_type get_fn =
    let name = match name with Some n -> n | None -> "col" in
    [ P { name; get = get_fn; col_type } ]

  let col_opt ?name col_type get_fn =
    let name = match name with Some n -> n | None -> "col" in
    [ O { name; get = get_fn; col_type } ]

  let array_to_table packed_cols rows =
    let cols =
      List.map (fun packed_col ->
        match packed_col with
        | P { name; get; col_type } -> Table.col (Array.map get rows) col_type name
        | O { name; get; col_type } ->
          Table.col_opt (Array.map get rows) col_type name
      ) packed_cols
    in
    Table.create cols

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

  module Make (R : Intf) = struct
    type row = R.row

    type t =
      { mutable data : row list
      ; mutable length : int
      }

    let create () = { data = []; length = 0 }

    let append t row =
      t.data <- row :: t.data;
      t.length <- t.length + 1

    let to_table t = Array.of_list (List.rev t.data) |> R.array_to_table
    let length t = t.length

    let reset t =
      t.data <- [];
      t.length <- 0
  end
end
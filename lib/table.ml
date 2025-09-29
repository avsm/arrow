(** Abstract table type *)
type t = C_wrapper.Table.t

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

type writer_col = C_wrapper.Writer.col

let concatenate = C_wrapper.Table.concatenate
let slice = C_wrapper.Table.slice
let num_rows = C_wrapper.Table.num_rows
let schema t =
  let wrapper_schema = C_wrapper.Table.schema t in
  (* Since Wrapper includes C_wrapper, use Obj.magic for now *)
  let c_wrapper_schema = (Obj.magic wrapper_schema : C_wrapper.Schema.t) in
  (* Convert C_wrapper.Schema.t to Schema.t *)
  Schema.of_c_wrapper c_wrapper_schema
let to_string_debug = C_wrapper.Table.to_string_debug
let add_column table name (column : C_wrapper.ChunkedArray.t) =
  (* Use the chunked array directly *)
  C_wrapper.Table.add_column table name column

let get_column table name : C_wrapper.ChunkedArray.t =
  (* Return the chunked array directly *)
  C_wrapper.Table.get_column table name
let add_all_columns = C_wrapper.Table.add_all_columns

let create (cols : writer_col list) : t = 
  (* Use the actual Writer.create_table function *)
  C_wrapper.Writer.create_table ~cols

let col (type a) (data : a array) (col_type : a col_type) name =
  match col_type with
  | Int -> C_wrapper.Writer.int data ~name
  | Float -> C_wrapper.Writer.float data ~name
  | Utf8 -> C_wrapper.Writer.utf8 data ~name
  | Date -> C_wrapper.Writer.date data ~name
  | Time_ns -> C_wrapper.Writer.time_ns data ~name
  | Span_ns -> C_wrapper.Writer.span_ns data ~name
  | Ofday_ns -> C_wrapper.Writer.ofday_ns data ~name
  | Bool -> C_wrapper.Writer.bitset (Valid.from_array data) ~name

let col_opt (type a) (data : a option array) (col_type : a col_type) name =
  match col_type with
  | Int -> C_wrapper.Writer.int_opt data ~name
  | Float -> C_wrapper.Writer.float_opt data ~name
  | Utf8 -> C_wrapper.Writer.utf8_opt data ~name
  | Date -> C_wrapper.Writer.date_opt data ~name
  | Time_ns -> C_wrapper.Writer.time_ns_opt data ~name
  | Span_ns -> C_wrapper.Writer.span_ns_opt data ~name
  | Ofday_ns -> C_wrapper.Writer.ofday_ns_opt data ~name
  | Bool -> C_wrapper.Writer.bitset_opt (Valid.from_array (Stdlib.Array.map (Option.value ~default:false) data)) ~valid:(Valid.from_array (Stdlib.Array.map Option.is_some data)) ~name

let named_col packed_col name =
  match packed_col with
  | P (typ_, data) -> col data typ_ name
  | O (typ_, data) -> col_opt data typ_ name

type column = [`Index of int | `Name of string]

let read (type a) table ~column (col_type : a col_type) : a array =
  let wrapper_table = (Obj.magic table : C_wrapper.Table.t) in
  let wrapper_column = match column with
    | `Index i -> `Index i
    | `Name s -> `Name s
  in
  match col_type with
  | Int -> C_wrapper.Column.read_int wrapper_table ~column:wrapper_column
  | Float -> C_wrapper.Column.read_float wrapper_table ~column:wrapper_column
  | Utf8 -> C_wrapper.Column.read_utf8 wrapper_table ~column:wrapper_column
  | Date -> C_wrapper.Column.read_date wrapper_table ~column:wrapper_column
  | Time_ns -> C_wrapper.Column.read_time_ns wrapper_table ~column:wrapper_column
  | Span_ns -> C_wrapper.Column.read_span_ns wrapper_table ~column:wrapper_column
  | Ofday_ns -> C_wrapper.Column.read_ofday_ns wrapper_table ~column:wrapper_column
  | Bool ->
    let bitset = C_wrapper.Column.read_bitset wrapper_table ~column:wrapper_column in
    Valid.to_array bitset

let read_opt (type a) table ~column (col_type : a col_type) : a option array =
  let wrapper_table = (Obj.magic table : C_wrapper.Table.t) in
  let wrapper_column = match column with
    | `Index i -> `Index i
    | `Name s -> `Name s
  in
  match col_type with
  | Int -> C_wrapper.Column.read_int_opt wrapper_table ~column:wrapper_column
  | Float -> C_wrapper.Column.read_float_opt wrapper_table ~column:wrapper_column
  | Utf8 -> C_wrapper.Column.read_utf8_opt wrapper_table ~column:wrapper_column
  | Date -> C_wrapper.Column.read_date_opt wrapper_table ~column:wrapper_column
  | Time_ns -> C_wrapper.Column.read_time_ns_opt wrapper_table ~column:wrapper_column
  | Span_ns -> C_wrapper.Column.read_span_ns_opt wrapper_table ~column:wrapper_column
  | Ofday_ns -> C_wrapper.Column.read_ofday_ns_opt wrapper_table ~column:wrapper_column
  | Bool ->
    let bitset, valid = C_wrapper.Column.read_bitset_opt wrapper_table ~column:wrapper_column in
    let length = Valid.length bitset in
    Stdlib.Array.init length (fun i ->
      if Valid.get valid i then Some (Valid.get bitset i) else None)
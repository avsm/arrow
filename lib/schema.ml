(** High-level abstraction over Arrow schema structures *)

module Flags = struct
  type t = int

  let none = 0

  let all ts = List.fold_left (lor) 0 ts

  let nullable = 2  (* C_wrapper.Schema.Flags.nullable_ *)
  let dictionary_ordered = 1  (* C_wrapper.Schema.Flags.dictionary_ordered_ *)
  let map_keys_sorted = 4  (* C_wrapper.Schema.Flags.map_keys_sorted_ *)

  let is_nullable t = t land nullable <> 0
  let is_dictionary_ordered t = t land dictionary_ordered <> 0
  let is_map_keys_sorted t = t land map_keys_sorted <> 0

  (* Internal conversion functions *)
  let of_c_wrapper_flags = Fun.id
  let to_c_wrapper_flags = Fun.id
end

type t = {
  format : Type.t;
  name : string;
  metadata : (string * string) list;
  flags : Flags.t;
  children : t list;
}

let create ~format ~name ?(metadata = []) ?(flags = Flags.none) ?(children = []) () =
  { format; name; metadata; flags; children }

let format t = t.format
let name t = t.name
let metadata t = t.metadata
let flags t = t.flags
let children t = t.children

let rec to_string t =
  let flags_str =
    let flag_names = [] in
    let flag_names = if Flags.is_nullable t.flags then "nullable" :: flag_names else flag_names in
    let flag_names = if Flags.is_dictionary_ordered t.flags then "dictionary_ordered" :: flag_names else flag_names in
    let flag_names = if Flags.is_map_keys_sorted t.flags then "map_keys_sorted" :: flag_names else flag_names in
    match flag_names with
    | [] -> ""
    | flags -> " [" ^ String.concat ", " flags ^ "]"
  in
  let metadata_str =
    match t.metadata with
    | [] -> ""
    | meta ->
      let meta_pairs = List.map (fun (k, v) -> k ^ "=" ^ v) meta in
      " {" ^ String.concat ", " meta_pairs ^ "}"
  in
  let children_str =
    match t.children with
    | [] -> ""
    | children ->
      let child_strs = List.map to_string children in
      " <" ^ String.concat ", " child_strs ^ ">"
  in
  t.name ^ ":" ^ (match t.format with
    | Type.Null -> "null"
    | Type.Boolean -> "bool"
    | Type.Int8 -> "int8"
    | Type.Uint8 -> "uint8"
    | Type.Int16 -> "int16"
    | Type.Uint16 -> "uint16"
    | Type.Int32 -> "int32"
    | Type.Uint32 -> "uint32"
    | Type.Int64 -> "int64"
    | Type.Uint64 -> "uint64"
    | Type.Float16 -> "float16"
    | Type.Float32 -> "float32"
    | Type.Float64 -> "float64"
    | Type.Binary -> "binary"
    | Type.Large_binary -> "large_binary"
    | Type.Utf8_string -> "utf8"
    | Type.Large_utf8_string -> "large_utf8"
    | Type.Decimal128 {precision; scale} -> Printf.sprintf "decimal128(%d,%d)" precision scale
    | Type.Fixed_width_binary {bytes} -> Printf.sprintf "fixed_width_binary(%d)" bytes
    | Type.Date32 `Days -> "date32"
    | Type.Date64 `Milliseconds -> "date64"
    | Type.Time32 unit ->
      (match unit with
       | `Seconds -> "time32[s]"
       | `Milliseconds -> "time32[ms]")
    | Type.Time64 unit ->
      (match unit with
       | `Microseconds -> "time64[us]"
       | `Nanoseconds -> "time64[ns]")
    | Type.Timestamp {precision; timezone} ->
      let unit_str = match precision with
        | `Seconds -> "s"
        | `Milliseconds -> "ms"
        | `Microseconds -> "us"
        | `Nanoseconds -> "ns"
      in
      Printf.sprintf "timestamp[%s, tz=%s]" unit_str timezone
    | Type.Duration unit ->
      let unit_str = match unit with
        | `Seconds -> "s"
        | `Milliseconds -> "ms"
        | `Microseconds -> "us"
        | `Nanoseconds -> "ns"
      in
      Printf.sprintf "duration[%s]" unit_str
    | Type.Interval unit ->
      (match unit with
       | `Months -> "interval_months"
       | `Days_time -> "interval_day_time")
    | Type.Struct -> "struct"
    | Type.Map -> "map"
    | Type.Unknown s -> "unknown(" ^ s ^ ")"
  ) ^ flags_str ^ metadata_str ^ children_str

(* Internal conversion functions for C interop *)
let rec of_c_wrapper (c_schema : C_wrapper.Schema.t) : t =
  { format = c_schema.format;
    name = c_schema.name;
    metadata = c_schema.metadata;
    flags = Flags.of_c_wrapper_flags c_schema.flags;
    children = List.map of_c_wrapper c_schema.children;
  }

let rec to_c_wrapper (schema : t) : C_wrapper.Schema.t =
  { format = schema.format;
    name = schema.name;
    metadata = schema.metadata;
    flags = Flags.to_c_wrapper_flags schema.flags;
    children = List.map to_c_wrapper schema.children;
  }
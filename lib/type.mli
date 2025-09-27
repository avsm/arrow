type t =
  | Null
  | Boolean
  | Int8
  | Uint8
  | Int16
  | Uint16
  | Int32
  | Uint32
  | Int64
  | Uint64
  | Float16
  | Float32
  | Float64
  | Binary
  | Large_binary
  | Utf8_string
  | Large_utf8_string
  | Decimal128 of
      { precision : int
      ; scale : int
      }
  | Fixed_width_binary of { bytes : int }
  | Date32 of [ `Days ]
  | Date64 of [ `Milliseconds ]
  | Time32 of [ `Seconds | `Milliseconds ]
  | Time64 of [ `Microseconds | `Nanoseconds ]
  | Timestamp of
      { precision : [ `Seconds | `Milliseconds | `Microseconds | `Nanoseconds ]
      ; timezone : string
      }
  | Duration of [ `Seconds | `Milliseconds | `Microseconds | `Nanoseconds ]
  | Interval of [ `Months | `Days_time ]
  | Struct
  | Map
  | Unknown of string

val of_cstring : string -> t
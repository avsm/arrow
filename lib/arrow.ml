module Type = Type
module Time = Time
module Valid = Valid
module Compression = Compression
module Table = Table
module Column = Column
module Builder = Builder
module IO = Io

(* Legacy modules for compatibility during transition *)
module Wrapper = Wrapper
module Parquet_reader = Parquet_reader
module File_reader = File_reader

(* Direct Parquet access *)
module Parquet = Parquet

let version = "1.0.0"
(* Parquet module tests using external Arrow interface *)
open Arrow.Parquet

let test_compression_types () =
  (* Test compression type definitions *)
  let compressions = [
    Uncompressed;
    Snappy;
    Gzip None;
    Gzip (Some 9);
    Brotli None;
    Brotli (Some 11);
    Lz4;
    Zstd 10;
    Lzo;
  ] in
  List.iter (fun c ->
    let str = Format.asprintf "%a" pp_compression c in
    Alcotest.(check bool)
      (Printf.sprintf "Compression %s has non-empty string representation" str)
      true
      (String.length str > 0)
  ) compressions

let test_compression_levels () =
  (* Test compression level extraction *)
  let test_level name expected compression =
    let level = Writer.compression_level compression in
    Alcotest.(check int) name expected level
  in

  test_level "Uncompressed level" 0 Uncompressed;
  test_level "Snappy level" 0 Snappy;
  test_level "Gzip default level" 6 (Gzip None);
  test_level "Gzip custom level" 1 (Gzip (Some 1));
  test_level "Brotli default level" 1 (Brotli None);
  test_level "Brotli custom level" 11 (Brotli (Some 11));
  test_level "Lz4 level" 0 Lz4;
  test_level "Zstd level" 10 (Zstd 10);
  test_level "Lzo level" 0 Lzo

let test_compression_to_int () =
  (* Test compression type to integer mapping *)
  let test_int name expected compression =
    let int_val = Writer.compression_to_int compression in
    Alcotest.(check int) name expected int_val
  in

  test_int "Uncompressed int" 0 Uncompressed;
  test_int "Snappy int" 1 Snappy;
  test_int "Gzip int" 2 (Gzip None);
  test_int "Brotli int" 3 (Brotli None);
  test_int "Lz4 int" 4 Lz4;
  test_int "Lzo int" 5 Lzo;
  test_int "Zstd int" 6 (Zstd 3)

let test_encoding_types () =
  (* Test encoding type definitions *)
  let encodings = [
    Plain;
    Dictionary;
    Run_length;
    Bit_packed;
    Delta_binary_packed;
    Delta_length_byte_array;
    Delta_byte_array;
    Rle_dictionary;
  ] in
  List.iter (fun e ->
    let str = Format.asprintf "%a" pp_encoding e in
    Alcotest.(check bool)
      (Printf.sprintf "Encoding %s has non-empty string representation" str)
      true
      (String.length str > 0)
  ) encodings

let test_physical_types () =
  (* Test physical type definitions *)
  let physical_types = [
    Boolean;
    Int32;
    Int64;
    Int96;
    Float;
    Double;
    Byte_array;
    Fixed_len_byte_array 16;
  ] in
  List.iter (fun p ->
    let str = Format.asprintf "%a" pp_physical_type p in
    Alcotest.(check bool)
      (Printf.sprintf "Physical type %s has non-empty string representation" str)
      true
      (String.length str > 0)
  ) physical_types

let test_logical_types () =
  (* Test logical type definitions *)
  let test_logical name expected actual =
    Alcotest.(check bool) name true (actual = expected)
  in

  test_logical "No logical type" No_logical_type No_logical_type;
  test_logical "String type" String String;
  test_logical "Json type" Json Json;
  test_logical "Bson type" Bson Bson;
  test_logical "Uuid type" Uuid Uuid;
  test_logical "Date type" Date Date;

  (* Test Time type *)
  let time_millis = Time { is_adjusted_utc = false; unit = `Millis } in
  let time_micros = Time { is_adjusted_utc = true; unit = `Micros } in
  test_logical "Time millis" time_millis time_millis;
  test_logical "Time micros" time_micros time_micros;

  (* Test Timestamp type *)
  let timestamp_nanos = Timestamp { is_adjusted_utc = true; unit = `Nanos } in
  test_logical "Timestamp nanos" timestamp_nanos timestamp_nanos;

  (* Test Decimal type *)
  let decimal = Decimal { scale = 2; precision = 10 } in
  test_logical "Decimal type" decimal decimal;

  (* Test Integer type *)
  let int8 = Integer { bit_width = 8; is_signed = true } in
  let uint32 = Integer { bit_width = 32; is_signed = false } in
  test_logical "Int8 type" int8 int8;
  test_logical "Uint32 type" uint32 uint32

let test_repetition_types () =
  (* Test repetition type definitions *)
  Alcotest.(check bool) "Required repetition" true (Required = Required);
  Alcotest.(check bool) "Optional repetition" true (Optional = Optional);
  Alcotest.(check bool) "Repeated repetition" true (Repeated = Repeated);

  (* Test that they're different *)
  Alcotest.(check bool) "Required != Optional" true (Required <> Optional);
  Alcotest.(check bool) "Required != Repeated" true (Required <> Repeated);
  Alcotest.(check bool) "Optional != Repeated" true (Optional <> Repeated)

let test_writer_properties () =
  (* Test default writer properties *)
  let props = default_writer_properties in

  Alcotest.(check bool) "Default compression is Snappy" true (props.compression = Snappy);
  Alcotest.(check bool) "Dictionary enabled by default" true props.dictionary_enabled;
  Alcotest.(check int64) "Default dictionary page size" 1048576L props.dictionary_page_size;
  Alcotest.(check int64) "Default data page size" 1048576L props.data_page_size;
  Alcotest.(check int64) "Default write batch size" 1024L props.write_batch_size;
  Alcotest.(check int64) "Default max row group length" 1048576L props.max_row_group_length;
  Alcotest.(check bool) "Default writer version is V2" true (props.writer_version = V2);
  Alcotest.(check string) "Default created_by string" "ocaml-parquet" props.created_by;
  Alcotest.(check bool) "Statistics enabled by default" true props.enable_statistics

let test_reader_properties () =
  (* Test default reader properties *)
  let reader_props = default_reader_properties in

  Alcotest.(check bool) "Memory map enabled by default" true reader_props.use_memory_map;
  Alcotest.(check int) "Default buffer size" 8192 reader_props.buffer_size;
  Alcotest.(check bool) "Prefetch row groups disabled by default" false reader_props.prefetch_row_groups;
  Alcotest.(check bool) "Verify checksums disabled by default" false reader_props.verify_checksums

let test_schema_creation () =
  (* Test schema node creation *)
  let schema : schema_node = {
    name = "root";
    physical_type = None;
    logical_type = Some No_logical_type;
    repetition = Required;
    children = [
      {
        name = "id";
        physical_type = Some Int64;
        logical_type = Some No_logical_type;
        repetition = Required;
        children = [];
      };
      {
        name = "name";
        physical_type = Some Byte_array;
        logical_type = Some String;
        repetition = Optional;
        children = [];
      };
      {
        name = "timestamp";
        physical_type = Some Int64;
        logical_type = Some (Timestamp {
          is_adjusted_utc = true;
          unit = `Micros;
        });
        repetition = Required;
        children = [];
      };
    ];
  } in

  let str = Format.asprintf "%a" pp_schema_node schema in
  Alcotest.(check bool) "Schema has non-empty string representation" true (String.length str > 0);

  (* Check schema structure *)
  Alcotest.(check string) "Root node name" "root" schema.name;
  Alcotest.(check bool) "Root has no physical type" true (schema.physical_type = None);
  Alcotest.(check int) "Root has 3 children" 3 (List.length schema.children);

  (* Check first child (id) *)
  let id_field = List.nth schema.children 0 in
  Alcotest.(check string) "ID field name" "id" id_field.name;
  Alcotest.(check bool) "ID field is Int64" true (id_field.physical_type = Some Int64);
  Alcotest.(check bool) "ID field is required" true (id_field.repetition = Required);

  (* Check second child (name) *)
  let name_field = List.nth schema.children 1 in
  Alcotest.(check string) "Name field name" "name" name_field.name;
  Alcotest.(check bool) "Name field is Byte_array" true (name_field.physical_type = Some Byte_array);
  Alcotest.(check bool) "Name field is optional" true (name_field.repetition = Optional);
  Alcotest.(check bool) "Name field has String logical type" true (name_field.logical_type = Some String);

  (* Check third child (timestamp) *)
  let timestamp_field = List.nth schema.children 2 in
  Alcotest.(check string) "Timestamp field name" "timestamp" timestamp_field.name;
  Alcotest.(check bool) "Timestamp field is Int64" true (timestamp_field.physical_type = Some Int64);
  Alcotest.(check bool) "Timestamp field is required" true (timestamp_field.repetition = Required);

  match timestamp_field.logical_type with
  | Some (Timestamp ts) ->
    Alcotest.(check bool) "Timestamp is adjusted to UTC" true ts.is_adjusted_utc;
    Alcotest.(check bool) "Timestamp unit is microseconds" true (ts.unit = `Micros)
  | _ -> Alcotest.fail "Timestamp field should have Timestamp logical type"

let test_version () =
  (* Test version string *)
  let version_str = version () in
  Alcotest.(check bool) "Version string is non-empty" true (String.length version_str > 0);
  Alcotest.(check bool) "Version contains parquet" true
    (String.lowercase_ascii version_str |> fun s -> String.contains s 'p');
  Alcotest.(check bool) "Version starts with parquet-cpp" true
    (String.length version_str >= 12 && String.sub version_str 0 12 = "parquet-cpp-")

let create_test_table ~num_rows =
  (* Helper to create test data *)
  let int_data = Array.init num_rows (fun i -> i) in
  let float_data = Array.init num_rows (fun i -> float_of_int i *. 1.5) in
  let string_data = Array.init num_rows (fun i -> Printf.sprintf "row_%d" i) in
  let bool_data = Array.init num_rows (fun i -> i mod 2 = 0) in
  let int_opt_data = Array.init num_rows (fun i ->
    if i mod 3 = 0 then None else Some i) in

  Arrow.Table.create [
    Arrow.Table.col int_data Arrow.Table.Int "integers";
    Arrow.Table.col float_data Arrow.Table.Float "floats";
    Arrow.Table.col string_data Arrow.Table.Utf8 "strings";
    Arrow.Table.col bool_data Arrow.Table.Bool "booleans";
    Arrow.Table.col_opt int_opt_data Arrow.Table.Int "optional_ints";
  ]

let test_write_read_roundtrip () =
  (* Test complete write/read roundtrip *)
  let table = create_test_table ~num_rows:100 in
  let filename = Filename.temp_file "test_parquet_" ".parquet" in

  (* Write table *)
  Arrow.IO.Parquet.write ~compression:Arrow.Parquet.Snappy table filename;

  (* Read back using Parquet reader *)
  let reader = Reader.open_file filename in
  let metadata = Reader.metadata reader in

  Alcotest.(check int64) "Metadata row count" 100L metadata.num_rows;
  Alcotest.(check bool) "Has row groups" true (List.length metadata.row_groups > 0);

  Reader.close reader;

  (* Read table data *)
  let read_table = Arrow.IO.Parquet.read filename in
  Alcotest.(check int) "Read table row count" 100 (Arrow.Table.num_rows read_table);

  (* Verify data integrity *)
  let ints = Arrow.Table.read read_table ~column:(`Name "integers") Arrow.Table.Int in
  let floats = Arrow.Table.read read_table ~column:(`Name "floats") Arrow.Table.Float in
  let strings = Arrow.Table.read read_table ~column:(`Name "strings") Arrow.Table.Utf8 in

  Alcotest.(check int) "First integer" 0 ints.(0);
  Alcotest.(check (float 1e-6)) "First float" 0.0 floats.(0);
  Alcotest.(check string) "First string" "row_0" strings.(0);

  Sys.remove filename

let test_compression_roundtrips () =
  (* Test different compression algorithms *)
  let table = create_test_table ~num_rows:50 in

  let test_compression_type name compression =
    let filename = Filename.temp_file "test_comp_" ".parquet" in
    Arrow.IO.Parquet.write ~compression table filename;
    let read_table = Arrow.IO.Parquet.read filename in
    Alcotest.(check int) (Printf.sprintf "%s preserves data" name) 50 (Arrow.Table.num_rows read_table);
    let file_size = (Unix.stat filename).st_size in
    Sys.remove filename;
    file_size
  in

  let uncompressed_size = test_compression_type "uncompressed" Arrow.Parquet.Uncompressed in
  let snappy_size = test_compression_type "snappy" Arrow.Parquet.Snappy in
  let gzip_size = test_compression_type "gzip" (Arrow.Parquet.Gzip (Some 6)) in

  (* Verify compression reduces file size *)
  Alcotest.(check bool) "Snappy compresses" true (snappy_size < uncompressed_size);
  Alcotest.(check bool) "Gzip compresses" true (gzip_size < uncompressed_size)

let test_empty_table () =
  (* Test empty table handling *)
  let table = Arrow.Table.create [
    Arrow.Table.col [||] Arrow.Table.Int "empty_int";
    Arrow.Table.col [||] Arrow.Table.Utf8 "empty_string";
  ] in

  let filename = Filename.temp_file "test_empty_" ".parquet" in
  Arrow.IO.Parquet.write table filename;

  let read_table = Arrow.IO.Parquet.read filename in
  Alcotest.(check int) "Empty table rows" 0 (Arrow.Table.num_rows read_table);

  let reader = Reader.open_file filename in
  let metadata = Reader.metadata reader in
  Alcotest.(check int64) "Empty metadata rows" 0L metadata.num_rows;
  Reader.close reader;

  Sys.remove filename

let test_metadata_access () =
  (* Test metadata access functionality *)
  let table = create_test_table ~num_rows:200 in
  let filename = Filename.temp_file "test_metadata_" ".parquet" in

  Arrow.IO.Parquet.write ~compression:(Arrow.Parquet.Gzip (Some 6)) table filename;

  let metadata = Arrow.IO.Parquet.metadata filename in
  Alcotest.(check int64) "Metadata reports correct rows" 200L metadata.num_rows;
  Alcotest.(check bool) "Metadata has row groups" true (List.length metadata.row_groups > 0);

  let first_rg = List.hd metadata.row_groups in
  Alcotest.(check int64) "First row group has rows" 200L first_rg.num_rows;
  Alcotest.(check int) "First row group has 5 columns" 5 (List.length first_rg.columns);

  Sys.remove filename

let () =
  let open Alcotest in
  run "Parquet tests" [
    "types", [
      test_case "Compression types" `Quick test_compression_types;
      test_case "Compression levels" `Quick test_compression_levels;
      test_case "Compression to int mapping" `Quick test_compression_to_int;
      test_case "Encoding types" `Quick test_encoding_types;
      test_case "Physical types" `Quick test_physical_types;
      test_case "Logical types" `Quick test_logical_types;
      test_case "Repetition types" `Quick test_repetition_types;
    ];
    "properties", [
      test_case "Writer properties" `Quick test_writer_properties;
      test_case "Reader properties" `Quick test_reader_properties;
    ];
    "schema", [
      test_case "Schema creation" `Quick test_schema_creation;
    ];
    "library", [
      test_case "Version info" `Quick test_version;
    ];
    "roundtrip", [
      test_case "Write/read roundtrip" `Quick test_write_read_roundtrip;
      test_case "Compression roundtrips" `Quick test_compression_roundtrips;
      test_case "Empty table" `Quick test_empty_table;
    ];
    "metadata", [
      test_case "Metadata access" `Quick test_metadata_access;
    ];
  ]
open Arrow.Parquet

let test_compression_types () =
  (* Test compression types *)
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
  (* Test that compression levels are properly extracted *)
  let test_level name expected compression =
    let module W = Writer in
    let level = W.compression_level compression in
    Alcotest.(check int) name expected level
  in

  test_level "Uncompressed has level 0" 0 Uncompressed;
  test_level "Snappy has level 0" 0 Snappy;
  test_level "Gzip default level is 6" 6 (Gzip None);
  test_level "Gzip with level 1" 1 (Gzip (Some 1));
  test_level "Gzip with level 9" 9 (Gzip (Some 9));
  test_level "Brotli default level is 1" 1 (Brotli None);
  test_level "Brotli with level 0" 0 (Brotli (Some 0));
  test_level "Brotli with level 11" 11 (Brotli (Some 11));
  test_level "Lz4 has level 0" 0 Lz4;
  test_level "Zstd with level 1" 1 (Zstd 1);
  test_level "Zstd with level 22" 22 (Zstd 22);
  test_level "Lzo has level 0" 0 Lzo

let test_compression_to_int () =
  (* Test that compression types map to correct integers *)
  let module W = Writer in
  let test_int name expected compression =
    let int_val = W.compression_to_int compression in
    Alcotest.(check int) name expected int_val
  in

  test_int "Uncompressed maps to 0" 0 Uncompressed;
  test_int "Snappy maps to 1" 1 Snappy;
  test_int "Gzip None maps to 2" 2 (Gzip None);
  test_int "Gzip Some 9 maps to 2" 2 (Gzip (Some 9));
  test_int "Brotli None maps to 3" 3 (Brotli None);
  test_int "Brotli Some 11 maps to 3" 3 (Brotli (Some 11));
  test_int "Lz4 maps to 4" 4 Lz4;
  test_int "Lzo maps to 5" 5 Lzo;
  test_int "Zstd maps to 6" 6 (Zstd 3);
  test_int "Zstd 22 maps to 6" 6 (Zstd 22)

let test_encoding_types () =
  (* Test encoding types *)
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
  (* Test physical types *)
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

let test_default_writer_properties () =
  let props = default_writer_properties in

  Alcotest.(check bool) "Default compression is Snappy" true (props.compression = Snappy);
  Alcotest.(check bool) "Dictionary is enabled by default" true props.dictionary_enabled;
  Alcotest.(check int64) "Default dictionary page size" 1048576L props.dictionary_page_size;
  Alcotest.(check int64) "Default data page size" 1048576L props.data_page_size;
  Alcotest.(check int64) "Default write batch size" 1024L props.write_batch_size;
  Alcotest.(check int64) "Default max row group length" 1048576L props.max_row_group_length;
  Alcotest.(check bool) "Default writer version is V2" true (props.writer_version = V2);
  Alcotest.(check string) "Default created_by string" "ocaml-parquet" props.created_by;
  Alcotest.(check bool) "Statistics are enabled by default" true props.enable_statistics

let test_default_reader_properties () =
  let reader_props = default_reader_properties in

  Alcotest.(check bool) "Memory map is enabled by default" true reader_props.use_memory_map;
  Alcotest.(check int) "Default buffer size" 8192 reader_props.buffer_size;
  Alcotest.(check bool) "Prefetch row groups is disabled by default" false reader_props.prefetch_row_groups;
  Alcotest.(check bool) "Verify checksums is disabled by default" false reader_props.verify_checksums

let test_schema_creation () =
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
  Alcotest.(check string) "First field is id" "id" id_field.name;
  Alcotest.(check bool) "id field is Int64" true (id_field.physical_type = Some Int64);
  Alcotest.(check bool) "id field is required" true (id_field.repetition = Required);

  (* Check second child (name) *)
  let name_field = List.nth schema.children 1 in
  Alcotest.(check string) "Second field is name" "name" name_field.name;
  Alcotest.(check bool) "name field is Byte_array" true (name_field.physical_type = Some Byte_array);
  Alcotest.(check bool) "name field is optional" true (name_field.repetition = Optional);
  Alcotest.(check bool) "name field has String logical type" true (name_field.logical_type = Some String);

  (* Check third child (timestamp) *)
  let timestamp_field = List.nth schema.children 2 in
  Alcotest.(check string) "Third field is timestamp" "timestamp" timestamp_field.name;
  Alcotest.(check bool) "timestamp field is Int64" true (timestamp_field.physical_type = Some Int64);
  Alcotest.(check bool) "timestamp field is required" true (timestamp_field.repetition = Required);

  match timestamp_field.logical_type with
  | Some (Timestamp ts) ->
    Alcotest.(check bool) "timestamp is adjusted to UTC" true ts.is_adjusted_utc;
    Alcotest.(check bool) "timestamp unit is microseconds" true (ts.unit = `Micros)
  | _ -> Alcotest.fail "timestamp field should have Timestamp logical type"

let test_logical_types () =
  (* Test various logical types *)
  let test_logical name expected actual =
    Alcotest.(check bool) name true (actual = expected)
  in

  test_logical "No logical type" No_logical_type No_logical_type;
  test_logical "String type" String String;
  test_logical "Json type" Json Json;
  test_logical "Bson type" Bson Bson;
  test_logical "Uuid type" Uuid Uuid;

  (* Test Date type *)
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
  (* Test repetition types *)
  Alcotest.(check bool) "Required repetition" true (Required = Required);
  Alcotest.(check bool) "Optional repetition" true (Optional = Optional);
  Alcotest.(check bool) "Repeated repetition" true (Repeated = Repeated);

  (* Test that they're different *)
  Alcotest.(check bool) "Required != Optional" true (Required <> Optional);
  Alcotest.(check bool) "Required != Repeated" true (Required <> Repeated);
  Alcotest.(check bool) "Optional != Repeated" true (Optional <> Repeated)

let test_version () =
  let version = version () in
  Alcotest.(check bool) "Version string is non-empty" true (String.length version > 0);
  Alcotest.(check bool) "Version contains 'parquet'" true
    (String.lowercase_ascii version |> fun s -> String.contains s 'p' && String.contains s 'a');
  (* Check it starts with expected prefix *)
  Alcotest.(check bool) "Version starts with parquet-cpp" true
    (String.length version >= 12 && String.sub version 0 12 = "parquet-cpp-")

(*****************************************************************************)
(* Test data generation and helpers                                          *)
(*****************************************************************************)

let generate_test_data ~num_rows =
  let int_data = Array.init num_rows (fun i -> i) in
  let float_data = Array.init num_rows (fun i -> float_of_int i *. 1.5) in
  let string_data = Array.init num_rows (fun i -> Printf.sprintf "row_%d" i) in
  let bool_data = Array.init num_rows (fun i -> i mod 2 = 0) in
  let int_opt_data = Array.init num_rows (fun i ->
    if i mod 3 = 0 then None else Some i) in
  let string_opt_data = Array.init num_rows (fun i ->
    if i mod 5 = 0 then None else Some (Printf.sprintf "opt_%d" i)) in
  (int_data, float_data, string_data, bool_data, int_opt_data, string_opt_data)

let create_test_table ~num_rows =
  let (int_data, float_data, string_data, bool_data, int_opt_data, string_opt_data) =
    generate_test_data ~num_rows in
  let cols = [
    Arrow.Table.col int_data Arrow.Table.Int ~name:"integers";
    Arrow.Table.col float_data Arrow.Table.Float ~name:"floats";
    Arrow.Table.col string_data Arrow.Table.Utf8 ~name:"strings";
    Arrow.Table.col bool_data Arrow.Table.Bool ~name:"booleans";
    Arrow.Table.col_opt int_opt_data Arrow.Table.Int ~name:"optional_ints";
    Arrow.Table.col_opt string_opt_data Arrow.Table.Utf8 ~name:"optional_strings";
  ] in
  Arrow.Table.create cols

let verify_arrays ~name expected actual =
  let len_expected = Array.length expected in
  let len_actual = Array.length actual in
  Alcotest.(check int)
    (Printf.sprintf "%s: array lengths match" name)
    len_expected len_actual;

  Array.iteri (fun i exp ->
    Alcotest.(check string)
      (Printf.sprintf "%s[%d]" name i)
      (Printf.sprintf "%s" exp)
      (Printf.sprintf "%s" actual.(i))
  ) expected

let verify_int_arrays ~name expected actual =
  verify_arrays ~name
    (Array.map string_of_int expected)
    (Array.map string_of_int actual)

let verify_float_arrays ~name expected actual =
  let len_expected = Array.length expected in
  let len_actual = Array.length actual in
  Alcotest.(check int)
    (Printf.sprintf "%s: array lengths match" name)
    len_expected len_actual;

  Array.iteri (fun i exp ->
    let diff = abs_float (exp -. actual.(i)) in
    Alcotest.(check bool)
      (Printf.sprintf "%s[%d]: %f ~= %f" name i exp actual.(i))
      true
      (diff < 1e-6)
  ) expected

let verify_bool_arrays ~name expected actual =
  verify_arrays ~name
    (Array.map string_of_bool expected)
    (Array.map string_of_bool actual)

let verify_opt_arrays ~name ~to_string expected actual =
  let len_expected = Array.length expected in
  let len_actual = Array.length actual in
  Alcotest.(check int)
    (Printf.sprintf "%s: array lengths match" name)
    len_expected len_actual;

  Array.iteri (fun i exp ->
    match exp, actual.(i) with
    | None, None -> ()
    | Some e, Some a ->
        Alcotest.(check string)
          (Printf.sprintf "%s[%d]" name i)
          (to_string e)
          (to_string a)
    | None, Some a ->
        Alcotest.fail (Printf.sprintf "%s[%d]: expected None, got Some %s" name i (to_string a))
    | Some e, None ->
        Alcotest.fail (Printf.sprintf "%s[%d]: expected Some %s, got None" name i (to_string e))
  ) expected

(*****************************************************************************)
(* Write/Read Roundtrip Tests *)
(*****************************************************************************)

let test_write_read_roundtrip ~compression ~num_rows () =
  let filename = Printf.sprintf "/tmp/test_parquet_%d_%s.parquet"
    num_rows
    (match compression with
     | Arrow.Compression.None -> "uncompressed"
     | Arrow.Compression.Snappy -> "snappy"
     | Arrow.Compression.Gzip -> "gzip"
     | Arrow.Compression.Brotli -> "brotli"
     | Arrow.Compression.Lz4 -> "lz4"
     | Arrow.Compression.Lz4_raw -> "lz4_raw"
     | Arrow.Compression.Zstd -> "zstd") in

  (* Create and write table *)
  let original_table = create_test_table ~num_rows in
  Arrow.Table.write_parquet original_table filename ~compression;

  (* Read back the table *)
  let read_table = Arrow.Parquet_reader.table filename in

  (* Verify the data *)
  let (expected_int, expected_float, expected_string, expected_bool, expected_int_opt, expected_string_opt) =
    generate_test_data ~num_rows in

  let actual_int = Arrow.Table.read read_table ~column:(`Name "integers") Arrow.Table.Int in
  let actual_float = Arrow.Table.read read_table ~column:(`Name "floats") Arrow.Table.Float in
  let actual_string = Arrow.Table.read read_table ~column:(`Name "strings") Arrow.Table.Utf8 in
  let actual_bool = Arrow.Table.read read_table ~column:(`Name "booleans") Arrow.Table.Bool in
  let actual_int_opt = Arrow.Table.read_opt read_table ~column:(`Name "optional_ints") Arrow.Table.Int in
  let actual_string_opt = Arrow.Table.read_opt read_table ~column:(`Name "optional_strings") Arrow.Table.Utf8 in

  verify_int_arrays ~name:"integers" expected_int actual_int;
  verify_float_arrays ~name:"floats" expected_float actual_float;
  verify_arrays ~name:"strings" expected_string actual_string;
  verify_bool_arrays ~name:"booleans" expected_bool actual_bool;
  verify_opt_arrays ~name:"optional_ints" ~to_string:string_of_int expected_int_opt actual_int_opt;
  verify_opt_arrays ~name:"optional_strings" ~to_string:(fun s -> s) expected_string_opt actual_string_opt;

  (* Clean up *)
  Sys.remove filename

let test_large_file ~compression () =
  test_write_read_roundtrip ~compression ~num_rows:10000 ()

let test_empty_table () =
  let filename = "/tmp/test_empty.parquet" in
  let cols = [
    Arrow.Table.col [||] Arrow.Table.Int ~name:"empty_int";
    Arrow.Table.col [||] Arrow.Table.Utf8 ~name:"empty_string";
  ] in
  let table = Arrow.Table.create cols in
  Arrow.Table.write_parquet table filename;

  let read_table = Arrow.Parquet_reader.table filename in
  let actual_int = Arrow.Table.read read_table ~column:(`Name "empty_int") Arrow.Table.Int in
  let actual_string = Arrow.Table.read read_table ~column:(`Name "empty_string") Arrow.Table.Utf8 in

  Alcotest.(check int) "Empty int array" 0 (Array.length actual_int);
  Alcotest.(check int) "Empty string array" 0 (Array.length actual_string);

  Sys.remove filename

let test_single_row () =
  test_write_read_roundtrip ~compression:Arrow.Compression.Snappy ~num_rows:1 ()

let test_metadata_preservation () =
  let filename = "/tmp/test_metadata.parquet" in
  let table = create_test_table ~num_rows:100 in

  (* Write with specific compression *)
  Arrow.Table.write_parquet table filename ~compression:Arrow.Compression.Gzip;

  (* Read metadata using the Parquet module *)
  let reader = Reader.open_file filename in
  let metadata = Reader.metadata reader in

  Alcotest.(check int64) "Row count matches" 100L metadata.num_rows;
  Alcotest.(check bool) "Has row groups" true (List.length metadata.row_groups > 0);

  (* Check first row group *)
  let first_rg = List.hd metadata.row_groups in
  Alcotest.(check int64) "Row group has correct row count" 100L first_rg.num_rows;
  Alcotest.(check int) "Row group has 6 columns" 6 (List.length first_rg.columns);

  Reader.close reader;
  Sys.remove filename

let test_batch_sizes () =
  let test_batch_size chunk_size =
    let filename = Printf.sprintf "/tmp/test_batch_%d.parquet" chunk_size in
    let table = create_test_table ~num_rows:1000 in

    Arrow.Table.write_parquet table filename ~chunk_size;

    let reader = Reader.open_file filename in
    let metadata = Reader.metadata reader in

    (* Verify data was written *)
    Alcotest.(check int64)
      (Printf.sprintf "Row count with chunk_size=%d" chunk_size)
      1000L
      metadata.num_rows;

    Reader.close reader;
    Sys.remove filename
  in

  List.iter test_batch_size [10; 100; 500; 1000; 10000]

let test_special_characters () =
  let filename = "/tmp/test_special_chars.parquet" in
  let special_strings = [|
    "simple";
    "with spaces";
    "with\ttabs";
    "with\nnewlines";
    "with\"quotes\"";
    "with'apostrophes'";
    "unicode: α β γ δ ε";
    "emoji: 😀 🎉 🚀";
    "";  (* empty string *)
  |] in

  let cols = [
    Arrow.Table.col special_strings Arrow.Table.Utf8 ~name:"special_chars";
  ] in
  let table = Arrow.Table.create cols in

  Arrow.Table.write_parquet table filename;
  let read_table = Arrow.Parquet_reader.table filename in
  let actual = Arrow.Table.read read_table ~column:(`Name "special_chars") Arrow.Table.Utf8 in

  verify_arrays ~name:"special_chars" special_strings actual;

  Sys.remove filename

let test_all_nulls () =
  let filename = "/tmp/test_all_nulls.parquet" in
  let all_none : int option array = Array.make 100 None in
  let cols = [
    Arrow.Table.col_opt all_none Arrow.Table.Int ~name:"all_nulls";
  ] in
  let table = Arrow.Table.create cols in

  Arrow.Table.write_parquet table filename;
  let read_table = Arrow.Parquet_reader.table filename in
  let actual = Arrow.Table.read_opt read_table ~column:(`Name "all_nulls") Arrow.Table.Int in

  Array.iteri (fun i v ->
    match v with
    | None -> ()
    | Some x -> Alcotest.fail (Printf.sprintf "Expected None at index %d, got Some %d" i x)
  ) actual;

  Sys.remove filename

(*****************************************************************************)
(* Main test suite *)
(*****************************************************************************)

let () =
  let open Alcotest in

  (* Generate compression test cases *)
  let compression_tests =
    let compressions = [
      ("uncompressed", Arrow.Compression.None);
      ("snappy", Arrow.Compression.Snappy);
      ("gzip", Arrow.Compression.Gzip);
      ("brotli", Arrow.Compression.Brotli);
      (* Note: Lz4 and Zstd are not currently supported for Parquet in Arrow *)
    ] in
    let sizes = [
      ("small", 10);
      ("medium", 100);
      ("large", 1000);
    ] in
    List.flatten (List.map (fun (comp_name, compression) ->
      List.map (fun (size_name, num_rows) ->
        test_case
          (Printf.sprintf "%s-%s" comp_name size_name)
          `Quick
          (test_write_read_roundtrip ~compression ~num_rows)
      ) sizes
    ) compressions)
  in

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
      test_case "Default writer properties" `Quick test_default_writer_properties;
      test_case "Default reader properties" `Quick test_default_reader_properties;
    ];
    "schema", [
      test_case "Schema creation" `Quick test_schema_creation;
    ];
    "library", [
      test_case "Version" `Quick test_version;
    ];
    "roundtrip", compression_tests;
    "large_files", [
      test_case "large-uncompressed" `Slow (test_large_file ~compression:Arrow.Compression.None);
      test_case "large-snappy" `Slow (test_large_file ~compression:Arrow.Compression.Snappy);
      test_case "large-gzip" `Slow (test_large_file ~compression:Arrow.Compression.Gzip);
    ];
    "edge_cases", [
      test_case "empty table" `Quick test_empty_table;
      test_case "single row" `Quick test_single_row;
      test_case "all nulls" `Quick test_all_nulls;
      test_case "special characters" `Quick test_special_characters;
    ];
    "features", [
      test_case "metadata preservation" `Quick test_metadata_preservation;
      test_case "batch sizes" `Quick test_batch_sizes;
    ];
  ]

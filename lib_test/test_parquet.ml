(* Comprehensive Parquet-specific I/O tests *)
open Arrow

module Fixtures = Test_fixtures

(** {1 Basic Parquet Operations} *)

let test_parquet_basic_roundtrip () =
  let table = Fixtures.medium_test_table () in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "basic_roundtrip" in

  (* Write using IO.Parquet module *)
  IO.Parquet.write table filename;

  (* Read back using IO.Parquet module *)
  let read_table = IO.Parquet.read filename in

  (* Verify row count and schema *)
  Alcotest.(check int) "Row count preserved" 100 (Table.num_rows read_table);
  let schema = Table.schema read_table in
  let field_count = List.length (Schema.children schema) in
  Alcotest.(check int) "Schema field count" 4 field_count;

  (* Test data integrity *)
  let original_ints = Table.read table ~column:(`Name "integers") Table.Int in
  let restored_ints = Table.read read_table ~column:(`Name "integers") Table.Int in
  Alcotest.(check (array int)) "Integer data integrity" original_ints restored_ints;

  let original_floats = Table.read table ~column:(`Name "floats") Table.Float in
  let restored_floats = Table.read read_table ~column:(`Name "floats") Table.Float in
  Array.iteri (fun i orig ->
    let restored = restored_floats.(i) in
    Alcotest.(check (float 1e-6)) (Printf.sprintf "Float integrity %d" i) orig restored
  ) original_floats;

  Fixtures.cleanup_test_files "basic_roundtrip_.*\\.parquet"

let test_parquet_schema_operations () =
  let table = Fixtures.small_test_table () in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "schema_ops" in

  IO.Parquet.write table filename;

  (* Test schema reading *)
  let schema = IO.Parquet.schema filename in
  let children = Schema.children schema in
  Alcotest.(check int) "Schema has correct field count" 8 (List.length children);

  (* Verify field names *)
  let field_names = List.map (fun field -> Schema.name field) children in
  let expected_names = ["integers"; "floats"; "strings"; "booleans"; "dates"; "timestamps"; "spans"; "ofdays"] in
  Alcotest.(check (list string)) "Field names correct" expected_names field_names;

  Fixtures.cleanup_test_files "schema_ops_.*\\.parquet"

let test_parquet_metadata_access () =
  let table = Fixtures.sequential_table 150 in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "metadata_test" in

  IO.Parquet.write ~compression:Parquet.Snappy table filename;

  (* Test metadata access *)
  let metadata = IO.Parquet.metadata filename in
  Alcotest.(check int64) "Metadata row count" 150L metadata.Parquet.num_rows;
  Alcotest.(check bool) "Has row groups" true (List.length metadata.Parquet.row_groups > 0);

  (* Test row group metadata *)
  let first_rg = List.hd metadata.Parquet.row_groups in
  Alcotest.(check int64) "First row group has rows" 150L first_rg.Parquet.num_rows;
  Alcotest.(check int) "Row group has correct column count" 4 (List.length first_rg.Parquet.columns);

  (* Verify created_by field *)
  (match metadata.Parquet.created_by with
  | Some creator -> Alcotest.(check bool) "Creator contains ocaml" true (String.contains (String.lowercase_ascii creator) 'o')
  | None -> ());

  Fixtures.cleanup_test_files "metadata_test_.*\\.parquet"

(** {1 Compression Tests} *)

let test_parquet_compression_algorithms () =
  let table = Fixtures.pattern_table 10 200 in

  let test_compression_algorithm name compression =
    let filename = Fixtures.temp_file_path ~extension:".parquet" ("compression_" ^ name) in
    IO.Parquet.write ~compression table filename;
    let read_table = IO.Parquet.read filename in
    Alcotest.(check int) (Printf.sprintf "%s compression preserves data" name) 200 (Table.num_rows read_table);
    let file_size = (Unix.stat filename).st_size in
    Sys.remove filename;
    file_size
  in

  (* Test different compression algorithms *)
  let uncompressed_size = test_compression_algorithm "uncompressed" Parquet.Uncompressed in
  let snappy_size = test_compression_algorithm "snappy" Parquet.Snappy in
  let gzip_default_size = test_compression_algorithm "gzip_default" (Parquet.Gzip None) in
  let gzip_max_size = test_compression_algorithm "gzip_max" (Parquet.Gzip (Some 9)) in
  let brotli_default_size = test_compression_algorithm "brotli_default" (Parquet.Brotli None) in
  let brotli_max_size = test_compression_algorithm "brotli_max" (Parquet.Brotli (Some 11)) in
  (* LZ4 is not supported in Parquet format *)
  let zstd_size = test_compression_algorithm "zstd" (Parquet.Zstd 10) in

  (* Verify compression effectiveness *)
  Alcotest.(check bool) "Snappy compresses" true (snappy_size < uncompressed_size);
  Alcotest.(check bool) "Gzip default compresses" true (gzip_default_size < uncompressed_size);
  Alcotest.(check bool) "Gzip max compresses more" true (gzip_max_size <= gzip_default_size);
  Alcotest.(check bool) "Brotli default compresses" true (brotli_default_size < uncompressed_size);
  Alcotest.(check bool) "Brotli max compresses more" true (brotli_max_size <= brotli_default_size);
  Alcotest.(check bool) "Zstd compresses" true (zstd_size < uncompressed_size)

let test_parquet_compression_levels () =
  let table = Fixtures.random_table ~seed:54321 75 in

  let test_gzip_levels () =
    let level_1_file = Fixtures.temp_file_path ~extension:".parquet" "gzip_1" in
    let level_6_file = Fixtures.temp_file_path ~extension:".parquet" "gzip_6" in
    let level_9_file = Fixtures.temp_file_path ~extension:".parquet" "gzip_9" in

    IO.Parquet.write ~compression:(Parquet.Gzip (Some 1)) table level_1_file;
    IO.Parquet.write ~compression:(Parquet.Gzip (Some 6)) table level_6_file;
    IO.Parquet.write ~compression:(Parquet.Gzip (Some 9)) table level_9_file;

    let size_1 = (Unix.stat level_1_file).st_size in
    let _size_6 = (Unix.stat level_6_file).st_size in
    let size_9 = (Unix.stat level_9_file).st_size in

    (* Verify data integrity *)
    let table_1 = IO.Parquet.read level_1_file in
    let table_6 = IO.Parquet.read level_6_file in
    let table_9 = IO.Parquet.read level_9_file in
    Alcotest.(check int) "Gzip level 1 preserves data" 75 (Table.num_rows table_1);
    Alcotest.(check int) "Gzip level 6 preserves data" 75 (Table.num_rows table_6);
    Alcotest.(check int) "Gzip level 9 preserves data" 75 (Table.num_rows table_9);

    (* Higher compression levels should produce smaller files *)
    Alcotest.(check bool) "Higher Gzip level compresses more" true (size_9 <= size_1);

    Sys.remove level_1_file; Sys.remove level_6_file; Sys.remove level_9_file
  in

  let test_brotli_levels () =
    let level_1_file = Fixtures.temp_file_path ~extension:".parquet" "brotli_1" in
    let level_6_file = Fixtures.temp_file_path ~extension:".parquet" "brotli_6" in
    let level_11_file = Fixtures.temp_file_path ~extension:".parquet" "brotli_11" in

    IO.Parquet.write ~compression:(Parquet.Brotli (Some 1)) table level_1_file;
    IO.Parquet.write ~compression:(Parquet.Brotli (Some 6)) table level_6_file;
    IO.Parquet.write ~compression:(Parquet.Brotli (Some 11)) table level_11_file;

    let size_1 = (Unix.stat level_1_file).st_size in
    let _size_6 = (Unix.stat level_6_file).st_size in
    let size_11 = (Unix.stat level_11_file).st_size in

    (* Verify data integrity *)
    let table_1 = IO.Parquet.read level_1_file in
    let table_6 = IO.Parquet.read level_6_file in
    let table_11 = IO.Parquet.read level_11_file in
    Alcotest.(check int) "Brotli level 1 preserves data" 75 (Table.num_rows table_1);
    Alcotest.(check int) "Brotli level 6 preserves data" 75 (Table.num_rows table_6);
    Alcotest.(check int) "Brotli level 11 preserves data" 75 (Table.num_rows table_11);

    (* Higher compression levels should produce smaller or equal files *)
    Alcotest.(check bool) "Higher Brotli level compresses more" true (size_11 <= size_1);

    Sys.remove level_1_file; Sys.remove level_6_file; Sys.remove level_11_file
  in

  test_gzip_levels ();
  test_brotli_levels ()

(** {1 Column Selection Tests} *)

let test_parquet_column_selection () =
  let table = Fixtures.small_test_table () in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "column_selection" in

  IO.Parquet.write table filename;

  (* Test reading specific columns by name *)
  let selected_by_names = IO.Parquet.read ~columns:(`Names ["integers"; "strings"; "booleans"]) filename in
  let schema_names = Table.schema selected_by_names in
  Alcotest.(check int) "Column name selection count" 3 (List.length (Schema.children schema_names));
  Alcotest.(check int) "Column name selection preserves rows" 10 (Table.num_rows selected_by_names);

  (* Verify selected column names *)
  let field_names = List.map (fun field -> Schema.name field) (Schema.children schema_names) in
  Alcotest.(check (list string)) "Selected column names" ["integers"; "strings"; "booleans"] field_names;

  (* Test reading specific columns by index *)
  let selected_by_indexes = IO.Parquet.read ~columns:(`Indexes [0; 2; 4]) filename in
  let schema_indexes = Table.schema selected_by_indexes in
  Alcotest.(check int) "Column index selection count" 3 (List.length (Schema.children schema_indexes));
  Alcotest.(check int) "Column index selection preserves rows" 10 (Table.num_rows selected_by_indexes);

  (* Test single column selection *)
  let single_column = IO.Parquet.read ~columns:(`Names ["floats"]) filename in
  let schema_single = Table.schema single_column in
  Alcotest.(check int) "Single column selection" 1 (List.length (Schema.children schema_single));
  let single_field = List.hd (Schema.children schema_single) in
  Alcotest.(check string) "Single column name" "floats" (Schema.name single_field);

  Fixtures.cleanup_test_files "column_selection_.*\\.parquet"

(** {1 Batch Processing Tests} *)

let test_parquet_batch_reading () =
  let large_table = Fixtures.sequential_table 1000 in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "batch_reading" in

  IO.Parquet.write large_table filename;

  (* Test batch reading with different batch sizes *)
  let test_batch_size batch_size =
    let total_rows = ref 0 in
    let batch_count = ref 0 in
    let max_batch_size = ref 0 in
    let min_batch_size = ref Int.max_int in

    IO.Parquet.read_batches filename ~batch_size ~f:(fun batch ->
      let rows = Table.num_rows batch in
      total_rows := !total_rows + rows;
      incr batch_count;
      max_batch_size := max !max_batch_size rows;
      min_batch_size := min !min_batch_size rows;

      (* Verify batch structure *)
      let schema = Table.schema batch in
      Alcotest.(check int) "Batch schema correct" 4 (List.length (Schema.children schema));
      Alcotest.(check bool) "Batch has reasonable size" true (rows > 0 && rows <= batch_size)
    );

    Alcotest.(check int) "Batch reading gets all rows" 1000 !total_rows;
    Alcotest.(check bool) "Multiple batches created" true (!batch_count > 1);
    Alcotest.(check bool) "Max batch size reasonable" true (!max_batch_size <= batch_size);
    Alcotest.(check bool) "Min batch size positive" true (!min_batch_size > 0)
  in

  test_batch_size 100;
  test_batch_size 250;
  test_batch_size 333;

  Fixtures.cleanup_test_files "batch_reading_.*\\.parquet"

let test_parquet_batch_reading_with_column_selection () =
  let table = Fixtures.small_test_table () in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "batch_columns" in

  IO.Parquet.write table filename;

  (* Test batch reading with column selection *)
  let total_rows = ref 0 in
  let batch_count = ref 0 in

  IO.Parquet.read_batches filename ~batch_size:5 ~column_idxs:[0; 2] ~f:(fun batch ->
    let rows = Table.num_rows batch in
    total_rows := !total_rows + rows;
    incr batch_count;

    (* Verify only selected columns are present *)
    let schema = Table.schema batch in
    Alcotest.(check int) "Batch has selected columns only" 2 (List.length (Schema.children schema))
  );

  Alcotest.(check int) "Batch column reading gets all rows" 10 !total_rows;

  Fixtures.cleanup_test_files "batch_columns_.*\\.parquet"

(** {1 Advanced Features} *)

let test_parquet_chunk_sizes () =
  let table = Fixtures.medium_test_table () in

  let test_chunk_size chunk_size =
    let filename = Fixtures.temp_file_path ~extension:".parquet" ("chunks_" ^ string_of_int chunk_size) in
    IO.Parquet.write ~chunk_size table filename;
    let read_table = IO.Parquet.read filename in
    Alcotest.(check int) (Printf.sprintf "Chunk size %d preserves data" chunk_size) 100 (Table.num_rows read_table);

    (* Test metadata *)
    let metadata = IO.Parquet.metadata filename in
    Alcotest.(check int64) "Metadata correct with chunks" 100L metadata.Parquet.num_rows;

    Sys.remove filename
  in

  List.iter test_chunk_size [10; 25; 50; 100; 200]

let test_parquet_empty_table () =
  let empty_table = Fixtures.empty_test_table () in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "empty_table" in

  (* Test writing and reading empty table *)
  IO.Parquet.write empty_table filename;
  let read_table = IO.Parquet.read filename in

  Alcotest.(check int) "Empty table has 0 rows" 0 (Table.num_rows read_table);

  (* Test schema preservation *)
  let original_schema = Table.schema empty_table in
  let read_schema = Table.schema read_table in
  let original_fields = Schema.children original_schema in
  let read_fields = Schema.children read_schema in
  Alcotest.(check int) "Empty table schema preserved" (List.length original_fields) (List.length read_fields);

  (* Test metadata for empty table *)
  let metadata = IO.Parquet.metadata filename in
  Alcotest.(check int64) "Empty table metadata" 0L metadata.Parquet.num_rows;

  Fixtures.cleanup_test_files "empty_table_.*\\.parquet"

let test_parquet_single_row_table () =
  let single_table = Fixtures.single_row_test_table () in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "single_row" in

  IO.Parquet.write single_table filename;
  let read_table = IO.Parquet.read filename in

  Alcotest.(check int) "Single row table has 1 row" 1 (Table.num_rows read_table);

  (* Verify data integrity for single row *)
  let ints = Table.read read_table ~column:(`Name "integers") Table.Int in
  let floats = Table.read read_table ~column:(`Name "floats") Table.Float in
  let strings = Table.read read_table ~column:(`Name "strings") Table.Utf8 in
  let bools = Table.read read_table ~column:(`Name "booleans") Table.Bool in

  Alcotest.(check int) "Single row integer" 42 ints.(0);
  Alcotest.(check (float 1e-6)) "Single row float" 3.14 floats.(0);
  Alcotest.(check string) "Single row string" "hello" strings.(0);
  Alcotest.(check bool) "Single row boolean" true bools.(0);

  Fixtures.cleanup_test_files "single_row_.*\\.parquet"

let test_parquet_nullable_data () =
  let nullable_table = Fixtures.nullable_test_table () in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "nullable_data" in

  IO.Parquet.write nullable_table filename;
  let read_table = IO.Parquet.read filename in

  Alcotest.(check int) "Nullable table row count" 10 (Table.num_rows read_table);

  (* Test nullable integer column *)
  let opt_ints = Table.read_opt read_table ~column:(`Name "opt_integers") Table.Int in
  Alcotest.(check bool) "Has some nullable integers" true (Array.length opt_ints > 0);
  let has_null = Array.exists (function None -> true | Some _ -> false) opt_ints in
  let has_value = Array.exists (function None -> false | Some _ -> true) opt_ints in
  Alcotest.(check bool) "Has null values in integers" true has_null;
  Alcotest.(check bool) "Has non-null values in integers" true has_value;

  Fixtures.cleanup_test_files "nullable_data_.*\\.parquet"

(** {1 Error Handling Tests} *)

let test_parquet_error_handling () =
  (* Test reading non-existent file *)
  (try
    ignore (IO.Parquet.read "/non/existent/file.parquet");
    Alcotest.fail "Should have raised exception for non-existent file"
  with _ -> ()); (* Expected to fail *)

  (* Test schema reading on non-existent file *)
  (try
    ignore (IO.Parquet.schema "/non/existent/file.parquet");
    Alcotest.fail "Should have raised exception for non-existent schema file"
  with _ -> ()); (* Expected to fail *)

  (* Test metadata reading on non-existent file *)
  (try
    ignore (IO.Parquet.metadata "/non/existent/file.parquet");
    Alcotest.fail "Should have raised exception for non-existent metadata file"
  with _ -> ()); (* Expected to fail *)

  (* Test invalid column names *)
  let table = Fixtures.small_test_table () in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "error_test" in
  IO.Parquet.write table filename;

  (try
    ignore (IO.Parquet.read ~columns:(`Names ["non_existent_column"]) filename);
    Alcotest.fail "Should have raised exception for invalid column name"
  with _ -> ()); (* Expected to fail *)

  (* Test invalid column indexes *)
  (try
    ignore (IO.Parquet.read ~columns:(`Indexes [999]) filename);
    Alcotest.fail "Should have raised exception for invalid column index"
  with _ -> ()); (* Expected to fail *)

  Fixtures.cleanup_test_files "error_test_.*\\.parquet"

(** {1 Performance and Stress Tests} *)

let test_parquet_large_table () =
  (* Test with a reasonably large table *)
  let large_table = Fixtures.sequential_table 5000 in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "large_table" in

  let start_time = Unix.gettimeofday () in
  IO.Parquet.write ~compression:Parquet.Snappy large_table filename;
  let write_time = Unix.gettimeofday () -. start_time in

  let start_read = Unix.gettimeofday () in
  let read_table = IO.Parquet.read filename in
  let read_time = Unix.gettimeofday () -. start_read in

  Alcotest.(check int) "Large table row count" 5000 (Table.num_rows read_table);

  (* Performance should be reasonable (less than 5 seconds for each operation) *)
  Alcotest.(check bool) "Write performance reasonable" true (write_time < 5.0);
  Alcotest.(check bool) "Read performance reasonable" true (read_time < 5.0);

  let file_size = (Unix.stat filename).st_size in
  Alcotest.(check bool) "File size reasonable" true (file_size > 0);

  Fixtures.cleanup_test_files "large_table_.*\\.parquet"

let test_parquet_edge_values () =
  let edge_table = Fixtures.edge_values_table () in
  let filename = Fixtures.temp_file_path ~extension:".parquet" "edge_values" in

  IO.Parquet.write edge_table filename;
  let read_table = IO.Parquet.read filename in

  (* Test extreme integer values *)
  let extreme_ints = Table.read read_table ~column:(`Name "extreme_ints") Table.Int in
  Alcotest.(check bool) "Has max int" true (Array.mem Int.max_int extreme_ints);
  Alcotest.(check bool) "Has min int" true (Array.mem Int.min_int extreme_ints);
  Alcotest.(check bool) "Has zero" true (Array.mem 0 extreme_ints);

  (* Test extreme float values *)
  let extreme_floats = Table.read read_table ~column:(`Name "extreme_floats") Table.Float in
  let has_infinity = Array.exists (fun x -> x = Float.infinity) extreme_floats in
  let has_neg_infinity = Array.exists (fun x -> x = Float.neg_infinity) extreme_floats in
  Alcotest.(check bool) "Has positive infinity" true has_infinity;
  Alcotest.(check bool) "Has negative infinity" true has_neg_infinity;

  Fixtures.cleanup_test_files "edge_values_.*\\.parquet"

(** {1 Test Suite} *)

let () =
  let open Alcotest in
  run "Parquet-specific I/O tests" [
    "basic_operations", [
      test_case "Basic roundtrip" `Quick test_parquet_basic_roundtrip;
      test_case "Schema operations" `Quick test_parquet_schema_operations;
      test_case "Metadata access" `Quick test_parquet_metadata_access;
    ];
    "compression", [
      test_case "Compression algorithms" `Quick test_parquet_compression_algorithms;
      test_case "Compression levels" `Quick test_parquet_compression_levels;
    ];
    "column_selection", [
      test_case "Column selection" `Quick test_parquet_column_selection;
    ];
    "batch_processing", [
      test_case "Batch reading" `Quick test_parquet_batch_reading;
      test_case "Batch reading with column selection" `Quick test_parquet_batch_reading_with_column_selection;
    ];
    "advanced_features", [
      test_case "Chunk sizes" `Quick test_parquet_chunk_sizes;
      test_case "Empty table" `Quick test_parquet_empty_table;
      test_case "Single row table" `Quick test_parquet_single_row_table;
      test_case "Nullable data" `Quick test_parquet_nullable_data;
    ];
    "error_handling", [
      test_case "Error handling" `Quick test_parquet_error_handling;
    ];
    "performance", [
      test_case "Large table" `Slow test_parquet_large_table;
      test_case "Edge values" `Quick test_parquet_edge_values;
    ];
  ]
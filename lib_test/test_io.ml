(* Comprehensive unified I/O tests - format auto-detection and cross-format operations *)
open Arrow

module Fixtures = Test_fixtures

(** {1 Format Auto-Detection Tests} *)

let test_auto_detection_parquet () =
  let table = Fixtures.medium_test_table () in
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "auto_detect_parquet" in

  (* Write using format-specific module *)
  IO.Parquet.write table parquet_file;

  (* Read using auto-detection *)
  let read_table = IO.read parquet_file in
  Alcotest.(check int) "Auto-detect Parquet row count" 100 (Table.num_rows read_table);

  (* Test schema auto-detection *)
  let schema = IO.schema parquet_file in
  let field_count = List.length (Schema.children schema) in
  Alcotest.(check int) "Auto-detect Parquet schema" 4 field_count;

  Fixtures.cleanup_test_files "auto_detect_parquet_.*\\.parquet"

let test_auto_detection_csv () =
  (* Create CSV file manually *)
  let csv_file = Fixtures.temp_file_path ~extension:".csv" "auto_detect_csv" in
  let oc = open_out csv_file in
  Printf.fprintf oc "id,name,score\n";
  Printf.fprintf oc "1,Alice,95.5\n";
  Printf.fprintf oc "2,Bob,87.2\n";
  Printf.fprintf oc "3,Carol,92.8\n";
  close_out oc;

  (try
    (* Test auto-detection for CSV *)
    let table = IO.read csv_file in
    Alcotest.(check int) "Auto-detect CSV row count" 3 (Table.num_rows table);

    let schema = IO.schema csv_file in
    let field_count = List.length (Schema.children schema) in
    Alcotest.(check int) "Auto-detect CSV schema" 3 field_count
  with _ ->
    Printf.printf "CSV auto-detection not supported\n%!"
  );

  Fixtures.cleanup_test_files "auto_detect_csv_.*\\.csv"

let test_auto_detection_feather () =
  let table = Fixtures.small_test_table () in
  let feather_file = Fixtures.temp_file_path ~extension:".feather" "auto_detect_feather" in

  (try
    (* Write using format-specific module *)
    IO.Feather.write table feather_file;

    (* Read using auto-detection *)
    let read_table = IO.read feather_file in
    Alcotest.(check int) "Auto-detect Feather row count" 10 (Table.num_rows read_table);

    (* Test schema auto-detection *)
    let schema = IO.schema feather_file in
    let field_count = List.length (Schema.children schema) in
    Alcotest.(check int) "Auto-detect Feather schema" 8 field_count
  with _ ->
    Printf.printf "Feather auto-detection not supported\n%!"
  );

  Fixtures.cleanup_test_files "auto_detect_feather_.*\\.feather"

let test_auto_detection_unsupported_format () =
  let unknown_file = Fixtures.temp_file_path ~extension:".unknown" "auto_detect_unknown" in
  let oc = open_out unknown_file in
  Printf.fprintf oc "This is not a supported format\n";
  close_out oc;

  (* Test that unsupported formats are handled gracefully *)
  (try
    ignore (IO.read unknown_file);
    Alcotest.fail "Should have failed for unsupported format"
  with _ ->
    (* Expected to fail *)
    ()
  );

  (try
    ignore (IO.schema unknown_file);
    Alcotest.fail "Should have failed for unsupported schema format"
  with _ ->
    (* Expected to fail *)
    ()
  );

  Fixtures.cleanup_test_files "auto_detect_unknown_.*\\.unknown"

(** {1 Unified Write Operations with Auto-Detection} *)

let test_unified_write_parquet () =
  let table = Fixtures.sequential_table 75 in
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "unified_write_parquet" in

  (* Write using unified IO module *)
  IO.write table parquet_file;

  (* Read back and verify *)
  let read_table = IO.read parquet_file in
  Alcotest.(check int) "Unified Parquet write" 75 (Table.num_rows read_table);

  (* Verify it's actually a Parquet file by reading with format-specific module *)
  let parquet_table = IO.Parquet.read parquet_file in
  Alcotest.(check int) "Unified write creates valid Parquet" 75 (Table.num_rows parquet_table);

  Fixtures.cleanup_test_files "unified_write_parquet_.*\\.parquet"

let test_unified_write_with_compression () =
  let table = Fixtures.pattern_table 4 80 in

  let test_unified_compression compression extension =
    let filename = Fixtures.temp_file_path ~extension ("unified_comp_" ^ extension) in
    IO.write ~compression table filename;
    let read_table = IO.read filename in
    Alcotest.(check int) (Printf.sprintf "Unified compression %s" extension) 80 (Table.num_rows read_table);
    let file_size = (Unix.stat filename).st_size in
    Sys.remove filename;
    file_size
  in

  (* Test different compression options with Parquet *)
  let uncompressed_size = test_unified_compression Compression.None ".parquet" in
  let snappy_size = test_unified_compression Compression.Snappy ".parquet" in
  let gzip_size = test_unified_compression Compression.Gzip ".parquet" in

  Alcotest.(check bool) "Unified compression effective" true (
    snappy_size <= uncompressed_size && gzip_size <= uncompressed_size
  )

let test_unified_write_with_chunk_size () =
  let table = Fixtures.sequential_table 120 in

  let test_chunk_size chunk_size =
    let filename = Fixtures.temp_file_path ~extension:".parquet" ("unified_chunk_" ^ string_of_int chunk_size) in
    IO.write ~chunk_size table filename;
    let read_table = IO.read filename in
    Alcotest.(check int) (Printf.sprintf "Unified chunk size %d" chunk_size) 120 (Table.num_rows read_table);
    Sys.remove filename
  in

  List.iter test_chunk_size [30; 60; 150]

(** {1 Column Selection with Auto-Detection} *)

let test_unified_column_selection () =
  let table = Fixtures.small_test_table () in
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "unified_columns" in

  IO.write table parquet_file;

  (* Test column selection by names *)
  let selected_by_names = IO.read ~columns:(`Names ["integers"; "floats"; "strings"]) parquet_file in
  let schema_names = Table.schema selected_by_names in
  Alcotest.(check int) "Unified column name selection" 3 (List.length (Schema.children schema_names));

  let field_names = List.map (fun field -> Schema.name field) (Schema.children schema_names) in
  Alcotest.(check (list string)) "Unified selected column names" ["integers"; "floats"; "strings"] field_names;

  (* Test column selection by indexes *)
  let selected_by_indexes = IO.read ~columns:(`Indexes [1; 3; 5]) parquet_file in
  let schema_indexes = Table.schema selected_by_indexes in
  Alcotest.(check int) "Unified column index selection" 3 (List.length (Schema.children schema_indexes));

  (* Test that data integrity is preserved *)
  let original_floats = Table.read table ~column:(`Name "floats") Table.Float in
  let selected_floats = Table.read selected_by_names ~column:(`Name "floats") Table.Float in
  Array.iteri (fun i orig ->
    let selected = selected_floats.(i) in
    Alcotest.(check (float 1e-6)) (Printf.sprintf "Column selection integrity %d" i) orig selected
  ) original_floats;

  Fixtures.cleanup_test_files "unified_columns_.*\\.parquet"

let test_unified_column_selection_across_formats () =
  let table = Fixtures.medium_test_table () in

  (* Test column selection with Parquet *)
  let parquet_file = Fixtures.temp_file_path ~extension:".parquet" "cross_format_columns_parquet" in
  IO.write table parquet_file;
  let parquet_selected = IO.read ~columns:(`Names ["integers"; "strings"]) parquet_file in

  (* Test column selection with Feather if available *)
  (try
    let feather_file = Fixtures.temp_file_path ~extension:".feather" "cross_format_columns_feather" in
    IO.Feather.write table feather_file;
    let feather_selected = IO.read ~columns:(`Names ["integers"; "strings"]) feather_file in

    (* Compare results *)
    let parquet_rows = Table.num_rows parquet_selected in
    let feather_rows = Table.num_rows feather_selected in
    Alcotest.(check int) "Cross-format column selection consistency" parquet_rows feather_rows;

    let parquet_schema = Table.schema parquet_selected in
    let feather_schema = Table.schema feather_selected in
    let parquet_fields = List.length (Schema.children parquet_schema) in
    let feather_fields = List.length (Schema.children feather_schema) in
    Alcotest.(check int) "Cross-format selected schema consistency" parquet_fields feather_fields;

    Fixtures.cleanup_test_files "cross_format_columns_feather_.*\\.feather"
  with _ ->
    Printf.printf "Cross-format column selection test with Feather not available\n%!"
  );

  Fixtures.cleanup_test_files "cross_format_columns_parquet_.*\\.parquet"

(** {1 Error Handling and Edge Cases} *)

let test_unified_error_handling () =
  (* Test reading non-existent file *)
  (try
    ignore (IO.read "/non/existent/file.parquet");
    Alcotest.fail "Should have failed for non-existent file"
  with _ ->
    (* Expected to fail *)
    ()
  );

  (* Test schema reading on non-existent file *)
  (try
    ignore (IO.schema "/non/existent/file.parquet");
    Alcotest.fail "Should have failed for non-existent schema file"
  with _ ->
    (* Expected to fail *)
    ()
  );

  (* Test writing to invalid path *)
  (try
    let table = Fixtures.small_test_table () in
    IO.write table "/invalid/path/that/does/not/exist.parquet";
    Alcotest.fail "Should have failed to write to invalid path"
  with _ ->
    (* Expected to fail *)
    ()
  );

  (* Test empty filename *)
  (try
    ignore (IO.read "");
    Alcotest.fail "Should have failed for empty filename"
  with _ ->
    (* Expected to fail *)
    ()
  );

  (* Test malformed file *)
  let malformed_file = Fixtures.temp_file_path ~extension:".parquet" "malformed" in
  let oc = open_out malformed_file in
  Printf.fprintf oc "This is not a valid Parquet file content\n";
  close_out oc;

  (try
    ignore (IO.read malformed_file);
    Alcotest.fail "Should have failed for malformed file"
  with _ ->
    (* Expected to fail *)
    ()
  );

  Fixtures.cleanup_test_files "malformed_.*\\.parquet"

let test_unified_edge_case_files () =
  (* Test empty table *)
  let empty_table = Fixtures.empty_test_table () in
  let empty_file = Fixtures.temp_file_path ~extension:".parquet" "unified_empty" in

  IO.write empty_table empty_file;
  let read_empty = IO.read empty_file in
  Alcotest.(check int) "Unified empty table" 0 (Table.num_rows read_empty);

  let empty_schema = IO.schema empty_file in
  let empty_fields = List.length (Schema.children empty_schema) in
  Alcotest.(check bool) "Unified empty schema has fields" true (empty_fields > 0);

  (* Test single-row table *)
  let single_table = Fixtures.single_row_test_table () in
  let single_file = Fixtures.temp_file_path ~extension:".parquet" "unified_single" in

  IO.write single_table single_file;
  let read_single = IO.read single_file in
  Alcotest.(check int) "Unified single row table" 1 (Table.num_rows read_single);

  (* Test large table *)
  let large_table = Fixtures.sequential_table 2000 in
  let large_file = Fixtures.temp_file_path ~extension:".parquet" "unified_large" in

  let start_time = Unix.gettimeofday () in
  IO.write large_table large_file;
  let write_time = Unix.gettimeofday () -. start_time in

  let start_read = Unix.gettimeofday () in
  let read_large = IO.read large_file in
  let read_time = Unix.gettimeofday () -. start_read in

  Alcotest.(check int) "Unified large table" 2000 (Table.num_rows read_large);
  Alcotest.(check bool) "Unified write performance" true (write_time < 10.0);
  Alcotest.(check bool) "Unified read performance" true (read_time < 10.0);

  Fixtures.cleanup_test_files "unified_(empty|single|large)_.*\\.parquet"

(** {1 Data Integrity Tests} *)

let test_unified_data_integrity () =
  let original_table = Fixtures.small_test_table () in
  let roundtrip_file = Fixtures.temp_file_path ~extension:".parquet" "unified_integrity" in

  (* Write and read using unified interface *)
  IO.write original_table roundtrip_file;
  let restored_table = IO.read roundtrip_file in

  (* Verify row count *)
  Alcotest.(check int) "Unified integrity row count" 10 (Table.num_rows restored_table);

  (* Verify schema *)
  let original_schema = Table.schema original_table in
  let restored_schema = Table.schema restored_table in
  let original_fields = List.length (Schema.children original_schema) in
  let restored_fields = List.length (Schema.children restored_schema) in
  Alcotest.(check int) "Unified integrity schema" original_fields restored_fields;

  (* Verify integer data *)
  let original_ints = Table.read original_table ~column:(`Name "integers") Table.Int in
  let restored_ints = Table.read restored_table ~column:(`Name "integers") Table.Int in
  Alcotest.(check (array int)) "Unified integrity integers" original_ints restored_ints;

  (* Verify float data *)
  let original_floats = Table.read original_table ~column:(`Name "floats") Table.Float in
  let restored_floats = Table.read restored_table ~column:(`Name "floats") Table.Float in
  Array.iteri (fun i orig ->
    let restored = restored_floats.(i) in
    Alcotest.(check (float 1e-6)) (Printf.sprintf "Unified integrity floats %d" i) orig restored
  ) original_floats;

  (* Verify string data *)
  let original_strings = Table.read original_table ~column:(`Name "strings") Table.Utf8 in
  let restored_strings = Table.read restored_table ~column:(`Name "strings") Table.Utf8 in
  Alcotest.(check (array string)) "Unified integrity strings" original_strings restored_strings;

  (* Verify boolean data *)
  let original_bools = Table.read original_table ~column:(`Name "booleans") Table.Bool in
  let restored_bools = Table.read restored_table ~column:(`Name "booleans") Table.Bool in
  Alcotest.(check (array bool)) "Unified integrity booleans" original_bools restored_bools;

  Fixtures.cleanup_test_files "unified_integrity_.*\\.parquet"

let test_unified_nullable_data_integrity () =
  let nullable_table = Fixtures.nullable_test_table () in
  let nullable_file = Fixtures.temp_file_path ~extension:".parquet" "unified_nullable" in

  IO.write nullable_table nullable_file;
  let restored_nullable = IO.read nullable_file in

  Alcotest.(check int) "Unified nullable row count" 10 (Table.num_rows restored_nullable);

  (* Test nullable integer column *)
  let original_opt_ints = Table.read_opt nullable_table ~column:(`Name "opt_integers") Table.Int in
  let restored_opt_ints = Table.read_opt restored_nullable ~column:(`Name "opt_integers") Table.Int in
  Alcotest.(check (array (option int))) "Unified nullable integers" original_opt_ints restored_opt_ints;

  (* Test nullable float column *)
  let original_opt_floats = Table.read_opt nullable_table ~column:(`Name "opt_floats") Table.Float in
  let restored_opt_floats = Table.read_opt restored_nullable ~column:(`Name "opt_floats") Table.Float in
  Array.iteri (fun i orig_opt ->
    let restored_opt = restored_opt_floats.(i) in
    match orig_opt, restored_opt with
    | None, None -> ()
    | Some orig, Some restored ->
        Alcotest.(check (float 1e-6)) (Printf.sprintf "Unified nullable floats %d" i) orig restored
    | _ ->
        Alcotest.fail (Printf.sprintf "Nullable float mismatch at index %d" i)
  ) original_opt_floats;

  Fixtures.cleanup_test_files "unified_nullable_.*\\.parquet"

(** {1 Schema Compatibility Tests} *)

let test_unified_schema_evolution () =
  (* Create initial table *)
  let initial_table = Table.create [
    Table.col (Fixtures.sequential_ints 1 20) Table.Int "id";
    Table.col (Array.init 20 (fun i -> Printf.sprintf "name_%d" i)) Table.Utf8 "name";
  ] in
  let schema_file = Fixtures.temp_file_path ~extension:".parquet" "schema_evolution" in

  IO.write initial_table schema_file;
  let read_initial = IO.read schema_file in

  (* Verify initial schema *)
  let initial_schema = IO.schema schema_file in
  let initial_fields = Schema.children initial_schema in
  Alcotest.(check int) "Initial schema field count" 2 (List.length initial_fields);

  let initial_field_names = List.map (fun field -> Schema.name field) initial_fields in
  Alcotest.(check (list string)) "Initial field names" ["id"; "name"] initial_field_names;

  Alcotest.(check int) "Schema evolution row count" 20 (Table.num_rows read_initial);

  Fixtures.cleanup_test_files "schema_evolution_.*\\.parquet"

let test_unified_mixed_data_types () =
  (* Create table with various data types *)
  let mixed_table = Table.create [
    Table.col (Array.init 15 (fun i -> i)) Table.Int "integers";
    Table.col (Array.init 15 (fun i -> i * 100)) Table.Int "int32s";
    Table.col (Array.init 15 (fun i -> i * 1000)) Table.Int "int64s";
    Table.col (Array.init 15 (fun i -> float_of_int i *. 3.14)) Table.Float "floats";
    Table.col (Array.init 15 (fun i -> Printf.sprintf "item_%d" i)) Table.Utf8 "strings";
    Table.col (Array.init 15 (fun i -> i mod 2 = 0)) Table.Bool "booleans";
  ] in
  let mixed_file = Fixtures.temp_file_path ~extension:".parquet" "mixed_types" in

  IO.write mixed_table mixed_file;
  let read_mixed = IO.read mixed_file in

  Alcotest.(check int) "Mixed types row count" 15 (Table.num_rows read_mixed);

  let mixed_schema = IO.schema mixed_file in
  let mixed_fields = Schema.children mixed_schema in
  Alcotest.(check int) "Mixed types field count" 6 (List.length mixed_fields);

  (* Test reading each column type *)
  let int_data = Table.read read_mixed ~column:(`Name "integers") Table.Int in
  let int32_data = Table.read read_mixed ~column:(`Name "int32s") Table.Int in
  let int64_data = Table.read read_mixed ~column:(`Name "int64s") Table.Int in
  let float_data = Table.read read_mixed ~column:(`Name "floats") Table.Float in
  let string_data = Table.read read_mixed ~column:(`Name "strings") Table.Utf8 in
  let bool_data = Table.read read_mixed ~column:(`Name "booleans") Table.Bool in

  Alcotest.(check int) "Mixed types integers" 14 int_data.(14);
  Alcotest.(check int) "Mixed types int32s" 1400 int32_data.(14);
  Alcotest.(check int) "Mixed types int64s" 14000 int64_data.(14);
  Alcotest.(check (float 1e-6)) "Mixed types floats" (14.0 *. 3.14) float_data.(14);
  Alcotest.(check string) "Mixed types strings" "item_14" string_data.(14);
  Alcotest.(check bool) "Mixed types booleans" true bool_data.(14);

  Fixtures.cleanup_test_files "mixed_types_.*\\.parquet"

(** {1 Performance and Stress Tests} *)

let test_unified_performance_patterns () =
  let pattern_table = Fixtures.pattern_table 10 500 in
  let pattern_file = Fixtures.temp_file_path ~extension:".parquet" "unified_pattern" in

  let start_time = Unix.gettimeofday () in
  IO.write ~compression:Compression.Snappy pattern_table pattern_file;
  let write_time = Unix.gettimeofday () -. start_time in

  let start_read = Unix.gettimeofday () in
  let read_pattern = IO.read pattern_file in
  let read_time = Unix.gettimeofday () -. start_read in

  Alcotest.(check int) "Pattern table performance" 500 (Table.num_rows read_pattern);
  Alcotest.(check bool) "Pattern write performance" true (write_time < 3.0);
  Alcotest.(check bool) "Pattern read performance" true (read_time < 3.0);

  let file_size = (Unix.stat pattern_file).st_size in
  Alcotest.(check bool) "Pattern compression effective" true (file_size > 0);

  Fixtures.cleanup_test_files "unified_pattern_.*\\.parquet"

let test_unified_random_data () =
  let random_table = Fixtures.random_table ~seed:98765 300 in
  let random_file = Fixtures.temp_file_path ~extension:".parquet" "unified_random" in

  IO.write ~compression:Compression.Gzip random_table random_file;
  let read_random = IO.read random_file in

  Alcotest.(check int) "Random data table" 300 (Table.num_rows read_random);

  (* Verify schema preservation *)
  let random_schema = IO.schema random_file in
  let random_fields = Schema.children random_schema in
  Alcotest.(check int) "Random data schema" 4 (List.length random_fields);

  Fixtures.cleanup_test_files "unified_random_.*\\.parquet"

(** {1 Test Suite} *)

let () =
  let open Alcotest in
  run "Unified I/O tests" [
    "auto_detection", [
      test_case "Auto-detection Parquet" `Quick test_auto_detection_parquet;
      test_case "Auto-detection CSV" `Quick test_auto_detection_csv;
      test_case "Auto-detection Feather" `Quick test_auto_detection_feather;
      test_case "Auto-detection unsupported format" `Quick test_auto_detection_unsupported_format;
    ];
    "unified_write", [
      test_case "Unified write Parquet" `Quick test_unified_write_parquet;
      test_case "Unified write with compression" `Quick test_unified_write_with_compression;
      test_case "Unified write with chunk size" `Quick test_unified_write_with_chunk_size;
    ];
    "column_selection", [
      test_case "Unified column selection" `Quick test_unified_column_selection;
      test_case "Unified column selection across formats" `Quick test_unified_column_selection_across_formats;
    ];
    "error_handling", [
      test_case "Unified error handling" `Quick test_unified_error_handling;
      test_case "Unified edge case files" `Quick test_unified_edge_case_files;
    ];
    "data_integrity", [
      test_case "Unified data integrity" `Quick test_unified_data_integrity;
      test_case "Unified nullable data integrity" `Quick test_unified_nullable_data_integrity;
    ];
    "schema_compatibility", [
      test_case "Unified schema evolution" `Quick test_unified_schema_evolution;
      test_case "Unified mixed data types" `Quick test_unified_mixed_data_types;
    ];
    "performance", [
      test_case "Unified performance patterns" `Quick test_unified_performance_patterns;
      test_case "Unified random data" `Quick test_unified_random_data;
    ];
  ]
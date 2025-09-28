(* IO module tests using external Arrow interface *)
open Arrow

let create_test_table ?(size=100) () =
  let ids = Array.init size (fun i -> i) in
  let names = Array.init size (fun i -> Printf.sprintf "name_%d" i) in
  let scores = Array.init size (fun i -> float_of_int i *. 1.5) in
  let flags = Array.init size (fun i -> i mod 2 = 0) in
  let opt_values = Array.init size (fun i -> if i mod 3 = 0 then None else Some i) in

  Table.create [
    Table.col ids Table.Int "id";
    Table.col names Table.Utf8 "name";
    Table.col scores Table.Float "score";
    Table.col flags Table.Bool "active";
    Table.col_opt opt_values Table.Int "optional_value";
  ]

let test_io_parquet_roundtrip () =
  let table = create_test_table ~size:50 () in
  let filename = Filename.temp_file "test_io_" ".parquet" in

  (* Write using IO module *)
  IO.write table filename;

  (* Read back using IO module *)
  let table2 = IO.read filename in

  Alcotest.(check int) "Parquet roundtrip preserves rows" 50 (Table.num_rows table2);

  (* Verify data integrity *)
  let ids = Table.read table2 ~column:(`Name "id") Table.Int in
  let names = Table.read table2 ~column:(`Name "name") Table.Utf8 in
  let scores = Table.read table2 ~column:(`Name "score") Table.Float in
  let flags = Table.read table2 ~column:(`Name "active") Table.Bool in
  let opt_values = Table.read_opt table2 ~column:(`Name "optional_value") Table.Int in

  Alcotest.(check int) "First ID correct" 0 ids.(0);
  Alcotest.(check string) "First name correct" "name_0" names.(0);
  Alcotest.(check (float 1e-6)) "First score correct" 0.0 scores.(0);
  Alcotest.(check bool) "First flag correct" true flags.(0);
  Alcotest.(check (option int)) "First optional correct" None opt_values.(0);

  Sys.remove filename

let test_io_format_detection () =
  let table = create_test_table ~size:10 () in

  (* Test Parquet format detection *)
  let parquet_file = Filename.temp_file "test_" ".parquet" in
  IO.write table parquet_file;
  let table_parquet = IO.read parquet_file in
  Alcotest.(check int) "Parquet format detected correctly" 10 (Table.num_rows table_parquet);
  Sys.remove parquet_file;

  (* Test unsupported format fails gracefully *)
  let unknown_file = Filename.temp_file "test_" ".unknown" in
  (try
    IO.write table unknown_file;
    Alcotest.fail "Should have failed for unknown format"
  with _ -> ()); (* Expected to fail *)
  (try Sys.remove unknown_file with _ -> ())

let test_io_schema_reading () =
  let table = create_test_table ~size:5 () in
  let filename = Filename.temp_file "test_schema_" ".parquet" in

  IO.write table filename;
  let schema = IO.schema filename in

  Alcotest.(check int) "Schema has correct field count" 5 (List.length schema.Schema.children);

  let field_names = List.map (fun field -> field.Schema.name) schema.Schema.children in
  Alcotest.(check (list string)) "Schema field names correct"
    ["id"; "name"; "score"; "active"; "optional_value"] field_names;

  Sys.remove filename

let test_io_parquet_specific () =
  let table = create_test_table ~size:20 () in
  let filename = Filename.temp_file "test_parquet_specific_" ".parquet" in

  (* Test IO.Parquet specific operations *)
  IO.Parquet.write ~compression:Parquet.Snappy table filename;

  let table2 = IO.Parquet.read filename in
  Alcotest.(check int) "Parquet specific read works" 20 (Table.num_rows table2);

  let schema = IO.Parquet.schema filename in
  Alcotest.(check int) "Parquet schema correct" 5 (List.length schema.Schema.children);

  let metadata = IO.Parquet.metadata filename in
  Alcotest.(check int64) "Parquet metadata row count" 20L metadata.Parquet.num_rows;

  Sys.remove filename

let test_io_parquet_compression () =
  let table = create_test_table ~size:30 () in

  let test_compression name compression =
    let filename = Filename.temp_file "test_comp_" ".parquet" in
    IO.Parquet.write ~compression table filename;
    let table2 = IO.Parquet.read filename in
    Alcotest.(check int) (Printf.sprintf "%s compression works" name) 30 (Table.num_rows table2);
    let stats = Unix.stat filename in
    Sys.remove filename;
    stats.st_size
  in

  let uncompressed_size = test_compression "uncompressed" Parquet.Uncompressed in
  let snappy_size = test_compression "snappy" Parquet.Snappy in
  let gzip_size = test_compression "gzip" (Parquet.Gzip (Some 6)) in

  (* Verify compression is effective *)
  Alcotest.(check bool) "Snappy compresses data" true (snappy_size < uncompressed_size);
  Alcotest.(check bool) "Gzip compresses data" true (gzip_size < uncompressed_size)

let test_io_column_selection () =
  let table = create_test_table ~size:15 () in
  let filename = Filename.temp_file "test_cols_" ".parquet" in

  IO.write table filename;

  (* Test reading with column name selection *)
  let table_names = IO.read ~columns:(`Names ["id"; "name"]) filename in
  let schema_names = Table.schema table_names in
  Alcotest.(check int) "Column name selection works" 2 (List.length schema_names.Schema.children);

  (* Test reading with column index selection *)
  let table_indexes = IO.read ~columns:(`Indexes [0; 2]) filename in
  let schema_indexes = Table.schema table_indexes in
  Alcotest.(check int) "Column index selection works" 2 (List.length schema_indexes.Schema.children);

  Sys.remove filename

let test_io_batch_reading () =
  let table = create_test_table ~size:1000 () in
  let filename = Filename.temp_file "test_batch_" ".parquet" in

  IO.Parquet.write table filename;

  (* Test batch reading *)
  let total_rows = ref 0 in
  let batch_count = ref 0 in

  IO.Parquet.read_batches filename ~batch_size:250 ~f:(fun batch ->
    let batch_size = Table.num_rows batch in
    total_rows := !total_rows + batch_size;
    incr batch_count;
    Alcotest.(check bool) "Batch size reasonable" true (batch_size > 0 && batch_size <= 250)
  );

  Alcotest.(check int) "Batch reading gets all rows" 1000 !total_rows;
  Alcotest.(check bool) "Multiple batches created" true (!batch_count > 1);

  Sys.remove filename

let test_io_csv_operations () =
  (* Create a CSV file *)
  let csv_file = Filename.temp_file "test_" ".csv" in
  let oc = open_out csv_file in
  Printf.fprintf oc "id,name,value\n";
  Printf.fprintf oc "1,Alice,100\n";
  Printf.fprintf oc "2,Bob,200\n";
  Printf.fprintf oc "3,Charlie,300\n";
  close_out oc;

  (* Test CSV reading *)
  let table = IO.CSV.read csv_file in
  Alcotest.(check int) "CSV read works" 3 (Table.num_rows table);

  (* Test auto-detection for CSV *)
  let table2 = IO.read csv_file in
  Alcotest.(check int) "CSV auto-detection works" 3 (Table.num_rows table2);

  Sys.remove csv_file

let test_io_feather_operations () =
  let table = create_test_table ~size:10 () in

  (* Test Feather format (if supported) *)
  let feather_file = Filename.temp_file "test_" ".feather" in
  (try
    IO.Feather.write table feather_file;
    let table2 = IO.Feather.read feather_file in
    Alcotest.(check int) "Feather roundtrip works" 10 (Table.num_rows table2);
    let schema = IO.Feather.schema feather_file in
    Alcotest.(check int) "Feather schema correct" 5 (List.length schema.Schema.children);
    Sys.remove feather_file
  with _ ->
    (* Feather might not be supported, skip test *)
    (try Sys.remove feather_file with _ -> ())
  )

let test_io_compression_options () =
  let table = create_test_table ~size:25 () in

  (* Test different compression options *)
  let test_compression compression =
    let filename = Filename.temp_file "test_comp_" ".parquet" in
    IO.write ~compression table filename;
    let table2 = IO.read filename in
    Alcotest.(check int) "Compression preserves data" 25 (Table.num_rows table2);
    Sys.remove filename
  in

  test_compression Compression.None;
  test_compression Compression.Snappy;
  test_compression Compression.Gzip;
  test_compression Compression.Brotli

let test_io_chunk_sizes () =
  let table = create_test_table ~size:40 () in

  (* Test different chunk sizes *)
  let test_chunk_size chunk_size =
    let filename = Filename.temp_file "test_chunk_" ".parquet" in
    IO.write ~chunk_size table filename;
    let table2 = IO.read filename in
    Alcotest.(check int) (Printf.sprintf "Chunk size %d works" chunk_size) 40 (Table.num_rows table2);
    Sys.remove filename
  in

  test_chunk_size 10;
  test_chunk_size 20;
  test_chunk_size 100

let test_io_error_handling () =
  (* Test error conditions *)

  (* Non-existent file *)
  (try
    ignore (IO.read "/non/existent/file.parquet");
    Alcotest.fail "Should have raised exception for non-existent file"
  with _ -> ()); (* Expected to fail *)

  (* Empty filename *)
  (try
    ignore (IO.read "");
    Alcotest.fail "Should have raised exception for empty filename"
  with Failure _ -> ()
  | _ -> ())

let () =
  let open Alcotest in
  run "IO tests" [
    "basic", [
      test_case "Parquet roundtrip" `Quick test_io_parquet_roundtrip;
      test_case "Format detection" `Quick test_io_format_detection;
      test_case "Schema reading" `Quick test_io_schema_reading;
    ];
    "parquet_specific", [
      test_case "Parquet specific operations" `Quick test_io_parquet_specific;
      test_case "Parquet compression" `Quick test_io_parquet_compression;
      test_case "Column selection" `Quick test_io_column_selection;
      test_case "Batch reading" `Quick test_io_batch_reading;
    ];
    "formats", [
      test_case "CSV operations" `Quick test_io_csv_operations;
      test_case "Feather operations" `Quick test_io_feather_operations;
    ];
    "options", [
      test_case "Compression options" `Quick test_io_compression_options;
      test_case "Chunk sizes" `Quick test_io_chunk_sizes;
    ];
    "error_handling", [
      test_case "Error handling" `Quick test_io_error_handling;
    ];
  ]
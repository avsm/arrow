(* Schema module tests using high-level API *)
open Arrow

let test_schema_creation () =
  (* Test creating basic schemas *)
  let schema1 = Schema.create ~format:Type.Int64 ~name:"test_int" () in
  Alcotest.(check string) "Schema name" "test_int" (Schema.name schema1);
  Alcotest.(check bool) "Schema format" true (Schema.format schema1 = Type.Int64);
  Alcotest.(check (list (pair string string))) "Schema metadata" [] (Schema.metadata schema1);
  Alcotest.(check bool) "Schema flags none" true (Schema.flags schema1 = Schema.Flags.none);
  Alcotest.(check (list int)) "Schema children" [] (List.map (fun _ -> 1) (Schema.children schema1))

let test_schema_with_metadata () =
  (* Test schema with metadata *)
  let metadata = [("key1", "value1"); ("key2", "value2")] in
  let schema = Schema.create ~format:Type.Utf8_string ~name:"test_string" ~metadata () in
  Alcotest.(check string) "Schema name" "test_string" (Schema.name schema);
  Alcotest.(check (list (pair string string))) "Schema metadata" metadata (Schema.metadata schema)

let test_schema_with_flags () =
  (* Test schema with flags *)
  let flags = Schema.Flags.all [Schema.Flags.nullable; Schema.Flags.dictionary_ordered] in
  let schema = Schema.create ~format:Type.Float64 ~name:"test_float" ~flags () in
  Alcotest.(check string) "Schema name" "test_float" (Schema.name schema);
  Alcotest.(check bool) "Schema nullable flag" true (Schema.Flags.is_nullable (Schema.flags schema));
  Alcotest.(check bool) "Schema dictionary ordered flag" true (Schema.Flags.is_dictionary_ordered (Schema.flags schema));
  Alcotest.(check bool) "Schema map keys sorted flag" false (Schema.Flags.is_map_keys_sorted (Schema.flags schema))

let test_schema_with_children () =
  (* Test schema with children *)
  let child1 = Schema.create ~format:Type.Int32 ~name:"id" () in
  let child2 = Schema.create ~format:Type.Utf8_string ~name:"name" () in
  let parent = Schema.create ~format:Type.Struct ~name:"record" ~children:[child1; child2] () in

  let children = Schema.children parent in
  Alcotest.(check int) "Parent has 2 children" 2 (List.length children);

  let child1_retrieved = List.nth children 0 in
  let child2_retrieved = List.nth children 1 in
  Alcotest.(check string) "First child name" "id" (Schema.name child1_retrieved);
  Alcotest.(check string) "Second child name" "name" (Schema.name child2_retrieved);
  Alcotest.(check bool) "First child type" true (Schema.format child1_retrieved = Type.Int32);
  Alcotest.(check bool) "Second child type" true (Schema.format child2_retrieved = Type.Utf8_string)

let test_schema_flag_operations () =
  (* Test flag operations *)
  let none_flags = Schema.Flags.none in
  Alcotest.(check bool) "None flags not nullable" false (Schema.Flags.is_nullable none_flags);
  Alcotest.(check bool) "None flags not dictionary ordered" false (Schema.Flags.is_dictionary_ordered none_flags);
  Alcotest.(check bool) "None flags not map keys sorted" false (Schema.Flags.is_map_keys_sorted none_flags);

  let nullable_flag = Schema.Flags.nullable in
  Alcotest.(check bool) "Nullable flag is nullable" true (Schema.Flags.is_nullable nullable_flag);
  Alcotest.(check bool) "Nullable flag not dictionary ordered" false (Schema.Flags.is_dictionary_ordered nullable_flag);

  let combined_flags = Schema.Flags.all [Schema.Flags.nullable; Schema.Flags.map_keys_sorted] in
  Alcotest.(check bool) "Combined flags nullable" true (Schema.Flags.is_nullable combined_flags);
  Alcotest.(check bool) "Combined flags not dictionary ordered" false (Schema.Flags.is_dictionary_ordered combined_flags);
  Alcotest.(check bool) "Combined flags map keys sorted" true (Schema.Flags.is_map_keys_sorted combined_flags)

let test_schema_to_string () =
  (* Test schema string representation *)
  let simple_schema = Schema.create ~format:Type.Int64 ~name:"simple_int" () in
  let schema_str = Schema.to_string simple_schema in
  Alcotest.(check bool) "Simple schema string contains name" true (String.contains schema_str 's');
  Alcotest.(check bool) "Simple schema string non-empty" true (String.length schema_str > 0);

  let complex_schema = Schema.create
    ~format:Type.Struct
    ~name:"complex"
    ~metadata:[("source", "test")]
    ~flags:Schema.Flags.nullable
    ~children:[
      Schema.create ~format:Type.Int32 ~name:"id" ();
      Schema.create ~format:Type.Utf8_string ~name:"name" ();
    ] () in
  let complex_str = Schema.to_string complex_schema in
  Alcotest.(check bool) "Complex schema string non-empty" true (String.length complex_str > 0);
  Alcotest.(check bool) "Complex schema string contains metadata" true (String.contains complex_str '=')

let test_decimal_type_schema () =
  (* Test decimal type in schema *)
  let decimal_schema = Schema.create
    ~format:(Type.Decimal128 {precision = 10; scale = 2})
    ~name:"price" () in
  let schema_str = Schema.to_string decimal_schema in
  Alcotest.(check bool) "Decimal schema string contains precision info" true (String.contains schema_str '(');
  Alcotest.(check string) "Decimal schema name" "price" (Schema.name decimal_schema)

let test_temporal_type_schemas () =
  (* Test temporal types in schemas *)
  let timestamp_schema = Schema.create
    ~format:(Type.Timestamp {precision = `Microseconds; timezone = "UTC"})
    ~name:"created_at" () in
  let timestamp_str = Schema.to_string timestamp_schema in
  Alcotest.(check bool) "Timestamp schema string contains timezone" true (String.contains timestamp_str 'U');

  let time32_schema = Schema.create
    ~format:(Type.Time32 `Milliseconds)
    ~name:"time_of_day" () in
  let time32_str = Schema.to_string time32_schema in
  Alcotest.(check bool) "Time32 schema string non-empty" true (String.length time32_str > 0);

  let duration_schema = Schema.create
    ~format:(Type.Duration `Nanoseconds)
    ~name:"elapsed" () in
  let duration_str = Schema.to_string duration_schema in
  Alcotest.(check bool) "Duration schema string non-empty" true (String.length duration_str > 0)

let test_schema_from_table () =
  (* Test getting schema from a table *)
  let table = Table.create [
    Table.col [|1; 2; 3|] Table.Int "id";
    Table.col [|"a"; "b"; "c"|] Table.Utf8 "name";
    Table.col [|1.0; 2.0; 3.0|] Table.Float "value";
  ] in

  let schema = Table.schema table in
  let children = Schema.children schema in
  Alcotest.(check int) "Table schema has 3 fields" 3 (List.length children);

  let field_names = List.map Schema.name children in
  Alcotest.(check (list string)) "Field names match" ["id"; "name"; "value"] field_names

let test_schema_with_optional_data () =
  (* Test schema for table with optional data *)
  let table = Table.create [
    Table.col [|1; 2; 3|] Table.Int "id";
    Table.col_opt [|Some "a"; None; Some "c"|] Table.Utf8 "optional_name";
    Table.col [|true; false; true|] Table.Bool "active";
  ] in

  let schema = Table.schema table in
  let children = Schema.children schema in
  Alcotest.(check int) "Optional table schema has 3 fields" 3 (List.length children);

  let field_names = List.map Schema.name children in
  Alcotest.(check (list string)) "Optional field names match" ["id"; "optional_name"; "active"] field_names

let () =
  let open Alcotest in
  run "Schema tests" [
    "creation", [
      test_case "Basic schema creation" `Quick test_schema_creation;
      test_case "Schema with metadata" `Quick test_schema_with_metadata;
      test_case "Schema with flags" `Quick test_schema_with_flags;
      test_case "Schema with children" `Quick test_schema_with_children;
    ];
    "flags", [
      test_case "Flag operations" `Quick test_schema_flag_operations;
    ];
    "string_representation", [
      test_case "Schema to string" `Quick test_schema_to_string;
    ];
    "specialized_types", [
      test_case "Decimal type schema" `Quick test_decimal_type_schema;
      test_case "Temporal type schemas" `Quick test_temporal_type_schemas;
    ];
    "integration", [
      test_case "Schema from table" `Quick test_schema_from_table;
      test_case "Schema with optional data" `Quick test_schema_with_optional_data;
    ];
  ]
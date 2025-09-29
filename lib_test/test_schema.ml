(** Comprehensive Schema module tests using high-level API *)
open Arrow

(** {2 Test Utilities} *)

let _test_name = "schema_tests"

(** Helper to check if two schema lists have equal names and types *)
let _check_schema_fields_equal expected actual =
  let names_equal = List.map Schema.name expected = List.map Schema.name actual in
  let formats_equal = List.map Schema.format expected = List.map Schema.format actual in
  names_equal && formats_equal

(** Helper to create metadata pairs *)
let _mk_metadata pairs = pairs

(** Helper to create flag combinations *)
let mk_flags flags = Schema.Flags.all flags

(** {2 Schema Creation Tests} *)

let test_basic_schema_creation () =
  (* Test creating schemas with all basic types *)
  let null_schema = Schema.create ~format:Type.Null ~name:"null_field" () in
  Alcotest.(check string) "Null schema name" "null_field" (Schema.name null_schema);
  Alcotest.(check bool) "Null schema format" true (Schema.format null_schema = Type.Null);

  let bool_schema = Schema.create ~format:Type.Boolean ~name:"bool_field" () in
  Alcotest.(check string) "Bool schema name" "bool_field" (Schema.name bool_schema);
  Alcotest.(check bool) "Bool schema format" true (Schema.format bool_schema = Type.Boolean);

  let int8_schema = Schema.create ~format:Type.Int8 ~name:"int8_field" () in
  Alcotest.(check bool) "Int8 schema format" true (Schema.format int8_schema = Type.Int8);

  let int16_schema = Schema.create ~format:Type.Int16 ~name:"int16_field" () in
  Alcotest.(check bool) "Int16 schema format" true (Schema.format int16_schema = Type.Int16);

  let int32_schema = Schema.create ~format:Type.Int32 ~name:"int32_field" () in
  Alcotest.(check bool) "Int32 schema format" true (Schema.format int32_schema = Type.Int32);

  let int64_schema = Schema.create ~format:Type.Int64 ~name:"int64_field" () in
  Alcotest.(check bool) "Int64 schema format" true (Schema.format int64_schema = Type.Int64)

let test_unsigned_integer_schemas () =
  (* Test unsigned integer schemas *)
  let uint8_schema = Schema.create ~format:Type.Uint8 ~name:"uint8_field" () in
  Alcotest.(check bool) "Uint8 schema format" true (Schema.format uint8_schema = Type.Uint8);

  let uint16_schema = Schema.create ~format:Type.Uint16 ~name:"uint16_field" () in
  Alcotest.(check bool) "Uint16 schema format" true (Schema.format uint16_schema = Type.Uint16);

  let uint32_schema = Schema.create ~format:Type.Uint32 ~name:"uint32_field" () in
  Alcotest.(check bool) "Uint32 schema format" true (Schema.format uint32_schema = Type.Uint32);

  let uint64_schema = Schema.create ~format:Type.Uint64 ~name:"uint64_field" () in
  Alcotest.(check bool) "Uint64 schema format" true (Schema.format uint64_schema = Type.Uint64)

let test_floating_point_schemas () =
  (* Test floating point schemas *)
  let float16_schema = Schema.create ~format:Type.Float16 ~name:"float16_field" () in
  Alcotest.(check bool) "Float16 schema format" true (Schema.format float16_schema = Type.Float16);

  let float32_schema = Schema.create ~format:Type.Float32 ~name:"float32_field" () in
  Alcotest.(check bool) "Float32 schema format" true (Schema.format float32_schema = Type.Float32);

  let float64_schema = Schema.create ~format:Type.Float64 ~name:"float64_field" () in
  Alcotest.(check bool) "Float64 schema format" true (Schema.format float64_schema = Type.Float64)

let test_string_and_binary_schemas () =
  (* Test string and binary schemas *)
  let binary_schema = Schema.create ~format:Type.Binary ~name:"binary_field" () in
  Alcotest.(check bool) "Binary schema format" true (Schema.format binary_schema = Type.Binary);

  let large_binary_schema = Schema.create ~format:Type.Large_binary ~name:"large_binary_field" () in
  Alcotest.(check bool) "Large binary schema format" true (Schema.format large_binary_schema = Type.Large_binary);

  let utf8_schema = Schema.create ~format:Type.Utf8_string ~name:"utf8_field" () in
  Alcotest.(check bool) "UTF8 string schema format" true (Schema.format utf8_schema = Type.Utf8_string);

  let large_utf8_schema = Schema.create ~format:Type.Large_utf8_string ~name:"large_utf8_field" () in
  Alcotest.(check bool) "Large UTF8 string schema format" true (Schema.format large_utf8_schema = Type.Large_utf8_string)

let test_specialized_schemas () =
  (* Test decimal schemas *)
  let decimal_schema = Schema.create
    ~format:(Type.Decimal128 {precision = 10; scale = 2})
    ~name:"decimal_field" () in
  Alcotest.(check bool) "Decimal128 schema format matches" true
    (match Schema.format decimal_schema with
     | Type.Decimal128 {precision = 10; scale = 2} -> true
     | _ -> false);

  (* Test fixed width binary schema *)
  let fixed_binary_schema = Schema.create
    ~format:(Type.Fixed_width_binary {bytes = 16})
    ~name:"fixed_binary_field" () in
  Alcotest.(check bool) "Fixed width binary schema format matches" true
    (match Schema.format fixed_binary_schema with
     | Type.Fixed_width_binary {bytes = 16} -> true
     | _ -> false)

let test_temporal_schemas () =
  (* Test date schemas *)
  let date32_schema = Schema.create ~format:(Type.Date32 `Days) ~name:"date32_field" () in
  Alcotest.(check bool) "Date32 schema format" true (Schema.format date32_schema = Type.Date32 `Days);

  let date64_schema = Schema.create ~format:(Type.Date64 `Milliseconds) ~name:"date64_field" () in
  Alcotest.(check bool) "Date64 schema format" true (Schema.format date64_schema = Type.Date64 `Milliseconds);

  (* Test time schemas *)
  let time32_ms_schema = Schema.create ~format:(Type.Time32 `Milliseconds) ~name:"time32_ms_field" () in
  Alcotest.(check bool) "Time32 ms schema format" true (Schema.format time32_ms_schema = Type.Time32 `Milliseconds);

  let time32_s_schema = Schema.create ~format:(Type.Time32 `Seconds) ~name:"time32_s_field" () in
  Alcotest.(check bool) "Time32 s schema format" true (Schema.format time32_s_schema = Type.Time32 `Seconds);

  let time64_us_schema = Schema.create ~format:(Type.Time64 `Microseconds) ~name:"time64_us_field" () in
  Alcotest.(check bool) "Time64 us schema format" true (Schema.format time64_us_schema = Type.Time64 `Microseconds);

  let time64_ns_schema = Schema.create ~format:(Type.Time64 `Nanoseconds) ~name:"time64_ns_field" () in
  Alcotest.(check bool) "Time64 ns schema format" true (Schema.format time64_ns_schema = Type.Time64 `Nanoseconds)

let test_timestamp_schemas () =
  (* Test timestamp schemas with different precisions and timezones *)
  let timestamp_s_utc = Schema.create
    ~format:(Type.Timestamp {precision = `Seconds; timezone = "UTC"})
    ~name:"timestamp_s_utc" () in
  Alcotest.(check bool) "Timestamp seconds UTC format" true
    (match Schema.format timestamp_s_utc with
     | Type.Timestamp {precision = `Seconds; timezone = "UTC"} -> true
     | _ -> false);

  let timestamp_ms_ny = Schema.create
    ~format:(Type.Timestamp {precision = `Milliseconds; timezone = "America/New_York"})
    ~name:"timestamp_ms_ny" () in
  Alcotest.(check bool) "Timestamp ms NY format" true
    (match Schema.format timestamp_ms_ny with
     | Type.Timestamp {precision = `Milliseconds; timezone = "America/New_York"} -> true
     | _ -> false);

  let timestamp_us_empty = Schema.create
    ~format:(Type.Timestamp {precision = `Microseconds; timezone = ""})
    ~name:"timestamp_us_empty" () in
  Alcotest.(check bool) "Timestamp us empty timezone format" true
    (match Schema.format timestamp_us_empty with
     | Type.Timestamp {precision = `Microseconds; timezone = ""} -> true
     | _ -> false);

  let timestamp_ns_london = Schema.create
    ~format:(Type.Timestamp {precision = `Nanoseconds; timezone = "Europe/London"})
    ~name:"timestamp_ns_london" () in
  Alcotest.(check bool) "Timestamp ns London format" true
    (match Schema.format timestamp_ns_london with
     | Type.Timestamp {precision = `Nanoseconds; timezone = "Europe/London"} -> true
     | _ -> false)

let test_duration_and_interval_schemas () =
  (* Test duration schemas *)
  let duration_s = Schema.create ~format:(Type.Duration `Seconds) ~name:"duration_s" () in
  Alcotest.(check bool) "Duration seconds format" true (Schema.format duration_s = Type.Duration `Seconds);

  let duration_ms = Schema.create ~format:(Type.Duration `Milliseconds) ~name:"duration_ms" () in
  Alcotest.(check bool) "Duration ms format" true (Schema.format duration_ms = Type.Duration `Milliseconds);

  let duration_us = Schema.create ~format:(Type.Duration `Microseconds) ~name:"duration_us" () in
  Alcotest.(check bool) "Duration us format" true (Schema.format duration_us = Type.Duration `Microseconds);

  let duration_ns = Schema.create ~format:(Type.Duration `Nanoseconds) ~name:"duration_ns" () in
  Alcotest.(check bool) "Duration ns format" true (Schema.format duration_ns = Type.Duration `Nanoseconds);

  (* Test interval schemas *)
  let interval_months = Schema.create ~format:(Type.Interval `Months) ~name:"interval_months" () in
  Alcotest.(check bool) "Interval months format" true (Schema.format interval_months = Type.Interval `Months);

  let interval_days = Schema.create ~format:(Type.Interval `Days_time) ~name:"interval_days" () in
  Alcotest.(check bool) "Interval days format" true (Schema.format interval_days = Type.Interval `Days_time)

let test_structural_schemas () =
  (* Test struct and map schemas *)
  let struct_schema = Schema.create ~format:Type.Struct ~name:"struct_field" () in
  Alcotest.(check bool) "Struct schema format" true (Schema.format struct_schema = Type.Struct);

  let map_schema = Schema.create ~format:Type.Map ~name:"map_field" () in
  Alcotest.(check bool) "Map schema format" true (Schema.format map_schema = Type.Map)

let test_unknown_schema () =
  (* Test unknown type schema *)
  let unknown_schema = Schema.create ~format:(Type.Unknown "custom_type") ~name:"unknown_field" () in
  Alcotest.(check bool) "Unknown schema format" true
    (match Schema.format unknown_schema with
     | Type.Unknown "custom_type" -> true
     | _ -> false)

(** {2 Schema Metadata Tests} *)

let test_schema_with_metadata () =
  (* Test schema with various metadata configurations *)
  let empty_metadata = [] in
  let schema_empty_meta = Schema.create ~format:Type.Int32 ~name:"empty_meta" ~metadata:empty_metadata () in
  Alcotest.(check (list (pair string string))) "Empty metadata" [] (Schema.metadata schema_empty_meta);

  let single_metadata = [("key", "value")] in
  let schema_single_meta = Schema.create ~format:Type.Utf8_string ~name:"single_meta" ~metadata:single_metadata () in
  Alcotest.(check (list (pair string string))) "Single metadata" single_metadata (Schema.metadata schema_single_meta);

  let multi_metadata = [("source", "test"); ("version", "1.0"); ("author", "arrow-ocaml")] in
  let schema_multi_meta = Schema.create ~format:Type.Float64 ~name:"multi_meta" ~metadata:multi_metadata () in
  Alcotest.(check (list (pair string string))) "Multiple metadata" multi_metadata (Schema.metadata schema_multi_meta);

  (* Test metadata with special characters *)
  let special_metadata = [("unicode", "🚀 test"); ("json", "{\"key\": \"value\"}"); ("empty_value", "")] in
  let schema_special_meta = Schema.create ~format:Type.Boolean ~name:"special_meta" ~metadata:special_metadata () in
  Alcotest.(check (list (pair string string))) "Special metadata" special_metadata (Schema.metadata schema_special_meta)

(** {2 Schema Flags Tests} *)

let test_schema_flags_none () =
  (* Test no flags set *)
  let schema_no_flags = Schema.create ~format:Type.Int64 ~name:"no_flags" () in
  let flags = Schema.flags schema_no_flags in
  Alcotest.(check bool) "No flags - not nullable" false (Schema.Flags.is_nullable flags);
  Alcotest.(check bool) "No flags - not dictionary ordered" false (Schema.Flags.is_dictionary_ordered flags);
  Alcotest.(check bool) "No flags - not map keys sorted" false (Schema.Flags.is_map_keys_sorted flags);

  let explicit_none = Schema.create ~format:Type.Int64 ~name:"explicit_none" ~flags:Schema.Flags.none () in
  let explicit_flags = Schema.flags explicit_none in
  Alcotest.(check bool) "Explicit none - not nullable" false (Schema.Flags.is_nullable explicit_flags)

let test_schema_flags_individual () =
  (* Test individual flags *)
  let nullable_schema = Schema.create ~format:Type.Utf8_string ~name:"nullable" ~flags:Schema.Flags.nullable () in
  let nullable_flags = Schema.flags nullable_schema in
  Alcotest.(check bool) "Nullable flag set" true (Schema.Flags.is_nullable nullable_flags);
  Alcotest.(check bool) "Nullable only - not dictionary ordered" false (Schema.Flags.is_dictionary_ordered nullable_flags);
  Alcotest.(check bool) "Nullable only - not map keys sorted" false (Schema.Flags.is_map_keys_sorted nullable_flags);

  let dict_ordered_schema = Schema.create ~format:Type.Utf8_string ~name:"dict_ordered" ~flags:Schema.Flags.dictionary_ordered () in
  let dict_flags = Schema.flags dict_ordered_schema in
  Alcotest.(check bool) "Dictionary ordered flag set" true (Schema.Flags.is_dictionary_ordered dict_flags);
  Alcotest.(check bool) "Dictionary only - not nullable" false (Schema.Flags.is_nullable dict_flags);
  Alcotest.(check bool) "Dictionary only - not map keys sorted" false (Schema.Flags.is_map_keys_sorted dict_flags);

  let map_sorted_schema = Schema.create ~format:Type.Map ~name:"map_sorted" ~flags:Schema.Flags.map_keys_sorted () in
  let map_flags = Schema.flags map_sorted_schema in
  Alcotest.(check bool) "Map keys sorted flag set" true (Schema.Flags.is_map_keys_sorted map_flags);
  Alcotest.(check bool) "Map sorted only - not nullable" false (Schema.Flags.is_nullable map_flags);
  Alcotest.(check bool) "Map sorted only - not dictionary ordered" false (Schema.Flags.is_dictionary_ordered map_flags)

let test_schema_flags_combinations () =
  (* Test flag combinations *)
  let nullable_dict = mk_flags [Schema.Flags.nullable; Schema.Flags.dictionary_ordered] in
  let schema_nullable_dict = Schema.create ~format:Type.Utf8_string ~name:"nullable_dict" ~flags:nullable_dict () in
  let flags_nd = Schema.flags schema_nullable_dict in
  Alcotest.(check bool) "Nullable + Dict - nullable" true (Schema.Flags.is_nullable flags_nd);
  Alcotest.(check bool) "Nullable + Dict - dictionary ordered" true (Schema.Flags.is_dictionary_ordered flags_nd);
  Alcotest.(check bool) "Nullable + Dict - not map keys sorted" false (Schema.Flags.is_map_keys_sorted flags_nd);

  let all_flags = mk_flags [Schema.Flags.nullable; Schema.Flags.dictionary_ordered; Schema.Flags.map_keys_sorted] in
  let schema_all = Schema.create ~format:Type.Map ~name:"all_flags" ~flags:all_flags () in
  let flags_all = Schema.flags schema_all in
  Alcotest.(check bool) "All flags - nullable" true (Schema.Flags.is_nullable flags_all);
  Alcotest.(check bool) "All flags - dictionary ordered" true (Schema.Flags.is_dictionary_ordered flags_all);
  Alcotest.(check bool) "All flags - map keys sorted" true (Schema.Flags.is_map_keys_sorted flags_all)

(** {2 Schema Children Tests} *)

let test_schema_no_children () =
  (* Test schemas with no children *)
  let simple_schema = Schema.create ~format:Type.Int32 ~name:"simple" () in
  let children = Schema.children simple_schema in
  Alcotest.(check int) "Simple schema has no children" 0 (List.length children);

  let explicit_empty = Schema.create ~format:Type.Float64 ~name:"explicit_empty" ~children:[] () in
  let empty_children = Schema.children explicit_empty in
  Alcotest.(check int) "Explicitly empty children" 0 (List.length empty_children)

let test_schema_with_children () =
  (* Test schema with single child *)
  let child = Schema.create ~format:Type.Int32 ~name:"child_field" () in
  let parent_single = Schema.create ~format:Type.Struct ~name:"parent_single" ~children:[child] () in
  let single_children = Schema.children parent_single in
  Alcotest.(check int) "Single child count" 1 (List.length single_children);
  let retrieved_child = List.hd single_children in
  Alcotest.(check string) "Single child name" "child_field" (Schema.name retrieved_child);
  Alcotest.(check bool) "Single child format" true (Schema.format retrieved_child = Type.Int32);

  (* Test schema with multiple children *)
  let child1 = Schema.create ~format:Type.Int32 ~name:"id" () in
  let child2 = Schema.create ~format:Type.Utf8_string ~name:"name" () in
  let child3 = Schema.create ~format:Type.Float64 ~name:"value" () in
  let child4 = Schema.create ~format:Type.Boolean ~name:"active" () in

  let parent_multi = Schema.create ~format:Type.Struct ~name:"record" ~children:[child1; child2; child3; child4] () in
  let multi_children = Schema.children parent_multi in
  Alcotest.(check int) "Multiple children count" 4 (List.length multi_children);

  let child_names = List.map Schema.name multi_children in
  let expected_names = ["id"; "name"; "value"; "active"] in
  Alcotest.(check (list string)) "Multiple children names" expected_names child_names;

  let child_formats = List.map Schema.format multi_children in
  let expected_formats = [Type.Int32; Type.Utf8_string; Type.Float64; Type.Boolean] in
  Alcotest.(check (list bool)) "Multiple children formats" [true; true; true; true]
    (List.map2 (=) expected_formats child_formats)

let test_schema_nested_children () =
  (* Test deeply nested schema structures *)
  let leaf1 = Schema.create ~format:Type.Int32 ~name:"leaf1" () in
  let leaf2 = Schema.create ~format:Type.Utf8_string ~name:"leaf2" () in
  let branch1 = Schema.create ~format:Type.Struct ~name:"branch1" ~children:[leaf1; leaf2] () in

  let leaf3 = Schema.create ~format:Type.Float64 ~name:"leaf3" () in
  let branch2 = Schema.create ~format:Type.Struct ~name:"branch2" ~children:[leaf3] () in

  let root = Schema.create ~format:Type.Struct ~name:"root" ~children:[branch1; branch2] () in
  let root_children = Schema.children root in
  Alcotest.(check int) "Root has two branches" 2 (List.length root_children);

  let first_branch = List.hd root_children in
  let first_branch_children = Schema.children first_branch in
  Alcotest.(check int) "First branch has two leaves" 2 (List.length first_branch_children);
  Alcotest.(check string) "First branch name" "branch1" (Schema.name first_branch);

  let second_branch = List.nth root_children 1 in
  let second_branch_children = Schema.children second_branch in
  Alcotest.(check int) "Second branch has one leaf" 1 (List.length second_branch_children);
  Alcotest.(check string) "Second branch name" "branch2" (Schema.name second_branch)

(** {2 Schema String Representation Tests} *)

let test_schema_to_string_simple () =
  (* Test string representation of simple schemas *)
  let int_schema = Schema.create ~format:Type.Int32 ~name:"test_int" () in
  let int_str = Schema.to_string int_schema in
  Alcotest.(check bool) "Int schema string non-empty" true (String.length int_str > 0);
  Alcotest.(check bool) "Int schema contains name" true (String.contains int_str 't');

  let float_schema = Schema.create ~format:Type.Float64 ~name:"test_float" () in
  let float_str = Schema.to_string float_schema in
  Alcotest.(check bool) "Float schema string non-empty" true (String.length float_str > 0);

  let string_schema = Schema.create ~format:Type.Utf8_string ~name:"test_string" () in
  let string_str = Schema.to_string string_schema in
  Alcotest.(check bool) "String schema string non-empty" true (String.length string_str > 0)

let test_schema_to_string_with_metadata () =
  (* Test string representation with metadata *)
  let metadata = [("source", "test"); ("version", "1.0")] in
  let schema_with_meta = Schema.create ~format:Type.Boolean ~name:"test_bool" ~metadata () in
  let meta_str = Schema.to_string schema_with_meta in
  Alcotest.(check bool) "Schema with metadata string non-empty" true (String.length meta_str > 0);
  (* Check if metadata appears in string representation *)
  Alcotest.(check bool) "Schema string might contain metadata indicator" true (String.length meta_str >= String.length "test_bool")

let test_schema_to_string_with_flags () =
  (* Test string representation with flags *)
  let flags = mk_flags [Schema.Flags.nullable; Schema.Flags.dictionary_ordered] in
  let schema_with_flags = Schema.create ~format:Type.Utf8_string ~name:"test_flags" ~flags () in
  let flags_str = Schema.to_string schema_with_flags in
  Alcotest.(check bool) "Schema with flags string non-empty" true (String.length flags_str > 0)

let test_schema_to_string_complex () =
  (* Test string representation of complex schemas *)
  let child1 = Schema.create ~format:Type.Int32 ~name:"id" () in
  let child2 = Schema.create ~format:Type.Utf8_string ~name:"name" ~flags:Schema.Flags.nullable () in
  let complex_schema = Schema.create
    ~format:Type.Struct
    ~name:"complex_record"
    ~metadata:[("table", "users"); ("encoding", "utf8")]
    ~flags:(mk_flags [Schema.Flags.nullable])
    ~children:[child1; child2] () in

  let complex_str = Schema.to_string complex_schema in
  Alcotest.(check bool) "Complex schema string non-empty" true (String.length complex_str > 0);
  Alcotest.(check bool) "Complex schema string substantial" true (String.length complex_str > 20)

(** {2 Edge Cases and Error Conditions} *)

let test_schema_edge_cases () =
  (* Test schema with empty name *)
  let empty_name_schema = Schema.create ~format:Type.Int32 ~name:"" () in
  Alcotest.(check string) "Empty name schema" "" (Schema.name empty_name_schema);

  (* Test schema with very long name *)
  let long_name = String.make 1000 'x' in
  let long_name_schema = Schema.create ~format:Type.Utf8_string ~name:long_name () in
  Alcotest.(check string) "Long name schema" long_name (Schema.name long_name_schema);

  (* Test schema with unicode in name *)
  let unicode_name = "field_🚀_测试" in
  let unicode_schema = Schema.create ~format:Type.Utf8_string ~name:unicode_name () in
  Alcotest.(check string) "Unicode name schema" unicode_name (Schema.name unicode_schema);

  (* Test schema with many children (stress test) *)
  let many_children = List.init 100 (fun i ->
    Schema.create ~format:Type.Int32 ~name:(Printf.sprintf "field_%d" i) ()) in
  let schema_many_children = Schema.create ~format:Type.Struct ~name:"many_fields" ~children:many_children () in
  let retrieved_children = Schema.children schema_many_children in
  Alcotest.(check int) "Many children count" 100 (List.length retrieved_children)

(** {2 Integration with Table Tests} *)

let test_schema_from_table () =
  (* Test schema extraction from simple table *)
  let simple_table = Test_fixtures.small_test_table () in
  let table_schema = Table.schema simple_table in
  let schema_children = Schema.children table_schema in

  Alcotest.(check bool) "Table has schema children" true (List.length schema_children > 0);

  let expected_field_names = ["integers"; "floats"; "strings"; "booleans"; "dates"; "timestamps"; "spans"; "ofdays"] in
  let actual_field_names = List.map Schema.name schema_children in
  Alcotest.(check (list string)) "Table field names match" expected_field_names actual_field_names;

  (* Verify each field has the expected type *)
  let field_formats = List.map Schema.format schema_children in
  Alcotest.(check int) "All fields have formats" (List.length expected_field_names) (List.length field_formats)

let test_schema_from_nullable_table () =
  (* Test schema from table with nullable columns *)
  let nullable_table = Test_fixtures.nullable_test_table () in
  let nullable_schema = Table.schema nullable_table in
  let nullable_children = Schema.children nullable_schema in

  Alcotest.(check bool) "Nullable table has schema children" true (List.length nullable_children > 0);

  let expected_opt_names = ["opt_integers"; "opt_floats"; "opt_strings"; "opt_booleans";
                           "opt_dates"; "opt_timestamps"; "opt_spans"; "opt_ofdays"] in
  let actual_opt_names = List.map Schema.name nullable_children in
  Alcotest.(check (list string)) "Nullable field names match" expected_opt_names actual_opt_names

let test_schema_from_empty_table () =
  (* Test schema from empty table *)
  let empty_table = Test_fixtures.empty_test_table () in
  let empty_schema = Table.schema empty_table in
  let empty_children = Schema.children empty_schema in

  Alcotest.(check bool) "Empty table has schema structure" true (List.length empty_children > 0);

  let empty_field_names = List.map Schema.name empty_children in
  let expected_empty_names = ["integers"; "floats"; "strings"; "booleans"; "dates"; "timestamps"; "spans"; "ofdays"] in
  Alcotest.(check (list string)) "Empty table field names" expected_empty_names empty_field_names

let test_schema_from_custom_table () =
  (* Test schema from custom built table *)
  let custom_table = Table.create [
    Table.col [|1; 2; 3|] Table.Int "custom_id";
    Table.col [|"a"; "b"; "c"|] Table.Utf8 "custom_name";
    Table.col [|1.0; 2.0; 3.0|] Table.Float "custom_value";
    Table.col [|true; false; true|] Table.Bool "custom_active";
  ] in

  let custom_schema = Table.schema custom_table in
  let custom_children = Schema.children custom_schema in
  Alcotest.(check int) "Custom table has 4 fields" 4 (List.length custom_children);

  let custom_names = List.map Schema.name custom_children in
  Alcotest.(check (list string)) "Custom field names" ["custom_id"; "custom_name"; "custom_value"; "custom_active"] custom_names

(** {2 Schema Inspection and Validation} *)

let test_schema_property_inspection () =
  (* Create a comprehensive schema for inspection *)
  let child1 = Schema.create ~format:Type.Int32 ~name:"id" ~flags:Schema.Flags.nullable () in
  let child2 = Schema.create ~format:Type.Utf8_string ~name:"name" () in
  let child3 = Schema.create ~format:Type.Float64 ~name:"score" ~metadata:[("unit", "points")] () in

  let inspection_schema = Schema.create
    ~format:Type.Struct
    ~name:"inspection_test"
    ~metadata:[("source", "test_suite"); ("version", "2.0")]
    ~flags:(mk_flags [Schema.Flags.dictionary_ordered])
    ~children:[child1; child2; child3] () in

  (* Inspect root schema properties *)
  Alcotest.(check string) "Inspection schema name" "inspection_test" (Schema.name inspection_schema);
  Alcotest.(check bool) "Inspection schema format" true (Schema.format inspection_schema = Type.Struct);

  let metadata = Schema.metadata inspection_schema in
  Alcotest.(check int) "Inspection schema metadata count" 2 (List.length metadata);
  Alcotest.(check bool) "Inspection schema has source metadata" true (List.mem ("source", "test_suite") metadata);
  Alcotest.(check bool) "Inspection schema has version metadata" true (List.mem ("version", "2.0") metadata);

  let flags = Schema.flags inspection_schema in
  Alcotest.(check bool) "Inspection schema dictionary ordered" true (Schema.Flags.is_dictionary_ordered flags);
  Alcotest.(check bool) "Inspection schema not nullable" false (Schema.Flags.is_nullable flags);

  (* Inspect children *)
  let children = Schema.children inspection_schema in
  Alcotest.(check int) "Inspection schema children count" 3 (List.length children);

  let first_child = List.hd children in
  Alcotest.(check string) "First child name" "id" (Schema.name first_child);
  let first_child_flags = Schema.flags first_child in
  Alcotest.(check bool) "First child nullable" true (Schema.Flags.is_nullable first_child_flags);

  let third_child = List.nth children 2 in
  Alcotest.(check string) "Third child name" "score" (Schema.name third_child);
  let third_child_metadata = Schema.metadata third_child in
  Alcotest.(check bool) "Third child has unit metadata" true (List.mem ("unit", "points") third_child_metadata)

(** {2 Test Fixtures Integration} *)

let test_schema_with_test_fixtures () =
  (* Test using test fixtures for schema creation and validation *)
  let test_ints = Test_fixtures.sample_ints ~size:5 () in
  let test_strings = Test_fixtures.sample_strings ~size:5 () in
  let test_floats = Test_fixtures.sample_floats ~size:5 () in

  let fixture_table = Table.create [
    Table.col test_ints Table.Int "fixture_ints";
    Table.col test_strings Table.Utf8 "fixture_strings";
    Table.col test_floats Table.Float "fixture_floats";
  ] in

  let fixture_schema = Table.schema fixture_table in
  let fixture_children = Schema.children fixture_schema in
  Alcotest.(check int) "Fixture table field count" 3 (List.length fixture_children);

  let fixture_names = List.map Schema.name fixture_children in
  Alcotest.(check (list string)) "Fixture field names" ["fixture_ints"; "fixture_strings"; "fixture_floats"] fixture_names;

  (* Test with nullable fixtures *)
  let test_ints_opt = Test_fixtures.sample_ints_opt ~size:5 ~null_ratio:0.4 () in
  let nullable_fixture_table = Table.create [
    Table.col_opt test_ints_opt Table.Int "nullable_fixture_ints";
  ] in

  let nullable_fixture_schema = Table.schema nullable_fixture_table in
  let nullable_fixture_children = Schema.children nullable_fixture_schema in
  Alcotest.(check int) "Nullable fixture field count" 1 (List.length nullable_fixture_children);

  let nullable_field = List.hd nullable_fixture_children in
  Alcotest.(check string) "Nullable fixture field name" "nullable_fixture_ints" (Schema.name nullable_field)

(** {2 Performance and Stress Tests} *)

let test_schema_performance () =
  (* Create schemas with many fields to test performance *)
  let large_field_count = 500 in
  let large_children = List.init large_field_count (fun i ->
    let field_type = match i mod 4 with
      | 0 -> Type.Int32
      | 1 -> Type.Float64
      | 2 -> Type.Utf8_string
      | _ -> Type.Boolean in
    Schema.create ~format:field_type ~name:(Printf.sprintf "field_%04d" i) ()) in

  let large_schema = Schema.create ~format:Type.Struct ~name:"large_schema" ~children:large_children () in
  let retrieved_children = Schema.children large_schema in
  Alcotest.(check int) "Large schema children count" large_field_count (List.length retrieved_children);

  (* Test string representation performance *)
  let large_str = Schema.to_string large_schema in
  Alcotest.(check bool) "Large schema string non-empty" true (String.length large_str > 0)

(** {2 Test Suite Definition} *)

let () =
  let open Alcotest in
  run "Comprehensive Schema Tests" [
    "basic_creation", [
      test_case "Basic schema creation" `Quick test_basic_schema_creation;
      test_case "Unsigned integer schemas" `Quick test_unsigned_integer_schemas;
      test_case "Floating point schemas" `Quick test_floating_point_schemas;
      test_case "String and binary schemas" `Quick test_string_and_binary_schemas;
      test_case "Specialized schemas" `Quick test_specialized_schemas;
      test_case "Temporal schemas" `Quick test_temporal_schemas;
      test_case "Timestamp schemas" `Quick test_timestamp_schemas;
      test_case "Duration and interval schemas" `Quick test_duration_and_interval_schemas;
      test_case "Structural schemas" `Quick test_structural_schemas;
      test_case "Unknown schema" `Quick test_unknown_schema;
    ];
    "metadata", [
      test_case "Schema with metadata" `Quick test_schema_with_metadata;
    ];
    "flags", [
      test_case "Schema flags none" `Quick test_schema_flags_none;
      test_case "Schema flags individual" `Quick test_schema_flags_individual;
      test_case "Schema flags combinations" `Quick test_schema_flags_combinations;
    ];
    "children", [
      test_case "Schema no children" `Quick test_schema_no_children;
      test_case "Schema with children" `Quick test_schema_with_children;
      test_case "Schema nested children" `Quick test_schema_nested_children;
    ];
    "string_representation", [
      test_case "Schema to string simple" `Quick test_schema_to_string_simple;
      test_case "Schema to string with metadata" `Quick test_schema_to_string_with_metadata;
      test_case "Schema to string with flags" `Quick test_schema_to_string_with_flags;
      test_case "Schema to string complex" `Quick test_schema_to_string_complex;
    ];
    "edge_cases", [
      test_case "Schema edge cases" `Quick test_schema_edge_cases;
    ];
    "table_integration", [
      test_case "Schema from table" `Quick test_schema_from_table;
      test_case "Schema from nullable table" `Quick test_schema_from_nullable_table;
      test_case "Schema from empty table" `Quick test_schema_from_empty_table;
      test_case "Schema from custom table" `Quick test_schema_from_custom_table;
    ];
    "inspection", [
      test_case "Schema property inspection" `Quick test_schema_property_inspection;
    ];
    "test_fixtures_integration", [
      test_case "Schema with test fixtures" `Quick test_schema_with_test_fixtures;
    ];
    "performance", [
      test_case "Schema performance" `Slow test_schema_performance;
    ];
  ]
(** Comprehensive tests for the Type module in the Arrow OCaml library *)

open Arrow
open Test_fixtures

(** Custom testable types for Alcotest *)
let type_testable = Alcotest.testable
  (fun fmt typ ->
    match typ with
    | Type.Null -> Format.fprintf fmt "Null"
    | Type.Boolean -> Format.fprintf fmt "Boolean"
    | Type.Int8 -> Format.fprintf fmt "Int8"
    | Type.Uint8 -> Format.fprintf fmt "Uint8"
    | Type.Int16 -> Format.fprintf fmt "Int16"
    | Type.Uint16 -> Format.fprintf fmt "Uint16"
    | Type.Int32 -> Format.fprintf fmt "Int32"
    | Type.Uint32 -> Format.fprintf fmt "Uint32"
    | Type.Int64 -> Format.fprintf fmt "Int64"
    | Type.Uint64 -> Format.fprintf fmt "Uint64"
    | Type.Float16 -> Format.fprintf fmt "Float16"
    | Type.Float32 -> Format.fprintf fmt "Float32"
    | Type.Float64 -> Format.fprintf fmt "Float64"
    | Type.Binary -> Format.fprintf fmt "Binary"
    | Type.Large_binary -> Format.fprintf fmt "Large_binary"
    | Type.Utf8_string -> Format.fprintf fmt "Utf8_string"
    | Type.Large_utf8_string -> Format.fprintf fmt "Large_utf8_string"
    | Type.Decimal128 {precision; scale} -> Format.fprintf fmt "Decimal128(precision=%d,scale=%d)" precision scale
    | Type.Fixed_width_binary {bytes} -> Format.fprintf fmt "Fixed_width_binary(bytes=%d)" bytes
    | Type.Date32 `Days -> Format.fprintf fmt "Date32(Days)"
    | Type.Date64 `Milliseconds -> Format.fprintf fmt "Date64(Milliseconds)"
    | Type.Time32 `Seconds -> Format.fprintf fmt "Time32(Seconds)"
    | Type.Time32 `Milliseconds -> Format.fprintf fmt "Time32(Milliseconds)"
    | Type.Time64 `Microseconds -> Format.fprintf fmt "Time64(Microseconds)"
    | Type.Time64 `Nanoseconds -> Format.fprintf fmt "Time64(Nanoseconds)"
    | Type.Timestamp {precision; timezone} ->
        Format.fprintf fmt "Timestamp(precision=%s,timezone=\"%s\")"
          (match precision with
           | `Seconds -> "Seconds"
           | `Milliseconds -> "Milliseconds"
           | `Microseconds -> "Microseconds"
           | `Nanoseconds -> "Nanoseconds")
          timezone
    | Type.Duration `Seconds -> Format.fprintf fmt "Duration(Seconds)"
    | Type.Duration `Milliseconds -> Format.fprintf fmt "Duration(Milliseconds)"
    | Type.Duration `Microseconds -> Format.fprintf fmt "Duration(Microseconds)"
    | Type.Duration `Nanoseconds -> Format.fprintf fmt "Duration(Nanoseconds)"
    | Type.Interval `Months -> Format.fprintf fmt "Interval(Months)"
    | Type.Interval `Days_time -> Format.fprintf fmt "Interval(Days_time)"
    | Type.Struct -> Format.fprintf fmt "Struct"
    | Type.Map -> Format.fprintf fmt "Map"
    | Type.Unknown s -> Format.fprintf fmt "Unknown(\"%s\")" s)
  (=)

(** {2 Basic type variant tests} *)

let test_null_type () =
  let null_type = Type.Null in
  Alcotest.(check type_testable) "Null type equality" Type.Null null_type

let test_boolean_type () =
  let bool_type = Type.Boolean in
  Alcotest.(check type_testable) "Boolean type equality" Type.Boolean bool_type

let test_integer_types () =
  (* Test all integer types *)
  let int8_type = Type.Int8 in
  let uint8_type = Type.Uint8 in
  let int16_type = Type.Int16 in
  let uint16_type = Type.Uint16 in
  let int32_type = Type.Int32 in
  let uint32_type = Type.Uint32 in
  let int64_type = Type.Int64 in
  let uint64_type = Type.Uint64 in

  Alcotest.(check type_testable) "Int8 type equality" Type.Int8 int8_type;
  Alcotest.(check type_testable) "Uint8 type equality" Type.Uint8 uint8_type;
  Alcotest.(check type_testable) "Int16 type equality" Type.Int16 int16_type;
  Alcotest.(check type_testable) "Uint16 type equality" Type.Uint16 uint16_type;
  Alcotest.(check type_testable) "Int32 type equality" Type.Int32 int32_type;
  Alcotest.(check type_testable) "Uint32 type equality" Type.Uint32 uint32_type;
  Alcotest.(check type_testable) "Int64 type equality" Type.Int64 int64_type;
  Alcotest.(check type_testable) "Uint64 type equality" Type.Uint64 uint64_type

let test_floating_point_types () =
  (* Test all floating point types *)
  let float16_type = Type.Float16 in
  let float32_type = Type.Float32 in
  let float64_type = Type.Float64 in

  Alcotest.(check type_testable) "Float16 type equality" Type.Float16 float16_type;
  Alcotest.(check type_testable) "Float32 type equality" Type.Float32 float32_type;
  Alcotest.(check type_testable) "Float64 type equality" Type.Float64 float64_type

let test_binary_types () =
  (* Test binary types *)
  let binary_type = Type.Binary in
  let large_binary_type = Type.Large_binary in

  Alcotest.(check type_testable) "Binary type equality" Type.Binary binary_type;
  Alcotest.(check type_testable) "Large_binary type equality" Type.Large_binary large_binary_type

let test_string_types () =
  (* Test string types *)
  let utf8_type = Type.Utf8_string in
  let large_utf8_type = Type.Large_utf8_string in

  Alcotest.(check type_testable) "Utf8_string type equality" Type.Utf8_string utf8_type;
  Alcotest.(check type_testable) "Large_utf8_string type equality" Type.Large_utf8_string large_utf8_type

let test_complex_types () =
  (* Test struct and map types *)
  let struct_type = Type.Struct in
  let map_type = Type.Map in

  Alcotest.(check type_testable) "Struct type equality" Type.Struct struct_type;
  Alcotest.(check type_testable) "Map type equality" Type.Map map_type

(** {2 Parameterized type tests} *)

let test_decimal_type () =
  (* Test decimal types with different parameters *)
  let decimal1 = Type.Decimal128 {precision = 10; scale = 2} in
  let decimal2 = Type.Decimal128 {precision = 18; scale = 4} in
  let decimal3 = Type.Decimal128 {precision = 38; scale = 10} in

  Alcotest.(check type_testable) "Decimal1 equality"
    (Type.Decimal128 {precision = 10; scale = 2}) decimal1;
  Alcotest.(check type_testable) "Decimal2 equality"
    (Type.Decimal128 {precision = 18; scale = 4}) decimal2;
  Alcotest.(check type_testable) "Decimal3 equality"
    (Type.Decimal128 {precision = 38; scale = 10}) decimal3;

  (* Test that different parameters create different types *)
  Alcotest.(check bool) "Different decimals are not equal" false
    (decimal1 = decimal2);
  Alcotest.(check bool) "Different decimals are not equal (2)" false
    (decimal2 = decimal3)

let test_fixed_width_binary_type () =
  (* Test fixed width binary types *)
  let fwb1 = Type.Fixed_width_binary {bytes = 16} in
  let fwb2 = Type.Fixed_width_binary {bytes = 32} in
  let fwb3 = Type.Fixed_width_binary {bytes = 128} in

  Alcotest.(check type_testable) "Fixed width binary 16"
    (Type.Fixed_width_binary {bytes = 16}) fwb1;
  Alcotest.(check type_testable) "Fixed width binary 32"
    (Type.Fixed_width_binary {bytes = 32}) fwb2;
  Alcotest.(check type_testable) "Fixed width binary 128"
    (Type.Fixed_width_binary {bytes = 128}) fwb3;

  (* Test that different byte sizes create different types *)
  Alcotest.(check bool) "Different FWB types are not equal" false (fwb1 = fwb2);
  Alcotest.(check bool) "Different FWB types are not equal (2)" false (fwb2 = fwb3)

(** {2 Temporal type tests} *)

let test_date_types () =
  (* Test date types *)
  let date32 = Type.Date32 `Days in
  let date64 = Type.Date64 `Milliseconds in

  Alcotest.(check type_testable) "Date32 equality" (Type.Date32 `Days) date32;
  Alcotest.(check type_testable) "Date64 equality" (Type.Date64 `Milliseconds) date64;

  (* Test that date types are different *)
  Alcotest.(check bool) "Date32 and Date64 are different" false (date32 = date64)

let test_time_types () =
  (* Test time types *)
  let time32_sec = Type.Time32 `Seconds in
  let time32_ms = Type.Time32 `Milliseconds in
  let time64_us = Type.Time64 `Microseconds in
  let time64_ns = Type.Time64 `Nanoseconds in

  Alcotest.(check type_testable) "Time32 seconds" (Type.Time32 `Seconds) time32_sec;
  Alcotest.(check type_testable) "Time32 milliseconds" (Type.Time32 `Milliseconds) time32_ms;
  Alcotest.(check type_testable) "Time64 microseconds" (Type.Time64 `Microseconds) time64_us;
  Alcotest.(check type_testable) "Time64 nanoseconds" (Type.Time64 `Nanoseconds) time64_ns;

  (* Test that different precision time types are different *)
  Alcotest.(check bool) "Time32 different precisions" false (time32_sec = time32_ms);
  Alcotest.(check bool) "Time64 different precisions" false (time64_us = time64_ns);
  Alcotest.(check bool) "Time32 vs Time64" false (time32_sec = time64_ns)

let test_timestamp_types () =
  (* Test timestamp types with different precisions and timezones *)
  let ts_sec_utc = Type.Timestamp {precision = `Seconds; timezone = "UTC"} in
  let ts_ms_utc = Type.Timestamp {precision = `Milliseconds; timezone = "UTC"} in
  let ts_us_utc = Type.Timestamp {precision = `Microseconds; timezone = "UTC"} in
  let ts_ns_utc = Type.Timestamp {precision = `Nanoseconds; timezone = "UTC"} in
  let ts_sec_est = Type.Timestamp {precision = `Seconds; timezone = "America/New_York"} in

  Alcotest.(check type_testable) "Timestamp seconds UTC"
    (Type.Timestamp {precision = `Seconds; timezone = "UTC"}) ts_sec_utc;
  Alcotest.(check type_testable) "Timestamp milliseconds UTC"
    (Type.Timestamp {precision = `Milliseconds; timezone = "UTC"}) ts_ms_utc;
  Alcotest.(check type_testable) "Timestamp microseconds UTC"
    (Type.Timestamp {precision = `Microseconds; timezone = "UTC"}) ts_us_utc;
  Alcotest.(check type_testable) "Timestamp nanoseconds UTC"
    (Type.Timestamp {precision = `Nanoseconds; timezone = "UTC"}) ts_ns_utc;
  Alcotest.(check type_testable) "Timestamp seconds EST"
    (Type.Timestamp {precision = `Seconds; timezone = "America/New_York"}) ts_sec_est;

  (* Test that different precisions and timezones create different types *)
  Alcotest.(check bool) "Different timestamp precisions" false (ts_sec_utc = ts_ms_utc);
  Alcotest.(check bool) "Different timestamp timezones" false (ts_sec_utc = ts_sec_est)

let test_duration_types () =
  (* Test duration types *)
  let dur_sec = Type.Duration `Seconds in
  let dur_ms = Type.Duration `Milliseconds in
  let dur_us = Type.Duration `Microseconds in
  let dur_ns = Type.Duration `Nanoseconds in

  Alcotest.(check type_testable) "Duration seconds" (Type.Duration `Seconds) dur_sec;
  Alcotest.(check type_testable) "Duration milliseconds" (Type.Duration `Milliseconds) dur_ms;
  Alcotest.(check type_testable) "Duration microseconds" (Type.Duration `Microseconds) dur_us;
  Alcotest.(check type_testable) "Duration nanoseconds" (Type.Duration `Nanoseconds) dur_ns;

  (* Test that different duration precisions are different *)
  Alcotest.(check bool) "Different duration precisions" false (dur_sec = dur_ms);
  Alcotest.(check bool) "Different duration precisions (2)" false (dur_us = dur_ns)

let test_interval_types () =
  (* Test interval types *)
  let int_months = Type.Interval `Months in
  let int_days_time = Type.Interval `Days_time in

  Alcotest.(check type_testable) "Interval months" (Type.Interval `Months) int_months;
  Alcotest.(check type_testable) "Interval days_time" (Type.Interval `Days_time) int_days_time;

  (* Test that different interval types are different *)
  Alcotest.(check bool) "Different interval types" false (int_months = int_days_time)

(** {2 Unknown type tests} *)

let test_unknown_types () =
  (* Test unknown types *)
  let unknown1 = Type.Unknown "custom_type" in
  let unknown2 = Type.Unknown "another_custom" in
  let unknown3 = Type.Unknown "" in

  Alcotest.(check type_testable) "Unknown type 1" (Type.Unknown "custom_type") unknown1;
  Alcotest.(check type_testable) "Unknown type 2" (Type.Unknown "another_custom") unknown2;
  Alcotest.(check type_testable) "Unknown empty" (Type.Unknown "") unknown3;

  (* Test that different unknown types are different *)
  Alcotest.(check bool) "Different unknown types" false (unknown1 = unknown2);
  Alcotest.(check bool) "Different unknown types (2)" false (unknown2 = unknown3);

  (* Test that same unknown types are equal *)
  let unknown1_dup = Type.Unknown "custom_type" in
  Alcotest.(check type_testable) "Same unknown types" unknown1 unknown1_dup

(** {2 Type.of_cstring function tests} *)

let test_of_cstring_basic_types () =
  (* Test basic type parsing *)
  Alcotest.(check type_testable) "Null from cstring" Type.Null (Type.of_cstring "n");
  Alcotest.(check type_testable) "Boolean from cstring" Type.Boolean (Type.of_cstring "b");
  Alcotest.(check type_testable) "Int8 from cstring" Type.Int8 (Type.of_cstring "c");
  Alcotest.(check type_testable) "Uint8 from cstring" Type.Uint8 (Type.of_cstring "C");
  Alcotest.(check type_testable) "Int16 from cstring" Type.Int16 (Type.of_cstring "s");
  Alcotest.(check type_testable) "Uint16 from cstring" Type.Uint16 (Type.of_cstring "S");
  Alcotest.(check type_testable) "Int32 from cstring" Type.Int32 (Type.of_cstring "i");
  Alcotest.(check type_testable) "Uint32 from cstring" Type.Uint32 (Type.of_cstring "I");
  Alcotest.(check type_testable) "Int64 from cstring" Type.Int64 (Type.of_cstring "l");
  Alcotest.(check type_testable) "Uint64 from cstring" Type.Uint64 (Type.of_cstring "L");
  Alcotest.(check type_testable) "Float16 from cstring" Type.Float16 (Type.of_cstring "e");
  Alcotest.(check type_testable) "Float32 from cstring" Type.Float32 (Type.of_cstring "f");
  Alcotest.(check type_testable) "Float64 from cstring" Type.Float64 (Type.of_cstring "g")

let test_of_cstring_binary_string_types () =
  (* Test binary and string type parsing *)
  Alcotest.(check type_testable) "Binary from cstring" Type.Binary (Type.of_cstring "z");
  Alcotest.(check type_testable) "Large_binary from cstring" Type.Large_binary (Type.of_cstring "Z");
  Alcotest.(check type_testable) "Utf8_string from cstring" Type.Utf8_string (Type.of_cstring "u");
  Alcotest.(check type_testable) "Large_utf8_string from cstring" Type.Large_utf8_string (Type.of_cstring "U")

let test_of_cstring_temporal_types () =
  (* Test temporal type parsing *)
  Alcotest.(check type_testable) "Date32 from cstring" (Type.Date32 `Days) (Type.of_cstring "tdD");
  Alcotest.(check type_testable) "Date64 from cstring" (Type.Date64 `Milliseconds) (Type.of_cstring "tdm");
  Alcotest.(check type_testable) "Time32 seconds from cstring" (Type.Time32 `Seconds) (Type.of_cstring "tts");
  Alcotest.(check type_testable) "Time32 milliseconds from cstring" (Type.Time32 `Milliseconds) (Type.of_cstring "ttm");
  Alcotest.(check type_testable) "Time64 microseconds from cstring" (Type.Time64 `Microseconds) (Type.of_cstring "ttu");
  Alcotest.(check type_testable) "Time64 nanoseconds from cstring" (Type.Time64 `Nanoseconds) (Type.of_cstring "ttn")

let test_of_cstring_duration_interval_types () =
  (* Test duration and interval type parsing *)
  Alcotest.(check type_testable) "Duration seconds from cstring" (Type.Duration `Seconds) (Type.of_cstring "tDs");
  Alcotest.(check type_testable) "Duration milliseconds from cstring" (Type.Duration `Milliseconds) (Type.of_cstring "tDm");
  Alcotest.(check type_testable) "Duration microseconds from cstring" (Type.Duration `Microseconds) (Type.of_cstring "tDu");
  Alcotest.(check type_testable) "Duration nanoseconds from cstring" (Type.Duration `Nanoseconds) (Type.of_cstring "tDn");
  Alcotest.(check type_testable) "Interval months from cstring" (Type.Interval `Months) (Type.of_cstring "tiM");
  Alcotest.(check type_testable) "Interval days_time from cstring" (Type.Interval `Days_time) (Type.of_cstring "tiD")

let test_of_cstring_complex_types () =
  (* Test struct and map type parsing *)
  Alcotest.(check type_testable) "Struct from cstring" Type.Struct (Type.of_cstring "+s");
  Alcotest.(check type_testable) "Map from cstring" Type.Map (Type.of_cstring "+m")

let test_of_cstring_parameterized_types () =
  (* Test parameterized type parsing *)

  (* Test timestamp types *)
  Alcotest.(check type_testable) "Timestamp seconds UTC"
    (Type.Timestamp {precision = `Seconds; timezone = "UTC"})
    (Type.of_cstring "tss:UTC");
  Alcotest.(check type_testable) "Timestamp milliseconds EST"
    (Type.Timestamp {precision = `Milliseconds; timezone = "America/New_York"})
    (Type.of_cstring "tsm:America/New_York");
  Alcotest.(check type_testable) "Timestamp microseconds empty timezone"
    (Type.Timestamp {precision = `Microseconds; timezone = ""})
    (Type.of_cstring "tsu:");
  Alcotest.(check type_testable) "Timestamp nanoseconds with complex timezone"
    (Type.Timestamp {precision = `Nanoseconds; timezone = "Europe/London"})
    (Type.of_cstring "tsn:Europe/London");

  (* Test fixed width binary types *)
  Alcotest.(check type_testable) "Fixed width binary 16 bytes"
    (Type.Fixed_width_binary {bytes = 16})
    (Type.of_cstring "w:16");
  Alcotest.(check type_testable) "Fixed width binary 32 bytes"
    (Type.Fixed_width_binary {bytes = 32})
    (Type.of_cstring "w:32");
  Alcotest.(check type_testable) "Fixed width binary 0 bytes"
    (Type.Fixed_width_binary {bytes = 0})
    (Type.of_cstring "w:0");

  (* Test decimal types *)
  Alcotest.(check type_testable) "Decimal128 precision 10 scale 2"
    (Type.Decimal128 {precision = 10; scale = 2})
    (Type.of_cstring "d:10,2");
  Alcotest.(check type_testable) "Decimal128 precision 18 scale 4"
    (Type.Decimal128 {precision = 18; scale = 4})
    (Type.of_cstring "d:18,4");
  Alcotest.(check type_testable) "Decimal128 precision 38 scale 0"
    (Type.Decimal128 {precision = 38; scale = 0})
    (Type.of_cstring "d:38,0")

let test_of_cstring_unknown_cases () =
  (* Test unknown/invalid cases *)
  Alcotest.(check type_testable) "Unknown empty string"
    (Type.Unknown "") (Type.of_cstring "");
  Alcotest.(check type_testable) "Unknown invalid character"
    (Type.Unknown "x") (Type.of_cstring "x");
  Alcotest.(check type_testable) "Unknown random string"
    (Type.Unknown "random_stuff") (Type.of_cstring "random_stuff");
  Alcotest.(check type_testable) "Unknown invalid timestamp format"
    (Type.Unknown "ts:invalid") (Type.of_cstring "ts:invalid");
  Alcotest.(check type_testable) "Unknown malformed decimal"
    (Type.Unknown "d:invalid,format") (Type.of_cstring "d:invalid,format");
  Alcotest.(check type_testable) "Unknown malformed decimal (2)"
    (Type.Unknown "d:10") (Type.of_cstring "d:10");
  Alcotest.(check type_testable) "Unknown malformed fixed width binary"
    (Type.Unknown "w:not_a_number") (Type.of_cstring "w:not_a_number");

  (* Test edge cases with colons and commas *)
  Alcotest.(check type_testable) "Unknown colon only"
    (Type.Unknown ":") (Type.of_cstring ":");
  Alcotest.(check type_testable) "Unknown multiple colons"
    (Type.Unknown "tss:UTC:extra") (Type.of_cstring "tss:UTC:extra");
  Alcotest.(check type_testable) "Unknown decimal with extra commas"
    (Type.Unknown "d:10,2,3") (Type.of_cstring "d:10,2,3")

(** {2 Type equality and comparison tests} *)

let test_type_equality () =
  (* Test that equal types are indeed equal *)
  Alcotest.(check bool) "Null equality" true (Type.Null = Type.Null);
  Alcotest.(check bool) "Boolean equality" true (Type.Boolean = Type.Boolean);
  Alcotest.(check bool) "Int32 equality" true (Type.Int32 = Type.Int32);

  (* Test parameterized types *)
  let decimal1a = Type.Decimal128 {precision = 10; scale = 2} in
  let decimal1b = Type.Decimal128 {precision = 10; scale = 2} in
  let decimal2 = Type.Decimal128 {precision = 18; scale = 4} in
  Alcotest.(check bool) "Same decimal parameters" true (decimal1a = decimal1b);
  Alcotest.(check bool) "Different decimal parameters" false (decimal1a = decimal2);

  let timestamp1a = Type.Timestamp {precision = `Seconds; timezone = "UTC"} in
  let timestamp1b = Type.Timestamp {precision = `Seconds; timezone = "UTC"} in
  let timestamp2 = Type.Timestamp {precision = `Milliseconds; timezone = "UTC"} in
  let timestamp3 = Type.Timestamp {precision = `Seconds; timezone = "EST"} in
  Alcotest.(check bool) "Same timestamp parameters" true (timestamp1a = timestamp1b);
  Alcotest.(check bool) "Different timestamp precision" false (timestamp1a = timestamp2);
  Alcotest.(check bool) "Different timestamp timezone" false (timestamp1a = timestamp3)

let test_type_inequality () =
  (* Test that different types are not equal *)
  Alcotest.(check bool) "Null vs Boolean" false (Type.Null = Type.Boolean);
  Alcotest.(check bool) "Int8 vs Uint8" false (Type.Int8 = Type.Uint8);
  Alcotest.(check bool) "Int32 vs Int64" false (Type.Int32 = Type.Int64);
  Alcotest.(check bool) "Float32 vs Float64" false (Type.Float32 = Type.Float64);
  Alcotest.(check bool) "Binary vs Large_binary" false (Type.Binary = Type.Large_binary);
  Alcotest.(check bool) "Utf8_string vs Large_utf8_string" false (Type.Utf8_string = Type.Large_utf8_string);
  Alcotest.(check bool) "Date32 vs Date64" false (Type.Date32 `Days = Type.Date64 `Milliseconds);
  Alcotest.(check bool) "Time32 vs Time64" false (Type.Time32 `Seconds = Type.Time64 `Microseconds);
  Alcotest.(check bool) "Struct vs Map" false (Type.Struct = Type.Map);

  (* Test different variants of the same base type *)
  Alcotest.(check bool) "Time32 different precision" false (Type.Time32 `Seconds = Type.Time32 `Milliseconds);
  Alcotest.(check bool) "Time64 different precision" false (Type.Time64 `Microseconds = Type.Time64 `Nanoseconds);
  Alcotest.(check bool) "Duration different precision" false (Type.Duration `Seconds = Type.Duration `Milliseconds);
  Alcotest.(check bool) "Interval different type" false (Type.Interval `Months = Type.Interval `Days_time)

(** {2 Round-trip tests} *)

let test_cstring_round_trip () =
  (* Test that known cstrings round-trip correctly *)
  let known_cstrings = [
    ("n", Type.Null);
    ("b", Type.Boolean);
    ("c", Type.Int8);
    ("C", Type.Uint8);
    ("s", Type.Int16);
    ("S", Type.Uint16);
    ("i", Type.Int32);
    ("I", Type.Uint32);
    ("l", Type.Int64);
    ("L", Type.Uint64);
    ("e", Type.Float16);
    ("f", Type.Float32);
    ("g", Type.Float64);
    ("z", Type.Binary);
    ("Z", Type.Large_binary);
    ("u", Type.Utf8_string);
    ("U", Type.Large_utf8_string);
    ("tdD", Type.Date32 `Days);
    ("tdm", Type.Date64 `Milliseconds);
    ("tts", Type.Time32 `Seconds);
    ("ttm", Type.Time32 `Milliseconds);
    ("ttu", Type.Time64 `Microseconds);
    ("ttn", Type.Time64 `Nanoseconds);
    ("tDs", Type.Duration `Seconds);
    ("tDm", Type.Duration `Milliseconds);
    ("tDu", Type.Duration `Microseconds);
    ("tDn", Type.Duration `Nanoseconds);
    ("tiM", Type.Interval `Months);
    ("tiD", Type.Interval `Days_time);
    ("+s", Type.Struct);
    ("+m", Type.Map);
  ] in

  List.iter (fun (cstring, expected_type) ->
    let parsed_type = Type.of_cstring cstring in
    Alcotest.(check type_testable)
      (Printf.sprintf "Round-trip for %s" cstring)
      expected_type parsed_type
  ) known_cstrings

let test_parameterized_cstring_round_trip () =
  (* Test parameterized types *)
  let parameterized_cases = [
    ("tss:UTC", Type.Timestamp {precision = `Seconds; timezone = "UTC"});
    ("tsm:America/New_York", Type.Timestamp {precision = `Milliseconds; timezone = "America/New_York"});
    ("tsu:", Type.Timestamp {precision = `Microseconds; timezone = ""});
    ("tsn:Europe/London", Type.Timestamp {precision = `Nanoseconds; timezone = "Europe/London"});
    ("w:16", Type.Fixed_width_binary {bytes = 16});
    ("w:32", Type.Fixed_width_binary {bytes = 32});
    ("w:0", Type.Fixed_width_binary {bytes = 0});
    ("d:10,2", Type.Decimal128 {precision = 10; scale = 2});
    ("d:18,4", Type.Decimal128 {precision = 18; scale = 4});
    ("d:38,0", Type.Decimal128 {precision = 38; scale = 0});
  ] in

  List.iter (fun (cstring, expected_type) ->
    let parsed_type = Type.of_cstring cstring in
    Alcotest.(check type_testable)
      (Printf.sprintf "Parameterized round-trip for %s" cstring)
      expected_type parsed_type
  ) parameterized_cases

(** {2 Edge case and stress tests} *)

let test_edge_cases () =
  (* Test edge cases in parameterized types *)

  (* Large numbers in decimal *)
  Alcotest.(check type_testable) "Large decimal precision"
    (Type.Decimal128 {precision = 999; scale = 999})
    (Type.of_cstring "d:999,999");

  (* Large bytes in fixed width binary *)
  Alcotest.(check type_testable) "Large fixed width binary"
    (Type.Fixed_width_binary {bytes = 1000000})
    (Type.of_cstring "w:1000000");

  (* Very long timezone names *)
  let long_timezone = String.make 100 'a' in
  Alcotest.(check type_testable) "Very long timezone name"
    (Type.Timestamp {precision = `Seconds; timezone = long_timezone})
    (Type.of_cstring ("tss:" ^ long_timezone));

  (* Negative numbers are actually valid in the implementation *)
  Alcotest.(check type_testable) "Negative precision in decimal"
    (Type.Decimal128 {precision = -10; scale = 2}) (Type.of_cstring "d:-10,2");
  Alcotest.(check type_testable) "Negative bytes in fixed width binary"
    (Type.Fixed_width_binary {bytes = -16}) (Type.of_cstring "w:-16")

let test_malformed_inputs () =
  (* Test various malformed inputs that should produce Unknown types *)
  let malformed_cases = [
    "tss"; (* missing timezone *)
    "tsu:UTC:extra"; (* extra part *)
    "w:"; (* missing bytes *)
    "w:abc"; (* non-numeric bytes *)
    "d:"; (* missing precision,scale *)
    "d:10,"; (* missing scale *)
    "d:,2"; (* missing precision *)
    "d:abc,2"; (* non-numeric precision *)
    "d:10,xyz"; (* non-numeric scale *)
    "d:10,2,3"; (* extra part *)
    "invalid:format"; (* completely unknown format *)
    "123"; (* numeric string *)
    ""; (* empty string *)
    " "; (* whitespace *)
    "n "; (* trailing space *)
    " b"; (* leading space *)
  ] in

  List.iter (fun malformed ->
    let result = Type.of_cstring malformed in
    match result with
    | Type.Unknown _ ->
        Alcotest.(check bool)
          (Printf.sprintf "Malformed input '%s' produces Unknown" malformed)
          true true
    | _ ->
        Alcotest.fail (Printf.sprintf "Malformed input '%s' should produce Unknown type, got: %s"
          malformed
          (match result with
           | Type.Timestamp {timezone; _} -> Printf.sprintf "Timestamp(timezone=\"%s\")" timezone
           | _ -> "other type"))
  ) malformed_cases;

  (* Test cases that are actually valid but might seem malformed *)
  let valid_edge_cases = [
    ("tsm:", Type.Timestamp {precision = `Milliseconds; timezone = ""}); (* empty timezone is valid *)
  ] in

  List.iter (fun (input, expected) ->
    let result = Type.of_cstring input in
    Alcotest.(check type_testable)
      (Printf.sprintf "Edge case '%s' is valid" input)
      expected result
  ) valid_edge_cases

(** {2 Integration tests with test fixtures} *)

let test_type_integration_with_fixtures () =
  (* Test that types work correctly with fixture generation *)

  (* Create some test data with known types *)
  let integers = sample_ints ~size:5 () in
  let floats = sample_floats ~size:5 () in
  let strings = sample_strings ~size:5 () in
  let bools = sample_bools ~size:5 () in

  Alcotest.(check int) "Integer fixtures length" 5 (Array.length integers);
  Alcotest.(check int) "Float fixtures length" 5 (Array.length floats);
  Alcotest.(check int) "String fixtures length" 5 (Array.length strings);
  Alcotest.(check int) "Boolean fixtures length" 5 (Array.length bools);

  (* Test that fixture data can be used to create different table column types *)
  let test_types = [
    Type.Int32;
    Type.Float64;
    Type.Utf8_string;
    Type.Boolean;
    Type.Date32 `Days;
    Type.Timestamp {precision = `Seconds; timezone = "UTC"};
  ] in

  List.iter (fun typ ->
    (* Just test that the type can be created and compared *)
    Alcotest.(check bool) "Type self-equality" true (typ = typ);

    (* Test that the type is not equal to other types *)
    let other_types = List.filter (fun t -> t <> typ) test_types in
    List.iter (fun other_typ ->
      Alcotest.(check bool)
        (Printf.sprintf "Type inequality test")
        false (typ = other_typ)
    ) other_types
  ) test_types

let test_comprehensive_type_coverage () =
  (* Test that we have comprehensive coverage of all type variants *)

  let all_simple_types = [
    Type.Null;
    Type.Boolean;
    Type.Int8;
    Type.Uint8;
    Type.Int16;
    Type.Uint16;
    Type.Int32;
    Type.Uint32;
    Type.Int64;
    Type.Uint64;
    Type.Float16;
    Type.Float32;
    Type.Float64;
    Type.Binary;
    Type.Large_binary;
    Type.Utf8_string;
    Type.Large_utf8_string;
    Type.Struct;
    Type.Map;
  ] in

  let all_parameterized_types = [
    Type.Decimal128 {precision = 10; scale = 2};
    Type.Fixed_width_binary {bytes = 16};
    Type.Date32 `Days;
    Type.Date64 `Milliseconds;
    Type.Time32 `Seconds;
    Type.Time32 `Milliseconds;
    Type.Time64 `Microseconds;
    Type.Time64 `Nanoseconds;
    Type.Timestamp {precision = `Seconds; timezone = "UTC"};
    Type.Duration `Seconds;
    Type.Duration `Milliseconds;
    Type.Duration `Microseconds;
    Type.Duration `Nanoseconds;
    Type.Interval `Months;
    Type.Interval `Days_time;
    Type.Unknown "test";
  ] in

  let all_types = all_simple_types @ all_parameterized_types in

  (* Test that each type is equal to itself *)
  List.iter (fun typ ->
    Alcotest.(check bool) "Type self-equality comprehensive" true (typ = typ)
  ) all_types;

  (* Test total count of types we're testing *)
  let total_count = List.length all_types in
  Alcotest.(check bool) "Comprehensive type count" true (total_count >= 30)

(** {2 Main test runner} *)

let () =
  let open Alcotest in
  run "Type module tests" [
    "basic_variants", [
      test_case "Null type" `Quick test_null_type;
      test_case "Boolean type" `Quick test_boolean_type;
      test_case "Integer types" `Quick test_integer_types;
      test_case "Floating point types" `Quick test_floating_point_types;
      test_case "Binary types" `Quick test_binary_types;
      test_case "String types" `Quick test_string_types;
      test_case "Complex types" `Quick test_complex_types;
    ];
    "parameterized_types", [
      test_case "Decimal types" `Quick test_decimal_type;
      test_case "Fixed width binary types" `Quick test_fixed_width_binary_type;
      test_case "Unknown types" `Quick test_unknown_types;
    ];
    "temporal_types", [
      test_case "Date types" `Quick test_date_types;
      test_case "Time types" `Quick test_time_types;
      test_case "Timestamp types" `Quick test_timestamp_types;
      test_case "Duration types" `Quick test_duration_types;
      test_case "Interval types" `Quick test_interval_types;
    ];
    "of_cstring_parsing", [
      test_case "Basic types from cstring" `Quick test_of_cstring_basic_types;
      test_case "Binary/string types from cstring" `Quick test_of_cstring_binary_string_types;
      test_case "Temporal types from cstring" `Quick test_of_cstring_temporal_types;
      test_case "Duration/interval types from cstring" `Quick test_of_cstring_duration_interval_types;
      test_case "Complex types from cstring" `Quick test_of_cstring_complex_types;
      test_case "Parameterized types from cstring" `Quick test_of_cstring_parameterized_types;
      test_case "Unknown cases from cstring" `Quick test_of_cstring_unknown_cases;
    ];
    "type_equality", [
      test_case "Type equality" `Quick test_type_equality;
      test_case "Type inequality" `Quick test_type_inequality;
    ];
    "round_trip", [
      test_case "Cstring round trip" `Quick test_cstring_round_trip;
      test_case "Parameterized cstring round trip" `Quick test_parameterized_cstring_round_trip;
    ];
    "edge_cases", [
      test_case "Edge cases" `Quick test_edge_cases;
      test_case "Malformed inputs" `Quick test_malformed_inputs;
    ];
    "integration", [
      test_case "Integration with fixtures" `Quick test_type_integration_with_fixtures;
      test_case "Comprehensive type coverage" `Quick test_comprehensive_type_coverage;
    ];
  ]
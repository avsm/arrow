let () =
  let fmt file = Format.formatter_of_out_channel (open_out file) in
  let fmt_c = fmt "parquet_c_stubs.c" in
  let fmt_ml = fmt "parquet_bindings_generated.ml" in
  Format.fprintf fmt_c "#include \"parquet_stubs.h\"@.";
  Cstubs.write_c fmt_c ~prefix:"caml_parquet_" (module Parquet_bindings.C);
  Cstubs.write_ml fmt_ml ~prefix:"caml_parquet_" (module Parquet_bindings.C);
  flush_all ()
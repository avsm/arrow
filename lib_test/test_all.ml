(* Main documentation for the restructured tests *)

(*
   This directory contains restructured Arrow tests using the external interface.

   The tests are organized into the following modules:

   - test_table.ml:   Tests for Arrow.Table functionality
   - test_builder.ml: Tests for Arrow.Builder functionality
   - test_column.ml:  Tests for Arrow.Column functionality
   - test_io.ml:      Tests for Arrow.IO functionality
   - test_parquet.ml: Tests for Arrow.Parquet functionality
   - test_valid.ml:   Tests for Arrow.Valid functionality

   To run all tests:
   dune exec lib_test/test_table.exe
   dune exec lib_test/test_builder.exe
   dune exec lib_test/test_column.exe
   dune exec lib_test/test_io.exe
   dune exec lib_test/test_parquet.exe
   dune exec lib_test/test_valid.exe

   Or run all tests at once:
   dune runtest lib_test
*)

let () =
  Printf.printf "=== Arrow Tests Documentation ===\n";
  Printf.printf "The tests in this directory use the external Arrow interface.\n";
  Printf.printf "Run 'dune runtest lib_test' to execute all tests.\n"
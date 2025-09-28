# Arrow Library Tests (Restructured)

This directory contains restructured tests for the Arrow library that use the **external interface** exclusively, avoiding the internal `Wrapper` module.

## Test Organization

The tests are organized by functionality into separate modules:

### Core Modules

- **`test_table.ml`** - Tests for `Arrow.Table` functionality
  - Table creation with various column types
  - Optional/nullable columns
  - Table operations (concatenation, slicing, column manipulation)
  - Time-related column types
  - Schema access

- **`test_builder.ml`** - Tests for `Arrow.Builder` functionality
  - Basic builders (Int, String, Float, Boolean)
  - Numeric builders (Int8, Int16, Int32, Int64, UInt variants)
  - Datetime builders (Date32, Date64, Time32, Time64, Timestamp, Duration)
  - Optional value handling
  - Row-based building patterns

- **`test_column.ml`** - Tests for `Arrow.Column` functionality
  - Reading columns by name and index
  - Various data types (integers, floats, strings, times)
  - Optional column reading
  - Bigarray reading for performance
  - Validity bitset access

### I/O and Formats

- **`test_io.ml`** - Tests for `Arrow.IO` functionality
  - Format auto-detection by file extension
  - Parquet, CSV, and Feather format support
  - Compression options
  - Batch reading for large files
  - Column selection during reading
  - Error handling

- **`test_parquet.ml`** - Tests for `Arrow.Parquet` functionality
  - Parquet-specific types and enums
  - Compression algorithms and levels
  - Schema creation and validation
  - Writer and reader properties
  - Metadata access
  - Complete write/read roundtrips

### Utility Modules

- **`test_valid.ml`** - Tests for `Arrow.Valid` (validity bitsets)
  - Creation and manipulation of validity bitsets
  - Integration with nullable columns
  - Boolean array conversions
  - Edge cases and boundary conditions

## Key Improvements

### 1. External Interface Usage
All tests use the public Arrow API:
- `Arrow.Table.*` instead of `Wrapper.Table.*`
- `Arrow.Builder.*` instead of `Wrapper.Builder.*`
- `Arrow.Column.*` instead of `Wrapper.Column.*`
- `Arrow.IO.*` for I/O operations
- `Arrow.Parquet.*` for Parquet-specific functionality

### 2. Better Organization
- Tests are grouped by functionality rather than mixing concerns
- Each test file focuses on a single module
- Clear separation between core functionality and I/O operations

### 3. Comprehensive Coverage
- All major data types (integers, floats, strings, booleans, dates, times)
- Optional/nullable value handling throughout
- Error conditions and edge cases
- Performance-oriented operations (bigarrays, batch processing)

### 4. Consistent Testing Patterns
- All tests use Alcotest framework consistently
- Helper functions for test data generation
- Proper cleanup of temporary files
- Clear test names and descriptions

## Running Tests

### Individual Test Modules
```bash
dune exec lib_test/test_table.exe     # Table functionality
dune exec lib_test/test_builder.exe   # Builder functionality
dune exec lib_test/test_column.exe    # Column operations
dune exec lib_test/test_io.exe        # I/O operations
dune exec lib_test/test_parquet.exe   # Parquet-specific
dune exec lib_test/test_valid.exe     # Validity bitsets
```

### All Tests
```bash
dune runtest lib_test                 # Run all tests
```

### Documentation
```bash
dune exec lib_test/test_all.exe       # Show test documentation
```

## Test Data Patterns

The tests use several common patterns for test data generation:

- **Sequential data**: Arrays with incrementing values for basic testing
- **Mixed patterns**: Alternating valid/null values for testing optional columns
- **Time series**: Date and time values with realistic temporal relationships
- **Type-specific ranges**: Using appropriate ranges for different integer sizes
- **Compressible data**: Repeating patterns to test compression effectiveness

## Dependencies

- `arrow` - The main Arrow library
- `alcotest` - Testing framework
- `unix` - Required for file operations in I/O tests

## Migration from Old Tests

These tests replace the previous test structure by:

1. **Removing Wrapper dependencies**: All `Wrapper.*` calls replaced with public API
2. **Improving modularity**: Related tests grouped together
3. **Adding comprehensive coverage**: More data types and edge cases
4. **Enhancing readability**: Better names and organization
5. **Future-proofing**: Using stable external interface

The new structure makes the tests more maintainable and ensures they continue to work as the internal implementation evolves.
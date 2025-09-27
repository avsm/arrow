#ifndef PARQUET_STUBS_H
#define PARQUET_STUBS_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Reader functions
void* parquet_open_file(const char* filename, bool use_memory_map,
                        int buffer_size, bool verify_checksums);
void* parquet_metadata(void* reader_ptr);
void* parquet_get_schema(void* reader_ptr);
int64_t parquet_num_rows(void* reader_ptr);
int parquet_num_row_groups(void* reader_ptr);
void* parquet_row_group_metadata(void* reader_ptr, int idx);
void parquet_close_reader(void* reader_ptr);

// Metadata extraction functions
int parquet_file_version(void* reader_ptr);
char* parquet_created_by(void* reader_ptr);
int64_t parquet_row_group_num_rows(void* reader_ptr, int rg_idx);
int64_t parquet_row_group_total_byte_size(void* reader_ptr, int rg_idx);
int parquet_row_group_num_columns(void* reader_ptr, int rg_idx);
const char* parquet_column_path(void* reader_ptr, int rg_idx, int col_idx);
int64_t parquet_column_num_values(void* reader_ptr, int rg_idx, int col_idx);
int64_t parquet_column_compressed_size(void* reader_ptr, int rg_idx, int col_idx);
int64_t parquet_column_uncompressed_size(void* reader_ptr, int rg_idx, int col_idx);
int parquet_column_compression(void* reader_ptr, int rg_idx, int col_idx);
int parquet_column_encoding(void* reader_ptr, int rg_idx, int col_idx);
bool parquet_column_has_statistics(void* reader_ptr, int rg_idx, int col_idx);
int64_t parquet_column_null_count(void* reader_ptr, int rg_idx, int col_idx);
int64_t parquet_column_distinct_count(void* reader_ptr, int rg_idx, int col_idx);

// Schema inspection functions
int parquet_schema_num_columns(void* schema_ptr);
const char* parquet_schema_column_name(void* schema_ptr, int i);
int parquet_schema_column_physical_type(void* schema_ptr, int i);
int parquet_schema_column_logical_type(void* schema_ptr, int i);
int parquet_schema_column_repetition(void* schema_ptr, int i);
int parquet_schema_column_max_definition_level(void* schema_ptr, int i);
int parquet_schema_column_max_repetition_level(void* schema_ptr, int i);
const char* parquet_schema_to_string(void* schema_ptr);
const char* parquet_schema_column_path(void* schema_ptr, int i);
int parquet_schema_column_field_id(void* schema_ptr, int i);
int parquet_schema_column_type_length(void* schema_ptr, int i);
int parquet_schema_column_decimal_scale(void* schema_ptr, int i);
int parquet_schema_column_decimal_precision(void* schema_ptr, int i);
int parquet_schema_column_time_unit(void* schema_ptr, int i);
bool parquet_schema_column_time_is_adjusted_utc(void* schema_ptr, int i);

// Writer functions
void* parquet_create_writer(const char* filename, int compression_int,
                           int compression_level, bool dictionary_enabled,
                           int64_t dictionary_page_size, int64_t data_page_size,
                           int64_t write_batch_size, int64_t max_row_group_length,
                           int writer_version, const char* created_by,
                           bool enable_statistics, void* schema_ptr);
void* parquet_writer_schema(void* writer_ptr);
void* parquet_close_writer(void* writer_ptr);

// Utility functions
const char* parquet_version();

#ifdef __cplusplus
}
#endif

#endif // PARQUET_STUBS_H
#include <memory>
#include <string>
#include <vector>
#include <fstream>
#include <mutex>
#include <parquet/api/reader.h>
#include <parquet/api/writer.h>
#include <parquet/metadata.h>
#include <parquet/properties.h>
#include <parquet/schema.h>
#include <parquet/types.h>
#include <arrow/io/file.h>
#include <parquet/parquet_version.h>
#include "parquet_stubs.h"

// Mutex for protecting static string buffers
static std::mutex static_string_mutex;

extern "C" {

// C-style wrappers for Ctypes compatibility
void* parquet_open_file(const char* filename, bool use_memory_map,
                        int buffer_size, bool verify_checksums) {
    try {
        parquet::ReaderProperties props;
        if (!use_memory_map) {
            props.disable_buffered_stream();
        }
        props.set_buffer_size(buffer_size);

        auto reader = parquet::ParquetFileReader::OpenFile(filename, use_memory_map, props);
        return reader.release();
    } catch (const std::exception& e) {
        // For C interface, return nullptr on error
        return nullptr;
    }
}

void* parquet_metadata(void* reader_ptr) {
    if (!reader_ptr) return nullptr;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    auto metadata = reader->metadata();
    /* FIXME: Returning raw pointer to shared_ptr managed object - lifetime not managed */
    return metadata.get();
}

void* parquet_get_schema(void* reader_ptr) {
    if (!reader_ptr) return nullptr;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    auto schema = reader->metadata()->schema();
    /* FIXME: const_cast removes const - violates const correctness */
    return const_cast<parquet::SchemaDescriptor*>(schema);
}

int64_t parquet_num_rows(void* reader_ptr) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    return reader->metadata()->num_rows();
}

int parquet_num_row_groups(void* reader_ptr) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    return reader->metadata()->num_row_groups();
}

void* parquet_row_group_metadata(void* reader_ptr, int idx) {
    if (!reader_ptr) return nullptr;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (idx < 0 || idx >= reader->metadata()->num_row_groups()) {
        return nullptr;
    }
    return reader_ptr; // Return reader so subsequent calls can use it
}

void parquet_close_reader(void* reader_ptr) {
    if (!reader_ptr) return;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    delete reader;
}

void* parquet_create_writer(const char* filename, int compression_int,
                           int compression_level, bool dictionary_enabled,
                           int64_t dictionary_page_size, int64_t data_page_size,
                           int64_t write_batch_size, int64_t max_row_group_length,
                           int writer_version, const char* created_by,
                           bool enable_statistics, void* schema_ptr) {
    try {
        parquet::WriterProperties::Builder builder;

        // Set compression
        parquet::Compression::type compression;
        switch (compression_int) {
            case 0: compression = parquet::Compression::UNCOMPRESSED; break;
            case 1: compression = parquet::Compression::SNAPPY; break;
            case 2: compression = parquet::Compression::GZIP; break;
            case 3: compression = parquet::Compression::BROTLI; break;
            case 4: compression = parquet::Compression::LZ4; break;
            case 5: compression = parquet::Compression::LZO; break;
            case 6: compression = parquet::Compression::ZSTD; break;
            default: compression = parquet::Compression::SNAPPY;
        }

        builder.compression(compression);

        // Apply compression level if specified (works for ZSTD, GZIP, BROTLI, etc.)
        if (compression_level > 0) {
            builder.compression_level(compression_level);
        }

        if (dictionary_enabled) {
            builder.enable_dictionary();
        } else {
            builder.disable_dictionary();
        }
        builder.data_pagesize(data_page_size);
        builder.write_batch_size(write_batch_size);
        builder.max_row_group_length(max_row_group_length);
        builder.created_by(created_by);

        if (enable_statistics) {
            builder.enable_statistics();
        } else {
            builder.disable_statistics();
        }

        if (writer_version == 1) {
            builder.version(parquet::ParquetVersion::PARQUET_1_0);
        } else {
            builder.version(parquet::ParquetVersion::PARQUET_2_LATEST);
        }

        auto properties = builder.build();

        // Schema conversion from OCaml representation needs to be implemented
        (void)schema_ptr;
        (void)dictionary_page_size; // Suppress unused warning

        // Create FileOutputStream using Arrow I/O
        auto output_result = arrow::io::FileOutputStream::Open(filename);
        if (!output_result.ok()) {
            return nullptr;
        }
        auto output_stream = output_result.ValueOrDie();

        // TODO: For now using a simple schema - will need proper schema conversion
        auto node = parquet::schema::PrimitiveNode::Make("test_column", parquet::Repetition::REQUIRED,
                                                        parquet::Type::INT64, parquet::ConvertedType::NONE);
        auto schema = std::static_pointer_cast<parquet::schema::GroupNode>(
            parquet::schema::GroupNode::Make("schema", parquet::Repetition::REQUIRED, {node}));

        // Create ParquetFileWriter
        auto writer = parquet::ParquetFileWriter::Open(output_stream, schema, properties);
        return writer.release();
    } catch (const std::exception& e) {
        return nullptr;
    }
}

void* parquet_writer_schema(void* writer_ptr) {
    if (!writer_ptr) return nullptr;
    auto writer = static_cast<parquet::ParquetFileWriter*>(writer_ptr);
    auto schema = writer->schema();
    return const_cast<void*>(static_cast<const void*>(schema));
}

void* parquet_close_writer(void* writer_ptr) {
    if (!writer_ptr) return nullptr;
    try {
        auto writer = static_cast<parquet::ParquetFileWriter*>(writer_ptr);
        writer->Close();
        auto metadata = writer->metadata();
        delete writer;
        /* FIXME: Returning raw pointer to metadata after deleting writer - use-after-free risk */
        return metadata.get();
    } catch (const std::exception& e) {
        delete static_cast<parquet::ParquetFileWriter*>(writer_ptr);
        return nullptr;
    }
}

const char* parquet_version() {
    // This one is initialized once and never changes, so mutex only needed for initialization
    static std::once_flag init_flag;
    static std::string version_str;
    std::call_once(init_flag, [](){
        version_str = "parquet-cpp-arrow version " +
                      std::to_string(PARQUET_VERSION_MAJOR) + "." +
                      std::to_string(PARQUET_VERSION_MINOR) + "." +
                      std::to_string(PARQUET_VERSION_PATCH);
    });
    return version_str.c_str();
}

int parquet_file_version(void* reader_ptr) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    return reader->metadata()->version();
}

char* parquet_created_by(void* reader_ptr) {
    if (!reader_ptr) return nullptr;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    const auto& created = reader->metadata()->created_by();
    /* FIXME: const_cast removes const and returns internal string pointer - dangerous */
    return created.empty() ? nullptr : const_cast<char*>(created.c_str());
}

const char* parquet_column_path(void* reader_ptr, int rg_idx, int col_idx) {
    if (!reader_ptr) return "";
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return "";
    auto rg = reader->metadata()->RowGroup(rg_idx);
    if (col_idx < 0 || col_idx >= rg->num_columns()) return "";

    auto col_descr = reader->metadata()->schema()->Column(col_idx);
    std::lock_guard<std::mutex> lock(static_string_mutex);
    static std::string path_str;
    path_str = col_descr->path()->ToDotString();
    return path_str.c_str();
}

const char* parquet_schema_column_name(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return "";
    return schema->Column(i)->name().c_str();
}

const char* parquet_schema_to_string(void* schema_ptr) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema) return "";

    std::lock_guard<std::mutex> lock(static_string_mutex);
    static std::string schema_str;
    schema_str = schema->ToString();
    return schema_str.c_str();
}

const char* parquet_schema_column_path(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return "";

    std::lock_guard<std::mutex> lock(static_string_mutex);
    static std::string path_str;
    path_str = schema->Column(i)->path()->ToDotString();
    return path_str.c_str();
}

// Metadata extraction functions - kept as C-style
int64_t parquet_row_group_num_rows(void* reader_ptr, int rg_idx) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return 0;
    return reader->metadata()->RowGroup(rg_idx)->num_rows();
}

int64_t parquet_row_group_total_byte_size(void* reader_ptr, int rg_idx) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return 0;
    return reader->metadata()->RowGroup(rg_idx)->total_byte_size();
}

int parquet_row_group_num_columns(void* reader_ptr, int rg_idx) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return 0;
    return reader->metadata()->RowGroup(rg_idx)->num_columns();
}

int64_t parquet_column_num_values(void* reader_ptr, int rg_idx, int col_idx) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return 0;
    auto rg = reader->metadata()->RowGroup(rg_idx);
    if (col_idx < 0 || col_idx >= rg->num_columns()) return 0;
    return rg->ColumnChunk(col_idx)->num_values();
}

int64_t parquet_column_compressed_size(void* reader_ptr, int rg_idx, int col_idx) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return 0;
    auto rg = reader->metadata()->RowGroup(rg_idx);
    if (col_idx < 0 || col_idx >= rg->num_columns()) return 0;
    return rg->ColumnChunk(col_idx)->total_compressed_size();
}

int64_t parquet_column_uncompressed_size(void* reader_ptr, int rg_idx, int col_idx) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return 0;
    auto rg = reader->metadata()->RowGroup(rg_idx);
    if (col_idx < 0 || col_idx >= rg->num_columns()) return 0;
    return rg->ColumnChunk(col_idx)->total_uncompressed_size();
}

int parquet_column_compression(void* reader_ptr, int rg_idx, int col_idx) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return 0;
    auto rg = reader->metadata()->RowGroup(rg_idx);
    if (col_idx < 0 || col_idx >= rg->num_columns()) return 0;
    return static_cast<int>(rg->ColumnChunk(col_idx)->compression());
}

int parquet_column_encoding(void* reader_ptr, int rg_idx, int col_idx) {
    if (!reader_ptr) return 0;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return 0;
    auto rg = reader->metadata()->RowGroup(rg_idx);
    if (col_idx < 0 || col_idx >= rg->num_columns()) return 0;

    // Get the first encoding (there may be multiple)
    const auto& encodings = rg->ColumnChunk(col_idx)->encodings();
    if (encodings.empty()) return 0;
    return static_cast<int>(encodings[0]);
}

bool parquet_column_has_statistics(void* reader_ptr, int rg_idx, int col_idx) {
    if (!reader_ptr) return false;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return false;
    auto rg = reader->metadata()->RowGroup(rg_idx);
    if (col_idx < 0 || col_idx >= rg->num_columns()) return false;
    return rg->ColumnChunk(col_idx)->is_stats_set();
}

int64_t parquet_column_null_count(void* reader_ptr, int rg_idx, int col_idx) {
    if (!reader_ptr) return -1;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return -1;
    auto rg = reader->metadata()->RowGroup(rg_idx);
    if (col_idx < 0 || col_idx >= rg->num_columns()) return -1;

    auto chunk = rg->ColumnChunk(col_idx);
    if (!chunk->is_stats_set()) return -1;

    auto stats = chunk->statistics();
    if (!stats) return -1;
    return stats->null_count();
}

int64_t parquet_column_distinct_count(void* reader_ptr, int rg_idx, int col_idx) {
    if (!reader_ptr) return -1;
    auto reader = static_cast<parquet::ParquetFileReader*>(reader_ptr);
    if (rg_idx < 0 || rg_idx >= reader->metadata()->num_row_groups()) return -1;
    auto rg = reader->metadata()->RowGroup(rg_idx);
    if (col_idx < 0 || col_idx >= rg->num_columns()) return -1;

    auto chunk = rg->ColumnChunk(col_idx);
    if (!chunk->is_stats_set()) return -1;

    auto stats = chunk->statistics();
    if (!stats) return -1;
    return stats->distinct_count();
}

// Schema inspection functions
int parquet_schema_num_columns(void* schema_ptr) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema) return 0;
    return schema->num_columns();
}

int parquet_schema_column_physical_type(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return -1;
    return static_cast<int>(schema->Column(i)->physical_type());
}

int parquet_schema_column_logical_type(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return -1;
    const auto logical_type = schema->Column(i)->logical_type();
    if (!logical_type) return -1;

    // Map logical types to integers for OCaml
    // Using LogicalType::Type enum values
    if (logical_type->is_string()) return 0;  // STRING
    if (logical_type->is_map()) return 1;     // MAP
    if (logical_type->is_list()) return 2;    // LIST
    if (logical_type->is_enum()) return 3;    // ENUM
    if (logical_type->is_decimal()) return 4; // DECIMAL
    if (logical_type->is_date()) return 5;    // DATE
    if (logical_type->is_time()) return 6;    // TIME
    if (logical_type->is_timestamp()) return 7; // TIMESTAMP
    if (logical_type->is_int()) return 8;     // INT
    if (logical_type->is_JSON()) return 9;    // JSON
    if (logical_type->is_BSON()) return 10;   // BSON
    if (logical_type->is_UUID()) return 11;   // UUID

    return -1;  // Unknown or no logical type
}

int parquet_schema_column_repetition(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return -1;

    // Return repetition type: 0=REQUIRED, 1=OPTIONAL, 2=REPEATED
    return static_cast<int>(schema->Column(i)->schema_node()->repetition());
}

int parquet_schema_column_max_definition_level(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return 0;
    return schema->Column(i)->max_definition_level();
}

int parquet_schema_column_max_repetition_level(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return 0;
    return schema->Column(i)->max_repetition_level();
}

int parquet_schema_column_field_id(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return -1;
    return schema->Column(i)->schema_node()->field_id();
}

int parquet_schema_column_type_length(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return 0;

    auto col = schema->Column(i);
    if (col->physical_type() == parquet::Type::FIXED_LEN_BYTE_ARRAY) {
        return col->type_length();
    }
    return 0;
}

int parquet_schema_column_decimal_scale(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return 0;

    const auto logical_type = schema->Column(i)->logical_type();
    if (logical_type && logical_type->is_decimal()) {
        auto decimal = std::dynamic_pointer_cast<const parquet::DecimalLogicalType>(logical_type);
        return decimal->scale();
    }
    return 0;
}

int parquet_schema_column_decimal_precision(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return 0;

    const auto logical_type = schema->Column(i)->logical_type();
    if (logical_type && logical_type->is_decimal()) {
        auto decimal = std::dynamic_pointer_cast<const parquet::DecimalLogicalType>(logical_type);
        return decimal->precision();
    }
    return 0;
}

int parquet_schema_column_time_unit(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return -1;

    const auto logical_type = schema->Column(i)->logical_type();
    if (!logical_type) return -1;

    // 0=MILLIS, 1=MICROS, 2=NANOS
    if (logical_type->is_timestamp()) {
        auto ts = std::dynamic_pointer_cast<const parquet::TimestampLogicalType>(logical_type);
        if (ts->time_unit() == parquet::LogicalType::TimeUnit::MILLIS) return 0;
        if (ts->time_unit() == parquet::LogicalType::TimeUnit::MICROS) return 1;
        if (ts->time_unit() == parquet::LogicalType::TimeUnit::NANOS) return 2;
    }
    if (logical_type->is_time()) {
        auto time = std::dynamic_pointer_cast<const parquet::TimeLogicalType>(logical_type);
        if (time->time_unit() == parquet::LogicalType::TimeUnit::MILLIS) return 0;
        if (time->time_unit() == parquet::LogicalType::TimeUnit::MICROS) return 1;
        if (time->time_unit() == parquet::LogicalType::TimeUnit::NANOS) return 2;
    }
    return -1;
}

bool parquet_schema_column_time_is_adjusted_utc(void* schema_ptr, int i) {
    auto schema = static_cast<const parquet::SchemaDescriptor*>(schema_ptr);
    if (!schema || i < 0 || i >= schema->num_columns()) return false;

    const auto logical_type = schema->Column(i)->logical_type();
    if (!logical_type) return false;

    if (logical_type->is_timestamp()) {
        auto ts = std::dynamic_pointer_cast<const parquet::TimestampLogicalType>(logical_type);
        return ts->is_adjusted_to_utc();
    }
    if (logical_type->is_time()) {
        auto time = std::dynamic_pointer_cast<const parquet::TimeLogicalType>(logical_type);
        return time->is_adjusted_to_utc();
    }
    return false;
}

} // extern "C"
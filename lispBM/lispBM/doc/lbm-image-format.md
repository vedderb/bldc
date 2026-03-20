# LispBM Image Format Specification

This document describes the binary format used by LispBM for serializing and deserializing runtime state to persistent storage (flash memory, files, etc.).

## Overview

The LispBM image format is designed for embedded systems where:
- The image resides in write-once memory (like flash)
- Images can be built incrementally by appending new fields
- The same image can be loaded at different memory addresses
- Both 32-bit and 64-bit architectures are supported
- Efficient sharing recovery is needed to handle complex data structures

## Image Structure

Images are stored in **little-endian** format and written **top-down** (from high addresses to low addresses). The image consists of multiple fields, each with a specific tag and format.

### Memory Layout

```
High Address (End of Image)
┌─────────────────────────────┐
│     IMAGE_INITIALIZED       │ ← Always at image_size - 1
├─────────────────────────────┤
│      VERSION_ENTRY          │ ← Optional version string
├─────────────────────────────┤
│     BINDING_CONST           │ ← Constant bindings
│     BINDING_FLAT            │ ← Flattened bindings  
├─────────────────────────────┤
│     SYMBOL_ENTRY            │ ← Symbol table entries
│   SYMBOL_LINK_ENTRY         │
├─────────────────────────────┤
│    EXTENSION_TABLE          │ ← C function extensions
├─────────────────────────────┤
│    SHARING_TABLE            │ ← Shared structure info
├─────────────────────────────┤
│   CONSTANT_HEAP_IX          │ ← Heap write pointer
└─────────────────────────────┘
Low Address (Start of Image) ← Constant heap data
```

## Field Types

### IMAGE_INITIALIZED (0xBEEF2001 / 0xBEEF4001)

**32-bit**: `0xBEEF2001`  
**64-bit**: `0xBEEF4001`

Single 32-bit word indicating a valid image exists. Always located at `image_size - 1`.

### VERSION_ENTRY (0x09)

```
┌─────────────────┐
│ 0x09            │ ← VERSION_ENTRY tag
├─────────────────┤
│ size (words)    │ ← Size of string in 32-bit words
├─────────────────┤
│ version string  │ ← Null-terminated string data
│ (padded to 32-bit boundary)
└─────────────────┘
```

Optional version string for the image.

### CONSTANT_HEAP_IX (0x02)

```
┌─────────────────┐
│ 0x02            │ ← CONSTANT_HEAP_IX tag  
├─────────────────┤
│ next_index      │ ← Next free position in const heap
└─────────────────┘
```

Tracks the write position in the constant heap for incremental builds.

### BINDING_CONST (0x03)

```
32-bit:                64-bit:
┌─────────────────┐    ┌─────────────────┐
│ 0x03            │    │ 0x03            │
├─────────────────┤    ├─────────────────┤
│ key             │    │ key (high)      │
├─────────────────┤    ├─────────────────┤
│ value           │    │ key (low)       │
└─────────────────┘    ├─────────────────┤
                       │ value (high)    │
                       ├─────────────────┤
                       │ value (low)     │
                       └─────────────────┘
```

Stores a binding where the value is a constant (lives in constant heap).

### BINDING_FLAT (0x04)

```
32-bit:                64-bit:
┌─────────────────┐    ┌─────────────────┐
│ 0x04            │    │ 0x04            │
├─────────────────┤    ├─────────────────┤
│ size (words)    │    │ size (words)    │
├─────────────────┤    ├─────────────────┤
│ key             │    │ key (high)      │
├─────────────────┤    ├─────────────────┤
│ flat_value_data │    │ key (low)       │ 
│ ...             │    ├─────────────────┤
└─────────────────┘    │ flat_value_data │
                       │ ...             │
                       └─────────────────┘
```

Stores a binding where the value is serialized as a flat value (see Flat Value Format).

### SYMBOL_ENTRY (0x06)

```
32-bit:                64-bit:
┌─────────────────┐    ┌─────────────────┐
│ 0x06            │    │ 0x06            │
├─────────────────┤    ├─────────────────┤
│ next_ptr        │    │ next_ptr (high) │
├─────────────────┤    ├─────────────────┤
│ id              │    │ next_ptr (low)  │
├─────────────────┤    ├─────────────────┤
│ name_ptr        │    │ id (high)       │
└─────────────────┘    ├─────────────────┤
                       │ id (low)        │
                       ├─────────────────┤
                       │ name_ptr (high) │
                       ├─────────────────┤
                       │ name_ptr (low)  │
                       └─────────────────┘
```

Symbol table entry linking symbol ID to name string.

### SYMBOL_LINK_ENTRY (0x07)

```
32-bit:                64-bit:
┌─────────────────┐    ┌─────────────────┐
│ 0x07            │    │ 0x07            │
├─────────────────┤    ├─────────────────┤
│ link_ptr        │    │ link_ptr (high) │
├─────────────────┤    ├─────────────────┤
│ next_ptr        │    │ link_ptr (low)  │
├─────────────────┤    ├─────────────────┤
│ id              │    │ next_ptr (high) │
├─────────────────┤    ├─────────────────┤
│ name_ptr        │    │ next_ptr (low)  │
└─────────────────┘    ├─────────────────┤
                       │ id (high)       │
                       ├─────────────────┤
                       │ id (low)        │
                       ├─────────────────┤
                       │ name_ptr (high) │
                       ├─────────────────┤
                       │ name_ptr (low)  │
                       └─────────────────┘
```

Symbol entry with a link pointer where the symbol ID is written during image boot.

### EXTENSION_TABLE (0x08)

```
┌─────────────────┐
│ 0x08            │ ← EXTENSION_TABLE tag
├─────────────────┤
│ num_extensions  │ ← Number of extension entries
├─────────────────┤
│ name_ptr        │ ← Pointer to extension name
├─────────────────┤
│ fptr            │ ← Function pointer
├─────────────────┤
│ ...             │ ← Repeat for each extension
└─────────────────┘
```

Table of C function extensions. Each entry contains a name pointer and function pointer.

### SHARING_TABLE (0x10)

```
┌─────────────────┐
│ 0x10            │ ← SHARING_TABLE tag
├─────────────────┤
│ num_entries     │ ← Number of shared addresses
├─────────────────┤
│ address         │ ← Shared structure address
├─────────────────┤
│ sized_flag      │ ← Boolean: has been sized
├─────────────────┤
│ flattened_flag  │ ← Boolean: has been flattened
├─────────────────┤
│ ...             │ ← Repeat for each shared address
└─────────────────┘
```

Tracks addresses of shared data structures to enable proper reconstruction during unflatten operations.

## Flat Value Format

Flat values are serialized using **big-endian** byte order and contain type tags followed by data:

### Basic Types

| Tag | Name | Format | Description |
|-----|------|--------|-------------|
| 0x01 | S_CONS | `[0x01]` | Cons cell (car/cdr follow) |
| 0x02 | S_SYM_VALUE | `[0x02][sym_id]` | Symbol by ID |
| 0x04 | S_BYTE_VALUE | `[0x04][byte]` | Single byte |
| 0x05 | S_I28_VALUE | `[0x05][4 bytes]` | 28-bit signed integer (32-bit) |
| 0x06 | S_U28_VALUE | `[0x06][4 bytes]` | 28-bit unsigned integer (32-bit) |
| 0x07 | S_I32_VALUE | `[0x07][4 bytes]` | 32-bit signed integer |
| 0x08 | S_U32_VALUE | `[0x08][4 bytes]` | 32-bit unsigned integer |
| 0x09 | S_FLOAT_VALUE | `[0x09][4 bytes]` | 32-bit IEEE float |
| 0x0A | S_I64_VALUE | `[0x0A][8 bytes]` | 64-bit signed integer |
| 0x0B | S_U64_VALUE | `[0x0B][8 bytes]` | 64-bit unsigned integer |
| 0x0C | S_DOUBLE_VALUE | `[0x0C][8 bytes]` | 64-bit IEEE double |
| 0x0D | S_LBM_ARRAY | `[0x0D][size][data...]` | Byte array |
| 0x0E | S_I56_VALUE | `[0x0E][8 bytes]` | 56-bit signed integer (64-bit) |
| 0x0F | S_U56_VALUE | `[0x0F][8 bytes]` | 56-bit unsigned integer (64-bit) |
| 0x10 | S_CONSTANT_REF | `[0x10][address]` | Reference to constant heap |
| 0x1F | S_LBM_LISP_ARRAY | `[0x1F][size][data...]` | Lisp array |

### Sharing Support

| Tag | Name | Format | Description |
|-----|------|--------|-------------|
| 0x20 | S_SHARED | `[0x20][address][data...]` | First occurrence of shared data |
| 0x21 | S_REF | `[0x21][address]` | Reference to shared data |

### Array Formats

**S_LBM_ARRAY**: `[0x0D][size:4 bytes][data:size bytes]`
- Contains raw byte data of specified size

**S_LBM_LISP_ARRAY**: `[0x1F][size:4 bytes][elements...]`
- Contains `size` Lisp values (each recursively flattened)

## Usage Patterns

### Building an Image

1. Initialize image with `lbm_image_init()`
2. Create image header with `lbm_image_create()`
3. Save various components:
   - `lbm_image_save_global_env()` - Variable bindings
   - `lbm_image_save_extensions()` - C extensions
   - `lbm_image_save_constant_heap_ix()` - Heap state

### Loading an Image

1. Call `lbm_image_exists()` to verify valid image
2. Call `lbm_image_boot()` to restore state:
   - Restores global environment bindings
   - Rebuilds symbol table
   - Restores extension table
   - Sets up constant heap

### Sharing Recovery

The sharing mechanism prevents duplication of shared data structures:

1. **Detection Phase**: Traverse all values to identify shared addresses
2. **Flattening Phase**: 
   - First occurrence: Write `S_SHARED` tag with full data
   - Subsequent occurrences: Write `S_REF` tag with address
3. **Unflattening Phase**:
   - `S_SHARED`: Create new value and record its address
   - `S_REF`: Look up previously created value by address

## Architecture Differences

### 32-bit vs 64-bit

- **Initialization marker**: 0xBEEF2001 (32-bit) vs 0xBEEF4001 (64-bit)
- **Pointer sizes**: All pointers/addresses use native word size
- **Integer types**: I28/U28 (32-bit) vs I56/U56 (64-bit) for native integers
- **Field layouts**: Multi-word fields split differently between architectures

### Endianness

- **Image fields**: Little-endian (native to target platforms)
- **Flat values**: Big-endian for cross-platform compatibility

## Implementation Notes

- Images support **incremental building** by allowing duplicate field types
- Later fields override earlier ones of the same type
- The format is designed for **write-once memory** (flash)
- All string data is null-terminated and padded to 32-bit boundaries
- Sharing tables are optional but recommended for complex data structures
# Simple Data Cache for Performance

Implements a simple in-memory cache for datasets to improve performance

## Public fields

- `cache_store`:

  Internal cache storage

- `max_size`:

  Maximum number of datasets to cache

- `max_memory`:

  Maximum memory usage (in bytes) Initialize the cache

## Methods

### Public methods

- [`DataCache$new()`](#method-DataCache-initialize)

- [`DataCache$get_cache_key()`](#method-DataCache-get_cache_key)

- [`DataCache$has()`](#method-DataCache-has)

- [`DataCache$get()`](#method-DataCache-get)

- [`DataCache$set()`](#method-DataCache-set)

- [`DataCache$get_cache_memory()`](#method-DataCache-get_cache_memory)

- [`DataCache$cleanup_cache()`](#method-DataCache-cleanup_cache)

- [`DataCache$cleanup_by_memory()`](#method-DataCache-cleanup_by_memory)

- [`DataCache$clear()`](#method-DataCache-clear)

- [`DataCache$get_stats()`](#method-DataCache-get_stats)

- [`DataCache$clone()`](#method-DataCache-clone)

------------------------------------------------------------------------

### `DataCache$new()`

#### Usage

    DataCache$new(max_size = 10, max_memory = 500 * 1024^2)

#### Arguments

- `max_size`:

  Maximum number of cached items

- `max_memory`:

  Maximum memory usage in bytes Get cache key for a dataset

------------------------------------------------------------------------

### `DataCache$get_cache_key()`

#### Usage

    DataCache$get_cache_key(path, nrows = NULL, cols = NULL)

#### Arguments

- `path`:

  File path

- `nrows`:

  Number of rows parameter

- `cols`:

  Columns parameter Check if data is cached

------------------------------------------------------------------------

### `DataCache$has()`

#### Usage

    DataCache$has(key)

#### Arguments

- `key`:

  Cache key Get cached data

------------------------------------------------------------------------

### `DataCache$get()`

#### Usage

    DataCache$get(key)

#### Arguments

- `key`:

  Cache key Cache data

------------------------------------------------------------------------

### `DataCache$set()`

#### Usage

    DataCache$set(key, data)

#### Arguments

- `key`:

  Cache key

- `data`:

  Data to cache

------------------------------------------------------------------------

### `DataCache$get_cache_memory()`

Get current cache memory usage in bytes

#### Usage

    DataCache$get_cache_memory()

#### Returns

Numeric total size of cached items in bytes

------------------------------------------------------------------------

### `DataCache$cleanup_cache()`

Evict the least recently accessed item when the cache reaches its size
limit

#### Usage

    DataCache$cleanup_cache()

#### Returns

Invisibly \`NULL\`, called for its side effect

------------------------------------------------------------------------

### `DataCache$cleanup_by_memory()`

Evict least recently accessed items until the cache fits within its
memory budget

#### Usage

    DataCache$cleanup_by_memory()

#### Returns

Invisibly \`NULL\`, called for its side effect

------------------------------------------------------------------------

### `DataCache$clear()`

Remove all cached data and trigger garbage collection

#### Usage

    DataCache$clear()

#### Returns

Invisibly \`NULL\`, called for its side effect

------------------------------------------------------------------------

### `DataCache$get_stats()`

Summarise cache usage: item count and memory figures

#### Usage

    DataCache$get_stats()

#### Returns

A list with item count and current/maximum memory and size

------------------------------------------------------------------------

### `DataCache$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataCache$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

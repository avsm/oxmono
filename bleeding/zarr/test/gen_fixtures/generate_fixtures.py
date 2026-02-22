#!/usr/bin/env python3
"""Generate Zarr v3 test fixtures for OCaml interoperability testing."""

import os
import shutil
import numpy as np
import zarr
from pathlib import Path

FIXTURES_DIR = Path(__file__).parent.parent / "fixtures" / "python"


def clean_fixtures():
    """Remove existing fixtures directory."""
    if FIXTURES_DIR.exists():
        shutil.rmtree(FIXTURES_DIR)
    FIXTURES_DIR.mkdir(parents=True, exist_ok=True)


def create_simple_float64():
    """Simple 2D float64 array with bytes codec."""
    print("Creating simple_float64...")
    store = zarr.storage.LocalStore(FIXTURES_DIR / "simple_float64")
    arr = zarr.create(
        store=store,
        shape=(100, 100),
        chunks=(10, 10),
        dtype='float64',
        fill_value=np.nan,
        codecs=[
            zarr.codecs.BytesCodec(endian='little'),
        ],
    )
    arr[:] = np.arange(10000).reshape(100, 100).astype('float64')
    print(f"  Created array with shape {arr.shape}, dtype {arr.dtype}")


def create_simple_int32():
    """Simple 2D int32 array."""
    print("Creating simple_int32...")
    store = zarr.storage.LocalStore(FIXTURES_DIR / "simple_int32")
    arr = zarr.create(
        store=store,
        shape=(50, 50),
        chunks=(10, 10),
        dtype='int32',
        fill_value=0,
        codecs=[
            zarr.codecs.BytesCodec(endian='little'),
        ],
    )
    arr[:] = np.arange(2500).reshape(50, 50).astype('int32')
    print(f"  Created array with shape {arr.shape}, dtype {arr.dtype}")


def create_gzip_compressed():
    """Float64 array with gzip compression."""
    print("Creating gzip_compressed...")
    store = zarr.storage.LocalStore(FIXTURES_DIR / "gzip_compressed")
    arr = zarr.create(
        store=store,
        shape=(100, 100),
        chunks=(20, 20),
        dtype='float64',
        fill_value=0.0,
        codecs=[
            zarr.codecs.BytesCodec(endian='little'),
            zarr.codecs.GzipCodec(level=5),
        ],
    )
    arr[:] = np.arange(10000).reshape(100, 100).astype('float64')
    print(f"  Created array with shape {arr.shape}, dtype {arr.dtype}")


def create_sharded_int32():
    """Sharded array with inner chunks."""
    print("Creating sharded_int32...")
    store = zarr.storage.LocalStore(FIXTURES_DIR / "sharded_int32")
    arr = zarr.create(
        store=store,
        shape=(256, 256),
        chunks=(64, 64),  # Shard shape
        dtype='int32',
        fill_value=0,
        codecs=[
            zarr.codecs.ShardingCodec(
                chunk_shape=(16, 16),  # Inner chunk shape
                codecs=[zarr.codecs.BytesCodec(endian='little')],
                index_codecs=[
                    zarr.codecs.BytesCodec(endian='little'),
                    zarr.codecs.Crc32cCodec(),
                ],
            )
        ],
    )
    arr[:] = np.arange(65536, dtype='int32').reshape(256, 256)
    print(f"  Created array with shape {arr.shape}, dtype {arr.dtype}")


def create_multidim_3d():
    """3D array."""
    print("Creating multidim_3d...")
    store = zarr.storage.LocalStore(FIXTURES_DIR / "multidim_3d")
    arr = zarr.create(
        store=store,
        shape=(32, 32, 32),
        chunks=(8, 8, 8),
        dtype='float32',
        fill_value=0.0,
        codecs=[
            zarr.codecs.BytesCodec(endian='little'),
        ],
    )
    arr[:] = np.arange(32768).reshape(32, 32, 32).astype('float32')
    print(f"  Created array with shape {arr.shape}, dtype {arr.dtype}")


def create_sparse_with_fill():
    """Sparse array with NaN fill value - only some chunks written."""
    print("Creating sparse_with_fill...")
    store = zarr.storage.LocalStore(FIXTURES_DIR / "sparse_with_fill")
    arr = zarr.create(
        store=store,
        shape=(100,),
        chunks=(10,),
        dtype='float64',
        fill_value=np.nan,
        codecs=[
            zarr.codecs.BytesCodec(endian='little'),
        ],
    )
    # Only write to first chunk
    arr[:10] = np.arange(10).astype('float64')
    print(f"  Created array with shape {arr.shape}, dtype {arr.dtype}")


def create_with_attributes():
    """Array with attributes."""
    print("Creating with_attributes...")
    store = zarr.storage.LocalStore(FIXTURES_DIR / "with_attributes")
    arr = zarr.create(
        store=store,
        shape=(20, 20),
        chunks=(10, 10),
        dtype='int32',
        fill_value=0,
        codecs=[
            zarr.codecs.BytesCodec(endian='little'),
        ],
    )
    arr[:] = np.arange(400).reshape(20, 20).astype('int32')
    arr.attrs['description'] = 'Test array with attributes'
    arr.attrs['units'] = 'meters'
    arr.attrs['scale_factor'] = 1.5
    print(f"  Created array with attributes: {dict(arr.attrs)}")


def create_various_dtypes():
    """Arrays with various data types."""
    dtypes = [
        ('dtype_bool', 'bool', False),
        ('dtype_int8', 'int8', 0),
        ('dtype_int16', 'int16', 0),
        ('dtype_int32', 'int32', 0),
        ('dtype_int64', 'int64', 0),
        ('dtype_uint8', 'uint8', 0),
        ('dtype_uint16', 'uint16', 0),
        ('dtype_uint32', 'uint32', 0),
        ('dtype_uint64', 'uint64', 0),
        ('dtype_float32', 'float32', 0.0),
        ('dtype_float64', 'float64', 0.0),
    ]

    for name, dtype, fill_value in dtypes:
        print(f"Creating {name}...")
        store = zarr.storage.LocalStore(FIXTURES_DIR / name)
        arr = zarr.create(
            store=store,
            shape=(10,),
            chunks=(10,),
            dtype=dtype,
            fill_value=fill_value,
            codecs=[
                zarr.codecs.BytesCodec(endian='little'),
            ],
        )
        if dtype == 'bool':
            arr[:] = np.array([True, False] * 5)
        else:
            arr[:] = np.arange(10).astype(dtype)


def main():
    print("Generating Zarr v3 test fixtures...")
    print(f"Output directory: {FIXTURES_DIR}")
    print()

    clean_fixtures()

    create_simple_float64()
    create_simple_int32()
    create_gzip_compressed()
    create_sharded_int32()
    create_multidim_3d()
    create_sparse_with_fill()
    create_with_attributes()
    create_various_dtypes()

    print()
    print("Done! Fixtures created successfully.")
    print(f"Total fixtures: {len(list(FIXTURES_DIR.iterdir()))}")


if __name__ == '__main__':
    main()

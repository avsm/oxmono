(** Main test runner for Zarr tests *)

let () =
  (* Convert QCheck tests to Alcotest format *)
  let qcheck_suite = List.map QCheck_alcotest.to_alcotest Test_qcheck.qcheck_tests in

  Alcotest.run "zarr" [
    ("data_type", Test_data_type.tests);
    ("fill_value", Test_fill_value.tests);
    ("metadata", Test_metadata.tests);
    ("ndarray", Test_ndarray.tests);
    ("chunk_grid", Test_chunk_grid.tests);
    ("chunk_key", Test_chunk_key.tests);
    ("codecs", Test_codecs.tests);
    ("store_memory", Test_store_memory.tests);
    ("store_filesystem", Test_store_filesystem.tests);
    ("array", Test_array.tests);
    ("group", Test_group.tests);
    ("sharding", Test_sharding.tests);
    ("integration", Test_integration.tests);
    ("spec_compliance", Test_spec_compliance.tests);
    ("python_interop", Test_python_interop.tests);
    ("qcheck", qcheck_suite);
  ]

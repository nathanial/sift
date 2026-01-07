import Lake
open Lake DSL

package sift where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]
  version := v!"0.0.1"

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.3"

@[default_target]
lean_lib Sift where
  roots := #[`Sift]

lean_lib SiftTests where
  globs := #[.submodules `SiftTests]

@[test_driver]
lean_exe sift_tests where
  root := `SiftTests.Main

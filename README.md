# ocaml-index-stats

Print the number of occurrences for items in the signatures of every modules in
the project.

## Installation

The tool can be installed in an Opam switch with:

```
opam pin git+https://github.com/Julow/ocaml-index-stats
```

## Usage

This uses Merlin's indexes so make sure to update them with:

```
dune build @ocaml-index
```

Then run the tool like this:

```
ocaml-index-stats
```

You can specify which modules to look at by passing paths to the command. For
example, in this project:

```
$ ocaml-index-stats lib/
module Ocaml_shape_utils__Compat (at lib/ocaml_shape_utils/compat/compat.ml)
  value  tpat_alias_ident 2 occurrences in 2 modules:
      lib/ocaml_shape_utils/compat/compat.pre54.ml,
      lib/ocaml_shape_utils/ocaml_shape_utils.ml
module Ocaml_shape_utils__ (at lib/ocaml_shape_utils/ocaml_shape_utils__.ml-gen)
  module Compat 0 occurrences in 0 modules
module Ocaml_shape_utils (at lib/ocaml_shape_utils/ocaml_shape_utils.ml)
  module Decl             0 occurrences in 0 modules
    type   t                   0 occurrences in 0 modules
    value  decl_kind_to_string 0 occurrences in 0 modules
    value  pp                  0 occurrences in 0 modules
  module Shap             1 occurrences in 1 modules: bin/stats.ml
    type   t                     0 occurrences in 0 modules
    value  reduce                1 occurrences in 1 modules: bin/stats.ml
    value  value                 1 occurrences in 1 modules: bin/stats.ml
    value  type_                 1 occurrences in 1 modules: bin/stats.ml
    value  extension_constructor 1 occurrences in 1 modules: bin/stats.ml
    value  class_                1 occurrences in 1 modules: bin/stats.ml
    value  class_type            1 occurrences in 1 modules: bin/stats.ml
    value  module_               1 occurrences in 1 modules: bin/stats.ml
    value  module_type           1 occurrences in 1 modules: bin/stats.ml
    value  pp                    0 occurrences in 0 modules
  type   cmt              2 occurrences in 2 modules:
      bin/stats.ml, lib/ocaml_index_utils/ocaml_index_utils.mli
  value  cmts_of_packages 0 occurrences in 0 modules
  value  cmt_of_path      1 occurrences in 1 modules: bin/main.ml
  value  pp               0 occurrences in 0 modules
module Ocaml_index_utils__Compat (at lib/ocaml_index_utils/compat/compat.ml)
  value  merlin_lid        3 occurrences in 2 modules:
      lib/ocaml_index_utils/compat/compat.pre54.ml,
      lib/ocaml_index_utils/ocaml_index_utils.ml
  value  sub_locs_of_ident 1 occurrences in 1 modules:
      lib/ocaml_index_utils/compat/compat.pre54.ml
module Ocaml_index_utils__ (at lib/ocaml_index_utils/ocaml_index_utils__.ml-gen)
  module Compat 0 occurrences in 0 modules
module Ocaml_index_utils (at lib/ocaml_index_utils/ocaml_index_utils.ml)
  type   t                   1 occurrences in 1 modules: bin/stats.ml
  value  scan_dune_build_dir 1 occurrences in 1 modules: bin/main.ml
  value  lookup_occurrences  1 occurrences in 1 modules: bin/stats.ml
  type   occurrences         0 occurrences in 0 modules
  value  occurrences         0 occurrences in 0 modules
module Fs_utils (at lib/fs_utils/fs_utils.ml)
  value  list_dir      1 occurrences in 1 modules:
      lib/ocaml_shape_utils/ocaml_shape_utils.ml
  value  _scan_dir     0 occurrences in 0 modules
  value  scan_dir      2 occurrences in 2 modules:
      bin/main.ml, lib/ocaml_index_utils/ocaml_index_utils.ml
  value  find_ml_files 0 occurrences in 0 modules
```

(Don't forget the `/` at the end of directory paths as this is based on `fpath`).

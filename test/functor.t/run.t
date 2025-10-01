  $ dune build @ocaml-index
  $ ocaml-index-stats
  module Functor__Map (at functor/map.ml)
    modtype S    no occurrences found
      type   t     no occurrences found
      type   key   no occurrences found
      value  empty no occurrences found
      value  add   no occurrences found
    module Make 1 occurrences in 1 modules: main.ml
      type   t     no occurrences found
      type   key   no occurrences found
      value  empty no occurrences found
      value  add   no occurrences found
  module Functor__Hetero_map (at functor/hetero_map.ml)
    modtype S    no occurrences found
      type   t     no occurrences found
      type   key   no occurrences found
      type   data  no occurrences found
      value  empty no occurrences found
      value  add   no occurrences found
    modtype K    no occurrences found
      type   key  no occurrences found
      type   data no occurrences found
    module Make no occurrences found
      type   t     no occurrences found
      type   key   no occurrences found
      type   data  no occurrences found
      value  empty no occurrences found
      value  add   no occurrences found
  module Functor (at functor/functor.ml-gen)
    module Hetero_map no definition found
    module Map        no definition found
  module Main (at main.ml)
    module M       no definition found
      type   t     4 occurrences in 1 modules: functor/map.mli
      type   key   1 occurrences in 1 modules: functor/map.mli
      value  empty 1 occurrences in 1 modules: functor/map.mli
      value  add   1 occurrences in 1 modules: functor/map.mli
    value  of_list no definition found

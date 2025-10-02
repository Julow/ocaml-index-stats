  $ dune build @ocaml-index
  $ ocaml-index-stats
  module Functor__Map (at functor/map.ml)
    modtype S    no occurrences found
      type   t     no occurrences found
      type   key   no occurrences found
      value  empty 1 occurrences in 1 modules: main.ml (1)
      value  add   1 occurrences in 1 modules: main.ml (1)
    Items of this module are used 2 times in 1 modules:
      main.ml (2)
    in these directories:
      
    module Make 1 occurrences in 1 modules: main.ml (1)
      type   t     no occurrences found
      type   key   no occurrences found
      value  empty 1 occurrences in 1 modules: main.ml (1)
      value  add   1 occurrences in 1 modules: main.ml (1)
    Items of this module are used 2 times in 1 modules:
      main.ml (2)
    in these directories:
      
  Items of this module are used 5 times in 1 modules:
    main.ml (5)
  in these directories:
    
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
    module Hetero_map no occurrences found
    module Map        no occurrences found
  module Main (at main.ml)
    module M       no occurrences found
      type   t     4 occurrences in 1 modules: functor/map.mli (4)
      type   key   1 occurrences in 1 modules: functor/map.mli (1)
      value  empty 1 occurrences in 1 modules: functor/map.mli (1)
      value  add   1 occurrences in 1 modules: functor/map.mli (1)
    Items of this module are used 7 times in 1 modules:
      functor/map.mli (7)
    in these directories:
      functor/ (7)
    value  of_list no occurrences found
  Items of this module are used 7 times in 1 modules:
    functor/map.mli (7)
  in these directories:
    functor/ (7)

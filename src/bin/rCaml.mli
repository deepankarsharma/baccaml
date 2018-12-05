open BacCaml

type arg =
  { file : string;
    main : string;
    ex_name : string;
    code : int array;
    annot : int array;
    reds : (string * int) list;
    greens : (string * int) list;
  }

type var =
  { redtbl : (string, int) Hashtbl.t;
    greentbl : (string, int) Hashtbl.t;
  }

val prepare_env : [< `Meta_method | `Meta_tracing] -> arg -> Jit_prep.env
val run : ([> `Meta_method | `Meta_tracing] -> arg -> 'a) -> 'a

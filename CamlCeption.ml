type expr =
    | Int of int
    | Plus of expr * expr
    | Minus of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | Var of string
    | Let of string * expr * expr
    | If of cond * expr * expr
    | Fun of string * expr
    | App of expr*expr
    | For of string*expr*expr*expr*expr
    | While of cond*expr*expr
    | End
and cond =
    | Not of cond
    | And of cond * cond
    | Or of cond * cond
    | Equal of expr * expr
    | Less of expr * expr
and valeur =
    | Ival of int
    | Fonction of {var: string; expression: expr; mutable env: (string * valeur) list}
    | Environment of (string * valeur) list
;;

let rec lookup str lst = match lst with
    | [] -> failwith ( "unbound variable: " ^ str )
    | (s, i)::_ when str = s -> i
    | _::q -> lookup str q;;

let rec in_env str lst = match lst with
    | [] -> false
    | (s, i)::q -> if s = str then true else in_env str q
and add_to_env (str, v) lst = if in_env str lst then (
        replace_in_env str v lst
    ) else (
        (str, v)::lst
    )
and replace_in_env str valeur lst = match lst with
    | [] -> failwith ("Fail to replace value" ^ str)
    | (s, v)::q -> if s = str then ((s, valeur)::q) else (s, v)::(replace_in_env str valeur q);;

let get_env s = match s with
    | Fonction(g) -> g.env
    | _ -> []
;;
let set_env s l = match s with
    | Fonction(g) -> g.env <- l
    | _ -> ();;

let rec compute lst ex = match ex with
    | Var v -> lookup v lst
    | Int i -> Ival(i)
    | If (c, g, d) -> if (compute_bool lst c) then compute lst g else compute lst d
    | Let(s, g, d) -> (match compute lst g with
        | Ival(i) -> compute (add_to_env (s, Ival(i)) lst) d
        | f -> set_env f (add_to_env (s, f) (get_env f)); compute (add_to_env (s, f) lst) d
        )
    | Plus(g, d) -> (match (compute lst g, compute lst d) with
        | (Ival(g1), Ival(d1)) -> Ival(g1 + d1)
        | _, _ -> failwith "mauvais type")
    | Minus(g, d) -> (match (compute lst g, compute lst d) with
        | (Ival(g1), Ival(d1)) -> Ival(g1 - d1)
        | _, _ -> failwith "mauvais type")
    | Mult(g, d) -> (match (compute lst g, compute lst d) with
        | (Ival(g1), Ival(d1)) -> Ival(g1 * d1)
        | _, _ -> failwith "mauvais type")
    | Div(g, d) -> (match (compute lst g, compute lst d) with
        | (Ival(g1), Ival(d1)) -> if d1 = 0 then (failwith "Division par zero") else Ival(g1 / d1)
        | _, _ -> failwith "mauvais type")
    | Fun(s, e) ->  Fonction{var=s; expression=e; env=lst}
    | App(f, e) -> (match (compute lst f) with
        | Fonction(f) -> compute ((f.var, (compute lst e))::f.env) f.expression
        | _ -> failwith "mauvais type")
    | For(str, debut, fin, e, suite) -> (match (compute lst debut, compute lst fin) with
        | (Ival(debut), Ival(fin)) -> let env = ref lst in if debut <= fin then (
                for i = debut to fin do
                    (match compute (add_to_env (str, Ival(i)) !env) e with
                        | Environment(envl) -> env := envl
                        | _ -> failwith "mauvais type"
                    )
                done;
            ) else (
                for i = debut downto fin do
                    (match compute (add_to_env (str, Ival(i)) !env) e with
                        | Environment(envl) -> env := envl
                        | _ -> failwith "mauvais type"
                    )
                done;
            );
            compute !env suite
        | _ -> failwith "mauvais type" )
    | While(cond, e, suite) -> let env = ref lst in while (compute_bool !env cond) do
        (match compute !env e with
            | Environment(envl) -> env := envl
            | _ -> failwith "mauvais type"
        );
        done;
        compute !env suite
    | End -> Environment(lst)
and compute_bool lst co = match co with
    | Not c -> not (compute_bool lst c)
    | And (c1, c2) -> (compute_bool lst c1) && (compute_bool lst c2)
    | Or (c1, c2) -> (compute_bool lst c1) || (compute_bool lst c2)
    | Equal(g, d) -> (match (compute lst g, compute lst d) with
        | (Ival(e1), Ival(e2)) -> e1 = e2
        | _, _ -> failwith "mauvais type")
    | Less(g, d) -> (match (compute lst g, compute lst d) with
        | (Ival(e1), Ival(e2)) -> e1 < e2
        | _, _ -> failwith "mauvais type")
;;

let fact x = 
  Let("fact", 
      Fun("n",
          If(Equal(Var("n"), Int(1)),
             Int(1),
             Mult(Var("n"),
                  App(Var("fact"),
                      Minus(Var("n"), Int(1))
                     )
                 )
            )
         ),
      App(Var("fact"), Int(x)));;

let test_for = 
  Let("s",
      Int(0),
      For("i",
          Int(1),
          Int(10),
          Let("s",
              Plus(Var("s"), Var("i")),
              End
             ),
          Var("s")
         )
     );;

let test_while =
  Let("s",
      Int(0),
      Let("i",
          Int(1),
          While(Less(Var("i"),
                     Int(11)),
                Let("s",
                    Plus(Var("s"),Var("i")),
                    Let("i",
                        Plus(Var("i"), Int(1)),
                        End)
                   ),
                End
               )
         )
     );;

compute [] test_while;;



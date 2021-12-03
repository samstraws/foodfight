(*open StringDict
  open AutoState

  type finfo = (string list * Category.category * int * int * int)
  type minfo = (string * Category.category * int * Database.dm_info)

  type food_dict = finfo StringDict.t
  type player_dict = string list StringDict.t
  type move_dict = minfo StringDict.t

  let rand_int up = Random.self_init(); Random.int up

  let add v chng = if v <= 0 || (v + chng <= 0) then 1
  else v + chng

  let mutate_move mdict chng mv_opt = 
  let open Database in
  match mv_opt with 
  | None -> mdict
  | Some mv -> 
    let (desc, c, use, pwr) = StringDict.find mv mdict 
    in if rand_int 2 = 0 then 
      let pwr' = match pwr with 
        | DAttack att -> DAttack (add att chng) 
        | DHeal h -> DHeal (add h chng)
        | DDrain drn -> DDrain (add drn chng)
      in StringDict.add mv (desc, c, use, pwr') mdict
    else StringDict.add mv (desc, c, add use chng, pwr) mdict

  (**mutates a food somewhat randomly - 
   changes move, health, offense, or defense*)
  let mutate_food mult mdict (mvs, cat, hlt, off, def) = 
  let chng = (rand_int 5 + 3) * mult in
  (*print_endline ("chng " ^ string_of_int chng); *)
  match rand_int (List.length mvs + 3) with 
  | 0 -> (mvs, cat, add hlt chng, off, def)
  | 1 -> (mvs, cat, hlt, add off chng, def)
  | 2 -> (mvs, cat, hlt, off, add def chng)
  | _ -> mdict := mutate_move !mdict chng (Constants.choose_random mvs); 
    (mvs, cat, hlt, off, def)

  let fprint (lst, cat, h, o, d) = 
  print_endline (string_of_int h ^ ", " ^ string_of_int o ^ ", "
                 ^ string_of_int d)

  (** [balance_food food mvdict bound] changes the [food] based on its winrate.
    Returns new food, move dictionary*)
  let rec balance_food nm info mdict fdict (low, up) = 
  let (winr, _) = AutoState.find_food_stats !fdict 100 nm
  in print_endline (nm ^ ": " ^ string_of_float winr);
  if winr < low || winr > up then begin 
    let info' = if winr < low then mutate_food 1 mdict info 
      else mutate_food ~-1 mdict info
    in fprint info'; fdict := StringDict.add nm info' !fdict; 
    balance_food nm info' mdict fdict (low, up)
  end
  else ()

  let balance_foods (low, up) = 
  let mdict = Database.moves
  in let fdict = Database.foods 
  in let stats = AutoState.all_food_stats !fdict
  in StringDict.iter (fun k v ->
      balance_food k (StringDict.find k !fdict) mdict fdict (low, up)) stats
  (*StringDict.iter (fun k v ->
    balance_food k v mdict fdict (low, up)) !fdict;*)
*)
type category = Sweet | Salty | Savory | Spicy | Sour | Bitter | Bland | Fresh
type effect = Weak | Neutral | Strong

(**[unequal_lst] stores categories as (Strong Category, Weak Categories). 
   Requires: Strong category must map to two Weak categories.
   Example: if [(Spicy, [Sweet, Savory])] Spicy is Strong against Savory, and 
   Savory is Weak against Spicy. *)
let unequal_lst = [(Spicy, [Sweet;]);  
                   (Salty, [Sour;]);
                   (Sweet, [Bitter;]);
                   (Savory, [Fresh; ]);
                   (Sour, [Spicy;]);
                   (Bitter, [Salty;]);
                   (Bland, [Savory;]);
                   (Fresh, [Bland;]);]


(**[match_item item lst] is whether the [item] is in the [lst]*)
let match_item cat lst = 
  match List.find_opt (fun x -> x = cat) lst with
  | None -> false
  | Some _ -> true

(** Can make any number of comparisons *)
let effectiveness c1 c2 =
  if c1 = c2 then Neutral
  else if List.assoc c1 unequal_lst |> match_item c2 then Strong
  else if List.assoc c2 unequal_lst |> match_item c1 then Weak
  else Neutral

let strength_mult = let open Constants in function 
    | Weak -> weak_mult
    | Neutral -> neutral_mult
    | Strong -> strong_mult



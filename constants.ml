let test_mode = true
let stats_on = true

let weak_mult = 0.6
let neutral_mult = 1.1
let strong_mult = 1.5

let variability = 6
let variance_on = if test_mode then false else true

let max_foods = 3
let drain_div = 3

(**[vary amount] introduces variability to [amount]*)
let vary amount = if variance_on
  then begin let chng = Random.self_init(); Random.int(variability)
    in if Random.int(2) = 0 then amount - chng else amount + chng
  end
  else amount

(** [damage_calc mult base_dmg off def] computes the damage dealt based on the 
    given weight [mult], base damage [base_dmg] and the offensive and defensive 
    values [off] and [def] respectively. *)
let damage_calc mult base_dmg off def = 
  let dmg = mult *. float_of_int (vary base_dmg) 
            +. float_of_int (off - def) *. 0.5
            |> int_of_float 
  in if dmg < 0 then 0 (*base is has no effect*)
  else dmg

(**[choose_random lst] is [Some] random elem from [lst] 
   or [None] if empty [lst]*)
let choose_random lst = 
  if (List.length lst <= 0) then None
  else let x = Random.self_init(); Random.int (List.length lst)
    in Some (List.nth lst x)








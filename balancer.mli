(*
open StringDict
open AutoState
(**Balancer balances the moves, foods, and players*)

type finfo = (string list * Category.category * int * int * int)
type minfo = (string * Category.category * int * Database.dm_info)

type food_dict = finfo StringDict.t
type player_dict = string list StringDict.t
type move_dict = minfo StringDict.t


(**[balance_foods mvdict fdict (low, up)] balances the foods in the Database
   such that the win-rates of each food roughly fall within the bounds given,
   [low] is lower % bound and [up] is upper % bound
   Note: requires Database to be mutable - unfortunately messes things up. 
   Not part of our 1500 LOC*)
val balance_foods : float * float -> unit
*)
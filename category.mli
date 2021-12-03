(**Represents the food type*)
type category = Sweet | Salty | Savory | Spicy | Sour | Bitter | Bland | Fresh

(**Represents the different effects types can have on each other*)
type effect = Weak | Neutral | Strong

(**[effectiveness c1 c2] is the effect of [c1] on [c2]. 
     [c1] represents the current attacker and [c2] is the opponent.
     Example: [effectiveness Spicy Savory] returns Strong. *)
val effectiveness : category -> category -> effect

(**[strength_mult eff] is the multiplier corresponding to [eff]*)
val strength_mult: effect -> float
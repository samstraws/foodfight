open StringDict
open Category

type dm_info =
  | DAttack of int
  | DHeal of int
  | DDrain of int

let moves =
  StringDict.(
    empty
    |> add "bean blast" ("The glorious smell of yesterday's leftovers", Savory, 
                         10, DAttack 14)
    |> add "bitter melon" ("Feel the bitterness of defeat", Bitter, 3, 
                           DDrain 12)
    |> add "bread crumbs" ("Bready to rumble", Bland, 1, DAttack 12)
    |> add "broccoli" ("A tree-sized broccoli", Bitter, 10, DAttack 25)
    |> add "burrito wrap" ("Let's wrap this up!", Bland, 8, DAttack 13)
    |> add "butter up" ("Serve up a beating...of deliciousness", Savory, 1, 
                        DDrain 26)
    |> add "cacao cleanse" ("Not your grandmother's milk chocolate", Bitter, 10, 
                            DAttack 15)
    |> add "calming broth" ("Healing power of soup", Savory, 3, DHeal 25)
    |> add "candy crush" ("Sate your sweet tooth", Sweet, 3, DAttack 18)
    |> add "carb boost" ("Get that energy!", Bland, 1, DHeal 30)
    |> add "carrot" ("24 carrot magic", Fresh, 2, DAttack 10)
    |> add "cheesy cheese" ("Smell the stinky goodness", Salty, 5, DAttack 25)
    |> add "chicken" ("Bawk bawk", Savory, 6, DAttack 15)
    |> add "cilantro" ("Might taste like soap", Bitter, 5, DAttack 30)
    |> add "cool cucumber" ("Fresh and crunchy", Fresh, 15, DAttack 11)
    |> add "corn starch" ("Staunch ally", Bland, 5, DAttack 21)
    |> add "couch potato" ("Take a break", Bland, 2, DHeal 25)
    |> add "cup of coffee" ("Taste the bitter grinds of hangover", Bitter, 1, 
                            DAttack 10)
    |> add "curry powder" ("Curry power", Spicy, 20, DAttack 15)
    |> add "egg" ("An eggcellent choice", Savory, 10, DAttack 16)
    |> add "fish" ("Sashimi it up", Savory, 5, DAttack 10)
    |> add "flaky crust" ("Crunchy and flaky", Bland, 5, DAttack 27)
    |> add "flour dust" ("Knead some more", Bland, 2, DDrain 30)
    |> add "gingerly" ("Watch out, it's spicy", Spicy, 1, DAttack 20)
    |> add "granny smith" ("She's vicious", Fresh, 15, DAttack 20)
    |> add "hard nut" ("Just nutty", Salty, 1, DAttack 25)
    |> add "jalapeno pop" ("Unleash the spicyness", Spicy, 15, DAttack 15)
    |> add "jamming with jam" ("Fruity spread", Sweet, 10, DAttack 13)
    |> add "kale combo" ("Taste the bitterness", Bitter, 12, DAttack 16)
    |> add "kimchi" ("Banish your opponent to the fermentation jar", Spicy, 7, 
                     DAttack 13)
    |> add "lemon zest" ("Zest up your life", Sour, 15, DAttack 10)
    |> add "lettuce wrap" ("Leafy choke hold", Bitter, 5, DAttack 27)
    |> add "mango mango" ("Mango Mango Mango", Fresh, 10, DAttack 13)
    |> add "marinara sauce" ("marinaranate your opponent", Savory, 15, 
                             DAttack 10)
    |> add "midnight snack" ("Sneak in a small boost", Bland, 1, DHeal 15)
    |> add "moo moo milk" ("Cream of the crop", Savory, 10, DAttack 19)
    |> add "mushroom" ("He's a fungi", Savory, 4, DAttack 25)
    |> add "noodle wind" ("Noodlin' around", Bland, 9, DAttack 14)
    |> add "onion" ("Without smell, it tastes like an apple", Sweet, 15, 
                    DAttack 20)
    |> add "orange juice" ("Squeeze the life out of your opponent", Sour, 1, 
                           DDrain 20)
    |> add "red pepper paste" ("Feel the burnnn", Spicy, 1, DAttack 40)
    |> add "rice" ("Spread the rice", Bland, 5, DAttack 11)
    |> add "salty salt" ("Just a pinch", Salty, 3, DAttack 17)
    |> add "shia la beef" ("He's beefy and thicc", Salty, 9, DAttack 25)
    |> add "soy sauce smack" ("Unleash the saltiness", Salty, 5, DAttack 25)
    |> add "strawberry slam" ("Berry powerful", Fresh, 1, DAttack 30)
    |> add "sugar rush" ("The sweet scent of victory", Sweet, 4, DDrain 16)
    |> add "tomatomato" ("Eat the tomatomatomato", Sour, 1, DAttack 22)
    |> add "vinegar" ("Unleash the sourness", Sour, 5, DAttack 10)
    |> add "wasabi" ("That feeling when your nose hurts", Spicy, 5, DAttack 13)
    |> add "whipped cream" ("You could survive on this", Sweet, 12, DAttack 15)
    |> add "yogurt" ("Probiotics!", Sour, 3, DAttack 11)
  )

let foods = 
  StringDict.
    (empty
     |> add "apple pie" 
       (["flaky crust"; "granny smith"; "carb boost"], Fresh, 54, 1, 10)
     |> add "burrito" (["cheesy cheese"; "bean blast"; "burrito wrap"],
                       Savory, 40, 15, 10)
     |> add "carbonara" (["noodle wind"; "shia la beef"; "corn starch"; "butter up"],
                         Bland, 32, 20, 12)
     |> add "cheese cake" (["cheesy cheese"; "moo moo milk"; "sugar rush"],
                           Sweet, 20, 16, 18)
     |> add "chicken noodle soup" (["chicken"; "noodle wind"; "calming broth"], 
                                   Savory, 48, 10, 18)
     |> add "chinese bitter melon" (["bitter melon"; "egg"; "onion"], Bitter, 
                                    60, 5, 8)
     |> add "curry" (["rice"; "chicken"; "curry powder"], Spicy, 52, 10, 10)
     |> add "hamburger" (["couch potato"; "shia la beef"; "cheesy cheese"], 
                         Savory, 48, 3, 1)
     |> add "hot and sour soup" (["vinegar"; "carrot"; "gingerly"], Sour, 
                                 60, 1, 20)
     |> add "hot chocolate" (["cacao cleanse"; "moo moo milk"; "midnight snack"]
                            , Sweet, 53, 9, 13)
     |> add "kale salad" (["kale combo"; "cool cucumber"; "broccoli"], Bitter, 
                          42, 12, 8)
     |> add "key lime pie" (["lemon zest"; "whipped cream"; "sugar rush"], Sour, 
                            68, 5, 8)
     |> add "kimchi fried rice" (["kimchi"; "red pepper paste"; "rice"], Spicy, 
                                 42, 6, 14)
     |> add "mango smoothie" (["mango mango"; "orange juice"; "yogurt"], Sweet, 
                              28, 14, 16)
     |> add "mole" (["cacao cleanse"; "jalapeno pop"; "lemon zest"], Savory, 
                    45, 18, 10)
     |> add "omelet" (["egg"; "moo moo milk"; "onion"; "carrot"], Savory, 
                      37, 13, 17)
     |> add "parfait" (["yogurt"; "strawberry slam"; "mango mango"], Fresh, 
                       41, 4, 18)
     |> add "pizza" (["tomatomato"; "marinara sauce"; "flour dust"], Salty, 
                     54, 1, 4)
     |> add "pretzels" (["salty salt"; "cheesy cheese"; "bread crumbs"], Salty, 
                        55, 5, 1)
     |> add "reese's" (["hard nut"; "cacao cleanse"; "candy crush"], Sweet, 
                       50, 10, 1)
     |> add "salad" (["cheesy cheese"; "cool cucumber"; "vinegar"], Fresh, 
                     60, 10, 5)
     |> add "sour slush" (["lemon zest"; "orange juice"; "candy crush"], Sour, 
                          41, 14, 10)
     |> add "sushi" (["wasabi"; "soy sauce smack"; "fish"], Salty, 23, 20, 14)
     |> add "toast" (["cup of coffee"; "bread crumbs"; "jamming with jam";
                      "butter up"], Bland, 28, 15, 14)
    ) 


let players = 
  StringDict.
    (empty
     |> add "sugar demon" (["cheese cake"; "mango smoothie"; "reese's"])
     |> add "health hammer"( ["salad"; "omelet"; "kale salad"])
     |> add "cheese gang" ( ["pizza"; "cheese cake"; "burrito"])
     |> add "vegiefruitarian" ( ["salad"; "mango smoothie"; "key lime pie"])
     |> add "dessert empire" ( ["apple pie";"cheese cake"; "key lime pie"])
     |> add "rice cooker" ( ["mole"; "kimchi fried rice"; "burrito"])
     |> add "breakfast bunch" (["toast"; "parfait"; "omelet"])
     |> add "east asia" (["chinese bitter melon"; "sushi"; "hot and sour soup"])
     |> add "c for classic" (["curry"; "carbonara"; "chicken noodle soup"])
     |> add "fast food" (["sour slush"; "hamburger"; "burrito"])
     |> add "cozy competitor" (["hot chocolate"; "chicken noodle soup"; 
                                "apple pie"])
    )

let play_num =
  StringDict.
    (empty 
     |> add "a" "vegiefruitarian"
     |> add "b" "sugar demon"
     |> add "c" "rice cooker"
     |> add "d" "health hammer"
     |> add "e" "fast food"
     |> add "f" "east asia"
     |> add "g" "dessert empire"
     |> add "h" "cozy competitor"
     |> add "i" "cheese gang"
     |> add "j" "c for classic"
     |> add "k" "breakfast bunch")
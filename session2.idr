-- Union types
data LondonHaskell = Hands Int
                   | On    String
                   | Idris Bool


data LondonHaskell2 : Type where
    Hands2 : LondonHaskell2
    On2 : String -> LondonHaskell2
    Idris2 : Bool -> LondonHaskell2
    TreeNode : LondonHaskell2 -> LondonHaskell2 -> LondonHaskell2

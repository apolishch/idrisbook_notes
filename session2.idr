-- Union types
data LondonHaskell = Hands Int
                   | On    String
                   | Idris Bool


data LondonHaskell2 : Type where
     Hands2 : LondonHaskell2
     On2 : String -> LondonHaskell2
     Idris2 : Bool -> LondonHaskell2
     TreeNode : LondonHaskell2 -> LondonHaskell2 -> LondonHaskell2
    
    
data Nat' = Z' | S' Nat'
 
data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
               
AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = Int -> AdderType k
               
                                             
Adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs              
Adder Z acc = acc
Adder (S k) acc = \next => Adder k (acc + next)


data Expr = Add Expr Expr
          | Mul Expr Expr
          | Val Int
          
          
Num Expr where
      (+) x y = Add x y
      (*) x y = Mul x y
      fromInteger x = Val (fromInteger x)
      
      
data Format = Number Format
            | Str Format
            | Lit String Format
            | End
            
            
PrintfType : Format -> Type
PrintfType (Number fmt) = Int -> PrintfType fmt
PrintfType (Str fmt) = String -> PrintfType fmt
PrintfType (Lit s fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show 1)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt(Lit s fmt) acc = printfFmt fmt (acc ++ s)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (x :: chars) = case toFormat chars of
                        (Lit s fmt) => Lit (strCons x s) fmt
                        fmt => Lit (cast x) fmt
                        
strPrintfType : String -> Type
strPrintfType = PrintfType . toFormat . unpack


printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt (toFormat (unpack fmt)) ""

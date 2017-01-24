import Data.Vect

allLengths : Vect n String -> Vect n Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

--insert : Ord elem => (x : elem) -> (xsSorted : Vect n elem) -> Vect (S len) elem
--insert x [] = [x]
--insert x (y :: xs) = case x < y of
--                         True => y :: insert x xs
--                         False => x :: y :: xs


--insertSort : Ord elem => Vect n elem -> Vect n elem
--insertSort [] = []
--insertSort (x :: xs) = let xsSorted = insertSort xs
--                       in (insert x xsSorted)

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

appendEx : (elem : Type) -> (n : Nat) -> (m : Nat) -> Vect n elem -> Vect m elem -> Vect (n+m) elem
appendEx elem Z m [] ys = ys
appendEx elem (S len) m (x :: xs) ys = x :: appendEx elem len m xs ys


appendBI : { t: Type } -> { a : Nat } -> { b : Nat } -> Vect a t -> Vect b t -> Vect (a+b) t
--appendBI xs ys = ?appendBI_rhs
appendBI [] ys = ys
appendBI (x :: xs) ys = x :: (append xs ys)

appendBIU : { t: _ } -> { a : _ } -> { b : _ } -> Vect a t -> Vect b t -> Vect (a + b) t
appendBIU [] ys = ys
appendBIU (x :: xs) ys = x :: appendBIU xs ys

my_length : Vect n elem -> Nat
my_length [] = Z
my_length (x :: xs) = 1 + my_length xs

my_length' : Vect n elem -> Nat
my_length' {n} xs = n

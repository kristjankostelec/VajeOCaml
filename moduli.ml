(* Zgodba, ki naj ostane neprevedena:

"Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83

 (optional) homework: read the rest of the introduction of the paper at
 https://people.mpi-sws.org/~dreyer/tor/papers/reynolds.pdf
 *)


(* Kompleksna števila so zahtevnejša. Pričnimo z Nat. *)


(* Definirajmo signaturo "NAT", ki določa strukturo naravnih števil. Ima
   osnovni tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in
   množenje. Hkrati naj vsebuje pretvorbe iz in v OCamlov "int" tip.*)

module type NAT = sig
    type t
    val eq   : t -> t -> bool
    val zero : t
	val one : t
	val sum : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val to_int : t -> int
	val from_int : int -> t
  end


(* Napiši modul, ki zgradi modul s podpisom tipa NAT z uporabo OCamlovega 
   "int" tipa.

   Opozorilo: Dokler ni implementiranih vseh funkcij v Nat_int se bo OCaml
   pritoževal. Temu se lahko izogneš tako, da funkcije, ki jih še niso napisane
   nadomestiš s 'failwith "later"'. *)
 
module Nat_int : NAT = struct

type t = int

let eq = (=)	
let zero = 0  
let one = 1
let sum a b = a + b
let sub a b = max 0 (a-b)
let mul a b = a * b  
let	to_int a = a  
let from_int a = max 0 a (*A JE TO SPLOH PORU OZIROMA KAKO TO SPLOH DELUJE!?!*)

end

(* Sedaj naravna števila naredi s pomočjo Peanovih aksiomov
   https://en.wikipedia.org/wiki/Peano_axioms
   
   Osnovni tip modula podaj kot vsotni tip, kjer imaš ničlo in pa 
   naslednjika nekega naravnega števila [Zero in Successor n].

   Funkcije implementiraj s pomočjo rekurzije. Števili m in n sta enaki, če
   sta obe 0, ali pa sta naslednika k in l, kjer sta k in l enaki števili. *)


module Pean : NAT = struct

type t = 
	|Zero
	|Sucs of t
  
let rec eq x y =
	match (x,y) with
	|Zero, Sucs _ -> false
	|Sucs _, Zero -> false
	|Zero, Zero -> true
	|Sucs a, Sucs b -> eq a b
	
let zero = Zero 

let one = Sucs Zero

let rec sum a b = 
	match a b with
	| Zero, Sucs x -> Sucs x
	| Sucs x, Sucs y -> sum (Sucs (Sucs x)) y 

let sub a b = failwith "later"
let mul a b = failwith "later"
let	to_int a = failwith "later"
let from_int a = failwith "later"
	

end

(* Definiraj signaturo modula kompleksnih števil.
   Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
   negacijo števila, konjugacijo, seštevanje, množenje, deljenje in inverz. *)

(* module type COMPLEX = sig
    type t
    val eq : t -> t -> bool
    ...
  end
 *)

(* Napiši kartezično implementacijo kompleksnih števil (torej z = x + iy).
   Deljenje je zahtevnejše, zato si ga lahko s 'failwith' trikom pustiš za kasneje.
 *)
 
(*
module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x.re = y.re && ...

  ...

end
 *)


(* Sedaj napiši še polarno implementacijo kompleksnih števil (torej z = r e^(i*fi) ).

   Seštevanje je v polarnih koordinatah zahtevnejše, zato najprej napiši druge reči. *)
   
(*
module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  let pi = 2. *. acos 0.
  let rad deg = (deg /. 180.) *. pi
  let deg rad = (rad /. pi) *. 180.

  let eq x y =

  ...

end
 *)

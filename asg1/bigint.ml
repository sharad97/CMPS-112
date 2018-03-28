(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])
	let first     = fst
	let second    = snd

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
						
						
	(* removes leading zero from int list *)
    let trim0 list =
        let rec trim0' list' = match list' with 
            | []       -> []
            | [0]      -> []
            | car::cdr -> 
                let cdr' = trim0' cdr 
				in match car,cdr' with 
                    | 0, []  -> []
                    | car, cdr' -> car::cdr' 
				in trim0' list	
	
	(* Compare *)
	let rec cmp list1 list2 = match (list1, list2) with
		| [],[] 					-> 0
        | list1, []                 -> 1
        | [], list2                 -> -1
        | car1::cdr1, car2::cdr2    ->
			let x = cmp cdr1 cdr2
			in if car2 < car1
            then 
				begin 
					1 
				end
            else if car2 > car1
            then 
				begin 
					-1 
				end
            else 
				begin 
					x 
				end
					
						
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)
		  
		  
	let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> trim0(sub' list1 [carry] 0)
        | [], list2, carry   -> trim0(sub' [carry] list2 0)
        | car1::cdr1, car2::cdr2, carry ->
			let c = car1 - carry
			in let c2 = car2
			in if (c2 > c)
			then 
				begin 
					let dif = (10 + c - c2)
					in  dif mod radix :: trim0 (sub' cdr1 cdr2 1)
				end
			else 
				begin 
					let dif = (c - c2)
					in  dif mod radix :: trim0(sub' cdr1 cdr2 0) 
				end
		  
		  
    let rec mul' value1 value2 =
         if (car value2) = 1
         then 
			begin
				value1
			end
         else
			begin 
				add' value1 (mul' value1 (trim0(sub' value2 [1] 0))) 0 
			end
		  

    let rec div' value1 value2 ans =
        if (cmp value1 value2) = 0
        then 
			begin
				ans, value1
			end
        else 
			begin
				div' (trim0(sub' value1 value2 0)) value2 (add' ans [1] 0)
			end

    let rec pow' value1 value2 = 
        if (car value2) = 1
        then 
			begin
				value1
			end
        else 
			begin
				mul' value1 (pow' value1 (trim0(sub' value2 [1] 0)))
				
			end
		


	(* Addition *)	
    let add (Bigint (sign_1, value1)) (Bigint (sign_2, value2)) =
        if sign_1 = sign_2
        then 
			begin
				Bigint (sign_1, add' value1 value2 0)
			end
        else if (cmp value2 value1) = 1
			 then 
				begin
					Bigint (sign_2, trim0(sub' value2 value1 0))
				end
             else 
				begin
					Bigint (sign_1, trim0(sub' value1 value2 0))
				end
			
	(* Subtraction *)	
	let sub (Bigint (sign_1, value1)) (Bigint (sign_2, value2)) =
        if sign_1 = sign_2 
		then 
			begin 
				if cmp value2 value1 = 1
				then 
					begin
						Bigint (Neg, trim0(sub' value2 value1 0))
					end	
				else 
					begin 
						Bigint (Pos, trim0(sub' value1 value2 0))
					end
			end		
        else if cmp value2 value1 = 1 
			 then 
				begin 
					Bigint (sign_1, add' value2 value1 0)
				end
			 else 
				begin
					Bigint (sign_2, add' value1 value2 0)
				end
		
		(* Multiplication *)		
    let mul (Bigint (sign_1, value1)) (Bigint (sign_2, value2)) =
		let multiple = mul' value1 value2
        in	if sign_1 <> sign_2
			then 
				begin 
					Bigint (Neg, multiple) 
				end
			else 
				begin 
					Bigint (Pos, multiple) 
				end

		 
	(* Division *)	
    let div (Bigint (sign_1, value1)) (Bigint (sign_2, value2)) =
		let divisor = div' value1 value2 [0]
		in	if sign_1 <> sign_2
			then 
				begin 
					Bigint(Neg, first(divisor)) 
				end
			else 
				begin 
					Bigint(Pos, first(divisor)) 
				end
		
	(* Remainder *)	
	let rem (Bigint (sign_1, value1)) (Bigint (sign_2, value2)) = 	
		let remainder = div' value1 value2 [0]
		in	if sign_1 <> sign_2
			then 
				begin 
					Bigint(Neg, second(remainder)) 
				end	
			else 
				begin 
					Bigint(sign_1, second(remainder)) 
				end
			
	(* Power *)	
    let pow (Bigint (sign_1, value1)) (Bigint (sign_2, value2)) =
		let power = pow' value1 value2
		in 	if sign_1 = Pos
			then 
				begin
					Bigint (sign_1, power )
				end
			else if rem (Bigint (Pos, value2)) (Bigint (Pos, [2])) <> (Bigint (Pos, [1]))
				 then
					begin
						Bigint (Pos, power)
					end
				 else 
					begin 
						Bigint (Neg, power)) 
					end
			else if sign_2 = Neg
			then 
				begin 
					zero 
				end


end

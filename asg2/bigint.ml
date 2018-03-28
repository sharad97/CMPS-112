(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen = 1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

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
                        
    (*modified from trimzeros.ml provided by professor*)
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

    (*comper*)
    let rec comp list1 list2 = match (list1, list2) with
        | [], []                 ->  0
        | list1, []              ->  1
        | [], list2              -> -1
        | car1::cdr1, car2::cdr2 -> 
            let x = comp cdr1 cdr2
            in if x = 0 && car1 <> car2
            then (if car2 > car1
                  then 
                      begin 
                          -1 
                      end
                  else if car2 < car1
                  then 
                      begin 
                          1 
                      end
                  else 
                      begin 
                          0
                      end)
            else x

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in sum mod radix :: add' cdr1 cdr2 (sum / radix)

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
                    let dif = (radix + c - c2)
                    in  dif mod radix :: trim0(sub' cdr1 cdr2 1)
                end
            else 
                begin 
                    let dif = (c - c2)
                    in  dif mod radix :: trim0(sub' cdr1 cdr2 0)
                end

    (* Addition *)
    let add (Bigint (sign_1, value1)) (Bigint (sign_2, value2)) =
        let subtract =trim0(sub' value1 value2 0)
        in if sign_1 = sign_2
        then 
            begin
                Bigint (sign_1, add' value1 value2 0)
            end
        else if (comp value2 value1) = 1
             then 
                begin
                    Bigint (sign_2, (reverse(subtract)))
                end
             else 
                begin
                    Bigint (sign_1, subtract)
                end

    (* Subtraction *)
    let sub (Bigint (sign_1, value1)) (Bigint (sign_2, value2)) =
        let subtract = trim0(sub' value1 value2 0)  
        in let addition = add' value1 value2 0
        in if sign_1 = sign_2 
        then 
            begin
                if comp value2 value1 = 1
                then
                    begin
                        Bigint (Neg, (reverse(subtract)))
                    end    
                else 
                    begin
                        Bigint (Pos, subtract)
                    end
            end        
        else if comp value2 value1 = 1
             then 
                begin
                    Bigint (sign_1, (reverse(addition)))
                end
             else 
                begin
                    Bigint (sign_2, addition)
                    end  


(* 
following codes are modified from 
muldivrem-trace.ml, mathfns-trace.ml
which were provided by professor 
*)


    let double number = add' number number 0

    let rec mul' (multiplier, powerof2, multiplicand') = 
        if (comp powerof2 multiplier) > 0
        then 
            begin 
                multiplier, [0] 
            end
        else let remainder, product = 
                mul'(multiplier, double powerof2, double multiplicand')
             in if (comp powerof2 remainder) < 0
                then 
                    begin 
                        trim0 (sub' remainder powerof2 0), 
                        add' product multiplicand' 0 
                    end
                else  
                    begin 
                        remainder, product 
                    end

    let rec divrem' (dividend, powerof2, divisor') =
        if (comp divisor' dividend) > 0
        then 
            begin 
                [0], dividend 
            end
        else let quotient, remainder = 
                divrem' (dividend, double powerof2, double divisor')
             in if (comp divisor' remainder) < 0
                then 
                    begin 
                        add' quotient powerof2 0, 
                        trim0 (sub' remainder divisor' 0) 
                    end
                else 
                    begin 
                        quotient, remainder 
                    end

    let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')
                    
    let even number = 
        let remainder = trim0 (let  _, divrems = divrem 
                               (number, [2]) in divrems) 
        in if comp remainder [] <> 0 
        then 
            begin
                false 
            end
        else
            begin 
                true
            end

    let rec pow'(base, expt, result) = match expt with
        | [0]   -> result
        | expt when even expt -> pow'(
            (let  _, muls = mul' (base,[1], base) in muls), 
            (let  _, divrems = divrem(expt, [2])in divrems), result) 
        | expt  -> pow'(base, sub' expt [1] 0, 
            (let  _, muls = mul'(base,[1],result) in muls))

    let mul (Bigint (neg1, multiplier)) (Bigint (neg2, multiplicand)) =
        let _, product = mul' (multiplier, [1], multiplicand)
        in if neg1 = Pos && neg2 = Neg 
        then 
            begin 
                Bigint (Neg,  product)
            end
        else 
            begin 
                Bigint (Pos,  product) 
            end    
 
    let div (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) =
        let quotient, _ = divrem (dividend, divisor)
        in if neg1 = Pos && neg2 = Neg 
        then 
            begin 
                Bigint(Neg,  quotient)
            end
        else 
            begin 
                Bigint(Pos, quotient)
            end        

    let rem (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) =
        let  _, remainder = divrem (dividend, divisor)
        in if neg1 = Pos && neg2 = Neg 
        then 
            begin 
                Bigint(Neg, remainder)
            end
        else 
            begin 
                Bigint(neg1, remainder)
            end

    let pow (Bigint (neg1, base)) (Bigint (neg2, expt)) =  
        if base >= [1] || even expt
        then 
            begin 
                Bigint (Pos, pow'(base, expt, [1])) 
            end
        else if expt < [0]
        then 
            begin 
                zero 
            end
        else 
            begin 
                Bigint (Neg, pow'(base, expt, [1])) 
            end

end


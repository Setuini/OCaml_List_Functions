
(* tail recursive reverse - reverses a list*)
let reverse lst =
	let rec reverse acc lst = match lst with
		| [] -> acc
		| hd::tl -> reverse (hd::acc) tl
	in reverse [] lst;;

(* tail recursive length - gives the length of a list *)
let length lst = 
	let rec length acc lst = match lst with
		| [] -> acc
		| hd::tl (acc+1) tl
	in length 0 lst;;

(* tail recursive range - creates a list with all elements from min to max*)
let range min max =
	let rec range acc min max =
		if min > max then Lst.rev acc
		else range (min::acc) (min+1) max
	in range [] min max;;

(* tail recursive replicate - creates a list of length n with all elements equal to x*)
let replicate n x =
	let rec replicate acc n x = 
		if n < 1 then acc
		else replicate (x::acc) (n-1) x
	in replicate [] n x;;

(* tail recursive take - takes the first n elements of a list *)
let take n lst =
	let rec take acc n lst =
		if n < 1 then reverse acc
		else match lst with
			| [] -> acc
			| hd::tl -> take (hd::acc) (n-1) tl
	in take [] n lst;;

(* tail recursive drop - drops the first n elements of a list *)
let rec drop n lst = if n < 1 then xs else match xs with
	| [] -> [];
	| hd::tl -> drop (n-1) tl;;

(* tail recursive sum - calculates the sum of all elements in a int list*)
let sum lst =
	let rec sum acc lst = match lst with
		| [] -> acc;
		| hd::tl -> sum (acc+hd) tl
	in sum 0 lst;;







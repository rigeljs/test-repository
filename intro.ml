(* Finger Exercises Homework assigment *)

(* The following command tells OCaml to use the "assert" library that
   defines the run_test command used below.  See the web pages for
   instructions on how to set up an Eclipse project for this program.
 *)
;; open Assert



(*************************************************************************)
(* Problem 1 (computing income tax) **************************************)

(* Assuming that the income is at least $100,000, output the corresponding
   tax liability according to the following schedules. Federal tax is 15%
   of the portion of income at or below $35,000 plus 25% of the portion
   of income above $35,000 and at or below $100,000 plus 35% of
   the remaining income. State tax is a flat 5% of all income in excess of
   $50,000.  

   For this problem, income and tax should be represented in *cents*,
   as integers. You should then use integer arithmetic to carry out 
   the calculation. 
*) 

let rec income_tax (income: int) : int =
	let under35 = (3500000 / 100) * 15 in
	let over35 = (6500000 / 100) * 25 in
	let remainder = ((income - 10000000) / 100) * 35  in
	let federal = under35 + over35 + remainder in
	let state = ((income - 5000000) / 100) * 5 in
	federal + state
  


(* 
 Here are two test cases for this problem.
*)

let test () : bool = 
  (income_tax 10000000) = 2400000
;; run_test "income_tax $100,000.00" test

let test () : bool = 
  (income_tax 12300000) = 3320000
;; run_test "income_tax $123,000.00" test


(* Here are two test case stubs. You need to edit them to produce real
   tests for the income tax function.  For each of the problems in the
   assignment, we provide some test cases like the ones
   above. However, just because your code passes the given tests does
   not mean that you will get full credit. When you submit your
   assignment, we will test it using DIFFERENT tests from the one
   above. To make sure that your solution is robust enough to pass our
   tests, you should think about what tests you can do to make sure
   that your program is correct.  

   STARTING FROM HW 03 WE WILL GRADE YOU ON THE QUALITY AND ROBUSTNESS
   OF YOUR TEST CASES IN YOUR STYLE GRADE.

   Please refer to the FAQ page for an explanation about test cases.
*)

let test () : bool = 
  (income_tax 50000000) = 18400000
;; run_test "income_tax $500000.00" test

let test () : bool = 
	(income_tax 10205000) = 2482000
;; run_test "income_tax $102050.00" test


(*************************************************************************)
(* Example (printing) ****************************************************)

(* Recall that OCaml files are composed of either top-level definitions,
   which begin with the 'let' keyword, or commands, which begin with two 
   semicolons. One useful command instructs OCaml to print text. *)

(* The print_endline function causes its string argument to appear in the 
   output window, much like System.out.println in Java. *) 

;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"
;; print_endline "Start of printing example"

(* Adding commands to print values can be very useful for debugging
   your assignment below. For example, consider the following 
   buggy function. *)

let day_after (day:string) : string = 
  begin match day with 
   | "Monday"    -> "Tuesday"
   | "Tuesday"   -> "Wednesday"
   | "Wednesday" -> "Thursday"
   | "Thursday"  -> "Friday"
   | "Friday"    -> "Saturday"
   | "Saturday"  -> "Sunday"
   | "Sunday"    -> "Monday"
   | _           -> failwith "Not a valid day"
  end

(* We can see that one of the test cases for this definition fails, so
   this definition definitely has a bug. *)

let test () : bool = 
  (day_after "Tuesday") = "Wednesday"
;; run_test "day_after Tuesday" test


(* But adding a print command lets us see what the erroneous result is... *)

;; print_endline ("The day after Tuesday is " ^ (day_after "Tuesday") ^ ".") 

(* (After running this example, you can fix the bug in the day_after
   function so that the test passes). *)

(* Note: If the result that you want to print is not a string, you
   need to convert it to be a string. OCaml includes some library
   functions to do this, such as string_of_int and string_of_bool.

   After you finish problem 1 above, uncomment the next command
	to demonstrate printing integer values.  
*)


;; print_endline ("Income tax on $100000.00 is " 
                  ^ (string_of_int (income_tax 10000000)) ^ " cents.")


(* Feel free to add whatever printing commands you like to this
homework assignment. The testing infrastructure will ignore all of the
output that your code produces. *)

;; print_endline "End of printing example"
;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"


(*************************************************************************)
(* Problem 2 (geometry) **************************************************)

(* Sometimes street magicians need to use crates as tables in their acts.
   Given the dimensions of a crate, find the largest surface area it can
   provide when used as a table. *)

let rec maximum_table_area (side1: int) (side2: int) (side3: int) : int =
  let option_1 = side1 * side2 in
	let option_2 = side1 * side3 in
	let option_3 = side2 * side3 in
	let max_1 =  
  if option_1 >= option_2 then option_1
	else option_2 in
  let max_2 = 
	if option_3 >= max_1 then option_3
	else max_1 in
	let total_max =
  if max_1 >= max_2 then max_1
	else max_2 in 
	total_max
	
	


let test () : bool =
  (maximum_table_area 1 2 3) = 6
;; run_test "maximum_table_area 1 2 3" test 

let test () : bool =
  (maximum_table_area 4 3 3) = 12
;; run_test "maximum_table_area 4 3 3" test 

let test () : bool = 
  (maximum_table_area 5 5 1) = 25
;; run_test "maximum_table_area 5 5 1" test

let test () : bool = 
  (maximum_table_area 3 3 3) = 9
;; run_test "maximum_table_area 3 3 3" test


(*************************************************************************)
(* Problem 3 (simulating robot movement) *********************************)

(* Help a robot move along its track (with spaces numbered 0 through 99) by 
   calculating its new position when given `dir` equal to "forward" or
   "backward" and `num_moves` indicating a non-negative number of spaces.
   Keep in mind that the robot can't move past the 0 or 99 spot so when it
   reaches either end it stays there. *)

let rec move_robot (pos: int) (dir: string) (num_moves: int) : int =
  let new_pos =
		if dir = "forward" then (pos + num_moves)
		else (pos - num_moves) in
	let valid_pos = 
		if new_pos < 0 then 0
		else if new_pos > 99 then 99
		else new_pos in
 	valid_pos
		

let test () : bool =
  (move_robot 10 "forward" 3) = 13
;; run_test "move_robot forward 3" test 

let test () : bool =
  (move_robot 1 "backward" 4 ) = 0
;; run_test "move_robot backward 4" test 

let test () : bool =
  (move_robot 2 "forward" 5) = 7
;; run_test "move_robot forward 5" test 

let test () : bool =
	(move_robot 99 "forward" 2) = 99
;; run_test "move_robot forward 2" test 


(*************************************************************************)
(* Problem 4 (Philadelphia geography) ************************************)



(* Philadelphia has a fairly logical layout: the numbered streets
   are typically one-way, and their direction is determined by their
   number and where you are in the city.

   Even streets go one way and odd streets go another:

     East of Broad (<14th): even go south, odd go north
     West of Broad (>14th): even go north, odd go south
     West Philly  (>=32nd): even go south, odd go north
     West Philly  (>=46th): two-way

   There are, however, a few exceptions.
     - 1st and 14th do not actually exist as street names---they're 
       called Front and Broad.  But we'll pretend they do.
     - Broad (14th), 25th, 38th, 41st, and 42nd are all two-way.
     - 24th and 59th go south.
     - 58th goes north.

   Write a program that returns one of four string values for each street
   number:
     - "N/A" when the street doesn't exist.  We only consider Front
       (=1st) through 69th Streets.
     - "N" when the street goes north.
     - "S" when the street goes south.
     - "NS" when the street is two-way.

   Welcome to Philadelphia! *)

let rec street_direction (st:int) : string =
  if st < 1 then "N/A"
	else if st = 14 || st = 25 then "NS"
	else if st = 38 || st = 41 then "NS"
	else if st = 42 then "NS"
	else if st = 24 || st = 59 then "S"
	else if st = 58 then "N"
	else if st > 69 then "N/A"
	else if st < 14 then 
		if st mod 2 = 1 then "N"
		else "S"
	else if st < 32 then
		if st mod 2 = 1 then "S"
		else "N"
	else if st < 46 then
		if st mod 2 = 1 then "N"
		else "S"
	else "NS"
	

let test () : bool =
  (street_direction 14) = "NS"
;; run_test "Broad is two-way" test 

let test () : bool =
  (street_direction 9) = "N"
;; run_test "9th goes north" test 

let test () : bool =
  (street_direction 18) = "N"
;; run_test "18th goes north" test 

let test () : bool =
  (street_direction 38) = "NS"
;; run_test "38th is two-way" test 

let test () : bool =
 (street_direction 70) = "N/A"
;; run_test "70th doesn't exist" test 


(*************************************************************************)

(* The remaining exercises provide practice with lists and recursion. 
   It is best to wait until after lecture 3 before continuing. *)

(*************************************************************************)
(* Problem 5 (exists) ****************************************************)

(* Write a function that determines whether at least one boolean value
   in its input list is true. *)

let rec exists (bools: bool list) : bool =
  begin match bools with
	| [] -> false
	| hd :: tl -> hd = true || exists tl
	end


let test () : bool =
  (exists [false; false]) = false
;; run_test "exists [false; false]" test 

let test () : bool =
  (exists [true; false; true]) = true
;; run_test "exists [true; false; true]" test 

let test () : bool =
  (exists [false; false; false; true]) = true
;; run_test "exists [false; false; false; true]" test 

let test () : bool =
  (exists [true; true; true; true]) = true
;; run_test "exists [true; true; true; true]" test 



(*************************************************************************)
(* Problem 6 (join) ****************************************************)

(* Write a function that takes a list of strings and "flattens" it into
   a single string. This function also takes an additional argument,  
   a separator string, which is interspersed between all of the strings in 
   the list. *)

(* Hint: the ^ operator concatenates two strings together. For example, 
   "a" ^ "bc" evaluates to "abc".  *)

let rec join (separator: string) (l : string list) : string = 
  begin match l with
	| [] -> ""
	| hd::[] -> hd ^ join separator []
	| hd::tl ->  hd ^ separator ^ join separator tl
	end

let test () : bool =
  (join "," ["a";"b";"c"]) = "a,b,c"
;; run_test "test_join1" test 

let test () : bool =
  (join "" ["a";"b";"c"]) =  "abc"
;; run_test "test_join2" test 

let test () : bool =
  (join "-" ["a";"b";"c"]) = "a-b-c"
;; run_test "test_join3" test 

let test () : bool =
  (join "x" []) = ""
;; run_test "test_join4" test 


(*************************************************************************)
(* Example (printing lists) **********************************************)

;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"
;; print_endline "Start of list printing example"

(* Once you have implemented the join function above, you can use it to print 
   out lists of strings. *)


;; print_endline (join "," ["a";"b";"c"])


(* If you would like to print a list of ints, you'll need to rewrite the join 
   function. *)

let rec int_join (separator: string) (l : int list) : string = 
    begin match l with
	| [] -> ""
	| hd::[] -> (string_of_int hd) ^ (int_join separator [])
	| hd::tl ->  (string_of_int hd) ^ separator ^ (int_join separator tl)
	end


;; print_endline ("[" ^ (int_join ";" [1;2;3]) ^ "]")


;; print_endline "End of list printing example"
;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"

(*************************************************************************)
(* Problem 7 (append) ****************************************************)

(* Write a function that takes lists l1 and l2 and returns a list
   containing all the items in l1 followed by all the items in l2. 

   OCaml provides the built in operator '@' that appends l1 and l2
   as in l1 @ l2.  Do *not* use the @ operator when completing this 
   problem.
*)

let rec append (l1:string list) (l2:string list) : string list =
 	begin match l1, l2 with
	| [], [] -> []
	| hd1::tl1, _ -> hd1 :: append tl1 l2
	| [], hd2::tl2 -> hd2::append l1 tl2


	end

let test () : bool =
  (append [] []) = []
;; run_test "append [] []" test

let test () : bool =
  (append ["1";"2"] ["3"]) = ["1";"2";"3"]
;; run_test "append [1;2] [3]" test

let test () : bool =
  (append [] ["4";"10"]) = ["4";"10"]
;; run_test "append [] [4;10]" test

let test () : bool =
  (append ["4";"10"] []) = ["4";"10"]
;; run_test "append [4;10] []" test



(*************************************************************************)
(* Problem 8 (finding dolls in a toy store) ******************************)

(* Write a function that checks whether a list of toys contains some
   particular toy. *)

let rec contains_str (list : string list) (elem : string) : bool =
  begin match list with
	| [] -> false
	| hd::tl -> if hd = elem then true else contains_str tl elem
	end

let test () : bool =
  (contains_str ["truck";"barbie";"top"] "barbie") = true
;; run_test "contains_str [truck; barbie; top] barbie" test

let test () : bool =
  (contains_str ["truck";"barbie";"top"] "woody") = false 
;; run_test "contains_str [truck; barbie; top] woody" test

let test () : bool =
  (contains_str [] "woody") = false
;; run_test "contains_str [] woody" test

let test () : bool =
  (contains_str ["woody"] "woody") = true
;; run_test "contains_str [woody] woody" test


(* Write a function that, given a list of toys and a list of dolls, filters the
   toys list so that only dolls remain. *)

let rec dolls_of (toys:string list) (dolls:string list) : string list =
	begin match toys, dolls with
	| [], _ -> []
	| _ , [] -> []
	| hd::tl, dolls -> if (contains_str dolls hd) then hd::(dolls_of tl dolls)  
										 else (dolls_of tl dolls)
	end

let test () : bool =
  (dolls_of ["truck";"barbie";"top"] ["barbie"; "woody"]) =  ["barbie"]
;; run_test "dolls_of [truck; barbie; top] [barbie; woody]" test

let test () : bool =
  (dolls_of [] ["barbie";"woody"]) = []
;; run_test "dolls_of [] [barbie; woody]" test

let test () : bool =
  (dolls_of ["top";"dreidel";"cone"] []) = []
;; run_test "dolls_of [top;dreidel;cone] []" test

let test () : bool =
  (dolls_of [] []) = []
;; run_test "dolls_of [] []" test


(*************************************************************************)
(* Problem 9 (merging lists) *********************************************)

(* Write a function that merges two input lists into a single list
   that contains all the elements from both input lists in alternating order: 
   the first, third, etc. elements come from the first input list and
   the second, fourth, etc. elements come from the second input list. 

   The length of the two lists do not need to be the same---the extra
   elements should just be at the end of the result.  *)

let rec merge (l1:int list) (l2:int list) : int list =
  begin match l1,l2 with
	| [],[] -> []
	| hd::tl, [] -> hd::(merge tl [])
	| [], hd::tl -> hd::(merge [] tl) 
	| hd1::tl1, l2 -> hd1::(merge l2 tl1)
	end


let test () : bool =
  (merge [1;3;5;7] [2;4;6;8]) = [1;2;3;4;5;6;7;8]
;; run_test "merge [1;3;5;7] [2;4;6;8]" test

let test () : bool =
  (merge [] [1;2;3]) = [1;2;3]
;; run_test "merge [] [1;2;3]" test

let test () : bool = 
  (merge [1;2;3] []) = [1;2;3]
;; run_test "merge [1;2;3] []" test

let test () : bool =
  (merge [] []) = []
;; run_test "merge [] []" test
  

(*************************************************************************)
(* Problem 10 (is_sorted) ************************************************)

(* Write a function that determines whether a given list of integers is
   SORTED -- that is, whether the elements appear in ascending order.  It's
   okay if the list has duplicates, so long as they're next to each other. *)

let rec is_sorted (l:int list) : bool =
  begin match l with
	| [] -> true
	| _::[] -> true
	| hd::md::tl -> if hd <= md then (is_sorted (md::tl)) else false
	end 

let test () : bool =
  (is_sorted [1;2;3]) = true
;; run_test "is_sorted [1;2;3]" test

let test () : bool =
  (is_sorted [3;2;1]) = false
;; run_test "is_sorted [3;2;1]" test

let test () : bool = 
  (is_sorted []) = true
;; run_test "is_sorted []" test

let test () : bool =
  (is_sorted [2]) = true
;; run_test "is_sorted[2]" test


(*************************************************************************)
(* Problem 11 (merge_sorted) *********************************************)

(* Write a function that takes two lists and, assuming they are both
   already sorted in ascending order, yields a merged list that is also
   sorted (and that contains all the elements from the two input lists). *)

let rec merge_sorted (l1:int list) (l2:int list) : int list =
	begin match l1, l2 with
	| [],[] -> []
	| hd1::tl1, hd2::tl2 -> if hd1 <= hd2 then hd1::(merge_sorted tl1 (hd2::tl2)) 
													else hd2::(merge_sorted (hd1::tl1) tl2)
	| [], hd::tl -> hd::merge_sorted [] tl
	| hd::tl, [] -> hd::merge_sorted tl []
	end

let test () : bool =
  (merge_sorted [2;7] [3;5;11]) = [2;3;5;7;11]
;; run_test "merge_sorted [2;7] [3;5;11]" test

let test () : bool =
  (merge_sorted [1;2;3] [4;5;6]) = [1;2;3;4;5;6]
;; run_test "merge_sorted [1;2;3] [4;5;6]" test

let test () : bool = 
  (merge_sorted [2;5;7] []) = [2;5;7]
;; run_test "merge_sorted [2;5;7]" test

let test () : bool =
  (merge_sorted [] []) = []
;; run_test "merge_sorted [] []" test


(*************************************************************************)
(* Problem 12 (permutations) *********************************************)

(* This one is a challenge problem, so we've made it worth 0 points --
   kudos only. *)

(* A PERMUTATION of a list l is a list that has the same elements as l
   but is not necessarily in the same order.

   Write a function that, given a list l, calculates ALL of the
   permutations of l (and returns them as a list).  For example,

       permutations [1;2;3]

   might yield

       [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]].

   (We say "might yield" here because we haven't specified the
   order of the permutations in the list returned by your function.
   For example, the result

       [[1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]; [1;2;3]]

   would also be correct.)

   Hint: Begin by writing a unit test or two, to make sure you understand the
   problem (even though you may need to rewrite them if your answer comes out
   in a different order, the exercise is useful).  Also, you'll probably want
   to break the problem down into one or more sub-problems, each of which can
   be solved by recursion. *)



(* Note: Do not remove or comment out this function stub, even if you
   choose not to attempt the challenge problem. Your file will not
   compile when you upload it for grading if 'permutations' is
   missing. *)

let rec permutations (l: int list) : int list list =
  failwith "permutations: unimplemented"

(* Note that you will also have to think about how to TEST
   permutations as there may be several correct solutions for each
   input. *)

(* The last part of this file is a print statement. When you see this line 
   as the result, you will know that all of the tests in this file have succeeded. *)
;; print_endline "intro.ml: ran to completion"

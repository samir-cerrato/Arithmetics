(* helper functions for testing -- you don't need to read these! *)

(* test a function that returns an int *)
fun testi (s : string) (n : int) (m : int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString m ^ "\n    Got: " ^ Int.toString n ^ "\n")

fun testii (s : string) (n : int * int) (m : int * int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => let val (x,y) = n
                     val (x',y') = m
                 in
                     print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString x' ^ " , " ^ Int.toString y'  ^ "\n    Got: " ^ Int.toString x ^ " , " ^ Int.toString y ^ "\n")
                 end

(* ********************************************************************** *)

(* READ THIS COMMENT!
 *
 * In this file there are various lines marked by a comment like so:
 *
 *    raise Fail "unimplemented"  (* DELETE THIS LINE *)
 *
 * You do not need to delete these lines immediately, but they should be gone by
 * the time you hand in your homework. They are placeholders for your
 * implementations of the functions specified in the homework. Without them,
 * this file would not load.
 *
 * If you remove such a line without implementing the function it is associated
 * with, this file will not load. Only remove such lines when you are ready to
 * implement their associated function.
 *)
(* Purpose: returns true if n is even, false otherwise.
   Assumes n is a natural number *)
fun evenP (n : int) : bool =
    case n
     of 0 => true
      | 1 => false
      | _ => evenP (n-2)

(* Purpose: returns true if n is odd, false otherwise.
   Assumes n is a natural number *)
fun oddP (n : int) : bool =
    case n
     of 0 => false
      | 1 => true
      | _ => oddP (n-2)

(* Purpose: returns m + n. Assumes m and n are natural numbers. *)
fun add (m : int, n : int) =
  case m of
    0 => n
  | _ => 1 + (add (m - 1, n))

(* Task: Implement and document this function. *)
(* DOCUMENTATION GOES HERE *)
(*Purpose: returns m * n. Assumes m and n are natural numbers.
    Examples:
    mult (4,3) = 12
    mult (0,1) = 0
    mult (1,2) = 1
    mult (2,3) = 6
    mult (4,5) = 20 *)
fun mult (m : int, n : int) : int =
    case n of
     0 => 0
   | _ => add(m, mult(m, (n-1))) 

fun test_mult() =
    (testi "m1" (mult(4,3)) 12 ;
     testi "m2" (mult(0,1)) 1 ;
     testi "m3" (mult (1,2)) 2 ;
     testi "m4" (mult (2,3)) 6 ;
     testi "m5" (mult (4,5)) 20
     )
        
(* Task: Implement and document this function. *)
(* DOCUMENTATION GOES HERE *)
(*Purpose: Divdes n by 2 and determines a pair of ints (x,y) such
that x + y = n, with x = y or x = y + 1
    Examples:
    halves 4 = (2,2)
    halves 2 = (1,1)
    halves 5 = (2,3)
    halves 6 = (3,3)
    halves 7 = (3,4)*)
fun halves (n : int) : int * int =
    case n of
     0 => (0,0)
   | 1 => (1,0)
   | _ => let val (x, y) = halves(n-2)
          in ((x+1), (y+1))
          end
          
fun test_halves() = 
    (testii "h1" (halves(4)) (2,2);
     testii "h2" (halves(2)) (1,1);
     testii "h3" (halves(5)) (2,3);
     testii "h4" (halves(6)) (3,3);
     testii "h5" (halves(7)) (3,4);
     )

(* Task: Implement this function. *)
(*  DOCUMENTATION GOES HERE *)
(*Purpose: divides n by d and evalutes the solution and the remainder
    Examples:
    divmod (100,10) = (10,0)
    divmod (10,3) = (3,1)
    divmod (20,5) = (4,0)
    divmod (33,10) = (3,3)
    divmod (25,25) = (1,0)*)
fun divmod (n : int, d : int) : int * int =
    case (d>n) of
      true => (0, n)
   | false => let val (q, r) = divmod((n-d), d)
              in (q + 1, r)
              end
    

fun test_divmod() =
    (testii "d1" (divmod(100,10)) (10,0);
     testii "d2" (divmod(10,3)) (3,1);
     testii "d3" (divmod(20,5)) (4,0);
     testii "d4" (divmod(33,10)) (3,3);
     testii "d5" (divmod(25,25)) (1,0)
     )

(* Task: Implement this function. *)
(* DOCUMENTATION GOES HERE *)
(*Purpose: sum the remainder of n divided by b with the value of
recursing on n divided by b*
Example: 
sum_digits(123,10) = 6
sum_digits(234,10) = 9
sum_digits(345,10) = 12
sum_digits(456,10) = 15
sum_digits(567,10) = 18)*)
fun sum_digits (n : int, b : int) : int =
    case n of
      0 => 0
    | _ => let val (q, r) = divmod(n,b)
               in r + sum_digits(q, b)
               end
    

fun test_sum_digits() =
    (testi "s1" (sum_digits(123,10)) 6;
     testi "s2" (sum_digits(234,10)) 9; 
     testi "s3" (sum_digits(345,10)) 12;
     testi "s4" (sum_digits(456,10)) 15;
     testi "s5" (sum_digits(567,10)) 18
     )

fun run() =
    (test_mult();
     test_halves();
     test_divmod();
     test_sum_digits())


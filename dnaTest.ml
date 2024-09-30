;; open Assert
;; open Dna
;; open Helices

(* The assertion library by default will run _all_ of the test cases
   associated with this program.  However, while debugging you may
   prefer to have the testing stop on the first failure that is
   encountered. Comment the line below to turn off first-failure. *)
;; stop_on_failure ()


(******************************************************************************)
(********************** REMEMBER TO WRITE TESTS FIRST! ************************)
(********************* WRITE ALL TESTS BELOW THIS POINT ***********************)
(******************************************************************************)

(* If you'd like to test your helper functions, you should do that in dna.ml.
   The only way you can test them in this file is by declaring them in
   dna.mli - however if you do so your code won't compile with the grading
   server because you don't submit your own dna.mli!

   If you'd like to write trees to use in your tests, put them in this file.
   When writing trees of depth greater than 1 for tests, please indent all
   children by 4 spaces, separate children by line breaks, and place parentheses
   similar to the following tree:

   LNode (
       LNode (
           LLeaf h1,
           h2,
           LLeaf h3
       ),
       h4,
       LLeaf h5
   )

*)

(************************ complementary_helix tests ***************************)

(* Here's an example test case.  Be sure to fill in the incomplete test cases
   we provided. *)

let test () : bool =
  complementary_helix [T; A] = [A; T]
;; run_test "complementary_helix singleton" test

let test () : bool =
  complementary_helix [C; T; T; C] = [G; A; A; G]
;; run_test "complementary_helix multi-element list" test

let test () : bool =
  complementary_helix [C; T; A; A; T; G; T] = [G; A; T; T; A; C; A]
;; run_test "complementary_helix multi-element list with all nucleotides" test

let test() : bool = 
  complementary_helix [] = [] 
;; run_test "complementary_helix empty list" test 

(*************************** hamming_distance tests ***************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
 * tests for this problem. *)

let test () : bool =
  hamming_distance [G; A] [G; T] = 1
;; run_test "hamming_distance one different nucleotide" test

let test () : bool = 
  hamming_distance [G; A; C; T; A] [G; A; C; T; A] = 0
;; run_test "hamming_distance all same nucleotides" test

let test () : bool = 
  hamming_distance [] [] = 0
;; run_test "hamming_distance both empty lists" test

let test () : bool = 
  hamming_distance [C; T; G; A; T] (complementary_helix [C; T; G; A; T])  = 5
;; run_test "hamming_distance all different nucleotides" test





(************************ decreasing_similarity tests *************************)

(* Be sure to add your own test cases -- we will be grading the tests for this
   problem. *)

let test() : bool = 
  (decreasing_similarity [])
;; run_test "decreasing_similarity empty list" test

let test() : bool = 
  (decreasing_similarity [chimpanzee])
;; run_test "decreasing_similarity singleton" test

let test() : bool = 
  (decreasing_similarity [chimpanzee; gorilla; orangutan; siamang; 
  pileated_gibbon; white_cheeked_gibbon; lar_gibbon]) 
;; run_test "decreasing_similarity on question_most_like_human list" test

let test() : bool = 
  not (decreasing_similarity [lar_gibbon; white_cheeked_gibbon; siamang]) 
;; run_test "decreasing_similarity increasing similarity" test

let test() : bool = 
  not (decreasing_similarity [chimpanzee; gorilla; siamang; orangutan])
;; run_test "decreasing_similarity decreasing similarity until the last
element" test


(**************************** acids_of_helix tests ****************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
   tests for this problem.  Also be sure to fill in the incomplete test case
   we provided. *)

let test () : bool =
  acids_of_helix [A; G; T; A; C] = [Met]
;; run_test "acids_of_helix single codon" test

let test () : bool =
  (acids_of_helix [A; G; T; A; C; T; A; C; T; C; C]) = [Met; Met; Arg] 
;; run_test "acids_of_helix multiple Mets at start" test

let test () : bool =
  (acids_of_helix [A; G; T; A; C; T; A; C; T; C; C; A; G; C; A; T; C; A; G; A]) 
  = [Met; Met; Arg; Ser] 
;; run_test "acids_of_helix end codon" test

let test () : bool =
  (acids_of_helix []) = [] 
;; run_test "acids_of_helix empty list" test

let test () : bool =
  (acids_of_helix [A]) = [] 
;; run_test "acids_of_helix singleton" test

let test () : bool =
  (acids_of_helix [A; G; C; T; A; A; C; G; T]) = [] 
;; run_test "acids_of_helix No Met" test

let test () : bool =
  (acids_of_helix [A; T; T; C; G; A; T; A; C; A; A; A]) = [Met; Phe] 
;; run_test "end codon before met" test


(************ Kudos problem: all_acids_of_helix tests *************************)

(* These tests are commented out because they are for the kudos problem.
   If you attempt this problem, remember to uncomment the tests. And write
   some of your own! *)

(*
let test () : bool =
  all_acids_of_helix [T; A; C; A; C; T] = [[Met]]
;; run_test "all_acids_of_helix  [T; A; C; A; C; T]" test

let test () : bool =
  all_acids_of_helix [T; A; C; A; C; T; T; A; C; A; C; T] = [[Met]; [Met]]
;; run_test "all_acids_of_helix [T; A; C; A; C; T; T; A; C; A; C; T]" test

let test () : bool =
  all_acids_of_helix [T; A; C; A; C; T; G; A; T; A; C; A; C; T] = [[Met]; [Met]]
;; run_test "all_acids_of_helix [T; A; C; A; C; T; G; A; T; A; C; A; C; T]" test
*)


(***************************** count_leaves tests *****************************)

(* Here are two example tests cases. Both trees should contain exactly four
   species. Don't forget to add your own test cases! *)

let test () : bool =
  count_leaves (greater_apes ()) = 4
;; run_test "count_leaves greater_apes" test

let test () : bool =
  count_leaves (lesser_apes ()) = 4
;; run_test "count_leaves lesser_apes" test

let test () : bool =
  count_leaves (Leaf chimpanzee) = 1
;; run_test "count_leaves just leaf" test

let test () : bool = 
  count_leaves (Node (greater_apes (), lesser_apes ())) = 8
;; run_test "count_leaves total tree with both greater and lesser apes" test


let test () : bool = 
  count_leaves (Node (Leaf chimpanzee, Leaf gorilla)) = 2
;; run_test "count_leaves tree only one node with two leaves" test

let test () : bool = 
  count_leaves 
  (Node (
    Node (
      Node (
        Leaf gorilla, Leaf chimpanzee), 
      Node (
        Leaf orangutan, Leaf human)
      ),
  Leaf human)) = 5

;; run_test "count_leaves complex tree" test

(**************************** helix_of_tree tests *****************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
   tests for this problem. *)

let test () : bool =
  helix_of_evtree (LNode (LLeaf [T], [A], LLeaf [G])) = [A]
;; run_test "helix_of_evtree lnode" test

let test () : bool =
  let helix_seq = [C; A; G; T; C] in
  helix_of_evtree (LLeaf helix_seq) = helix_seq
;; run_test "helix_of_evtree only leaf" test

let test () : bool =
  helix_of_evtree (LNode (LLeaf [], [], LLeaf [])) = []
;; run_test "helix_of_evtree lnode with only empty lists" test

let test () : bool =
  let helix_seq = [C; A; G; T; C] in
  let helix_seq2 = [A; T; G; A; A] in 
  helix_of_evtree (
  LNode ( 
    LNode ( 
      LLeaf [G], 
      [A; G; T],
      LNode (
        LLeaf [A],
        helix_seq,
        LLeaf [A]
      )
    ), 
    helix_seq2,
    LLeaf [T]
  )
  ) = helix_seq2
;; run_test "helix_of_evtree depth 4" test

let test () : bool =
  let labeled_t:labeled_evtree = 
    LNode ( 
      LNode ( 
        LLeaf [G], 
        [A; G; T],
        LLeaf [A]
      ), 
      [],
      LLeaf [T]
    ) in
  helix_of_evtree (labeled_t) = []
;; run_test "helix_of_evtree depth 3" test


(*************************** unlabel_evtree tests *****************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
   tests for this problem. *)

let test () : bool =
  unlabel_evtree (LNode (LLeaf [T], [A], LLeaf [G])) =
  Node (Leaf [T], Leaf [G])
;; run_test "unlabel_evtree depth-2 evtree" test

let test () : bool =
  unlabel_evtree (LLeaf [G; A; C]) = Leaf [G; A; C]
;; run_test "unlabel_evtree only leaf" test

let test () : bool = 
  let seq1 = [C; G; A; T] in
  let seq2 = [G; G; A] in
  let seq3 = [A; T; C; G] in
  let node1_helix = (guess_parent_helix seq1 seq2) in
  let t:labeled_evtree = 
  LNode (
    LNode ((LLeaf seq1), node1_helix, (LLeaf seq2)),
    (guess_parent_helix node1_helix seq3),
    LNode (LLeaf seq3, seq3, LLeaf seq3)
  ) in
  unlabel_evtree t =  Node (
    Node (Leaf seq1, Leaf seq2), 
    Node (Leaf seq3, Leaf seq3)
  ) 
  
;; run_test "unlabel_evtree depth-3 evtree" test 

let test() : bool = 
  let seq1 = [C; G; A; T] in
  let seq2 = [G; G; A] in
  let seq3 = [A; T; C; G] in
  let seq4 = [A] in
  let seq5 = [G; G; G; G] in 
  let seq6 = [T; T] in 
  (*these labels don't matter bc we are unlabeling the tree*)
  let dep2_helix2 = [A; A; G; G] in
  let dep4_helix = [A; A] in
  let dep3_helix1 = [G;A;G] in
  let dep2_helix1 = [G; G] in
  let dep1_helix = [G; G; G] in
  let t: evtree = 
    Node (
      Node (
        Node (
          Node (
            Leaf seq1, Leaf seq2
          ),
          Node (
            Leaf seq3, Leaf seq3
          )
        ), 
        Leaf seq4
      ), 
      Node (
        Leaf seq5, Leaf seq6
      )
    ) in

  let labeled_t = 
    LNode (
      LNode (
        LNode (
          LNode (
            LLeaf seq1, dep4_helix, LLeaf seq2
          ),
          dep3_helix1, 
          LNode (
            LLeaf seq3, seq3, LLeaf seq3
          )
        ), 
        dep2_helix1,
        LLeaf seq4
      ), 
      dep1_helix, 
      LNode (
        LLeaf seq5, dep2_helix2, LLeaf seq6
      )
    ) in
    unlabel_evtree labeled_t = t

;; run_test "unlabel_evtree depth-5 evtree" test 

(************************* guess_parent_helix tests ***************************)

(* Here's an example test case.  Be sure to fill in the incomplete test case
   we provided *)

let test () : bool =
  guess_parent_helix [T; C; A] [G; C; A] = [A; C; A]
;; run_test "guess_parent_helix one difference" test

let test () : bool =
  guess_parent_helix [A; C; G; G; T; A; C]
    [C; T; G; C; T; A; A] = [A; A; G; A; T; A; A]
;; run_test "guess_parent_helix multiple differences" test


let test () : bool =
  guess_parent_helix [A; C; G;]
    [C; T; G; C; T; T; T] = [A; A; G; C; T; T; T]
;; run_test "guess_parent_helix x1 shorter than x2" test

let test () : bool =
  guess_parent_helix [] [] = []
;; run_test "guess_parent_helix empty lists" test

let test () : bool =
  guess_parent_helix [G] [C] = [A]
;; run_test "guess_parent_helix singletons" test

let test() : bool = 
  let seq = [C; T; G; C; T; T; T] in
  guess_parent_helix seq seq = seq
;; run_test "guess_parent_helix no differences" test

(************************ add_ancestor_labels tests ***************************)

(* Here's an example test case.  Be sure to fill in the incomplete test cases
   we provided and don't forget to add your own test cases! *)

let test () : bool =
  add_ancestor_labels (Node (Leaf [T], Leaf [G])) =
  LNode (LLeaf [T], [A], LLeaf [G])
;; run_test "add_ancestor_labels depth-2 evtree" test

let test () : bool =
  add_ancestor_labels (Node (Leaf [T; C], Leaf [T; C])) =
  LNode (LLeaf [T; C], [T; C], LLeaf [T; C])
  
;; run_test "add_ancestor_labels also depth-2 evtree" test

let test () : bool =
  add_ancestor_labels (Leaf [T; C]) = LLeaf [T; C]
;; run_test "add_ancestor_labels leaf" test


let test () : bool =
  let seq1 = [C; G; A; T] in
  let seq2 = [G; G; A] in
  let seq3 = [A; T; C; G] in
  let node1_helix = (guess_parent_helix seq1 seq2) in
  let t:evtree = 
  
  Node (
    Node (Leaf seq1, Leaf seq2), 
    Node (Leaf seq3, Leaf seq3)
  ) in
  
  add_ancestor_labels t = 
  LNode (
    LNode ((LLeaf seq1), node1_helix, (LLeaf seq2)),
    (guess_parent_helix node1_helix seq3),
    LNode (LLeaf seq3, seq3, LLeaf seq3)
  )
;; run_test "add_ancestor_labels depth-3 evtree" test

let test() : bool = 
  let seq1 = [C; G; A; T] in
  let seq2 = [G; G; A] in
  let seq3 = [A; T; C; G] in
  let seq4 = [A] in
  let seq5 = [G; G; G; G] in 
  let seq6 = [T; T] in 
  
  let dep2_helix2 = guess_parent_helix seq5 seq6 in
  let dep4_helix = (guess_parent_helix seq1 seq2) in
  let dep3_helix1 = guess_parent_helix dep4_helix seq3 in
  let dep2_helix1 = guess_parent_helix dep3_helix1 seq4 in
  let dep1_helix = guess_parent_helix dep2_helix1 dep2_helix2 in
  let t: evtree = 
    Node (
      Node (
        Node (
          Node (
            Leaf seq1, Leaf seq2
          ),
          Node (
            Leaf seq3, Leaf seq3
          )
        ), 
        Leaf seq4
      ), 
      Node (
        Leaf seq5, Leaf seq6
      )
    ) in

  let labeled_t = 
    LNode (
      LNode (
        LNode (
          LNode (
            LLeaf seq1, dep4_helix, LLeaf seq2
          ),
          dep3_helix1, 
          LNode (
            LLeaf seq3, seq3, LLeaf seq3
          )
        ), 
        dep2_helix1,
        LLeaf seq4
      ), 
      dep1_helix, 
      LNode (
        LLeaf seq5, dep2_helix2, LLeaf seq6
      )
    ) in
    add_ancestor_labels t = labeled_t

;; run_test "add_ancestor_labels depth-5 evtree" test

(************************ parent_child_hamming tests **************************)

(* Here we give you one example test.  Add your own tests--we will grade the
   test cases for this problem. Be sure to test for evtrees of greater depth. *)

let test () : bool =
  parent_child_hamming (LNode (LLeaf [T], [A], LLeaf [G])) = 2
;; run_test "parent_child_hamming depth-2 evtree, all different" test

let test() : bool = 
  parent_child_hamming (LLeaf [T; A; C; G; A]) = 0
;; run_test "parent_child_hamming only leaf" test

let test() : bool = 
  parent_child_hamming (LLeaf []) = 0
;; run_test "parent_child_hamming only leaf with empty list" test

let test () : bool =
  parent_child_hamming (
    LNode (LLeaf [T; A; C; G; A], [T; A; C; G; A], LLeaf [T; A; C; G; A])) = 0
;; run_test "parent_child_hamming depth-2 evtree, all same" test

let test () : bool =
  let seq1 = [C; G; A; T] in
  let seq2 = [G; G; A] in
  let seq3 = [G; G; G; G] in 
  let seq4 = [T; T] in 
  let dep2_helix1 = guess_parent_helix seq1 seq2 in
  let dep2_helix2 = guess_parent_helix seq3 seq4 in
  let dep1_helix = guess_parent_helix dep2_helix1 dep2_helix2 in
  parent_child_hamming (
    LNode (
      LNode (LLeaf seq1, dep2_helix1, LLeaf seq2), 
      dep1_helix,
      LNode (LLeaf seq3, dep2_helix2, LLeaf seq4)
    )
  ) = hamming_distance seq1 dep2_helix1 + hamming_distance seq2 dep2_helix1
  + hamming_distance seq3 dep2_helix2 + hamming_distance seq4 dep2_helix2 
  + hamming_distance dep2_helix1 dep1_helix 
  + hamming_distance dep2_helix2 dep1_helix
;; run_test "parent_child_hamming depth-3 evtree" test

let test() : bool = 
  let seq1 = [C; G; A; T] in
  let seq2 = [G; G; A] in
  let seq3 = [A; T; C; G] in
  let seq4 = [A] in
  let seq5 = [G; G; G; G] in 
  let seq6 = [T; T] in 
  
  let dep2_helix2 = guess_parent_helix seq5 seq6 in
  let dep4_helix = (guess_parent_helix seq1 seq2) in
  let dep3_helix1 = guess_parent_helix dep4_helix seq3 in
  let dep2_helix1 = guess_parent_helix dep3_helix1 seq4 in
  let dep1_helix = guess_parent_helix dep2_helix1 dep2_helix2 in
  parent_child_hamming (
    LNode (
      LNode (
        LNode (
          LNode (
            LLeaf seq1, dep4_helix, LLeaf seq2
          ),
          dep3_helix1, 
          LNode (
            LLeaf seq3, seq3, LLeaf seq3
          )
        ), 
        dep2_helix1,
        LLeaf seq4
      ), 
      dep1_helix, 
      LNode (
        LLeaf seq5, dep2_helix2, LLeaf seq6
      )
    )
  ) = hamming_distance seq1 dep4_helix + hamming_distance seq2 dep4_helix
  + hamming_distance seq5 dep2_helix2 + hamming_distance seq6 dep2_helix2
  + hamming_distance dep1_helix dep2_helix2 
  + hamming_distance dep1_helix dep2_helix1
  + hamming_distance seq4 dep2_helix1 + hamming_distance dep3_helix1 dep2_helix1
  + hamming_distance dep4_helix dep3_helix1 + hamming_distance seq3 dep3_helix1

;; run_test "parent_child_hamming depth-5 evtree, all combinations" test



(*************************** simplest_evtree tests ****************************)

(* Here are two example test cases. Don't forget to add your own test cases!
 *
   Note: If you want to write a test that fails on purpose, then you can use
   run_failing_test. run_failing_test will fail if the function returns true or
   false, and will pass if the function throws an error *)

let test () : bool =
  simplest_evtree [] = (LLeaf [], 0)
;; run_failing_test "simplest_evtree empty" test

let test () : bool =
  let t1 = LNode (LLeaf [A], [T], LLeaf [C]) in
  let t2 = LNode (
               LLeaf [G; T],
               [A; T],
               LNode (
                   LLeaf [T; T],
                   [T; T],
                   LLeaf [C; G]
               )
           ) in
  simplest_evtree [t1; t2] = (t1, 2)
;; run_test "simplest_evtree two evtree list" test

let test () : bool =
  simplest_evtree [LLeaf [A;T]] = (LLeaf [A; T], 0)
;; run_test "simplest_evtree singleton being leaf" test

let test () : bool =
  let t1 = LNode (
               LLeaf [G; T],
               [A; T],
               LNode (
                   LLeaf [T; T],
                   [T; T],
                   LLeaf [C; G]
               )
           ) in 
  let t2 = LNode (
    LLeaf [G],
    [A],
    LNode (
      LLeaf [T],
      [T],
      LLeaf [T]
    )
  ) in
  simplest_evtree [t1; t2] = (t2, 2)
;; run_test "simplest_evtree two evtree with same depth" test

let test () : bool =
  (*depth 2 evtree*)
  let t1 = LNode (LLeaf [G; G; C; G; G; G; C; C; C], [A; A; A; A; A], 
    LLeaf (complementary_helix [G; G; C; G; G; G; C; C; C])) in 
  (*depth 3 evtree*)
  let t2 = 
  LNode (
    LLeaf [G],
    [A; A; A; A; A; A; A],
    LNode (
      LLeaf [T; A; G; C; C; C; C],
      [T; A; A; A; A; A; A;],
      LLeaf [T; G; C; G; G; G; G]
    )
  ) in
  (*depth 4 evtree*)
  let t3 = 
    LNode (
      LNode (
        LNode (
          LLeaf [G; G], 
          [G; G], 
          LLeaf [G; G]
        ), 
        [A; G],
        LLeaf [A]
      ), 
      [A; G],
      LNode (
        LLeaf [C],
        [A],
        LLeaf [A]
      )
    ) in


  simplest_evtree [t1; t2; t3] = (t3, parent_child_hamming t3)
;; run_test "simplest_evtree three evtrees with diff depths" test


(******************** simplest_greater_ape_evtree tests ***********************)

(* Refactoring is the process of rewriting your code to make it simpler and
   more general without changing its behavior. Therefore, it is easy to write
   test cases for the new version---they should compute the same answers
   that we did before. Because of this, we don't have to write any new test
   cases! *)

let test () : bool =
  find_simplest_evtree gorilla human chimpanzee orangutan =
  simplest_greater_ape_evtree ()
;; run_test "simplest_greater_ape_evtree" test

let test () : bool =
  find_simplest_evtree lar_gibbon pileated_gibbon siamang white_cheeked_gibbon =
  simplest_lesser_ape_evtree ()
;; run_test "simplest_lesser_ape_evtree" test

;; print_endline "dnaTest.ml: ran to completion."

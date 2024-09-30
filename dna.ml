;; open Assert
;; open Helices

(* The Assert library by default will run _all_ of the test cases
   associated with this program.  While debugging, you may prefer to
   have testing stop and report the first failure that is encountered;
   the command below requests this behavior.  Remove or comment out
   the line if you want to see all of the test errors at once. *)
;; stop_on_failure ()

(* -------------------------------------------------------------------------- *)
(* ----- Computing Human Evolution ------------------------------------------ *)
(* -------------------------------------------------------------------------- *)

(* Biologists estimate the "relatedness" of different species by comparing
   their genetic codes. As techniques to gather genetic information
   have become more sophisticated, it has become common to use computer
   programs to aid in analyzing genetic information. Your task in this homework
   is to develop functions that help in understanding genetic data. In
   particular, one task that you will perform is to use this data to determine
   the genetic similarity of different species. You will be doing this by
   analyzing sample DNA (provided below) for several ape species.

   To achieve this goal, you will be guided through writing several OCaml
   functions that can be combined for processing DNA strands and generating
   candidate genetic trees to be used for analysis.

   DO NOT FEAR THE SCIENCE! While this assignment uses real biological data, we
   will walk you through all the necessary concepts and terminology.

   Happy coding! :) *)

(* Unlike the previous homework, we've given you a number of INCOMPLETE tests in
   the dnaTest.ml file. The first provided test for each problem is
   ready to go, but for some of the test cases you will need to
   provide the expected value, which you should calculate by
   hand. Before implementing each function, complete its corresponding
   test section in dnaTest.ml. We use the OCaml primitive "failwith"
   to cause an error so you know you have to fill in something.

   The test cases for all functions in this assignment are found in the file
   dnaTest.ml. *)

(* You can view supplementary links and images at

     https://www.cis.upenn.edu/~cis1200/current/hw/hw02/

   Biologists use trees (Figure A on the above web page) to show how species
   evolved (or might have evolved) from ancestor species. To practice using
   enumeration and recursion over lists and trees, you are going to
   write a program that automatically generates hypothesis trees like the one
   in Figure A.
*)

(* -------------------------------------------------------------------------- *)
(* ----- DNA Data representation -------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Your program inputs will be real samples of DNA, the genetic code that
   describes how to build organisms (Figure B).

   Each double helix of DNA consists of two complementary strands,
   each one a sequence of the nucleotides adenine, thymine, guanine,
   and cytosine. Adenine always appears opposite thymine, and guanine
   always appears opposite cytosine.

   At the top of this file, we imported the type definitions for
   nucleotides and helices from `helices.ml`.  We have duplicated the
   definitions here for your convenience:

   type nucleotide =
    | G (* guanine *)
    | C (* cytosine *)
    | A (* adenine *)
    | T (* thymine *)

    type helix = nucleotide list *)

(* -------------------------------------------------------------------------- *)
(* ----- Ape DNA Sequences -------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* In `helices.ml`, we have also defined DNA sequences taken from the
   genetic code of 8 different ape species. This is real data
   extracted from the Entrez Nucleotide database
   (http://www.ncbi.nlm.nih.gov/nuccore)

   Feel free to take a look at that file and see the sequence data! *)

(* To help with printing trees that contain this data, we provide the
   following function to convert helices to the corresponding species
   name. *)

(* Given a helix, output the ape name (e.g. "Orangutan") or "Not an
   Ape!" if the helix doesn't match any of the ape helices. *)

let ape_names : (helix * string) list =
  [ (lar_gibbon,           "Lar Gibbon")
  ; (pileated_gibbon,      "Pileated Gibbon")
  ; (siamang,              "Siamang")
  ; (white_cheeked_gibbon, "White Cheeked Gibbon")
  ; (orangutan,            "Orangutan")
  ; (gorilla,              "Gorilla")
  ; (chimpanzee,           "Chimpanzee")
  ; (human,                "Human")
  ]

let rec lookup_ape_name (ape:helix) (l:(helix * string) list) : string =
  begin match l with
  | [] -> "Not an Ape!"
  | (h,name)::hs -> if ape = h then name else lookup_ape_name ape hs
  end

let string_of_ape (ape: helix) : string =
  lookup_ape_name ape ape_names


(* -------------------------------------------------------------------------- *)
(* ----- Problem 1: Compute the complementary helix ------------------------- *)
(* -------------------------------------------------------------------------- *)

(* For this assignment, we represent individual nucleotides using an
   enumeration (user defined datatype) and represent DNA sequences using
   a list of nucleotides. The list corresponds to just one helix in the
   double helix. However, since nucleotides always appear in a complementary
   fashion, we -- i.e., you :) -- can easily calculate the other helix... *)

(* Given a single helix, compute the other half of the double helix. Remember
   to start each problem by first finishing the corresponding test section in
   dnaTest.ml. *)

let rec complementary_helix (x: helix) : helix =
  (*A-T and C-G*)
  begin match x with 
  |[] -> []
  |head::tail -> 
    if head = A then T::(complementary_helix tail)
    else if head = T then A::(complementary_helix tail)
    else if head = G then C::(complementary_helix tail)
    else G::(complementary_helix tail)
  end


(* -------------------------------------------------------------------------- *)
(* ----- Problem 2: Compute the hamming distance between two helices -------- *)
(* -------------------------------------------------------------------------- *)

(* Given two equal length helices, your next task is to compute the number of
   differences between them, i.e., the number of nucleotides that would need
   to be changed to transform one helix into the other.

   NOTE: You should ASSUME that this function will always be applied to two
   lists with the same number of elements.  Do NOT test the lengths of the
   lists when implementing this function.  If you do so, your code may be
   ungradeable because it is TOO SLOW (e.g., it would require a length
   calculation to occur at every recursive invocation).  It does not matter
   what your function returns when applied to different length lists. We
   won't test that case. *)

let rec hamming_distance (x1: helix) (x2: helix) : int =
  begin match x1, x2 with 
  |head1::tail1, head2::tail2 -> 
    (*if the two elements aren't the same, add 1 to hamming distance*)
    if head1 = head2 then hamming_distance tail1 tail2
    else 1 + hamming_distance tail1 tail2
  (*lists will be the same # of elements, so only [] []*)
  |_,_ -> 0 
  end

(* We can use the hamming_distance function to determine the similarities
   between various species.

   After you have a correct implementation of hamming_distance,
   uncomment the following printing commands, run the program to see
   the output in the console, and then fill in the answer to
   'question_most_like_human' below. *)

let mutation_string (ape: helix) : string =
  "Number of mutations between humans and " ^ (string_of_ape ape)
  ^ ": " ^ (string_of_int (hamming_distance human ape)) ^ "\n"


   ;; print_endline (mutation_string gorilla)
   ;; print_endline (mutation_string lar_gibbon)
   ;; print_endline (mutation_string pileated_gibbon)
   ;; print_endline (mutation_string siamang)
   ;; print_endline (mutation_string white_cheeked_gibbon)
   ;; print_endline (mutation_string orangutan)
   ;; print_endline (mutation_string chimpanzee)


(* Next, based on the number of mutations that were printed in the
   console, order the seven non-human ape helices by their similarity
   to humans, with the *most* human-like helix appearing first in the
   list.

   Your answer should be hard-coded, e.g. [chimp; gorilla; ...]

   Note that you may have to scroll up in the terminal window to see
   the output! *)

let question_most_like_human () : helix list =
  [chimpanzee; gorilla; orangutan; siamang; pileated_gibbon; 
  white_cheeked_gibbon; lar_gibbon]

(* To test your answer, write a function that determines whether a
   list of helices is ordered in STRICTLY DECREASING similarity to humans.
   (This function should return true for lists that are empty or that
   contain exactly one helix.) *)

let rec decreasing_similarity (apes: helix list) : bool =
  begin match apes with 
  (*the number of mutations should increase as we go down the list*)
  |head::second::tail -> 
    (hamming_distance head human) < (hamming_distance second human) &&
    decreasing_similarity (second::tail)
  (*singleton or empty will always be true*)
  |_ -> true
  end

(* -------------------------------------------------------------------------- *)
(* ----- Problem 3: Decode a helix to get an amino acid chain --------------- *)
(* -------------------------------------------------------------------------- *)

(* DNA describes how to build organisms by encoding instructions for making
   amino acid chains, which in turn enable basic cellular functions. In this
   problem, you will write functions that determine the amino acid chains that
   are encoded in a particular helix.

   The key to this decoding is that every nucleotide triplet in the helix
   encodes one of 20 acids, or else indicates the end of the chain. *)

type acid =
  | Ala | Arg | Asn | Asp | Cys | Glu | Gln | Gly | His | Ile | Leu | Lys | Met
  | Phe | Pro | Ser | Thr | Trp | Tyr | Val | END

(* Given a nucleotide triplet, the function acid_of_triplet outputs a decoded
   acid or END. *)

let acid_of_triplet (n1: nucleotide) (n2: nucleotide) (n3: nucleotide)
  : acid =
  begin match (n1, n2, n3) with
    | (A, A, A) -> Phe | (A, A, G) -> Phe | (A, A, T) -> Leu | (A, A, C) -> Leu
    | (G, A, A) -> Leu | (G, A, G) -> Leu | (G, A, T) -> Leu | (G, A, C) -> Leu
    | (T, A, A) -> Ile | (T, A, G) -> Ile | (T, A, T) -> Ile | (T, A, C) -> Met
    | (C, A, A) -> Val | (C, A, G) -> Val | (C, A, T) -> Val | (C, A, C) -> Val
    | (A, G, A) -> Ser | (A, G, G) -> Ser | (A, G, T) -> Ser | (A, G, C) -> Ser
    | (G, G, A) -> Pro | (G, G, G) -> Pro | (G, G, T) -> Pro | (G, G, C) -> Pro
    | (T, G, A) -> Thr | (T, G, G) -> Thr | (T, G, T) -> Thr | (T, G, C) -> Thr
    | (C, G, A) -> Ala | (C, G, G) -> Ala | (C, G, T) -> Ala | (C, G, C) -> Ala
    | (A, T, A) -> Tyr | (A, T, G) -> Tyr | (A, T, T) -> END | (A, T, C) -> END
    | (G, T, A) -> His | (G, T, G) -> His | (G, T, T) -> Gln | (G, T, C) -> Gln
    | (T, T, A) -> Asn | (T, T, G) -> Asn | (T, T, T) -> Lys | (T, T, C) -> Lys
    | (C, T, A) -> Asp | (C, T, G) -> Asp | (C, T, T) -> Glu | (C, T, C) -> Glu
    | (A, C, A) -> Cys | (A, C, G) -> Cys | (A, C, T) -> END | (A, C, C) -> Trp
    | (G, C, A) -> Arg | (G, C, G) -> Arg | (G, C, T) -> Arg | (G, C, C) -> Arg
    | (T, C, A) -> Ser | (T, C, G) -> Ser | (T, C, T) -> Arg | (T, C, C) -> Arg
    | (C, C, A) -> Gly | (C, C, G) -> Gly | (C, C, T) -> Gly | (C, C, C) -> Gly
  end

(* Your next task: given a helix, decode its first acid chain. This is
   done as follows...

   1. Start decoding by scanning the helix for the first occurrence of
      the sequence [T; A; C] (which encodes the acid Met). This starts
      an acid chain with Met as its head element.

   2. After the first occurrence of [T; A; C], decode the following
      nucleotides in the helix in triplets and build a list of the
      results.

   3. Stop decoding when either (a) we run out of triplets or (b) we
      encounter one of the triplets encoding the END acid ([A; T; T],
      [A; C; T], or [A; T; C]). If we encounter the END acid, the END
      marker should NOT be included in the output acid list.

   You should disregard any nucleotides that might happen to occur
   before the first occurrence of Met (including END markers).  *)
  
(*helper func: calc rest of the sequence after the MET codon is found*)
let rec calc_acids (x: helix) : acid list = 
  begin match x with
    (*need to have at least three nucleotides to form an acid*)
  | first::second::third::tail -> 
    (*STOP if encountering an END acid*)
    if first = A && (second = T || second = C) && (third = C || third = T) 
      then []
    (*calculating the acid and then calling function on the remaining list*)
    else (acid_of_triplet first second third)::(calc_acids tail)
  | _ -> []
  end

let rec acids_of_helix (x: helix) : acid list =
  begin match x with
  |first::second::third::tail -> 
    (*look for the MET sequence*)
    if first = T && second = A && third = C then Met::(calc_acids tail)
    else acids_of_helix (second::third::tail)
  (*need to be at least three nucleotides to have it equate to an acid*)
  |_ -> []
  end

(* -------------------------------------------------------------------------- *)
(* ----- Kudos problem: Decode all acid chains in a helix ------------------- *)
(* -------------------------------------------------------------------------- *)

(* The next problem is not required. It is worth zero points, but it
   will challenge your ability to work with recursive functions over
   lists. If you want to attempt this problem, you may want to come
   back to it after you have finished the rest of the assignment.
   If you choose not to complete this problem, do not comment out
   the stub below, as it is necessary for the autograder to compile your
   homework. *)

(* Problem 3 above asks you to decode the first acid chain of a
   helix. However, a given helix may actually describe a sequence of
   several chains. Your job now is to write a function that produces
   *all* of these chains, not just the first one.

   A few points to observe:
   - You should look for the next acid chain after the end triplet of the
     previous chain.
   - The beginning of the next chain (TAC) may not be immediately
     after the end of the previous one: there could be any number of
     nucleotides in between.
   - You should not decode overlapping chains. If, in the middle
     of one amino acid-encoding nucleotide sequence there's another
     T-A-C, you shouldn't also return that acid list.
   - We've given you a few tests for this problem in dnaTest.ml. *)

let rec all_acids_of_helix (x: helix) : acid list list =
  failwith "all_acids_of_helix: unimplemented"

(* -------------------------------------------------------------------------- *)
(* ----- Problem 4:  Representing phylogenetic trees ------------------------ *)
(* -------------------------------------------------------------------------- *)

(* ----- READ THIS BEFORE STARTING, NO CODE REQUIRED UNTIL INSTRUCTED -----

    Now that we have a DNA datatype and some ape data, we can analyze
   this data to figure out the relationship between the various
   present-day species. That relationship is described by
   evolutionary (or phylogenetic) trees, which show how the common
   ancestors for each species relate to each other. Specifically, we
   will take four ape helices and:

   1. First, generate all possible evolutionary trees with the ape DNA
      at the leaves. Each tree represents a possible set of family
      relationships among the given species.

   2. Next, take these generated trees and fill in hypothetical
      genetic codes for the ancestor species. These hypothetical codes
      will be stored in an auxiliary data structure called a
      labeled_evtree (where internal nodes also store genetic
      information).

   3. Then, estimate each labeled_evtree's complexity, and guess at how
      many evolutionary events were required to create the given tree.

   4. Finally, return the simplest (i.e., most biologically plausible)
      tree, stripping off the guessed ancestors.

   Before completing the remaining problems of this assignment, you
   should read through all of them so that you can see how they fit
   together and correspond to the four steps above. *)

(* An evolutionary evtree is a binary tree whose leaves are labeled
   with DNA helices.

   Note that the type 'evtree' defined here has a slightly different
   shape different from the datatype 'tree' introduced in lecture 5.
   Both are "binary trees" in the sense that each position in the tree
   has either zero or two children.  However, the data associated with
   the two constructors is a bit different.  Here, a Leaf (with no
   children) is labeled with a helix, while a Node (with two children)
   has no label.

                         tree                 evtree
                         __________________________________________
     Leaf constructor    no children          no children
                         unlabeled            labeled with a helix

     Node constructor    two children         two children
                         labeled with an int  unlabeled
*)

type evtree =
  | Leaf of helix
  | Node of evtree * evtree

(* In this homework assignment, we will evaluate our algorithm using
   the *real* evolutionary tree that scientists believe best describes
   the relationships between ape species. The real tree is shown in
   Figure A on the homework website
   (http://www.cis.upenn.edu/~cis1200/current/hw/hw02/).

   For example, we can represent the left side of the tree from Figure A as
   follows: *)

let greater_apes () : evtree =
  Node (
    Leaf orangutan,
    Node (
      Leaf gorilla,
      Node (
        Leaf human,
        Leaf chimpanzee
      )
    )
  )

(* The following function can be used to display evolutionary trees in
   the console. Given an evtree whose leaves are labeled with the ape
   helices, it outputs a string containing an "ascii art" picture of
   the evtree.  You are not expected to follow the details of how it
   works. *)

let rec string_of_evtree (r: evtree) : string =
  let spaces (n: int) : string = String.make n ' ' in
  let dashes (n: int) : string = String.make n '-' in

  let rec zip ((t1, w1): string list * int) ((t2, w2): string list * int)
    : string list * int =
    let rec aux (t1: string list) (t2: string list) : string list =
      begin match t1, t2 with
        | [], _  -> List.map (fun s -> spaces w1 ^ s) t2
        | _ , [] -> List.map (fun s -> s ^ spaces w2) t1
        | t :: ts, s :: ss -> (t ^ s) :: (aux ts ss)
      end
    in
    let h1 = w1 / 2 in
    let h2 = w2 / 2 in
    let line1 = spaces h1 ^ dashes (h1 + h2 + 2) ^ spaces h2 in
    let line2 = spaces h1 ^ "|" ^ spaces (h1 + h2) ^ "|" ^ spaces h2 in
    (line1 :: line2 :: aux t1 t2, w1 + w2 + 2)
  in

  let rec aux (t: evtree) : string list * int =
    begin match t with
      | Leaf x ->
        let str1 = " " ^ string_of_ape x ^ " " in
        let str2 =
          if (String.length str1) mod 2 = 0 then str1
          else " " ^ str1
        in
        ([str2], String.length str2)
      | Node (left, right) ->
        zip (aux left) (aux right)
    end
  in
  let (strs, w) = aux r in
  let strs2 = (spaces (w/2) ^ " |" ^ (spaces (w/2))) :: strs in
  (String.concat "\n" strs2) ^ "\n"

(* These commands display the greater_apes evtree in the console when
   your program runs. Before continuing, make sure that you understand
   the connection between this display, the definition of
   'greater_apes' above, and Figure A. *)

;; print_endline "Greater apes (from Figure A)"
;; print_endline "----------------------------"
;; print_endline (string_of_evtree (greater_apes ()))

(* TODO: Your next job is to fill in the evtree for the lesser apes, from
   the same figure. *)

let lesser_apes () : evtree =
  Node (Leaf white_cheeked_gibbon, 
    Node (Leaf siamang, 
      Node (Leaf lar_gibbon, Leaf pileated_gibbon)
    )
  )

(* Once you have defined this evtree, uncomment the following commands
   and make sure that it matches the one in Figure A. *)

   
   ;; print_endline "Lesser apes (from Figure A)"
   ;; print_endline "---------------------------"
   ;; print_endline (string_of_evtree (lesser_apes ()))


(* One way to test that you have defined 'lesser_apes' correctly is to
   count the number of species that it contains. We can calculate this
   number by counting the number of leaf nodes in the evtree.

   Remember that type evtree is a recursive datatype. When you solve
   count_leaves (as well as the rest of the problems in this
   assignment) keep in mind that your function should utilize
   recursion. Use the evtree type definition above to help you come up
   with your patterns for pattern matching.

   Implement the following function. *)

let rec count_leaves (t: evtree) : int =
  begin match t with
  | Leaf _ -> 1
  | Node (lt, rt) -> (count_leaves lt) + (count_leaves rt)
  end

(* -------------------------------------------------------------------------- *)
(* ----------------- Generating Possible Phylogenetic Trees ----------------- *)
(* -------------------------------------------------------------------------- *)

(* Now that we have a representation for phylogenetic trees, we can
   begin Step 1 of our DNA analysis: generating all possible evtrees by
   listing all distinct evtree structures with the input helices at the
   leaves. Because internal nodes correspond to ancestor species, we
   will (later on) guess the internal labels before evaluating
   functions that compute tree complexity.

   The following function (whose body we provide for you) generates
   all possible trees for a set of four present-day species... *)

(* Given four helices that should appear on the leaves of each output
   evtree, return a list of all distinct evtree structures with the input
   helices at their leaves. *)

let rec all_evtrees (x1: helix) (x2: helix) (x3: helix) (x4: helix)
  : evtree list =
  let tree1 (x1: helix) (x2: helix) (x3: helix) (x4: helix): evtree =
    Node (Node (Leaf x1, Leaf x2),  Node (Leaf x3, Leaf x4)) in
  let tree2 (x1: helix) (x2: helix) (x3: helix) (x4: helix): evtree =
    Node (Leaf x1, Node (Leaf x2, Node (Leaf x3, Leaf x4))) in
  [tree1 x1 x2 x3 x4; tree1 x1 x3 x2 x4; tree1 x1 x4 x2 x3;
   tree2 x1 x2 x3 x4; tree2 x1 x3 x2 x4; tree2 x1 x4 x2 x3;
   tree2 x2 x1 x3 x4; tree2 x2 x3 x1 x4; tree2 x2 x4 x1 x3;
   tree2 x3 x1 x2 x4; tree2 x3 x2 x1 x4; tree2 x3 x4 x1 x2;
   tree2 x4 x1 x2 x3; tree2 x4 x2 x1 x3; tree2 x4 x3 x1 x2]

let all_greater_ape_evtrees () : evtree list =
  all_evtrees gorilla human chimpanzee orangutan

let all_lesser_ape_evtrees () : evtree list =
  all_evtrees lar_gibbon pileated_gibbon siamang white_cheeked_gibbon

(* If you would like to see the candidate evolutionary trees for the
   greater apes, uncomment the following lines. Note that the evtree
   we are hoping to pick out (from Figure A) is third from the end. *)

let rec string_of_evtree_list (ts: evtree list) : string =
  begin match ts with
    | []       -> ""
    | hd :: tl -> string_of_evtree hd ^ "\n\n" ^ string_of_evtree_list tl
  end

(*
   ;; print_endline "Possible evolutionary trees (greater apes)"
   ;; print_endline "------------------------------------------"
   ;; print_endline (string_of_evtree_list (all_greater_ape_evtrees ()))
*)


(* -------------------------------------------------------------------------- *)
(* ------------------- Problem 5:  Guessing ancestors  ---------------------- *)
(* -------------------------------------------------------------------------- *)

(* Now on to step 2 of our roadmap: comparing the generated
   evolutionary trees from above, with the help of an intermediate
   data structure called a 'labeled_evtree'. This data structure is
   almost like the evolutionary tree, except that internal nodes are
   labeled with guesses about the dna data of ancestor species. *)

type labeled_evtree =
  | LLeaf of helix
  | LNode of labeled_evtree * helix * labeled_evtree

(* The following helper function (for you to write) will be useful
   later on: *)

(* Find the helix contained within the root node of a given
   labeled_evtree.  This function should output the helix in the leaf
   when the evtree is just a leaf. The type definition will be helpful
   here. *)

let helix_of_evtree (t: labeled_evtree) : helix =
  begin match t with 
  |LLeaf helix -> helix
  |LNode (_, helix, _) -> helix
  end

(* Our goal is to compare labeled trees, but it is worth thinking
   first about what we will do once we find the best one. We would
   like to produce an (unlabeled) evolutionary tree, so we will need
   to strip off the guessed ancestors.

   Given a labeled_evtree as defined above, we can do this by
   returning an evtree of identical structure with the helix in each
   Node removed. Don't forget to add another test for unlabel_evtree
   in dnaTest.ml. *)

let rec unlabel_evtree (t: labeled_evtree) : evtree =
  begin match t with
  |LLeaf h -> Leaf h
  |LNode(lt, _, rt) -> Node ((unlabel_evtree lt), (unlabel_evtree rt))
  end

(* -------------------------------------------------------------------------- *)

(* The next step is to label the internal nodes of our generated
   trees. See Figure C (on the instruction page for the homework) for
   a completely labeled candidate evtree whose helices are of length
   four. Figure C is sample output for add_ancestor_labels, which
   you'll write below. *)

(* BEFORE WRITING add_ancestor_labels, READ CAREFULLY THROUGH THE HELPER
   FUNCTION guess_parent_helix AND FILL IN THE REQUIRED TEST CASE RESULTS. *)

(* Assuming x1 and x2 are valid helices, guess the valid helix that
   labels their parent node in an evolutionary tree. The guessed helix
   is computed using nucleotide-by-nucleotide comparison of x1 and x2:
   if the nucleotides match, the corresponding parent nucleotide is
   the same. If they don't match, the corresponding parent nucleotide
   is (arbitrarily) A. *)


let rec guess_parent_helix (x1: helix) (x2: helix) : helix =
  begin match x1, x2 with
    | [], [] -> []
    | h1 :: t1, h2 :: t2 ->
      (if h1 = h2 then h1 else A) :: guess_parent_helix t1 t2
    (*for this case, just input the rest of the nucleotide seq*)
    | _, h2::t2 -> h2 :: guess_parent_helix [] t2
    | h1::t1, _ -> h1 :: guess_parent_helix t1 []
  end

(* -------------------------------------------------------------------------- *)

(* Now we want to use the guess_parent_helix function to fill in the
   guess the DNA of all the ancestor species in an evolutionary tree,
   given the DNA of their present-day descendants. *)

(* Given an unlabeled evtree, generate an evtree with the same shape
   but with all internal nodes properly labeled. Use
   guess_parent_helix to compute the appropriate label for a parent
   node, given the labels of its two children.

   Some points to remember...

   - Your function should work for an evtree of _any_ height; make sure
     to test this prior to continuing with the assignment. We have
     provided a test stub reminding you to test this in dnaTest.ml.

   - You should NOT assume that all input trees are "balanced"; that
     is, the left and right sub-trees of any node may in general have
     different numbers of nodes!

   - Keep in mind: if you find yourself writing the same segment of code
     multiple times, that is often an indicator that that segment can be
     stored as a variable to be reused in multiple places. This will also
     make your code run faster! :) *)

let rec add_ancestor_labels (t: evtree) : labeled_evtree =
  begin match t with 
  | Leaf seq -> LLeaf seq
  | Node (lt, rt) -> 
    (*labeling the left and right subtrees first*)
    let labeled_lt = add_ancestor_labels lt in
    let labeled_rt = add_ancestor_labels rt in
    (*getting the helixes of the left and right subtrees*)
    let lt_seq = helix_of_evtree labeled_lt in
    let rt_seq = helix_of_evtree labeled_rt in
    (*making the node*)
    LNode (labeled_lt, (guess_parent_helix lt_seq rt_seq), labeled_rt)
  end 

(* Once your test cases for add_ancestor_labels pass, this function
   can be used to label all of the trees for the greater and lesser
   apes.

   To label ALL of the trees in the list of candidates, we define (for
   you) the following helper function: *)

let rec add_ancestor_labels_list (ts: evtree list) : labeled_evtree list =
  begin match ts with
    | []       -> []
    | hd :: tl -> add_ancestor_labels hd :: add_ancestor_labels_list tl
  end

(* Labeled trees for lesser and greater apes. *)

let labeled_greater_ape_evtrees () : labeled_evtree list =
  add_ancestor_labels_list (all_greater_ape_evtrees ())

let labeled_lesser_ape_evtrees () : labeled_evtree list =
  add_ancestor_labels_list (all_lesser_ape_evtrees ())


(* -------------------------------------------------------------------------- *)
(* ----- Problem 6: Parent-Child Hamming Distance --------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Now let's move to step 3. We want to compare the labeled_evtrees by
   calculating their "complexity" and then choosing the simplest /
   most plausible one. We measure the complexity of an evtree by summing
   the number of mutations needed to convert between its parent and
   child DNA sequences. *)

(* Given a labeled evtree, compute the sum of Hamming distances between
   all parent-child pairs (where "parent-child pair" refers to a
   DIRECT parent-child relationship, not just an ancestor relationship).

   Your solution should use the hamming_distance function from Problem
   2. Make sure to check what arguments hamming_distance requires. *)

let rec parent_child_hamming (t: labeled_evtree) : int =
  begin match t with 
  (*the leaf has no children, so its parent child hamming is 0*)
  |LLeaf seq -> 0
  |LNode (lt, seq, rt) -> 
    (*getting the helixes of both left and right subtree*)
    let lt_seq = helix_of_evtree lt in
    let rt_seq = helix_of_evtree rt in
    (*need to calc the hamming distance of the helix with both children*)
    (hamming_distance seq lt_seq) + (hamming_distance seq rt_seq) + 
    (*calling the func on both subtrees*)
    (parent_child_hamming lt) + (parent_child_hamming rt)
  end

(* -------------------------------------------------------------------------- *)
(* ----- Problem 7: Finding the simplest evtree ----------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Finally we are at step 4: Given a list of labeled evtrees, we want
   to find the simplest one in the list, using the
   parent_child_hamming function defined above to estimate the
   complexity of the evtree.

   The simplest_evtree function should, given a list of labeled_evtrees,
   return both the simplest evtree in the list and its complexity. If
   you find two simplest evtrees -- i.e., two evtrees that both have the
   lowest complexity -- return the one that appears first in the list.

   Remember that evtree A is considered simpler than evtree B if the
   complexity of A is less than the complexity of B.

   You can assume that this function will only be applied to a
   non-empty list of labeled_evtrees. Your function should throw an
   error (using failwith) if it is passed an empty list.

   As always, make sure you add test cases for this function. *)

let rec simplest_evtree (ts: labeled_evtree list) : labeled_evtree * int =
  (*throwing an error if the input is empty list*)
  begin match ts with 
  |[] -> failwith "empty list"
  (*stopping recursion reaching the last element*)
  |[head] -> (head, parent_child_hamming head)
  |head::tail -> 
    (*getting the tree with the lowest complexity in the tail*)
    let (t2, complexity2) = (simplest_evtree tail) in
    (*comparing that with the head*)
    let complexity = parent_child_hamming head in
    if complexity <= complexity2 then (head, complexity)
    else (t2, complexity2)
  end
    
    



(* -------------------------------------------------------------------------- *)

(* Now we are ready to put everything together to see how well
   calculating evolutionary trees using the parent-child hamming
   distance complexity measure works. *)

let simplest_greater_ape_evtree () : evtree =
  let (t, _) = simplest_evtree (labeled_greater_ape_evtrees ()) in
  unlabel_evtree t

(* We can compare this computed evtree with the one in Figure A, to
   see that this methodology works well for greater apes. Uncomment
   the following printing commands to make sure that the result
   matches the following diagram:

    Computed evolutionary tree for greater apes
    -------------------------------------------
                            |
          -------------------------
          |                       |
      Orangutan      ------------------
                    |                |
                  Gorilla     ------------
                              |          |
                            Human  Chimpanzee

   Note: If this part of the program runs slowly, you'll want to recomment
   these commands before you submit. *)

   (*
   ;; print_endline "Computed evolutionary evtree for greater apes"
   ;; print_endline "-------------------------------------------"
   ;; print_endline (string_of_evtree (simplest_greater_ape_evtree ())) *)

(* However, trying this out for lesser apes produces something close
   to, but not quite exactly, what we wanted. Uncomment the printing
   commands below to see how close we get to Figure A. The computed
   evolutionary evtree should match the following diagram:

    Computed evolutionary evtree for lesser apes
    ------------------------------------------
                                    |
                  -----------------------------------
                  |                                 |
        -----------------              ------------------
        |               |              |                |
  Lar Gibbon   Pileated Gibbon   Siamang  White Cheeked Gibbon *)

let simplest_lesser_ape_evtree () : evtree =
  let (t, _) = simplest_evtree (labeled_lesser_ape_evtrees ()) in
  unlabel_evtree t

   (*
   ;; print_endline "Computed evolutionary evtree for lesser apes"
   ;; print_endline "------------------------------------------"
   ;; print_endline (string_of_evtree (simplest_lesser_ape_evtree ())) *)


(* -------------------------------------------------------------------------- *)
(* ----- Problem 8: Refactoring --------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Once you have solved a problem, it is a good idea to go back, look
   for repeated patterns, and generalize your solution. In this case,
   if you look closely at the computation of the simplest trees for
   the lesser and greater apes, you can see that we are doing the same
   thing twice. Capture steps 1-4 with a single function that starts
   with *any* four ape helices and outputs the computed simplest
   evtree.

   Note that your solution should be short (no more than 4 lines long)
   and you should be able to reuse much of the code that you have
   already developed in this assignment. In particular, your
   implementation should not require writing any new helper
   functions. *)

let find_simplest_evtree (x1: helix) (x2: helix) (x3: helix) (x4: helix)
                       : evtree =
  (*computing all the possible ev trees*)
  let tl = all_evtrees x1 x2 x3 x4 in
  (*labeling the trees*)
  let labeled_tl = add_ancestor_labels_list tl in
  (*getting the simplest tree, dont care abt complexity score*)
  let simplest_tree,_ = simplest_evtree labeled_tl in
  (*unlabeling it*)
  unlabel_evtree simplest_tree



;; print_endline "dna.ml: ran to completion."

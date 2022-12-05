(import /util :prefix "")

(def COLUMN_COUNT 9)

(defn make-instr [n from to]
  {:n n
   :from (- from 1) # the crates are one-indexed the tricksies
   :to (- to 1)})

(defn unmux-crates [crates]
  "Returns an array of arrays of 1-char strings."
  (def out (map (fn [_] @[]) (range COLUMN_COUNT)))
  # Loop top-down, so all the stacks will be backwards ...
  # then reverse! them
  (loop [row :in crates
         colidx :range [0 COLUMN_COUNT]]
    (if-let [crate (row colidx)]
      (array/push (out colidx) crate)))
  (map reverse! out)
  out)

(def peg ~{:crate (* "[" ':a "]")
           :crate-space (/ "   " ,false)
           :crate-row (group ,(split (+ :crate :crate-space) " "))
           :crates (group ,(split :crate-row "\n"))

           :sep-row (* "\n" (any (+ :d " ")) "\n\n")

           :digit (/ ':d+ ,scan-number)
           :instr (/ (* "move " :digit " from " :digit " to " :digit) ,make-instr)
           :instrs (group ,(split :instr "\n"))

           :main (* :crates :sep-row :instrs)})

(defn execute-instr/1! [stacks instr]
  "Exectute the instr on the stacks and return the mutated stacks"
  (for _ 0 (instr :n)
    (def crate (array/pop (stacks (instr :from))))
    (array/push (stacks (instr :to)) crate))
  stacks)

(defn execute-instr/2! [stacks instr]
  (def {:n n :from from :to to} instr)

  (def stack1 (stacks from))
  (def stack1-leftover (- (length stack1) n))
  (def stack1p (array/slice stack1 0 stack1-leftover))
  (def moved (array/slice stack1 stack1-leftover -1))
  (array/concat (stacks to) moved)
  (put stacks from stack1p)
  stacks)

(defn part1 [input]
  (def [stacks instrs] (peg/match peg input))
  (def stacks (unmux-crates stacks))
  (map |(execute-instr/1! stacks $) instrs)
  (apply string (map last stacks)))

(defn part2 [input]
  (def [stacks instrs] (peg/match peg input))
  (def stacks (unmux-crates stacks))
  (map |(execute-instr/2! stacks $) instrs)
  (apply string (map last stacks)))

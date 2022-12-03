(import /util :prefix "")

(defn rucksack-split [line]
  (def len/2 (/ (length line) 2))
  [(string/slice line 0 len/2) (string/slice line len/2 -1)])

(def peg/1 ~{:rucksack (/ ':a+ ,rucksack-split)
             :main ,(split :rucksack "\n")})

(defn priority [ch]
  (if (<= (chr "a") ch (chr "z"))
    (- ch 0x60)
    (+ (- ch 0x40) 26)))

# For each elt in the first, check if it's in all the others and if so return it.
(defn in-all [xs]
  (find (fn [elt0] (all
                     (fn [trailer] (find |(= $ elt0) trailer))
                     (drop 1 xs)))
        (xs 0)))

(defn part1 [input]
  (def rucksacks (peg/match peg/1 input))
  (sum (map |(-> $ in-all priority) rucksacks)))

# 7211 too low

(def peg/2 ~{:rucksack ':a+
             :main ,(split :rucksack "\n")})

(defn chunk [xs n]
  (def len (length xs))
  (def chunk-count (/ len n))
  (def out (array/new chunk-count))
  (for i 0 chunk-count
    (def idx (* i n))
    (array/push out
                (array/slice xs idx (+ idx n))))
  out)

(defn part2 [input]
  (def elves (chunk (peg/match peg/2 input) 3))
  # (pp elves)
  (sum (map |(-> $ in-all priority)
            elves)))
# 2642 too low

(def test ```vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
```)

(defn main [& args]
  (pp (part2 test)))

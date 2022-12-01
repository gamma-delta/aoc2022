(def peg ~{:num (/ ':d+ ,scan-number)
           :elf (group (any (* :num (? "\n"))))
           :main (* (any (* :elf (? "\n"))))})

(defn part1 [input]
  (def elves (peg/match peg input))
  (apply max (map sum elves)))

(defn part2 [input]
  (def elves (peg/match peg input))
  (sum (take 3 (sort (map sum elves) >))))

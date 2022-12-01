(import /util :prefix "")

(def peg ~{:num (/ ':d+ ,scan-number)
           :elf (group ,(split :num "\n"))
           :main ,(split :elf "\n\n")})

(defn part1 [input]
  (def elves (peg/match peg input))
  (apply max (map sum elves)))

(defn part2 [input]
  (def elves (peg/match peg input))
  (sum (take 3 (sort (map sum elves) >))))

# (pp (peg/match (split ':d+ ",") "1,23,45,678,"))


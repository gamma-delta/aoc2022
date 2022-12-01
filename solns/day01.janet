(defmacro delim [main sep]
  ~'(*
      ,main
      (any (* ,sep ,main))))

(def peg ~{:num (/ ':d+ ,scan-number)
           :elf (group ,(delim :num "\n"))
           :main ,(delim :elf "\n\n")})

(defn part1 [input]
  (def elves (peg/match peg input))
  (apply max (map sum elves)))

(defn part2 [input]
  (def elves (peg/match peg input))
  (sum (take 3 (sort (map sum elves) >))))

# (pp (peg/match (delim ':d+ ",") "1,23,45,678,"))


(import /util :prefix "")

(def peg ~{:num (replace ':d+ ,scan-number)
           :elf (group (* :num "-" :num))
           :pair (group (* :elf "," :elf))
           :main ,(split :pair "\n")})

(defn fully-contains? [[x0 x1] [y0 y1]]
  (or (<= x0 y0 y1 x1)
      (<= y0 x0 x1 y1)))

(defn any-overlap? [[x0 x1] [y0 y1]]
  (or (<= x0 y0 x1)
      (<= y0 x0 y1)))

(defn part1 [input]
  (def pairs (peg/match peg input))
  (count |(apply fully-contains? $) pairs))

(defn part2 [input]
  (def pairs (peg/match peg input))
  (count |(apply any-overlap? $) pairs))

(defn main [& args]
  (pp (any-overlap? [5 10] [1 3])))

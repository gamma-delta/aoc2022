(import /util :prefix "")

(defn all-different [s]
  (def freq (frequencies s))
  (= (length s) (length freq)))

(defn find-start [pkt diffct]
  (def len (length pkt))
  (label out
    (for i diffct len
      (def slice (string/slice pkt (- i diffct) i))
      (if (all-different slice)
        (return out i)))
    (error "never found a header")))

(defn part1 [input]
  (find-start input 4))

# 105 too low

(defn part2 [input]
  (find-start input 14))

(defn main [& args]
  (print (find-start "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)))

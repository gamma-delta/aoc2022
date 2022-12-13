(import /util :prefix "")

(def peg
  ~{:value (+ :list :number)
    :number (replace ':d+ ,scan-number)
    :list (* "["
             (group (+
                      (* (some (* :value ",")) :value)
                      :value
                      ""))
             "]")

    :packet-pair (replace (* :list "\n" :list)
                          ,(fn [l r] {:left l :right r}))})

(defn ord? [left right]
  (cond
    (and (number? left) (number? right))
    (cond
      (< left right) :lt
      (> left right) :gt
      :eq)
    (and (indexed? left) (indexed? right))
    (label out
      (map (fn [subl subr]
             (def ord (ord? subl subr))
             (if (not= ord :eq)
               (return out ord))) left right)
      # if we made it here then everything in the lists is equivalent
      (def [lenl lenr] [(length left) (length right)])
      (cond
        (< lenl lenr) :lt
        (> lenl lenr) :gt
        :eq))
    (do
      (def [l2 r2] (if (number? left)
                     [[left] right]
                     [left [right]]))
      (ord? l2 r2))))

(defn part1 [input]
  (def peg1 (merge peg ~{:main ,(split :packet-pair "\n\n")}))
  (def packets (peg/match peg1 input))
  (reduce (fn [acc [{:left left :right right} idx]]
            (def ord (ord? left right))
            # (printf "[%3d] %P <=> %P %j" idx left right ord)
            (if (= :eq ord)
              (printf "sneaky! [%3d] %P :eq" idx left))
            (+ acc
               (if (= :lt ord)
                 (inc idx)
                 0)))
          0 (map tuple packets (range (length packets)))))

# 6667 too high

(defn part2 [input]
  (def LOCATOR1 [[2]])
  (def LOCATOR2 [[6]])

  (def peg2 (merge
              peg
              ~{:main (any (* :list :s*))}))
  (def packets (peg/match peg2 input))
  (def packets (array/concat packets [LOCATOR1 LOCATOR2]))
  (sort packets (fn [a b] (= (ord? a b) :lt)))

  (def idx1 (find-index (partial deep= LOCATOR1) packets))
  (def idx2 (find-index (partial deep= LOCATOR2) packets))
  # (printf "%P\n [%d %d] " packets idx1 idx2)
  (* (inc idx1) (inc idx2)))

(defn main [& args]
  (pp (part2 ```[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]```)))

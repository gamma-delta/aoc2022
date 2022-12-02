(import /util :prefix "")

(defn shape-score [shape]
  (match shape
    :rock 1
    :paper 2
    :scissors 3
    else (errorf "invalid shape %v" else)))

(defn compete [me you]
  (cond
    (= me you) :tie
    (or
      (= [me you] [:paper :rock])
      (= [me you] [:rock :scissors])
      (= [me you] [:scissors :paper])) :win
    :loss))

(defn compete-score [result]
  (match result
    :loss 0
    :tie 3
    :win 6
    else (errorf "invalid result %v" else)))

(defn score [me you]
  (def result (compete me you))
  (+
    (shape-score me)
    (compete-score result)))


(defn unpack-shape [s]
  (case s
    "A" :rock "X" :rock
    "B" :paper "Y" :paper
    "C" :scissors "Z" :scissors))

(def peg/pt1 ~{:entry (replace '1 ,unpack-shape)
               :row (group (* :entry " " :entry))
               :main ,(split :row "\n")})

(defn unpack-instr [s]
  (case s
    "X" :lose
    "Y" :draw
    "Z" :win))

(def peg/pt2 ~{:shape (replace '1 ,unpack-shape)
               :strat (replace '1 ,unpack-instr)
               :row (group (* :shape " " :strat))
               :main ,(split :row "\n")})

(defn atium [you reqd-result]
  (in {[:rock :lose] :scissors
       [:rock :draw] :rock
       [:rock :win] :paper

       [:paper :lose] :rock
       [:paper :draw] :paper
       [:paper :win] :scissors

       [:scissors :lose] :paper
       [:scissors :draw] :scissors
       [:scissors :win] :rock} [you reqd-result]))

###

(defn part1 [input]
  (def strategy (peg/match peg/pt1 input))
  (sum (map (fn [[you me]] (score me you)) strategy)))

(defn part2 [input]
  (def strategy (peg/match peg/pt2 input))
  (sum (map (fn [[you reqd-result]]
              (def me (atium you reqd-result))
              (score me you)) strategy)))
# 12449 too low


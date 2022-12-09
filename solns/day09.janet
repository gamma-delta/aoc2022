(import /util :prefix "")

(defn unpack-dir [chr]
  (case chr
    "U" :up
    "D" :down
    "L" :left
    "R" :right
    (errorf "unknown direction %j" chr)))

(def peg (peg/compile
           ~{:dir (/ ':w ,unpack-dir)
             # sneaky, there's multiple digit numbers
             :num (/ ':d+ ,scan-number)
             :instr (group (* :dir " " :num))
             :main ,(split :instr "\n")}))

(defn new-state []
  {:head [0 0] :tail [0 0]})

(defn deltas [dir]
  (case dir
    :up [0 1] # math convention, not gfx
    :down [0 -1]
    :left [-1 0]
    :right [1 0]))

(defn unpack-instr [head [dir amt]]
  "turn the instruction into a sequence of head positions"
  (def pointing (deltas dir))
  (map (fn [dpar]
         (def ds (map (partial * dpar) pointing))
         (map + ds head))
       (range 1 (+ amt 1))))

(defn unpack-instrs [instrs]
  (var *head* [0 0])
  (def out @[])
  (loop [instr :in instrs]
    (def sequence (unpack-instr *head* instr))
    (array/concat out sequence)
    (set *head* (last sequence)))
  out)

(defn signum [x]
  (cond
    (> 0 x) -1
    (= 0 x) 0
    1))

(defn updated-tail [head old-tail]
  (def dx (- (head 0) (old-tail 0))) (def dy (- (head 1) (old-tail 1)))
  (def dx-sign (signum dx)) (def dy-sign (signum dy))
  (cond
    # still close, no change
    (and (<= (math/abs dx) 1) (<= (math/abs dy) 1)) old-tail
    # we're far enough to demand a change
    # if the change is strictly orthagonal, the perp signum will be 0
    # and if its not, the signums will point the diagonal
    # create a tuple out of this because map always returns an array ... 
    # and so we double-count some things otherwise
    (apply tuple (map + old-tail [dx-sign dy-sign]))))

(defn part1 [input]
  (def instrs (peg/match peg input))
  (def head-seq (unpack-instrs instrs))
  # golang "trick" of using a map[T]bool as a set
  (def point-tracker @{[0 0] true})
  (reduce (fn [tail new-head]
            (def new-tail (updated-tail new-head tail))
            (put point-tracker new-tail true)
            # (printf "%P" new-tail)
            new-tail)
          [0 0] head-seq)
  (length point-tracker))

# 8725 too high

# oh my god part 2 is disgusting

(defn part2 [input]
  (def instrs (peg/match peg input))
  (def head-seq (unpack-instrs instrs))
  # i could optimize this and calculate each point together, and i'd have to if there was like, 
  # simulate a million ropes
  # but fuck that
  (def last-rope (reduce (fn [prev-seq _]
                           (def out @[])
                           (reduce (fn [tail new-head]
                                     (def new-tail (updated-tail new-head tail))
                                     (array/push out new-tail)
                                     new-tail) [0 0] prev-seq)
                           out) head-seq (range 9)))
  # there's gotta be a better way to do this
  (def point-tracker @{[0 0] true})
  (loop [point :in last-rope]
    (put point-tracker point true))
  (length point-tracker))

(defn main [& args]
  (printf "%P" (part2 `R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
`)))

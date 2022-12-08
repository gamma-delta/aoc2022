(import /util :prefix "")

(defn is-visible? [grid [ix iy]]
  (def height-here (grid [ix iy]))
  # (printf "checking %d @ %d,%d" height-here ix iy)
  # it is blocked iff all 4 directions are blocked
  # for a direction, it is blocked if even one tree is taller
  (def blocked?
    (all
      (fn [[ox oy]]
        (label leave # i do wish janet formatting was a little more lenient sometimes
          (loop [dpar :range [1 999999]
                 :let [x (+ ix (* ox dpar))
                       y (+ iy (* oy dpar))
                       cmper (grid [x y])]
                 :while cmper]
            # (printf "[% 2d,% 2d] cmping against %d @ %d,%d" ox oy cmper x y)
            (if (>= cmper height-here)
              # then this tree is taller, so it's blocked in this direction
              (return leave true)))
          false))
      [[-1 0] [1 0] [0 -1] [0 1]]))
  (not blocked?))

(def peg (peg/compile
           # drop so we don't push a ton of references to the map
           ~{:tree (drop (replace
                           (* ':d (argument 0) (column) (line))
                           # not sure why we have to subtract *two* from the column here
                           ,(fn [digit map col line] (put map [(- col 2) (- line 1)] (scan-number digit)))))
             :trees ,(split (any :tree) "\n")
             :main (* :trees (argument 0))}))

(defn parse-input [input]
  (def matched (peg/match peg input 0 @{}))
  (matched 0))

(defn part1 [input]
  (def grid (parse-input input))
  (count (fn [pos] (is-visible? grid pos)) (keys grid)))

(defn scenic-score [grid [ix iy]]
  (def height-here (grid [ix iy]))
  # (printf "checking %d @ %d,%d" height-here ix iy)
  (apply *
         (map
           (fn [[ox oy]]
             (var dpar 1)
             (forever
               (let [x (+ ix (* ox dpar))
                     y (+ iy (* oy dpar))
                     cmper (grid [x y])]
                 # (printf "  cmping against %j @ %d,%d" cmper x y)
                 # if we hit a nil, then it's not a tree, so sub 1 and return
                 (if (not cmper) (do
                                   (-- dpar) (break)))
                 (if (>= cmper height-here)
                   # then either we've found the edge of a clearing or a taller tree
                   (break))
                 (++ dpar)))
             # (printf "^ score for [%2d,%2d] is %P" ox oy dpar)
             dpar)
           [[-1 0] [1 0] [0 -1] [0 1]])))

(defn part2 [input]
  (def grid (parse-input input))
  (max-of (map (fn [pos] (scenic-score grid pos)) (keys grid))))

(defn main [& args]
  (def grid (parse-input `30373
25512
65332
33549
35390`))
  (print (scenic-score grid [2 3])))

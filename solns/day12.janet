(import /util :prefix "")
(import /prioq)

(defn A* [map start goal]
  ```
  Returns the came-from table, a mapping of positions to the position
  that pointed to them the most efficiently.
  You can walk backwards from it until the start.
    
  https://www.redblobgames.com/pathfinding/a-star/introduction.html 
  ```
  (defn heuristic [a b]
    (+
      (math/abs (- (a 0) (b 0)))
      (math/abs (- (a 1) (b 1)))))
  # Store the frontier as [pos cost]
  # We want items with LOWER cost to be more important
  (defn q-cmp [[_ c0] [_ c1]]
    (< c0 c1))

  (def frontier (prioq/new q-cmp))
  (prioq/add! frontier [start 0])
  (def came-from @{})
  (def cost-so-far @{start 0})

  (while (not (prioq/empty? frontier))
    (def [current _] (prioq/get! frontier))
    (if (= current goal)
      (break))
    (def height-here (map current))

    (loop [[dx dy] :in [[-1 0] [1 0] [0 -1] [0 1]]]
      (def npos [(+ dx (current 0)) (+ dy (current 1))])
      (if-let [nheight (map npos)
               _test (<= (- nheight height-here) 1)]
        # then this is a valid neighbor
        (do
          # the graph.cost(current, next) term in the rbgames article 
          # is always 1 here
          (def new-cost (+ 1 (cost-so-far current)))
          (if (or
                (not (get cost-so-far npos))
                (< new-cost (get cost-so-far npos)))
            (do
              (put cost-so-far npos new-cost)
              (def priority (+ new-cost (heuristic goal npos)))
              (prioq/add! frontier [npos priority])
              (put came-from npos current)))))))

  came-from)

(defn parse-input [input]
  (def peg
    (peg/compile
      ~{:elev (replace
                (* (line) (column) (argument 0) '(range "az"))
                ,(fn [y x out ch]
                   (def height (- (ch 0) (chr "a")))
                   (put (out :map) [(dec x) (dec y)] height)))
        :start (replace
                 (* (line) (column) (argument 0) "S")
                 ,(fn [y x out]
                    (def startpos [(dec x) (dec y)])
                    (put (out :map) startpos 0)
                    (put out :start startpos)))
        :end (replace
               (* (line) (column) (argument 0) "E")
               ,(fn [y x out]
                  (def endpos [(dec x) (dec y)])
                  (put (out :map) endpos 25)
                  (put out :end endpos)))
        :line (any (+ :elev :start :end))
        :lines ,(split :line "\n")
        :main (replace
                (* (argument 0) :lines)
                ,(fn [out & args] out))}))
  (def pegged (peg/match peg input 0 @{:map @{}}))
  (pegged 0))

(defn part1 [input]
  (def {:map map :start start :end end} (parse-input input))
  # (printf "%P -> %P" start end)
  (def lookup (A* map start end))

  (def path @[])
  (do
    (var *cursor* end)
    (while (not= *cursor* start)
      (array/push path *cursor*)
      (set *cursor* (lookup *cursor*))))

  # (loop [pos :in (reverse path)]
  #   (printf "(%3d, %3d) %d" (pos 0) (pos 1) (map pos)))
  # (pp start)
  (length path))

# 500 too high

# Part 2 is a good use of djikstra!
# we start at the "end" and look for a 0 elevation

(defn djikstra [map start]
  (defn q-cmp [[_ c0] [_ c1]]
    (< c0 c1))

  (def frontier (prioq/new q-cmp))
  (prioq/add! frontier [start 0])
  (def came-from @{})
  (def cost-so-far @{start 0})

  (def zeroes @{})

  (while (not (prioq/empty? frontier))
    (def [current _] (prioq/get! frontier))
    (def height-here (map current))

    (loop [[dx dy] :in [[-1 0] [1 0] [0 -1] [0 1]]]
      (def npos [(+ dx (current 0)) (+ dy (current 1))])
      (if-let [nheight (map npos)
               # we need to step *down*, at most, 1
               _test (<= (- height-here nheight) 1)]
        # then this is a valid neighbor
        (do
          (if (= nheight 0)
            (put zeroes npos true))

          (def new-cost (+ 1 (cost-so-far current)))
          (if (or
                (not (get cost-so-far npos))
                (< new-cost (get cost-so-far npos)))
            (do
              (put cost-so-far npos new-cost)
              (prioq/add! frontier [npos new-cost])
              (put came-from npos current)))))))
  [zeroes came-from])

(defn part2 [input]
  (def {:map map :end end} (parse-input input))
  (def [zeroes lookup] (djikstra map end))
  # (printf "%P\n%P" zeroes lookup)

  (def [min-dist target]
    (reduce (fn [[known-min known-target] zeropos]
              (var cursor zeropos)
              (var count 0)
              (while (not= cursor end)
                (++ count)
                (set cursor (lookup cursor)))
              (if (< count known-min)
                [count zeropos]
                [known-min known-target]))
            [math/inf nil] (keys zeroes)))
  # (pp target)
  min-dist)

(defn main [& args]
  (pp (part2 ```Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi```)))

(import /util :prefix "")

(def peg (peg/compile
           ~{:noop (replace "noop" ,(fn [] {:op :noop}))
             :num (replace '(* (? "-") :d+) ,scan-number)
             :addx (replace (* "addx " :num) ,(fn [num] {:op :addx :num num}))

             :opcode (+ :noop :addx)

             :main ,(split :opcode "\n")}))

(defn sleeptime [opc]
  (case (opc :op)
    :noop 1
    :addx 2))

# let's try some janet OOP
(defn new-executor [tape]
  # cycle is one-indexed dammit
  @{:tape tape :x 1 :ip 0 :cycle 0 :sleep (sleeptime (tape 0))
    :tick (fn [self]
            (++ (self :cycle))
            (if (<= (self :sleep) 0)
              # We need to sleep, *then* mess with the state
              (let [opc ((self :tape) (self :ip))]
                # first do the execution we've been "doing" this whole time
                (case (opc :op)
                  :noop nil
                  :addx (+= (self :x) (opc :num)))
                # then figure out how long to sleep on the next instr
                (++ (self :ip))
                (set (self :sleep) (sleeptime ((self :tape) (self :ip))))))
            (-- (self :sleep))
            self)
    :signal-strength (fn [self]
                       (* (self :cycle) (self :x)))
    :print (fn [self]
             (printf "%3d] X: %3d  IP: %3d  SL: %d  SS: %d"
                     (self :cycle)
                     (self :x)
                     (self :ip)
                     (self :sleep)
                     (:signal-strength self)))})

(defn part1 [input]
  (def opcodes (peg/match peg input))
  (def executor (new-executor opcodes))
  (var *accumulated* 0)
  (loop [_ :range-to [0 220]]
    (if (zero? (mod (- (executor :cycle) 20) 40))
      (do
        # Gwuh, commenting and uncommenting this made a *lot* of headaches 
        # because it would then move the +='ing to the false branch
        # (:print executor)
        (+= *accumulated* (:signal-strength executor))))
    (:tick executor))
  *accumulated*)

# 14660 too low
# 436812 too high
# 15480 too low

(defn part2 [input]
  (def opcodes (peg/match peg input))
  (def executor (new-executor opcodes))
  (def screen @{})
  (loop [_ :range [0 240]]
    (def cy (executor :cycle))
    (def x (mod cy 40)) # x is 0-indexed, apparently
    (def y (math/floor (/ cy 40)))

    # Not sure why i need to tick it *after* calculating the positions
    # i expect there's an off-by-one error somewhere this is undoing
    (:tick executor)
    (def sprite-lit? (<= (math/abs (- (executor :x) x)) 1))
    (if sprite-lit?
      (put screen [x y] true)))
  # then draw, i guess
  (loop [y :range [0 6]]
    (loop [x :range [0 40]]
      (prin (if (screen [x y]) "#" ".")))
    (print)))

(defn main [& args]
  (part2 (slurp "solns/day10-test")))

# (defn main [& args]
#   (def tape (peg/match peg `noop
# addx 3
# addx -5`))
#   (def executor (new-executor tape))
#   (loop [cy :range [0 6]]
#     (:print executor)
#     (:tick executor)))


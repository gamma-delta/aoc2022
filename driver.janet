(defn run-day [day &opt verbose]
  (default verbose false)
  (def padded-day (string/format "%.02d" day))
  (def input (do
               (def infile (file/open (string "inputs/day" padded-day) :rn))
               (file/read infile :all)))
  (def solvers (dofile (string "solns/day" padded-day ".janet")))

  (if verbose
    (print "running day " day))
  (def {'part1 {:value solver1}} solvers)
  (print (solver1 input))
  (if-let [solver2 (get solvers 'part2)]
    (print ((solver2 :value) input))
    (print "no part 2, skipping...")))

(defn main [& args]
  (def idx (scan-number (args 1)))
  (run-day idx true))

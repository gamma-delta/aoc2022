(defmacro split [main sep]
  ~'(*
      ,main
      (any (* ,sep ,main))))

(defn binary-search [q less-than?]
  ```
  Searches `q` for an index. Returns the index where you can insert the item compared
  with `less-than?` 
   
  `less-than?` should return true iff its argument should go before the ideal location;
  so it should close over its argument.
  `q` must be sorted by the `less-than?` function.
  
  For example, if you have a list of ascending numbers and want to find where to put a new
  number `y`, you might call `(binary-search my-nums |(< $ y))`.
  ```
  (var *min* 0)
  (var *max* (- (length q) 1))
  (while (<= *min* *max*)
    (def probe-idx (math/floor (/ (+ *min* *max*) 2)))
    (def probe (q probe-idx))
    # (printf "%j <-%j-> %j : %j" *min* probe-idx *max* probe)
    (if (less-than? probe)
      (set *min* (+ probe-idx 1))
      (set *max* (- probe-idx 1))))
  *min*)

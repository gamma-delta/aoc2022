(import /util :prefix "")

(defn new [more-important?]
  ```
  A prioq is stored as an array of elements.
  
  Items are sorted by `more-important?`. `(more-important? x y)` should
  return if `x` should be `get!`ted before `y`.
  
  We store it with the highest priority at the end, because
  popping is cheaper than popping front.
  ```
  {:lt? more-important?
   :arr @[]})

(defn add! [q elt]
  ```
  Add an element to the queue at the correct location, mutating it.
  
  Returns the index of the new item.
  ```
  # have the comparator "backwards" to have high priority at the end
  (def idx (binary-search (q :arr) (fn [elt-here] ((q :lt?) elt elt-here))))
  (array/insert (q :arr) idx elt)
  idx)

(defn get! [q]
  ```
  Removes and returns the element with highest priority
  from the queue.

  Errors if the queue is empty.
  ```
  (def arr (q :arr))
  (if (empty? arr)
    (errorf "tried to get! from an empty prioq")
    (array/pop arr)))

(defn- main [& args]
  # property based testing babey
  (def q (new
           (fn [[xp _] [yp _]] (< xp yp))))
  (for i 0 100_000
    (add! q [(math/random) i]))
  # (printf "%P" q)
  (reduce (fn [prev-prio [prio _]]
            (assert (>= prev-prio prio) (printf "%j < %j" prev-prio prio))
            prio)
          # 2 will always be greater than anything in the q.
          2 (q :arr)))

(import /util :prefix "")

# A node is a @{:data file-or-directory :other-stuff}
# A directory is a Table of filenames to nodes
# A file is a byte-size Number
# The filesystem root is a directory

(def peg (peg/compile
           ~{:cmd (* "$ " (+ :cd :ls))

             :cd (* "cd " :cd-arg)

             # use fns here because the behavior for replace with a struct is to get the struct val
             # janet `const` when
             :cd-arg (+
                       (replace "/" ,(fn [] {:cmd :cd-root}))
                       (replace ".." ,(fn [] {:cmd :cd-up}))
                       (replace '(some :S) ,(fn [name] {:cmd :cd :target name})))

             :ls (* "ls\n" ,(split :ls-returned "\n"))
             :ls-returned (+ :ls-dir :ls-file)
             :ls-dir (* "dir " (replace
                                 '(some :S)
                                 ,(fn [dirname] {:cmd :ls-entry
                                                 :type :dir
                                                 :name dirname})))
             :ls-file (replace
                        (* ':d+ " " '(some :S))
                        ,(fn [size name] {:cmd :ls-entry
                                          :type :file
                                          :name name :size (scan-number size)}))

             :main ,(split :cmd "\n")}))

(defn new-fs []
  @{:data @{}})

# a shell is {:root root :path <an array of {:seg ... :node ...}>)
# :seg   string for the path taken here
# :node  gc'ed in directory from that path
(defn new-shell [fs-root]
  {:root fs-root :path @[{:seg "ROOT" :node fs-root}]})

(defn get-cwd [{:root root :path path}]
  (if (= 0 (length path))
    root
    (last path)))

(defn update-kb [shell cmd]
  (match (cmd :cmd)
    :cd-root (let [len (length (shell :path))]
               (array/remove (shell :path) 1 (- len 1)))
    :cd-up (array/pop (shell :path))
    :cd (let [target (cmd :target)
              cwd-entry (get-cwd shell)
              cwd-node ((cwd-entry :node) :data)]
          (if (number? cwd-node)
            (errorf "tried to cd into a file oh no"))
          (if-let [child (cwd-node target)]
            (array/push (shell :path) {:seg target :node child})
            (errorf "todo: do we allow cding into nodes we dunno about")))
    :ls-entry (let [{:type type :name name} cmd
                    cwd-entry (get-cwd shell)
                    cwd-node ((cwd-entry :node) :data)]
                (if (get cwd-node name)
                  (errorf "todo: do we allow redundant telling about files"))
                (put cwd-node name @{:data (match type
                                             :dir @{}
                                             :file (cmd :size))})))
  nil)

(defn annotate-with-size [node]
  "Returns the size"
  (def data (node :data))
  (match (type data)
    :number data
    :table (if-let [cached-size (get node :size)]
             cached-size
             (let [subsize (reduce
                             (fn [acc subnode] (+ acc (annotate-with-size subnode)))
                             0 (values data))]
               (put node :size subsize)
               subsize))))

(defn prepare-fs [input]
  (def cmds (peg/match peg input))
  (def fs (new-fs))
  (def shell (new-shell fs))
  (loop [cmd :in cmds] (update-kb shell cmd))
  (annotate-with-size fs)
  fs)

(defn find-size-sum [node threshold]
  (def data (node :data))
  (if (number? data)
    0
    (do
      (def size (node :size))
      (def size* (if (<= size threshold)
                   size
                   0))
      (reduce (fn [acc subnode] (+ acc (find-size-sum subnode threshold)))
              size* (values data)))))

(defn find-size-sum [node threshold]
  (def data (node :data))
  (if (number? data)
    0
    (do
      (def size (node :size))
      (def size* (if (<= size threshold)
                   size
                   0))
      (reduce (fn [acc subnode] (+ acc (find-size-sum subnode threshold)))
              size* (values data)))))

(defn part1 [input]
  (def fs (prepare-fs input))
  (def MAX_THRESHOLD 100_000)
  (find-size-sum fs MAX_THRESHOLD))

# 4699703 wrong

(defn find-min-deletable [node delete-size known-min]
  (def data (node :data))
  (if (number? data)
    math/inf
    (do
      (def size (node :size))
      (def known-min* (if (>= size delete-size)
                        (min size known-min) # then this is large enough to clear
                        known-min)) # then its not
      (reduce (fn [acc subnode] (min acc (find-min-deletable subnode delete-size acc)))
              known-min* (values data)))))

(defn part2 [input]
  (def fs (prepare-fs input))
  (def TOTAL_SPACE 70_000_000)
  (def REQD_SPACE 30_000_000)
  (def used-space (fs :size))
  (def unused-space (- TOTAL_SPACE used-space))
  (def must-delete (- REQD_SPACE unused-space))
  # (printf "must delete %j bytes" must-delete)
  (find-min-deletable fs must-delete math/inf))

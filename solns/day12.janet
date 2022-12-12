(import /util :prefix "")

(defn main [& args]
  (def test [1 3 7 11 15 39 111 1111 7604 7605 7606 9999 1_000 0x41414141])
  (pp (binary-search test |(< $ 0x42424242))))

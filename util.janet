(defmacro split [main sep]
  ~'(*
      ,main
      (any (* ,sep ,main))))

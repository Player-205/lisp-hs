(def defmacro 
  (macro (name args body) 
    (def name (macro args body))
    )
  )

(defmacro defn (name args body)  
    def name (lambda args body)
  )


(def nil ())

(defmacro printLn (args)
    (print args)
    (print "\n")
  )

(defmacro null? (l) = l nil)


(printLn "Wake up, Neo!")
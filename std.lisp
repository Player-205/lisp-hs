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

(defmacro symbol?  (arg) = (typeof arg) "Symbol"     )
(defmacro keyword? (arg) = (typeof arg) "KeyWord"    )
(defmacro string?  (arg) = (typeof arg) "String"     )
(defmacro bool?    (arg) = (typeof arg) "Bool"       )
(defmacro integer? (arg) = (typeof arg) "Integer"    )
(defmacro float?   (arg) = (typeof arg) "Float"      )
(defmacro list?    (arg) = (typeof arg) "List"       )
(defmacro lambda?  (arg) = (typeof arg) "Lambda"     )
(defmacro macro?   (arg) = (typeof arg) "Macro"      )
(defmacro env?     (arg) = (typeof arg) "Environment")



(defmacro module (name body)
  (def name env)
  (eval-in name body))


(defmacro import-qualified-as (module name args)
  cond (null? args) ()
  (
    (def (++ name (car 'args)) 
      (eval-in module 
        (eval (car 'args))))
    (import-qualified-as module name (cdr 'args))
  ))


(defmacro import-qualified (module args)
  import-qualified-as module (++ 'module ".") args)

(defmacro import (module args) 
  import-qualified-as module "" args)


(module math (
  (defn inc (n) + 1 n)
))


(printLn "Wake up, Neo...")
(printLn "The Matrix has you...")
(printLn "Follow the white rabbit.")
(printLn "")
(printLn "Knock, knock, Neo.")

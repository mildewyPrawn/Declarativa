;; necesitas un interprete de lisp
;; yo uso clisp.

;; $ clisp
;; [1] > (load "cantaros")
;; [2] > (factorial N)

(defvar llena "Llena ")

(defun example-print-numbers (X Y Z)
  "imprime las tres variables separadas"
  (write-line (format nil "~A ~A ~A Hola" X Y Z))
  10 ;; regresa 10, por que por qué no :v
  )

(print (list 1 2 3 4))

(defun add-to-list (X)
  "Agrega a una lista"
  (defvar l ())
  (push X l)
  )

(defun cantaros (X Y Z)
  "Se busca representar Z como combinaciones de X, Y. Suponemos que Z es mayor"
    
  )

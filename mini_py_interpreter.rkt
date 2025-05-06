#lang eopl

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identifier> = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
' (
    (white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number ((or "-" "") (arbno digit) "." (arbno digit)) number)
  )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)

    (expression ("print(" expression ")") print-exp)

    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
    (expression ("if" bool-type "then" expression "else" expression "end") if-exp)
    (expression ("for" identifier "in" expression "do" expression "done") for-exp)
    (expression ("while" bool-type "do" expression "done") while-exp)

    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ( "(" expression (arbno expression) ")") app-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) letrec-exp)
    (expression ("set" identifier "=" expression) set-exp)

    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) var-decl-exp)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression) const-decl-exp)
    (expression ("rec" (separated-list identifier "=" expression ",") "in" expression) rec-decl-exp)

    ;predefined data types
    (expression (list-type) list-exp)
    (expression (tuple-type) tuple-exp)
    (expression (dict-type) dict-exp)
    (expression (bool-type) bool-exp)

    ;list def
    (list-type ("[" expression (arbno "," expression) "]") non-empty-list)
    (list-type ("empty-list") empty-list)

    ;tuple def
    (tuple-type ("tuple[" expression (arbno "," expression) "]") non-empty-tuple)
    (tuple-type ("empty-tuple") empty-tuple)

    ;dict def
    (dict-type ("{" identifier "=" expression (arbno "," identifier "=" expression) "}") dict)
    
    ;bool def
    (bool-type ("true") true-bool)
    (bool-type ("false") false-bool)
    
    ;bools prims
    (bool-type (pred-prim "(" expression "," expression ")") pred-prim-app)
    (bool-type (bin-prim "(" bool-type "," bool-type ")") bin-prim-app)
    (bool-type (un-prim "(" bool-type ")") un-prim-app)

    ;lists prims
    (expression ("create-list(" expression (arbno "," expression) ")") create-list-prim)
    (expression ("empty-list?(" list-type ")") empty-list?-prim)
    (expression ("list?(" expression ")") list?-prim)
    (expression ("list-append(" expression "," expression ")") list-append-prim)
    (expression ("list-head(" expression ")") list-head-prim)
    (expression ("list-tail(" expression ")") list-tail-prim)
    (expression ("ref-list(" expression "," expression ")") ref-list-prim)
    (expression ("set-list(" expression "," expression "," expression ")") set-list-prim)

    ;tuples prims
    (expression ("create-tuple(" expression (arbno "," expression) ")") create-tuple-prim)
    (expression ("empty-tuple?(" tuple-type ")") empty-tuple?-prim)
    (expression ("tuple?(" expression ")") tuple?-prim)
    (expression ("tuple-head(" expression ")") tuple-head-prim)
    (expression ("tuple-tail(" expression ")") tuple-tail-prim)
    (expression ("ref-tuple(" expression "," expression ")") ref-tuple-prim)

    ;dicts prims
    (expression ("create-dict(" identifier "=" expression (arbno "," identifier "=" expression) ")") create-dict-prim)
    (expression ("dict?(" expression ")") dict?-prim)
    (expression ("ref-dict(" expression "," expression ")") ref-dict-prim)
    (expression ("set-dict(" expression "," expression "," expression ")") set-dict-prim)

    ;binary bool prims
    (bin-prim ("and") and-prim)
    (bin-prim ("or") or-prim)

    ;unary bool prims
    (un-prim ("not") not-prim)

    ;predicate bool prims
    (pred-prim ("<") lower-prim)
    (pred-prim (">") greater-prim)
    (pred-prim ("<=") lower-equal-prim)
    (pred-prim (">=") greater-equal-prim)
    (pred-prim ("==") equal-prim)
    (pred-prim ("!=") not-equal-prim)

    ;int prims
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
  )
)


;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?))
;  (proc-exp
;   (ids (list-of symbol?))
;   (body expression?))
;  (app-exp
;   (proc expression?)
;   (args (list-of expression?)))
;  (letrec-exp
;   (proc-names (list-of symbol?))
;   (idss (list-of (list-of symbol?)))
;   (bodies (list-of expression?))
;   (body-letrec expression?))
;  (begin-exp
;   (exp expression?)
;   (exps (list-of expression?)))
;  (set-exp
;   (id symbol?)
;   (rhs expression?)))
;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))

;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z f)
;     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) ())) ())))
;                      (empty-env)))
;     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**************************************************************************************

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (print-exp (exp) 
        (cases expression exp
          (list-exp (list-type) (print-list list-type env))
          (tuple-exp (tuple-type) (print-tuple tuple-type env))
          (dict-exp (dict-type) (print-dict dict-type env))
          (else (display (eval-expression exp env)))
        )
      )
      (primapp-exp (prim rands)
        (let ((args (eval-primapp-exp-rands rands env)))
          (apply-primitive prim args)
        )
      )
      (if-exp (bool-type true-exp false-exp)
        (if (eval-bool-type bool-type env)
          (eval-expression true-exp env)
          (eval-expression false-exp env)
        )
      )
      (let-exp (ids rands body)
        (let ((args (eval-let-exp-rands rands env)))
          (eval-expression body (extend-env ids args env))
        )
      )
      (proc-exp (ids body) (closure ids body env)
      )
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env)) (args (eval-rands rands env)))
          (if (procval? proc)
            (apply-procedure proc args)
            (eopl:error 'eval-expression "Attempt to apply non-procedure ~s" proc)
          )
        )
      )
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body (extend-env-recursively proc-names idss bodies env))
      )
      (set-exp (id rhs-exp)
        (begin
          (setref! (apply-env-ref env id) (eval-expression rhs-exp env))
        )
      )     
      (begin-exp (exp exps)
        (let loop ((acc (eval-expression exp env)) (exps exps))
          (if (null? exps) 
            acc
            (loop (eval-expression (car exps) env) (cdr exps))
          )
        )
      )
      (var-decl-exp (ids rands body) 
        (let ([args (eval-var-decl-exp-rands rands env)])
          (eval-expression body (extend-env ids args env))
        )
      )

      ;list and list prims
      (list-exp (l) l)
      (create-list-prim (first rest) (non-empty-list first rest))
      (empty-list?-prim (exp) 
        (cases list-type exp
          (empty-list () #t)
          (else #f)
        )
      )
      (list?-prim (exp) 
        (cases expression exp
          (list-exp (_) #t)
          (else #f)
        )
      )
      (list-head-prim (e) 
        (let ([l (eval-expression e env)])
          (cases list-type l
            (non-empty-list (first rest) (eval-expression first env))
            (empty-list () (eopl:error 'list-head "Cannot apply 'list-head' to an empty list"))
          )
        )       
      )
      (list-tail-prim (e)
        (let ([l (eval-expression e env)])
          (cases list-type l
            (non-empty-list (first rest) 
              (if (null? rest)
                (empty-list)
                (non-empty-list (car rest) (cdr rest))
              )
            )
            (empty-list () (eopl:error 'list-tail "Cannot apply 'list-tail' to an empty list"))
          )
        )
      )
      (list-append-prim (e1 e2)
        (let ([eval-exp-1 (eval-expression e1 env)] [eval-exp-2 (eval-expression e2 env)] [id (var-exp->id e1)])
          (cases list-type eval-exp-1
            (non-empty-list (f1 r1) 
              (if (list-type? eval-exp-2)
                (let ([f2 (list->first eval-exp-2)] [r2 (list->rest eval-exp-2)])
                  (setref! (apply-env-ref env id) (non-empty-list f1 (append-aux r1 (cons f2 r2))))
                )
                (setref! (apply-env-ref env id) (non-empty-list f1 (append-aux r1 (list e2))))
              )
            )
            (empty-list () (setref! (apply-env-ref env id) (non-empty-list e2 empty)))
          ) 
        )    
      )
      (ref-list-prim (e1 e2)
        (let ([eval-exp-1 (eval-expression e1 env)] [idx (eval-expression e2 env)])
          (cases list-type eval-exp-1
            (non-empty-list (first rest) (ref-list/tuple-aux (cons first rest) idx env) )
            (empty-list () (eopl:error 'ref-list "Index out of range"))
          )  
        )
      )
      (set-list-prim (e1 e2 e3)
        (let ([eval-exp1 (eval-expression e1 env)] [idx (eval-expression e2 env)] [id (var-exp->id e1)])
          (cases list-type eval-exp1
            (non-empty-list (first rest) 
              (setref! (apply-env-ref env id) (set-list-aux (cons first rest) idx e3))
            )
            (empty-list () (eopl:error 'set-list "Index out of range"))
          )
        )
      )

      ;tuple and tuple prims
      (tuple-exp (tuple) tuple)
      (create-tuple-prim (first rest) (non-empty-tuple first rest))
      (empty-tuple?-prim (exp) 
        (cases tuple-type exp
          (empty-tuple () #t)
          (else #f)
        )
      )
      (tuple?-prim (exp) 
        (cases expression exp
          (tuple-exp (_) #t)
          (else #f)
        )
      )
      (tuple-head-prim (e) 
        (let ([t (eval-expression e env)])
          (cases tuple-type t
            (non-empty-tuple (first rest) (eval-expression first env))
            (empty-tuple () (eopl:error 'tuple-head "Cannot apply 'tuple-head' to an empty tuple"))
          )
        )       
      )
      (tuple-tail-prim (e)
        (let ([t (eval-expression e env)])
          (cases tuple-type t
            (non-empty-tuple (first rest) 
              (if (null? rest)
                (empty-tuple)
                (non-empty-tuple (car rest) (cdr rest))
              )
            )
            (empty-tuple () (eopl:error 'tuple-tail "Cannot apply 'tuple-tail' to an empty tuple"))
          )
        )
      )
      (ref-tuple-prim (e1 e2)
        (let ([eval-exp-1 (eval-expression e1 env)] [idx (eval-expression e2 env)])
          (cases tuple-type eval-exp-1
            (non-empty-tuple (first rest) (ref-list/tuple-aux (cons first rest) idx env) )
            (empty-tuple () (eopl:error 'ref-tuple "Index out of range"))
          )  
        )
      )

      ;dict and dict prims
      (dict-exp (d) d)
      (create-dict-prim (id rand ids rands) (dict id rand ids rands))
      (dict?-prim (exp) 
        (cases expression exp
          (dict-exp (_) #t)
          (else #f)
        )
      )
      (ref-dict-prim (e1 e2)
        (let ([eval-exp-1 (eval-expression e1 env)] [search-key (var-exp->id e2)])
          (cases dict-type eval-exp-1
            (dict (key val keys vals) 
              (if (eqv? search-key key)
                (eval-expression val env)
                (ref-dict-aux keys vals search-key env)
              )
            )
          )
        )   
      )
      (set-dict-prim (e1 e2 e3)
        (let ([eval-exp-1 (eval-expression e1 env)] [search-key (var-exp->id e2)] [id (var-exp->id e1)])
          (cases dict-type eval-exp-1
            (dict (key val keys vals) (setref! (apply-env-ref env id) (set-dict-aux key val keys vals search-key e3)))
          )
        )
      )

      ;booleans
      (bool-exp (exp) (eval-bool-type exp env))
      
      ;for cicle
      (for-exp (identifier e2 e3)
        (cases expression e2
          (var-exp (id) 
            (let* 
              (
                [list-type (apply-env env id)] 
                [first (list->first list-type)] 
                [rest (list->rest list-type)]
                [vals (eval-let-exp-rands (cons first rest) env)]
              )
              (for-exp-aux identifier vals e3 env)
            )
          )
          (list-exp (list-type) 
            (let* 
              (
                [first (list->first list-type)] 
                [rest (list->rest list-type)]
                [vals (eval-let-exp-rands (cons first rest) env)]
              )
              (for-exp-aux identifier vals e3 env)
            )
          )
          (else (eopl:error 'for "Not supported type for iterable"))
        )
      )

      ;while cicle
      (while-exp (bool-type body)
        (let loop ([truth-val (eval-bool-type bool-type env)])
          (if truth-val
            (begin
              (eval-expression body env)
              (loop (eval-bool-type bool-type env))
            )
            'done
          )
        )
      )

      (else 'meFalta)
    )
  )
)

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
              (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

(define eval-var-decl-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-var-decl-exp-rand x env))
         rands)))

(define eval-var-decl-exp-rand
  (lambda (rand env)
    (indirect-target (eval-expression rand env))))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1)))))

;*******************************************************************************************
;for-exp

; función auxiliar utilizada para la implementación de un ciclo for. En cada iteración se extiende el
; ambiente con el id indicado y el valor correspondiente y se realiza la respectiva evaluación del
; body en este nuevo ambiente, este proceso termina hasta que ya no hayan más valores en vals y por
; ende no iteraciones a realizar.
(define for-exp-aux
  (lambda (id vals body env)
    (if (null? vals)
      'done
      (begin 
        body
        (eval-expression body (extend-env (list id) (list (car vals)) env))
        (display "\n")
        (for-exp-aux id (cdr vals) body env)  
      )
    )
  )
)

;*******************************************************************************************
;print-exp

; estas funciones hacen más amigable el lenguaje permitiendo mostrarle al usuario una traducción de la
; sintaxis abstracta que se entrega al trabajar con listas/tuplas/diccionarios.

; función auxiliar utilizada para la implementación de print en el case list-exp. "Imprime" una lista.
(define print-list
  (lambda (l env)
    (cases list-type l
      (non-empty-list (first rest) (display (eval-primapp-exp-rands (cons first rest) env)))
      (empty-list () (display 'empty-list))
    )
  )
)

; función auxiliar utilizada para la implementación de print en el case tuple-exp. "Imprime" una tupla.
(define print-tuple
  (lambda (t env)
    (cases tuple-type t
      (non-empty-tuple (first rest) (display (eval-primapp-exp-rands (cons first rest) env)))
      (empty-tuple () (display 'empty-tuple))
    )
  )
)

; función auxiliar utilizada para la implementación de print en el case dict-exp. "Imprime" un dict.
(define print-dict
  (lambda (d env)
    (cases dict-type d
      (dict (key val keys vals) 
        (display 
          (append-aux 
            (list (list key (eval-expression val env))) 
            (print-rest-dict-pairs keys vals env)
          )
        )
      )
    )
  )
)

; función auxiliar utilizada para imprimir los keys y vals de un diccionario después del primer par
; llave, valor.
(define print-rest-dict-pairs
  (lambda (keys vals env)
    (if (null? keys)
      empty
      (cons 
        (list (car keys) (eval-expression (car vals) env)) 
        (print-rest-dict-pairs (cdr keys) (cdr vals) env)
      )
    )
  )
)

;*******************************************************************************************
;Bools

; función auxixiliar utilizada para implementar procesar types booleanos.
(define eval-bool-type
  (lambda (exp env)
    (cases bool-type exp
      (pred-prim-app (pred-prim exp1 exp2) (eval-pred-prim pred-prim exp1 exp2 env))
      (bin-prim-app (bin-prim type1 type2) (eval-bin-prim bin-prim type1 type2 env))
      (un-prim-app (un-prim type) (eval-un-prim un-prim type env))
      (true-bool () #t)
      (false-bool () #f)
    )
  )
)

; función auxiliar utilizada para evaluar predicados.
(define eval-pred-prim
  (lambda (prim exp1 exp2 env)
    (cases pred-prim prim
      (lower-prim () 
        (let ([val1 (eval-expression exp1 env)] [val2 (eval-expression exp2 env)])
          (< val1 val2)
        )
      )
      (greater-prim () 
        (let ([val1 (eval-expression exp1 env)] [val2 (eval-expression exp2 env)])
          (> val1 val2)
        )
      )
      (lower-equal-prim () 
        (let ([val1 (eval-expression exp1 env)] [val2 (eval-expression exp2 env)])
          (<= val1 val2)
        )
      )
      (greater-equal-prim () 
        (let ([val1 (eval-expression exp1 env)] [val2 (eval-expression exp2 env)])
          (>= val1 val2)
        )
      )
      (equal-prim () 
        (let ([val1 (eval-expression exp1 env)] [val2 (eval-expression exp2 env)])
          (equal? val1 val2)
        )
      )
      (not-equal-prim () 
        (let ([val1 (eval-expression exp1 env)] [val2 (eval-expression exp2 env)])
          (not (equal? val1 val2))
        )
      )
    )
  )
)

; función auxiliar utilizada para evaluar valores mediante primitivas booleanas binarias.
(define eval-bin-prim
  (lambda (prim type1 type2 env)
    (cases bin-prim prim
      (and-prim () 
        (let ([val1 (eval-bool-type type1 env)] [val2 (eval-bool-type type2 env)])
          (and val1 val2)
        )
      )
      (or-prim ()
        (let ([val1 (eval-bool-type type1 env)] [val2 (eval-bool-type type2 env)])
          (or val1 val2)
        )
      )
    )
  )
)

; función auxiliar utilizada para evaluar valores mediante primitivas booleanas unarias.
(define eval-un-prim
  (lambda (prim type env)
    (cases un-prim prim
      (not-prim () 
        (let ([val (eval-bool-type type env)])
          (not val)
        )
      )
    )
  )
)

;*******************************************************************************************
;Lists/Tuples

; función auxiliar utilizada para implementar la primitiva ref-list. Retorna el valor en el índice
; indicado.
(define ref-list/tuple-aux
  (lambda (l idx env)
    (if (null? l)
      (eopl:error 'ref-list "Index out of range")
      (if (= idx 0)
        (eval-expression (car l) env)
        (ref-list/tuple-aux (cdr l) (- idx 1) env)
      )
    )
  )
)

; función auxiliar utilizada para implementar la primitiva set-list. Retorna una nueva lista donde el
; valor en el índice indicado es reemplazado por el nuevo valor indicado.
(define set-list-aux
  (lambda (l idx val)
    (if (null? l)
      (eopl:error 'set-list "Index out of range")
      (if (= idx 0)
        (non-empty-list val (cdr l))
        (non-empty-list (car l) (replace-nth-element (cdr l) idx val))
      )
    )
  )
)

; función auxiliar que reemplaza el n-ésimo elemento de una lista por el valor indicado.
(define replace-nth-element
  (lambda (l idx val)
    (cond
      ([null? l] (eopl:error 'set-list "Index out of range"))
      ([= idx 0] (cons val (cdr l)))
      (else (cons (car l) (replace-nth-element (cdr l) (- idx 1) val)))
    )
  )
)

;*******************************************************************************************
;Dicts

; función auxiliar utilizada para implementar la primitiva ref-dict. Retorna el valor asociado a la
; llave indicada.
(define ref-dict-aux
  (lambda (ids rands search-id env)
    (if (null? ids)
      (eopl:error 'ref-dict "Key not found")
      (if (eqv? search-id (car ids))
        (eval-expression (car rands) env)
        (ref-dict-aux (cdr ids) (cdr rands) search-id env)
      )
    )
  )
)

; función auxiliar utilizada para implementar la primitiva set-dict. Retorna un nuevo dict donde
; la llave indicada es reemplazada por el nuevo valor indicado.
(define set-dict-aux
  (lambda (key val keys vals search-key new-val)
    (cond 
      ([eqv? search-key key] (dict key new-val keys vals))
      ([null? keys] (eopl:error 'set-dict "Key not found"))
      (else (dict key val keys (replace-dict-vals-element keys vals search-key new-val)))
    )
  )
)

; función auxiliar que recorre desde la segunda key hasta la última y reemplaza el valor 
; correspondiente a esa key con el nuevo valor.
(define replace-dict-vals-element
  (lambda (keys vals search-key new-val)
    (cond
      ([null? keys] (eopl:error 'set-dict "Key not found"))
      ([eqv? search-key (car keys)] (cons new-val (cdr vals)))
      (else (cons (car vals) (replace-dict-vals-element (cdr keys) (cdr vals) search-key new-val)))
    )
  )
)

;*******************************************************************************************
;Extractors

; extractor que a partir de una expresión tipo var-exp retorna el símbolo de esta.
(define var-exp->id
  (lambda (var)
    (cases expression var
      (var-exp (id) id)
      (else (eopl:error 'exp->id "Not a var-exp"))
    )
  )
)

; extractor que a partir de un list-type retorna el primer elemento de este.
(define list->first
  (lambda (l)
    (cases list-type l
      (non-empty-list (first rest) first)
      (empty-list () empty)
    )
  )
)

; extractor que a partir de un list-type retorna todos los elementos menos el primero de este.
(define list->rest
  (lambda (l)
    (cases list-type l
      (non-empty-list (first rest) rest)
      (empty-list () empty)
    )
  )
)

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env)))
            ) 
            (iota len) idss bodies
          ) 
          env
        )
      )
    )
  )
)

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    ;(begin
     ; (display env)
      ;(display "jajajaj ")
      (deref (apply-env-ref env sym))))
    ;)
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (cond
      ([number? x] #t)
      ([procval? x] #t)
      ([list-type? x] #t)
      ([dict-type? x] #t)
      ([tuple-type? x] #t)
      ([bool-type? x] #t)
      (else #f)
    )
  )
)

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
      ((ref 
        (cases target (primitive-deref ref)
          (direct-target (expval1) ref)
          (indirect-target (ref1) ref1))
      ))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;; append-aux:
;; Propósito:
;; L1, L2 -> L1 + L2: Procedimiento que toma dos listas y
;; retorna la concatenación de ambas.
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define append-aux
  (lambda (L1 L2)
    (if (null? L1) L2
      (cons (car L1) (append-aux (cdr L1) L2))
    )
  )
)

(define (exact-nonnegative-integer? v)
  (and (exact? v) (not (negative? v)))
)

;******************************************************************************************
;Pruebas



;(scan&parse)
;(interpretador)
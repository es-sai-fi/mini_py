#lang eopl

;*******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales y ligadura local

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
;;                  ::= if <expresion> then <expresion> else <expresion>
;;                  ::= let (identifier = expression)* in expression
;;  <primitive>     ::= + | - | * | add1 | sub1 
;;                  ::= eval-circuit | connect-circuits | merge-circuits
;;  <circuit>       ::= <gate_list>
;;  <gate_list>     ::= empty | <gate> <gate_list>
;;  <gate>          ::= <identifier> <type> <input list>
;;  <type>          ::= and | or | not | xor
;;  <input_list>    ::= empty | <bool> <input_list> | <identifier> <input_list>
;;  <bool>          ::= True | False
;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
  '(
    (white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
  )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    (program (expression) a-program)

    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression (primitive "("(separated-list expression ",")")") primapp-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression (circuit) circuit-exp)
    (expression (type) type-exp)
    (expression ("'" identifier) connect-circuits-identifier-exp)
    (expression ("True") true-bool-exp)
    (expression ("False") false-bool-exp)

    (circuit ("circuit" "(" gate-list ")") a-circuit)

    (gate-list ("empty-gate-list") empty-gate-list)
    (gate-list ("gate-list" "(" gate (arbno gate) ")") a-gate-list)

    (gate ("gate" "(" identifier type input-list ")") a-gate)

    (type ("and") and-type)
    (type ("or") or-type)
    (type ("xor") xor-type)
    (type ("not") not-type)

    (input-list ("empty-input-list") empty-input-list)
    (input-list ("input-list" "(" expression (arbno expression) ")") an-input-list)

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") connect-circuits-prim)
    (primitive ("merge-circuits") merge-circuits-prim)
  )
)

;Construidos automáticamente:
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter))
)

;*******************************************************************************************
;Parser, Scanner, Interfaz

;FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter)
)

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter)
)

;El Interpretador (FrontEnd + Evaluación + señal para lectura )
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser scanner-spec-simple-interpreter grammar-simple-interpreter)
  )
)
;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body) (eval-expression body (init-env)))
    )
  )
)

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(c1)
     (list (a-circuit (empty-gate-list)))
     (empty-env)
    )
  )
)

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
        (let 
          ((args (eval-rands rands env)))         
          (apply-primitive prim args env)
        )
      )
      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)
        )
      )
      (let-exp (ids rands body)
        (let 
          ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))
        )
      )
      (circuit-exp (circuit) circuit)
      (type-exp (type) type)
      (connect-circuits-identifier-exp (identifier) identifier)
      (true-bool-exp () #t)
      (false-bool-exp () #f)
      (else exp)
    )
  )
)

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands))
)

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env))
)

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args env)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (eval-circuit-prim ()
        (let ([circ (car args)])
          (eval-gate-list (circuit->gate-list circ) env)
        )
      )
      (connect-circuits-prim ()
        (let ([c1 (car args)] [c2 (cadr args)] [old-id (caddr args)])
          (connect-circuits c1 c2 old-id)
        )
      )
      (merge-circuits-prim ()
        (let ([c1 (car args)] [c2 (cadr args)] [type (caddr args)] [id (cadddr args)] )
          (merge-circuits c1 c2 type id)
        )
      )
    )
  )
)

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))
  )
)

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?)) (vals (list-of scheme-value?)) (env environment?))
)

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record) ;llamado al constructor de ambiente vacío 
  )
)       

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)
  )
) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let 
          ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-env env sym)
          )
        )
      )
    )
  )
)

;*******************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)
  )
)

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else 
        (let 
          ((list-index-r (list-index pred (cdr ls))))
          (if (number? list-index-r)
            (+ list-index-r 1)
            #f
          )
        )
      )
    )
  )
)

; función auxiliar que conecta dos circuitos de forma secuencial, es decir, se cambia alguna entrada
; del circuito 2 por la salida del circuito 1. La entrada a cambiar es especificada por el usuario.
(define connect-circuits
  (lambda (c1 c2 old-id)
    (let 
      (
        [c1-first-gate (gate-list->first (circuit->gate-list c1))] 
        [c1-rest-gates (gate-list->rest(circuit->gate-list c1))]
        [c2-first-gate (gate-list->first (circuit->gate-list c2))]
        [c2-rest-gates (gate-list->rest(circuit->gate-list c2))]
        [new-id (return-last-identifier c1)]
      )
      (if (or (null? c1-first-gate) (null? c2-first-gate))
        (eopl:error 'connect-circuits "No connection for a non empty circuit and an empty circuit")
        (a-circuit
          (a-gate-list
            c1-first-gate
            (append-aux c1-rest-gates (map (lambda (g) (replace-gate-input-list g old-id new-id)) (cons c2-first-gate c2-rest-gates)))
          )
        )
      )
    )
  )
)

; función que reemplaza el input-list de un gate por el resultado de llamar a replace-input-list con
; el input-list del gate, el id a reemplazar y el id por el cual se va a reemplazar.
(define replace-gate-input-list
  (lambda (g old-id new-id)
    (cases gate g
      (a-gate (id type input-list)
        (a-gate id type (replace-input-list input-list old-id new-id))
      )
    )
  )
)

; función que reemplaza un input de un input-list por el resultado de llamar a replace-input con
; el input del input-list, el id a reemplazar y el id por el cual se va a reemplazar.
(define replace-input-list
  (lambda (i-list old-id new-id)
    (cases input-list i-list
      (empty-input-list () i-list)
      (an-input-list (first rest)
        (an-input-list
          (replace-input first old-id new-id)
          (map (lambda (i) (replace-input i old-id new-id)) rest)
        )
      )
    )
  )
)

; función que reemplaza el input como tal. Si el input equivale al id que se va a reemplazar entonces
; se reemplaza por el id a reemplazar.
(define replace-input
  (lambda (input old-id new-id)
    (cases expression input
      (var-exp (id)
        (if (eqv? id old-id)
          (var-exp new-id)
          input
        )
      )
      (else input)
    )
  )
)

; función auxiliar que conecta dos circuitos de forma paralela sobre un gate de tipo or | and | xor | not
; y con identificador especificado por el usuario
(define merge-circuits
  (lambda (c1 c2 type id)
    (let 
      (
        [c1-first-gate (gate-list->first (circuit->gate-list c1))] 
        [c1-rest-gates (gate-list->rest(circuit->gate-list c1))]
        [c2-first-gate (gate-list->first (circuit->gate-list c2))]
        [c2-rest-gates (gate-list->rest(circuit->gate-list c2))]
      )
      (if (or (null? c1-first-gate) (null? c2-first-gate))
        (eopl:error 'merge-circuits "No merge for a non empty circuit and an empty circuit")
        (a-circuit 
          (a-gate-list 
            c1-first-gate
            (append-aux 
              (append-aux c1-rest-gates (cons c2-first-gate c2-rest-gates)) 
              (list 
                (a-gate id type 
                  (an-input-list 
                    (var-exp (return-last-identifier c1)) 
                    (list (var-exp (return-last-identifier c2)))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

; extractor que a partir de un circuito retorna su gate-list
(define circuit->gate-list
  (lambda (circ)
    (cases circuit circ
      (a-circuit (gate-list) gate-list)
    )
  )
)

; extractor que a partir de un gate-list retorna su primer gate
(define gate-list->first
  (lambda (glist)
    (cases gate-list glist
      (empty-gate-list () empty)
      (a-gate-list (first rest) first)
    )
  )
)

; extractor que a partir de un gate-list retorna el resto de gates
(define gate-list->rest
  (lambda (glist)
    (cases gate-list glist
      (empty-gate-list () empty)
      (a-gate-list (first rest) rest)
    )
  )
)

; extractor que a partir de un gate retorna su identificador
(define gate->identifier
  (lambda (g)
    (cases gate g
      (a-gate (id type input-list) id)
    )
  )
)

; extractor que a partir de un gate retorna su type
(define gate->type
  (lambda (g)
    (cases gate g
      (a-gate (id type input-list) type)
    )
  )
)

; función que a partir de un circuito retorna el identificador de su último gate
(define return-last-identifier
  (lambda (circ)
    (let* ([glist (circuit->gate-list circ)] [first (gate-list->first glist)] [rest (gate-list->rest glist)])
      (if (null? first)
        (eopl:error 'return-last-identifier "No last identifier for an empty circuit")
        (return-last-identifier-aux first rest)
      )
    )
  )
)

; función auxiliar de return-last-identifier utilizada para recursión
(define return-last-identifier-aux
  (lambda (first rest)
    (if (null? rest)
      (gate->identifier first)
      (return-last-identifier-aux (car rest) (cdr rest))
    )
  )
)

; función que procesa un gate-list aplicandole cases y obtieniendo los elementos de este para luego
; invocar a la función auxiliar eval-gate-list-aux.
(define eval-gate-list
  (lambda (glist env)
    (let ([first (gate-list->first glist)] [rest (gate-list->rest glist)])
      (if (null? first)
        (eopl:error 'eval-gate-list "No evaluation for gate-list with no gates")
        (eval-gate-list-aux first rest env)
      )
    )
  )
)

; función auxiliar recursiva utilizada para la evaluación de un gate-list.
(define eval-gate-list-aux
  (lambda (first rest env)
    (let* ([res (eval-gate first env)] [newEnv (extend-env (list (car res)) (list (cadr res)) env)])
      (if (null? rest) 
        (cadr res)
        (eval-gate-list-aux (car rest) (cdr rest) newEnv)
      )
    )    
  )
)

; función que permite evaluar un gate de acuerdo a su tipo. Retorna una lista donde el primer elemento
; es el id del gate que se evaluó y el segundo elemento es el valor de verdad.
(define eval-gate
  (lambda (g env)
    (cases gate g
      (a-gate (id t inputList) 
        (cases input-list inputList
          (an-input-list (first rest) 
            (cases type t
              (and-type () (list id (and (eval-input-item first env) (eval-input-item (car rest) env))))
              (or-type () (list id (or (eval-input-item first env) (eval-input-item (car rest) env))))
              (not-type () (list id (not (eval-input-item first env))))
              (xor-type () (list id (xor (eval-input-item first env) (eval-input-item (car rest) env))))
            )
          )
          (empty-input-list () (eopl:error 'eval-gate "No evaluation for gate with no inputs"))
        )
      )
    )
  )
)

; función que permite procesar los inputs de un input-list. Esta función es necesaria porque se requiere
; tener un control de que tipo de expressions pueden ser procesadas en un input-list puesto que por
; definición un input-list recibe cualquier tipo de expression.
(define eval-input-item
  (lambda (input env)
    (cases expression input
      (true-bool-exp () #t)
      (false-bool-exp () #f)
      (var-exp (id) (apply-env env id))
      (else (eopl:error 'eval-input-item "Unsupported type for input-item"))
    )
  )
)

; función utilizada para implementar xor en nuestro lenguaje.
(define xor 
  (lambda (a b)
    (and (or a b) (not (and a b)))
  )
)

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

(interpretador)

;; Ejemplos eval-circuit

"let C1 = circuit(empty-gate-list) in eval-circuit(C1)"

; 3.1

"let A = True C1 = circuit(gate-list(gate(G1 not input-list(A)))) in eval-circuit(C1)"

; 3.2

"let A = True B = True C1 = circuit(gate-list(gate(G1 and input-list(A B)))) in eval-circuit(C1)"

; 3.3

"let A = True B = True C1 = circuit(gate-list(gate(G1 or input-list(A B)) gate(G2 and input-list(A B)) gate(G3 not input-list(G2)) gate(G4 and input-list(G1 G3)))) in eval-circuit(C1)"
"let A = False B = True C1 = circuit(gate-list(gate(G1 or input-list(A B)) gate(G2 and input-list(A B)) gate(G3 not input-list(G2)) gate(G4 and input-list(G1 G3)))) in eval-circuit(C1)"
"let A = False B = False C1 = circuit(gate-list(gate(G1 or input-list(A B)) gate(G2 and input-list(A B)) gate(G3 not input-list(G2)) gate(G4 and input-list(G1 G3)))) in eval-circuit(C1)"
"let A = True B = False C1 = circuit(gate-list(gate(G1 or input-list(A B)) gate(G2 and input-list(A B)) gate(G3 not input-list(G2)) gate(G4 and input-list(G1 G3)))) in eval-circuit(C1)"

;; Ejemplos connect-circuits

; connect 3.1 y 3.2 en A

"let C1 = circuit(gate-list(gate(G1 not input-list(A)))) C2 = circuit(gate-list(gate(G2 and input-list(A B)))) in connect-circuits(C1, C2, 'A)"

; connect 3.1 y 3.3 en A

"let C1 = circuit(gate-list(gate(G1 not input-list(A)))) C2 = circuit(gate-list(gate(G2 or input-list(A B)) gate(G3 and input-list(A B)) gate(G4 not input-list(G3)) gate(G5 and input-list(G2 G4)))) in connect-circuits(C1, C2, 'A)"

; connect 3.2 y 3.3 en A

"let C1 = circuit(gate-list(gate(G1 and input-list(A B)))) C2 = circuit(gate-list(gate(G2 or input-list(A B)) gate(G3 and input-list(A B)) gate(G4 not input-list(G3)) gate(G5 and input-list(G2 G4)))) in connect-circuits(C1, C2, 'A)"

; connect 3.1 y 3.2 en B

"let C1 = circuit(gate-list(gate(G1 not input-list(A)))) C2 = circuit(gate-list(gate(G2 and input-list(A B)))) in connect-circuits(C1, C2, and, 'B)"

; connect 3.1 y 3.3 en B

"let C1 = circuit(gate-list(gate(G1 not input-list(A)))) C2 = circuit(gate-list(gate(G2 or input-list(A B)) gate(G3 and input-list(A B)) gate(G4 not input-list(G3)) gate(G5 and input-list(G2 G4)))) in connect-circuits(C1, C2, 'B)"

; connect 3.2 y 3.3 en B

"var C1 = circuit(gate-list(gate(G1 and input-list(A B)))), C2 = circuit(gate-list(gate(G2 or input-list(A B)) gate(G3 and input-list(A B)) gate(G4 not input-list(G3)) gate(G5 and input-list(G2 G4)))) in connect-circuits(C1, C2, B)"

;; Ejemplos merge-circuits

; merge 3.1 y 3.2

"let C1 = circuit(gate-list(gate(G1 not input-list(A)))) C2 = circuit(gate-list(gate(G2 and input-list(A B)))) in merge-circuits(C1, C2, and, 'G3)"

; merge 3.1 y 3.3

"var C1 = circuit(gate-list(gate(G1 not input-list(A)))), C2 = circuit(gate-list(gate(G2 or input-list(A B)) gate(G3 and input-list(A B)) gate(G4 not input-list(G3)) gate(G5 and input-list(G2 G4)))) in merge-circuits(C1, C2, and, 'G6)"

; merge circuitos anteriores

"let C1 = circuit(gate-list(gate(G1 not input-list(A)))) C2 = circuit(gate-list(gate(G2 and input-list(A B)))) C3 = circuit(gate-list(gate(G4 not input-list(A)))) C4 = circuit(gate-list(gate(G5 or input-list(A B)) gate(G6 and input-list(A B)) gate(G7 not input-list(G6)) gate(G8 and input-list(G5 G7)))) in merge-circuits(merge-circuits(C1, C2, and, 'G3), merge-circuits(C3, C4, or, 'G9), xor, 'G10)"
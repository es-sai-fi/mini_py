#lang eopl

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial, asignación de variables, paso por valor,
;;;;; paso por referencia, ciclos while y for, declaración de variables mutables e inmutables,
;;;;; circuitos y sus primitivas, y POO.

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <bool-type>
;;                      <bool-exp (b-type)>
;;                  ::= <list-type>
;;                      <list-exp (l-type)>
;;                  ::= <tuple-type>
;;                      <tuple-exp (t-type)>
;;                  ::= <dict-type>
;;                      <dict-exp (d-type)>
;;                  ::= <string-type>
;;                      <string-exp (str-type)>
;;                  ::= <hex-type>
;;                      <hex-exp (h-type)>
;;
;;                  ::= empty-list?(<list-type>)
;;                      <empty-list?-prim (l-type)>
;;                  ::= list?(<expression>)
;;                      <list?-prim (exp)>
;;                  ::= list-append(<expression> , <expression>)
;;                      <list-append-prim (exp1 exp2)> 
;;                  ::= list-head(<expression>)
;;                      <list-head-prim (exp)> 
;;                  ::= list-tail(<expression>)
;;                      <list-tail-prim (exp)> 
;;                  ::= ref-list(<expression> , <expression>)
;;                      <ref-list-prim (exp)> 
;;                  ::= set-list(<expression>, <expression> , <expression>)
;;                      <set-list-prim (exp1 exp2 exp3)>
;;
;;                  ::= empty-tuple?(<tuple-type>)
;;                      <empty-tuple?-prim (t-type)>
;;                  ::= tuple?(<expression>)
;;                      <tuple?-prim (exp)>
;;                  ::= tuple-head(<expression>)
;;                      <tuple-head-prim (exp)> 
;;                  ::= tuple-tail(<expression>)
;;                      <tuple-tail-prim (exp)> 
;;                  ::= ref-tuple(<expression> , <expression>)
;;                      <ref-tuple-prim (exp)>
;;
;;                  ::= dict?(<expression>)
;;                      <dict?-prim (exp)>
;;                  ::= ref-dict(<expression> , <expression>)
;;                      <ref-dict-prim (exp)> 
;;                  ::= set-dict(<expression>, <expression> , <expression>)
;;                      <set-dict-prim (exp1 exp2 exp3)>
;;
;;                  ::= hex-to-dec(<expression>)
;;                      <hex-to-dec-prim (exp)>
;;                  ::= dec-to-hex(<expression>)
;;                      <dec-to-hex-prim (exp)>
;;                  ::= sum-hex(<expression> , <expression>)
;;                      <sum-hex-prim (exp1 exp2)> 
;;                  ::= sub-hex(<expression> , <expression>)
;;                      <sub-hex-prim (exp1 exp2)> 
;;                  ::= mulp-hex(<expression> , <expression>)
;;                      <mulp-hex-prim (exp1 exp2)> 
;;                  ::= sub1-hex(<expression>)
;;                      <sub-hex-prim (exp)>
;;                  ::= add1-hex(<expression>)
;;                      <add1-hex-prim (exp)>
;;
;;                  ::= eval-circuit(<expression>)
;;                      <eval-circuit-prim (exp)>
;;                  ::= connect-circuits(<expression> , <expression> , <identifier>)
;;                      <connect-circuits-prim (exp1 exp2 id)>
;;                  ::= merge-circuits(<expression> , <expression> , <type> , <identifier>)
;;                      <merge-circuits (exp1 exp2 type id)>
;;
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;
;;                  ::= if <bool-type> then <expresion> else <expression> end
;;                      <if-exp (b-type exp2 exp3)>
;;                  ::= while <bool-type> do <expresion> done
;;                      <while-exp (b-type body)>
;;
;;                  ::= var {<identifier> = <expression>}*(,) in <expression>
;;                      <var-decl-exp (ids rands body)>
;;                  ::= const {<identifier> = <expression>}*(,) in <expression>
;;                      <const-decl-exp (ids rands body)>
;;                  ::= rec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <rec-decl-exp(proc-names idss bodies bodyletrec)>
;;
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*(,))
;;                      <app-exp proc rands>
;;                  ::= begin <expression> {; <expression>}* end
;;
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;
;;  <bool-type>     ::= <pred-prim>(<expression> , <expression>)
;;                      <pred-prim-app (prim exp1 exp2)>
;;                  ::= <bin-prim>(<expression> , <expression>)
;;                      <bin-prim-app (prim exp1 exp2)>
;;                  ::= <un-prim>(<expression>)
;;                      <un-prim-app (prim exp)>
;;                  ::= true
;;                      <true-bool ()>
;;                  ::= false
;;                      <false-bool ()>
;;  <dict-type>     ::= dict({<identifier> = <expression>}+(,))
;;                      <dict (key val keys vals)>
;;  <list-type>     ::= list({<expression>}+(,))
;;                      <non-empty-list (first rest)>
;;                  ::= empty-list
;;                      <empty-list ()>
;;  <tuple-type>    ::= tuple({<expression>}+(,))
;;                      <non-empty-tuple (first rest)>
;;                  ::= empty-tuple
;;                      <empty-tuple ()>
;;  <circuit-type>  ::= <gate-list>
;;  <gate-list>     ::= empty | <gate> <gate_list>
;;  <gate>          ::= <identifier> <type> <input list>
;;  <type>          ::= and | or | not | xor
;;  <input-list>    ::= empty | <bool> <input_list> | <identifier> <input_list>
;;  <bin-prim>      ::= and
;;                  ::= <and-prim ()>
;;                  ::= or
;;                  ::= <or-prim ()>
;;  <un-prim>       ::= not
;;                  ::= <not-prim ()>
;;  <pred-prim>     ::= <
;;                  ::= <lower-prim ()>
;;                  ::= >
;;                  ::= <greater-prim ()>
;;                  ::= <=
;;                  ::= <lower-equal-prim ()>
;;                  ::= >=
;;                  ::= <greater-equal-prim ()>
;;                  ::= ==
;;                  ::= <equal-prim ()>
;;                  ::= !=
;;                  ::= <not-equal-prim ()>
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

    (expression ("proc(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ( "(" expression (separated-list expression ",") ")") app-exp)
    (expression ("set" identifier "=" expression) set-exp)

    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) var-decl-exp)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression) const-decl-exp)
    (expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) rec-decl-exp)

    ;predefined data types
    (expression (list-type) list-exp)
    (expression (tuple-type) tuple-exp)
    (expression (dict-type) dict-exp)
    (expression (bool-type) bool-exp)
    (expression (string-type) string-exp)
    (expression (hex-type) hex-exp)
    (expression (circuit-type) circuit-exp)

    ;circuit def
    (circuit-type ("circuit" "(" gate-list ")") circuit)

    ;gate-list def
    (gate-list ("empty-gate-list") empty-gate-list)
    (gate-list ("gate-list" "(" gate (arbno gate) ")") a-gate-list)

    ;gate def
    (gate ("gate" "(" identifier type input-list ")") a-gate)

    (type ("and") and-type)
    (type ("or") or-type)
    (type ("xor") xor-type)
    (type ("not") not-type)

    ;input-list def
    (input-list ("empty-input-list") empty-input-list)
    (input-list ("input-list" "(" expression (arbno expression) ")") an-input-list)

    ;hex def
    (hex-type ("x16(-" (arbno number) ")") neg-hex)
    (hex-type ("x16(+" (arbno number) ")") pos-hex)

    ;str def
    (string-type ("#" identifier) a-str)

    ;list def
    (list-type ("list(" expression (arbno "," expression) ")") non-empty-list)
    (list-type ("empty-list") empty-list)

    ;tuple def
    (tuple-type ("tuple(" expression (arbno "," expression) ")") non-empty-tuple)
    (tuple-type ("empty-tuple") empty-tuple)

    ;dict def
    (dict-type ("dict(" identifier "=" expression (arbno "," identifier "=" expression) ")") dict)
    
    ;bool def
    (bool-type ("true") true-bool)
    (bool-type ("false") false-bool)
    
    ;bools prims
    (bool-type (pred-prim "(" expression "," expression ")") pred-prim-app)
    (bool-type (bin-prim "(" expression "," expression ")") bin-prim-app)
    (bool-type (un-prim "(" expression ")") un-prim-app)

    ;lists prims
    (expression ("empty-list?" "(" list-type ")") empty-list?-prim)
    (expression ("list?" "(" expression ")") list?-prim)
    (expression ("list-append" "(" expression "," expression ")") list-append-prim)
    (expression ("list-head" "(" expression ")") list-head-prim)
    (expression ("list-tail" "(" expression ")") list-tail-prim)
    (expression ("ref-list" "(" expression "," expression ")") ref-list-prim)
    (expression ("set-list" "(" expression "," expression "," expression ")") set-list-prim)

    ;tuples prims
    (expression ("empty-tuple?" "(" tuple-type ")") empty-tuple?-prim)
    (expression ("tuple?" "(" expression ")") tuple?-prim)
    (expression ("tuple-head" "(" expression ")") tuple-head-prim)
    (expression ("tuple-tail" "(" expression ")") tuple-tail-prim)
    (expression ("ref-tuple" "(" expression "," expression ")") ref-tuple-prim)

    ;dicts prims
    (expression ("dict?" "(" expression ")") dict?-prim)
    (expression ("ref-dict" "(" expression "," identifier ")") ref-dict-prim)
    (expression ("set-dict" "(" expression "," identifier "," expression ")") set-dict-prim)

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

    ;hex prims
    (expression ("hex-to-dec(" expression ")") hex-to-dec-prim)
    (expression ("dec-to-hex(" expression ")") dec-to-hex-prim)
    (expression ("sum-hex(" expression "," expression ")") sum-hex-prim)
    (expression ("sub-hex(" expression "," expression  ")") sub-hex-prim)
    (expression ("add1-hex(" expression ")") add1-hex-prim)
    (expression ("sub1-hex(" expression ")") sub1-hex-prim)
    (expression ("mulp-hex(" expression "," expression  ")") mulp-hex-prim)

    ;string prims
    (expression ("str-concat" "(" expression "," expression ")") str-concat-prim)
    (expression ("str-len" "(" expression ")") str-len-prim)

    ;circuit prims
    (expression ("eval-circuit(" expression ")") eval-circuit-prim)
    (expression ("connect-circuits(" expression "," expression "," identifier ")") connect-circuits-prim)
    (expression ("merge-circuits(" expression "," expression "," type "," identifier ")") merge-circuits-prim)

    ;int prims
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
  )
)

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
      (list (direct-target 1) (direct-target 5) (direct-target 10))
      (list #f #f #f)
      (empty-env)
    )
  )
)

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
          (string-exp (str-type) (display (string-type->str str-type)))
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
      (proc-exp (ids body) (closure ids body env))
      (app-exp (rator rands)
        (let* 
          (
            [proc (eval-expression rator env)]
            [args (eval-rands rands env)]
            [const-tags (make-list (length args) #f)]
          )
          (if (procval? proc)
            (apply-procedure proc args const-tags)
            (eopl:error 'eval-expression "Attempt to apply non-procedure ~s" proc)
          )
        )
      )

      (set-exp (id rhs-exp)
        (begin
          (setref! (apply-env-ref env id) (eval-expression rhs-exp env) env)
          'setted
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
      
      ;decls
      (var-decl-exp (ids rands body) 
        (let* ([args (eval-let/var/const-exp-rands rands env)] [const-tags (make-list (length args) #f)])
          (eval-expression body (extend-env ids args const-tags env))
        )
      )
      (const-decl-exp (ids rands body)
        (let* ([args (eval-let/var/const-exp-rands rands env)] [const-tags (make-list (length args) #t)])
          (eval-expression body (extend-env ids args const-tags env))
        )
      )
      (rec-decl-exp (proc-names idss bodies rec-body)
        (eval-expression rec-body (extend-env-recursively proc-names idss bodies env))
      )

      ;list and list prims
      (list-exp (l) l)
      (empty-list?-prim (exp) 
        (let ([l (eval-expression exp env)])
          (cases list-type l
            (empty-list () #t)
            (else #f)
          )
        )
      )
      (list?-prim (exp) 
        (let ([l (eval-expression exp env)])
          (cases list-type l
            (non-empty-list (first rest) #t)
            (empty-list () #t)
          )
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
                (let ([f2 (list-type->first eval-exp-2)] [r2 (list-type->rest eval-exp-2)])
                  (begin
                    (setref! (apply-env-ref env id) (non-empty-list f1 (append-aux r1 (cons f2 r2))) env)
                    'setted
                  )
                )
                (begin
                  (setref! (apply-env-ref env id) (non-empty-list f1 (append-aux r1 (list e2))) env)
                  'setted
                )
              )
            )
            (empty-list () 
              (begin
                (setref! (apply-env-ref env id) (non-empty-list e2 empty) env) 
                'setted
              )
            )
          ) 
        )    
      )
      (ref-list-prim (e1 e2)
        (let ([eval-exp-1 (eval-expression e1 env)] [idx (eval-expression e2 env)])
          (if (and (exact? idx) (positive? idx))
            (cases list-type eval-exp-1
              (non-empty-list (first rest) (ref-list/tuple-aux (cons first rest) idx env) )
              (empty-list () (eopl:error 'ref-list "Index out of range"))
            )
            (eopl:error 'ref-list-prim "Index not an exact/positive number")
          )  
        )
      )
      (set-list-prim (e1 e2 e3)
        (let ([eval-exp1 (eval-expression e1 env)] [idx (eval-expression e2 env)] [id (var-exp->id e1)])
          (cases list-type eval-exp1
            (non-empty-list (first rest) 
              (begin
                (setref! (apply-env-ref env id) (set-list-aux (cons first rest) idx e3) env)
                'setted
              )
            )
            (empty-list () (eopl:error 'set-list "Index out of range"))
          )
        )
      )

      ;tuple and tuple prims
      (tuple-exp (tuple) tuple)
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
          (if (and (exact? idx) (positive? idx))
            (cases tuple-type eval-exp-1
              (non-empty-tuple (first rest) (ref-list/tuple-aux (cons first rest) idx env) )
              (empty-tuple () (eopl:error 'ref-tuple "Index out of range"))
            )  
            (eopl:error 'ref-tuple-prim "Index not an exact/positive number")
          )
        )
      )

      ;dict and dict prims
      (dict-exp (d) d)
      (dict?-prim (exp) 
        (cases expression exp
          (dict-exp (_) #t)
          (else #f)
        )
      )
      (ref-dict-prim (e1 search-key)
        (let ([eval-exp-1 (eval-expression e1 env)])
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
      (set-dict-prim (e1 search-key e2)
        (let ([eval-exp-1 (eval-expression e1 env)] [id (var-exp->id e1)])
          (cases dict-type eval-exp-1
            (dict (key val keys vals) 
              (begin
                (setref! (apply-env-ref env id) (set-dict-aux key val keys vals search-key e2) env) 
                'setted
              )
            )
          )
        )
      )

      ;string and string prims
      (string-exp (str) str)
      (str-concat-prim (e1 e2)
        (let*
          (
            [eval-exp-1 (eval-expression e1 env)] 
            [eval-exp-2 (eval-expression e2 env)]
            [str1 (string-type->str eval-exp-1)]
            [str2 (string-type->str eval-exp-2)]
          )
          (a-str (string->symbol (string-append str1 str2)))
        )
      )
      (str-len-prim (e)
        (let* ([eval-exp (eval-expression e env)] [str (string-type->str eval-exp)])
          (string-length str)
        )
      )

      ;hex and hex prims
      (hex-exp (hex) 
        (let ([vals (hex-type->vals hex)])
          (if (valid-hex? vals)
            hex
            (eopl:error 'hex-type "Not valid hexadecimal number")
          )
        )
      )
      (hex-to-dec-prim (exp)
        (let ([eval-exp-1 (eval-expression exp env)])
          (hex-to-dec-prim-aux eval-exp-1)
        )
      )
      (dec-to-hex-prim (exp)
        (let ([eval-exp(eval-expression exp env)])
          (if (exact? eval-exp)
            (dec-to-hex-prim-aux eval-exp)
            (eopl:error 'dec-to-hex-prim "Val not an exact number")
          )
        )
      )
      (sum-hex-prim (e1 e2)
        (let* 
          (
            [eval-exp-1 (eval-expression e1 env)] 
            [eval-exp-2 (eval-expression e2 env)]
            [dec1 (hex-to-dec-prim-aux eval-exp-1)]
            [dec2 (hex-to-dec-prim-aux eval-exp-2)]
          )
          (dec-to-hex-prim-aux (+ dec1 dec2))
        )
      )
      (sub-hex-prim (e1 e2)
        (let* 
          (
            [eval-exp-1 (eval-expression e1 env)] 
            [eval-exp-2 (eval-expression e2 env)]
            [dec1 (hex-to-dec-prim-aux eval-exp-1)]
            [dec2 (hex-to-dec-prim-aux eval-exp-2)]
          )
          (dec-to-hex-prim-aux (- dec1 dec2))
        )
      )
      (mulp-hex-prim (e1 e2)
        (let* 
          (
            [eval-exp-1 (eval-expression e1 env)] 
            [eval-exp-2 (eval-expression e2 env)]
            [dec1 (hex-to-dec-prim-aux eval-exp-1)]
            [dec2 (hex-to-dec-prim-aux eval-exp-2)]
          )
          (dec-to-hex-prim-aux (* dec1 dec2))
        )
      )
      (sub1-hex-prim (e)
        (let* 
          (
            [eval-exp (eval-expression e env)] 
            [dec (hex-to-dec-prim-aux eval-exp)]
          )
          (dec-to-hex-prim-aux (- dec 1))
        )
      )
      (add1-hex-prim (e)
        (let* 
          (
            [eval-exp (eval-expression e env)] 
            [dec (hex-to-dec-prim-aux eval-exp)]
          )
          (dec-to-hex-prim-aux (+ dec 1))
        )
      )

      ;circuit and circuit prims
      (circuit-exp (circ) circ)
      (eval-circuit-prim (exp)
        (let* ([eval-exp (eval-expression exp env)] [gate-list (circuit->gate-list eval-exp)])
          (eval-gate-list gate-list env)
        )
      )
      (connect-circuits-prim (e1 e2 id)
        (let* 
          (
            [eval-exp-1 (eval-expression e1 env)] 
            [eval-exp-2 (eval-expression e2 env)]
            [glist1 (circuit->gate-list eval-exp-1)]
            [glist2 (circuit->gate-list eval-exp-2)]
          )
          (connect-circuits-prim-aux glist1 glist2 id)
        )
      )
      (merge-circuits-prim (e1 e2 type id)
        (let* 
          (
            [eval-exp-1 (eval-expression e1 env)] 
            [eval-exp-2 (eval-expression e2 env)]
            [glist1 (circuit->gate-list eval-exp-1)]
            [glist2 (circuit->gate-list eval-exp-2)]
          )
          (merge-circuits-prim-aux glist1 glist2 type id)
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
                [first (list-type->first list-type)] 
                [rest (list-type->rest list-type)]
                [vals (eval-let/var/const-exp-rands (cons first rest) env)]
              )
              (for-exp-aux identifier vals e3 env)
            )
          )
          (list-exp (list-type) 
            (let* 
              (
                [first (list-type->first list-type)] 
                [rest (list-type->rest list-type)]
                [vals (eval-let/var/const-exp-rands (cons first rest) env)]
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

      (else exp)
    )
  )
)

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)
  )
)

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
        (indirect-target
          (let ((ref (apply-env-ref env id)))
            (cases target (primitive-deref ref)
              (direct-target (expval) ref)
              (indirect-target (ref1) ref1)
            )
          )
        )
      )
      (else (direct-target (eval-expression rand env)))
    )
  )
)

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-let/var/const-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let/var/const-rand x env)) rands)
  )
)

(define eval-let/var/const-rand
  (lambda (rand env)
    (cases expression rand
      (set-exp (id val) (eopl:error 'eval-let/var/const-rand "Can't use set on a declaration"))
      (set-list-prim (e1 e2 e3) (eopl:error 'eval-let/var/const-rand "Can't use set on a declaration"))
      (set-dict-prim (e1 id e2) (eopl:error 'eval-let/var/const-rand "Can't use set on a declaration"))
      (else (direct-target (eval-expression rand env)))
    )  
  )
)

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
    )
  )
)

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
        (eval-expression body (extend-env (list id) (list (car vals)) (list #t) env))
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
;Circuits

; función que permite evaluar un gate de acuerdo a su tipo.
(define eval-gate
  (lambda (g env)
    (cases gate g
      (a-gate (id t i-list) 
        (cases input-list i-list
          (an-input-list (first rest) 
            (cases type t
              (and-type () (and (eval-expression first env) (eval-expression (car rest) env)))
              (or-type () (or (eval-expression first env) (eval-expression (car rest) env)))
              (not-type () (not (eval-expression first env)))
              (xor-type () (xor (eval-expression first env) (eval-expression (car rest) env)))
            )
          )
          (empty-input-list () (eopl:error 'eval-gate "No evaluation for gate with no inputs"))
        )
      )
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
        (let loop([first first] [rest rest] [env env])
          (let*   
            (
              [res (eval-gate first env)] 
              [newEnv (extend-env (list (gate->identifier first)) (list (direct-target res)) (list #t) env)]
            )
            (if (null? rest) 
              res
              (loop (car rest) (cdr rest) newEnv)
            )
          )
        )
      )
    )
  )
)

; función auxiliar utilizada para implementar la primitiva merge-circuits-prim que conecta dos 
; circuitos de forma paralela sobre un gate de tipo or | and | xor | not y con identificador 
; especificado por el usuario
(define merge-circuits-prim-aux
  (lambda (glist1 glist2 type id)
    (let 
      (
        [c1-first-gate (gate-list->first glist1)] 
        [c1-rest-gates (gate-list->rest glist1)]
        [c2-first-gate (gate-list->first glist2)]
        [c2-rest-gates (gate-list->rest glist2)]
      )
      (if (or (null? c1-first-gate) (null? c2-first-gate))
        (eopl:error 'merge-circuits "No merge for a non empty circuit and an empty circuit")
        (circuit 
          (a-gate-list 
            c1-first-gate
            (append-aux 
              (append-aux c1-rest-gates (cons c2-first-gate c2-rest-gates)) 
              (list 
                (a-gate id type 
                  (an-input-list 
                    (var-exp (return-last-identifier glist1)) 
                    (list (var-exp (return-last-identifier glist2)))
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

; función auxiliar utilizada para implementar la primitiva connect-circuits-prim que conecta dos 
; circuitos de forma secuencial, es decir, se cambia alguna entrada del circuito 2 por la salida 
; del circuito 1. La entrada a cambiar es especificada por el usuario.
(define connect-circuits-prim-aux
  (lambda (glist1 glist2 old-id)
    (let 
      (
        [c1-first-gate (gate-list->first glist1)] 
        [c1-rest-gates (gate-list->rest glist1)]
        [c2-first-gate (gate-list->first glist2)]
        [c2-rest-gates (gate-list->rest glist2)]
        [new-id (return-last-identifier glist1)]
      )
      (if (or (null? c1-first-gate) (null? c2-first-gate))
        (eopl:error 'connect-circuits "No connection for a non empty circuit and an empty circuit")
        (circuit
          (a-gate-list
            c1-first-gate
            (append-aux 
              c1-rest-gates 
              (map 
                (lambda (g) (replace-gate-input-list g old-id new-id)) 
                (cons c2-first-gate c2-rest-gates)
              )
            )
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


;*******************************************************************************************
;Hexadecimals

; función auxiliar utilizada para la implementación de la primitiva dec-to-hex-prim. Crea un hexadeci-
; mal con la lista retornada por dec->hex.
(define dec-to-hex-prim-aux
  (lambda (val)
    (if (>= val 0)
      (pos-hex (dec->hex val))
      (neg-hex (dec->hex (- val)))
    )
  )
)

; función auxiliar que retorna una lista con la representación hexadecimal de un número decimal
; (en la práctica solo funciona con enteros).
(define dec->hex
  (lambda (val)
    (if (= val 0)
      (list 0)
      (let loop([num val] [acum empty])
        (if (= num 0)
          acum
          (loop (quotient num 16) (append-aux acum (list (remainder num 16))))
        )
      )
    )
  )
)

; función auxiliar que que convierte una representación basada en listas de un hexadecimal a un
; número decimal (en la práctica retorna solo enteros).
(define hex->dec
  (lambda (vals n)
    (if (null? vals)
      0
      (+ (hex->dec (cdr vals) (+ n 1)) (* (car vals) (expt 16 n)))
    )
  )
)

; función auxiliar utilizada para la implementación de la primitiva hex-to-dec-prim-aux. Retorna el
; valor correspondiente a la representación hexadecimal mediante listas ingresada.
(define hex-to-dec-prim-aux
  (lambda (h-type)
    (cases hex-type h-type
      (pos-hex (vals) (hex->dec vals 0))
      (neg-hex (vals) (- (hex->dec vals 0)))
    )
  )
)

; función auxiliar que verifica si los valores de un número hexadecimal son válidos.
(define valid-hex?
  (lambda (vals)
    (let 
      (
        [valid-range? (valid-hex-range? vals)]
        [integers? (all-integer? vals)]
      )
      (and valid-range? integers?)
    )
  )
)

; función auxiliar que verifica si los valores de una lista se encuentran en el rango (-16, 16).
(define valid-hex-range?
  (lambda (vals)
    (cond
      ([null? vals] #t)
      ([and (< (car vals) 16) (> (car vals) -16)] (valid-hex-range? (cdr vals)))
      (else #f)
    )
  )
)

; función auxiliar que verifica si todos los valores de una lista son ints.
(define all-integer?
  (lambda (vals)
    (cond
      ([null? vals] #t)
      ([integer? (car vals)] (all-integer? (cdr vals)))
      (else #f)
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
        (let ([val1 (eval-expression type1 env)] [val2 (eval-expression type2 env)])
          (and val1 val2)
        )
      )
      (or-prim ()
        (let ([val1 (eval-expression type1 env)] [val2 (eval-expression type2 env)])
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
        (let ([val (eval-expression type env)])
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

; extractor que a partir de un circuito retorna su gate-list
(define circuit->gate-list
  (lambda (circ)
    (cases circuit-type circ
      (circuit (gate-list) gate-list)
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

; extractor que a partir de un string-type retorna el símbolo asociado a este y posteriormente lo
; convierte a string.
(define string-type->str
  (lambda (str)
    (cases string-type str
      (a-str (symb) (symbol->string symb))
      (else (eopl:error 'string-type->str "Not a string type"))
    )
  )
)

; extractor que a partir de un hex-type retorna los valores de este.
(define hex-type->vals
  (lambda (h-type)
    (cases hex-type h-type
      (pos-hex (vals) vals)
      (neg-hex (vals) vals)
    )
  )
)

; extractor que a partir de una expresión tipo var-exp retorna el símbolo de esta.
(define var-exp->id
  (lambda (var)
    (cases expression var
      (var-exp (id) id)
      (else (eopl:error 'var-exp->id "Not a var-exp"))
    )
  )
)

; extractor que a partir de un list-type retorna el primer elemento de este.
(define list-type->first
  (lambda (l)
    (cases list-type l
      (non-empty-list (first rest) first)
      (empty-list () empty)
    )
  )
)

; extractor que a partir de un list-type retorna todos los elementos menos el primero de este.
(define list-type->rest
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
   (env environment?)
  )
)

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args const-tags)
    (cases procval proc
      (closure (ids body env) (eval-expression body (extend-env ids args const-tags env)))
    )
  )
)

;**************************************************************************************
;Definición tipos de datos referencia y blanco. Extractor ref->pos.

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?))
)

(define-datatype reference reference?
  (a-ref (position integer?) (vec vector?) (const-tags vector?))
)

; extractor que a partir de una referencia retorna la posición de esta.
(define ref->pos
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec const-tags) pos)
    )
  )
)

; extractor que a partir de una referencia retorna el vector de const-tags asociado al env en el que
; se encuentra la referencia.
(define ref->const-tags
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec const-tags) const-tags)
    )
  )
)

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)
    (const-tags vector?)
    (env environment?)
  )
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
  (lambda (syms vals const-tags env)
    (extended-env-record syms (list->vector vals) (list->vector const-tags) env)
  )
)

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)) (const-tags (make-vector len #f)))
        (let ((env (extended-env-record proc-names vec const-tags old-env)))
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
        (cons next (loop (+ 1 next)))
      )
    )
  )
)

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))
  )
)

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals const-tags env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
            (a-ref pos vals const-tags)
            (apply-env-ref env sym)
          )
        )
      )
    )
  )
)

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (cond
      ([number? x] #t)
      ([boolean? x] #t)
      ([procval? x] #t)
      ([list-type? x] #t)
      ([dict-type? x] #t)
      ([tuple-type? x] #t)
      ([string-type? x] #t)
      ([hex-type? x] #t)
      ([circuit-type? x] #t)
      (else #f)
    )
  )
)

(define ref-to-direct-target?
  (lambda (x)
    (and 
      (reference? x)
      (cases reference x
        (a-ref (pos vec const-tags)
          (cases target (vector-ref vec pos)
            (direct-target (v) #t)
            (indirect-target (v) #f)
          )
        )
      )
    )
  )
)

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
        (cases target (primitive-deref ref1)
          (direct-target (expval) expval)
          (indirect-target (p) (eopl:error 'deref "Illegal reference: ~s" ref1))
        )
      )
    )
  )
)

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec const-tags) (vector-ref vec pos))
    )
  )
)

(define setref!
  (lambda (ref expval env)
    (let*
      (
        [ref 
          (cases target (primitive-deref ref)
            (direct-target (expval1) ref)
            (indirect-target (ref1) ref1)
          )
        ]
        [pos (ref->pos ref)]
        [const-tags (ref->const-tags ref)]
        [constant? (vector-ref const-tags pos)]
      )
      (if constant?
        (eopl:error 'setref! "Can't modify the state of a constant var")
        (primitive-setref! ref (direct-target expval))
      )
    )
  )
)

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec const-tags) (vector-set! vec pos val))
    )
  )
)

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
      (else 
        (let ((list-index-r (list-index pred (cdr ls))))
          (if (number? list-index-r)
            (+ list-index-r 1)
            #f
          )
        )
      )
    )
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

; función auxiliar que crea una lista de longitud n donde cada elemento toma el valor del val
; ingresado.
(define make-list
  (lambda (len val)
    (if (= len 0)
      empty
      (cons val (make-list (- len 1) val))
    )
  )
)

; función auxiliar que a partir de un gate-list retorna el identificador de su último gate
(define return-last-identifier
  (lambda (glist)
    (let ([first (gate-list->first glist)] [rest (gate-list->rest glist)])
      (if (null? first)
        (eopl:error 'return-last-identifier "No last identifier for an empty gate-list")
        (let loop([first first] [rest rest])
          (if (null? rest)
            (gate->identifier first)
            (loop (car rest) (cdr rest))
          )
        )
      )
    )
  )
)

; función utilizada para implementar xor en nuestro lenguaje.
(define xor 
  (lambda (a b)
    (and (or a b) (not (and a b)))
  )
)

;******************************************************************************************
;Pruebas



;(scan&parse)
;(interpretador)
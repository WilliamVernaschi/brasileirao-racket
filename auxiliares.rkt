#lang racket

(require examples)
(provide merge-sort
         take
         drop
         group-by
         string-split)

;; list list FuncaoComparadora -> list
;; Dadas duas listas ordenadas, retorna uma outra lista ordenada
;; contendo os elementos das outras duas, as operaçoes de comparação
;; são feitas com a função comparadora `cmp`
(define (merge-lists lstA lstB cmp)
  (cond
    [(empty? lstA) lstB]
    [(empty? lstB) lstA]
    [else
     (if (cmp (first lstA) (first lstB))
         (cons (first lstA) (merge-lists (rest lstA) lstB cmp))
         (cons (first lstB) (merge-lists lstA (rest lstB) cmp)))]))


;; list -> list
;; Retorna os primeiros `size` elementos de `lst`, dado
;; que (<= size (length lst))
(define (take lst size)
  (cond
    [(= size 0) empty]
    [else
     (cons (first lst) (take (rest lst) (sub1 size)))]))

(examples
 (check-equal? (take empty 0) empty)
 (check-equal? (take (list "a" "b" "c") 0) empty)
 (check-equal? (take (list "a" "b" "c") 1) (list "a"))
 (check-equal? (take (list "a" "b" "c") 2) (list "a" "b"))
 (check-equal? (take (list "a" "b" "c") 3) (list "a" "b" "c")))

;; list -> list
;; Remove os primeiros `size` elementos de `lst`, dado
;; que (<= size (length lst))
(define (drop lst size)
  (cond
    [(= size 0) lst]
    [else
     (drop (rest lst) (sub1 size))]))

(examples
 (check-equal? (drop empty 0) empty)
 (check-equal? (drop (list "a" "b" "c") 0) (list "a" "b" "c"))
 (check-equal? (drop (list "a" "b" "c") 1) (list "b" "c"))
 (check-equal? (drop (list "a" "b" "c") 2) (list "c"))
 (check-equal? (drop (list "a" "b" "c") 3) empty))
    
;; list FuncaoComparadora -> list
;; Ordena uma lista utilizando `cmp` como
;; comparador entre os elementos em O(nlogn).
(define (merge-sort lst cmp)
  (cond
    [(empty? lst) empty] ; tamanho 0
    [(empty? (rest lst)) lst] ; tamanho 1
    [else
     (define metade (floor (/ (length lst) 2)))
     (merge-lists (merge-sort (take lst metade) cmp)
                  (merge-sort (drop lst metade) cmp)
                  cmp)]))

(examples
 (check-equal? (merge-sort empty <) empty)
 (check-equal? (merge-sort (list 1) <) (list 1))
 (check-equal? (merge-sort (list 2 1) <) (list 1 2))
 (check-equal? (merge-sort (list 3 2 1) <) (list 1 2 3))
 (check-equal? (merge-sort (list 4 3 2 1) <) (list 1 2 3 4))
 (check-equal? (merge-sort (list 2 14 1 4 6 2 1 9 2 5) <) (list 1 1 2 2 2 4 5 6 9 14)))

;; Dado uma lista não vazia '(a1 a2 a3 a4 ... )
;; retorna o tamanho da maior subsequência tal que f(a1) = f(a2) = f(a3) ... f(ak)
(define (tamanho-grupo-atual f lst)
  (cond
    [(empty? (rest lst)) 1]
    [else
     (if
      (equal? (f (first lst)) (f (second lst)))
      (add1 (tamanho-grupo-atual f (rest lst)))
      1)]))

(examples
 (check-equal? (tamanho-grupo-atual (lambda (x) x) (list 1 2 3)) 1)
 (check-equal? (tamanho-grupo-atual (lambda (x) x) (list 1 1 2 1 1 1)) 2)
 (check-equal? (tamanho-grupo-atual (lambda (x) (not (= (modulo x 7) 0))) (list 2 3 5 5 11 313)) 6))

;; Agrupa elementos contíguos que possuem o mesmo valor
;; quando a função f é aplicada sobre eles.
;; list -> listof list
(define (group-by f lst)
  (cond
    [(empty? lst) empty]
    [else
     (define tamanho-grupo (tamanho-grupo-atual f lst))
     (cons (take lst tamanho-grupo) (group-by f (drop lst tamanho-grupo)))]))

(examples
 (check-equal? (group-by (lambda (x) x) empty)
               empty)
 (check-equal? (group-by (lambda (x) x) (list 1 2 3 3 2 2 2 1))
               (list (list 1) (list 2) (list 3 3) (list 2 2 2) (list 1)))
 (check-equal? (group-by (lambda (x) (modulo x 2)) (list 1 3 8 4 2 7 7))
               (list (list 1 3) (list 8 4 2) (list 7 7))))


;; String -> listof String
;; dado uma string, separa ela em uma lista de strings, onde
;; o delimitador entre listas de string é o espaço.
(define (string-split str)
  (define lst (string->list str))
  (define não-começa-com-espaço? (lambda (l) (not (equal? (first l) #\space))))
  (define não-é-espaço? (lambda (c) (not (equal? c #\space))))
  
  (map list->string
       (filter não-começa-com-espaço?
               (group-by não-é-espaço? lst))))

(examples
 (check-equal? (string-split "oi tudo bem?") (list "oi" "tudo" "bem?"))
 (check-equal? (string-split " tem   espaços demais  !  ") (list "tem" "espaços" "demais" "!"))
 (check-equal? (string-split "stringona") (list "stringona"))
 (check-equal? (string-split "") empty))

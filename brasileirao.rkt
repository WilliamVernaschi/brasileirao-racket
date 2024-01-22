#lang racket

(require examples)

(struct resultado (nome-t1 placar-t1 nome-t2 placar-t2))
(struct desempenho (nome pontuacao saldo))

(define (merge-lists lstA lstB cmp)
  (cond
    [(empty? lstA) lstB]
    [(empty? lstB) lstA]
    [else
     (if (cmp (first lstA) (first lstB))
         (cons (first lstA) (merge-lists (rest lstA) lstB cmp))
         (cons (first lstB) (merge-lists lstA (rest lstB) cmp)))]))

(define (merge-sort lst cmp)
  (cond
    [(empty? lst) empty] ; tamanho 0
    [(empty? (rest lst)) lst] ; tamanho 1
    [else
     (let ([half (floor (/ (length lst) 2))])
       (merge-lists (merge-sort (take lst half) cmp)
                    (merge-sort (drop lst half) cmp)
                    cmp))]))

(examples
 (check-equal? (merge-sort empty <) empty)
 (check-equal? (merge-sort (list 1) <) (list 1))
 (check-equal? (merge-sort (list 2 1) <) (list 1 2))
 (check-equal? (merge-sort (list 3 2 1) <) (list 1 2 3))
 (check-equal? (merge-sort (list 4 3 2 1) <) (list 1 2 3 4))
 (check-equal? (merge-sort (list 2 14 1 4 6 2 1 9 2 5) <) (list 1 1 2 2 2 4 5 6 9 14)))

(displayln (merge-sort (map string->number (file->string "numeros"))))
  
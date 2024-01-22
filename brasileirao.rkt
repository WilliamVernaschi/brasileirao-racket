#lang racket

(require examples)

(struct resultado (nome-t1 placar-t1 nome-t2 placar-t2))
(struct desempenho (nome saldo pontuacao))

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

(define (string->resultado sresultado)
  (define split (string-split sresultado))
  (resultado (first split) (second split) (third split) (fourth split)))

(define (calcula-saldos resultado)
  (define saldo-t1 (- (resultado-placar-t1 resultado)
                      (resultado-placar-t2 resultado)))
  (list saldo-t1 (- saldo-t1)))


(define (calcula-pontuacoes saldos)
  (define (calcula-pontuacao saldo)
    (cond
      [(= saldo 0) 1]
      [(> saldo 0) 3]
      [else 0]))
  (map calcula-pontuacao saldos))
                             
(define (calcula-desempenhos resultados)
  (cond
    [(empty? resultados) empty]
    [else
     (define saldo-times (calcula-saldos (first resultados)))
     (define pontuacao-times (calcula-pontuacoes saldo-times))
     (define nome-times (list (resultado-nome-t1 (first resultados))
                              (resultado-nome-t2 (first resultados))))
     (cons
      (desempenho (first nome-times) (first saldo-times) (first pontuacao-times))
      (cons
       (desempenho (second nome-times) (second (saldo-times) (second pontuacao-times)))
       (calcula-desempenhos (rest resultados))))]))
     
(define (organiza-desempenhos desempenhos)
  (define desempenhos-ordenados-por-nome
    (merge-sort desempenhos (lambda (d1 d2)
                              (< (desempenho-nome d1)
                                 (desempenho-nome d2)))))
  (acumula-desempenhos desempenhos-ordenados-por-nome))

(define (comp-times d1 d2)
  (cond
    [()]

(define (classifica desempenho-times)
  (merge-sort desempeho-times comp-times))


;; listof String -> listof String
(define (classifica-times sresultados)
  ;; Transforma a lista de strings da entrada em uma lista de resultados
  (define resultados (map string->resultado sresultados))
  
  ;; Transforma a lista de resultados em uma lista de desempenhos
  (define desempenho (calcula-desempenhos resultados))

  ;; Transforma a lista de desempenhos de cada jogo em uma lista
  ;; do desempenho acumulado de cada um dos times.
  (define desempenhos-acumulado (organiza-desempenhos desempenhos))

  (define classificacao (classifica desempenhos-times))

  (map desempenho->string classificacao))

  

  
  
                                    

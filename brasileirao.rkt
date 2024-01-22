#lang racket

(require examples)

(struct resultado (nome-t1 placar-t1 nome-t2 placar-t2) #:transparent)
(struct desempenho (nome saldo pontuacao num-vitorias) #:transparent)

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


;; String -> resultado
;; Dado uma string da forma que representa o placar de um jogo, retorna
;; um resultado.
(define (string->resultado sresultado)
  (define split (string-split sresultado))
  (resultado (first split) (string->number (second split))
             (third split) (string->number(fourth split))))

(examples
 (check-equal? (string->resultado "Sao-Paulo 1 Atletico-MG 2")
               (resultado "Sao-Paulo" 1 "Atletico-MG" 2))
 (check-equal? (string->resultado "A    3     Time2    10 ")
               (resultado "A" 3 "Time2" 10)))

;; Resultado -> listof Integer
;; Dado o resultado de um jogo, retorna uma lista
;; contendo o saldo de gols dos dois times.
(define (calcula-saldos resultado)
  (define saldo-t1 (- (resultado-placar-t1 resultado)
                      (resultado-placar-t2 resultado)))
  (list saldo-t1 (- saldo-t1)))

(examples
 (check-equal? (calcula-saldos (resultado "Corinthians" 7 "Santos" 1)) (list 6 -6))
 (check-equal? (calcula-saldos (resultado "Sao-Paulo" 2 "Palmeiras" 2)) (list 0 0)))

;; listof Integer -> listof Integer
;; Dado uma lista contendo o saldo de gols de um jogo
;; entre dois times, retorna uma lista contendo a
;; pontuação de cada um dos dois times.
(define (calcula-pontuacoes saldos)
  (define (calcula-pontuacao saldo)
    (cond
      [(> saldo 0) 3]
      [(< saldo 0) 0]
      [else 1]))
  (map calcula-pontuacao saldos))

(examples
 (check-equal? (calcula-pontuacoes (list 3 -3)) (list 3 0))
 (check-equal? (calcula-pontuacoes (list -2 2)) (list 0 3))
 (check-equal? (calcula-pontuacoes (list 0 0)) (list 1 1)))

;; listof resultado -> listof desempenho
;; Dado uma lista contendo o resultado de n jogos, retorna
;; uma lista contendo 2n desempenhos dos times.
(define (calcula-desempenhos resultados)
  (cond
    [(empty? resultados) empty]
    [else
     (define saldo-times (calcula-saldos (first resultados)))
     (define pontuacao-times (calcula-pontuacoes saldo-times))
     (define nome-times (list (resultado-nome-t1 (first resultados))
                              (resultado-nome-t2 (first resultados))))
     (define vitoria (list
                      (if (> (first saldo-times) 0) 1 0)
                      (if (> (second saldo-times) 0) 1 0)))
     (cons
      (desempenho (first nome-times) (first saldo-times) (first pontuacao-times) (first vitoria))
      (cons
       (desempenho (second nome-times) (second saldo-times) (second pontuacao-times) (second vitoria))
       (calcula-desempenhos (rest resultados))))]))

(examples
 (check-equal?
  (calcula-desempenhos (list (resultado
                              "Corinthians" 3 "Palmeiras" 1)
                             (resultado
                              "Cuiabá" 2 "Bragantino" 3)
                             (resultado
                              "Bragantino" 1 "Corinthians" 1)))
  (list
   (desempenho "Corinthians" 2 3 1)
   (desempenho "Palmeiras" -2 0 0)
   (desempenho "Cuiabá" -1 0 0)
   (desempenho "Bragantino" 1 3 1)
   (desempenho "Bragantino" 0 1 0)
   (desempenho "Corinthians" 0 1 0))))
  
;; listof desempenho -> listof desempenho
;; Dado uma lista do desempenho dos times em cada um dos jogos,
;; onde um mesmo time aparece várias vezes, retorna uma lista
;; com o desempenho acumulado de cada um dos times em todos os jogos,
;; olhe os exemplos.
(define (acumula-desempenhos desempenhos)
  (define desempenhos-ordenados-por-nome
    (merge-sort desempenhos (lambda (d1 d2)
                              (string<? (desempenho-nome d1)
                                 (desempenho-nome d2)))))
  (define desempenho-times (group-by (lambda (x) (desempenho-nome x)) desempenhos-ordenados-por-nome))
  (define (acumula-rec des)
    (cond
      [(empty? des) empty]
      [else
       (cons (foldr (lambda (d1 d2) (desempenho
                                     (desempenho-nome d1)
                                     (+ (desempenho-saldo d1) (desempenho-saldo d2))
                                     (+ (desempenho-pontuacao d1) (desempenho-pontuacao d2))
                                     (+ (desempenho-num-vitorias d1) (desempenho-num-vitorias d2))))
                    (desempenho "" 0 0 0)
                    (first des))
            (acumula-rec (rest des)))]))
  (acumula-rec desempenho-times))

(examples
 (check-equal?
  (acumula-desempenhos
   (list
   (desempenho "Corinthians" 2 3 1)
   (desempenho "Palmeiras" -2 0 0)
   (desempenho "Cuiabá" -1 0 0)
   (desempenho "Bragantino" 1 3 1)
   (desempenho "Bragantino" 0 1 0)
   (desempenho "Corinthians" 0 1 0)))
  (list
   (desempenho "Bragantino" 1 4 1)
   (desempenho "Corinthians" 2 4 1)
   (desempenho "Cuiabá" -1 0 0)
   (desempenho "Palmeiras" -2 0 0))))


(define (comp-times d1 d2)
  (cond
    [(not (equal?
           (desempenho-pontuacao d1) (desempenho-pontuacao d2)))
     (> (desempenho-pontuacao d1) (desempenho-pontuacao d2))]

    [(not (equal?
           (desempenho-num-vitorias d1) (desempenho-num-vitorias d2)))
     (> (desempenho-num-vitorias d1) (desempenho-num-vitorias d2))]
    
    [(not (equal?
           (desempenho-saldo d1) (desempenho-saldo d2)))
     (> (desempenho-saldo d1) (desempenho-saldo d2))]

    [else (string<? (desempenho-nome d1) (desempenho-nome d2))]))

(define (classifica desempenho-times)
  (merge-sort desempenho-times comp-times))

(define (desempenho->string desempenho)
  (string-append (desempenho-nome desempenho) " "
                 (number->string (desempenho-pontuacao desempenho)) " "
                 (number->string (desempenho-num-vitorias desempenho)) " "
                 (number->string (desempenho-saldo desempenho))))


;; listof String -> listof String
(define (classifica-times sresultados)
  ;; Transforma a lista de strings da entrada em uma lista de resultados
  (define resultados (map string->resultado sresultados))
  
  ;; Transforma a lista de resultados em uma lista de desempenhos
  (define desempenhos (calcula-desempenhos resultados))

  ;; Transforma a lista de desempenhos de cada jogo em uma lista
  ;; do desempenho acumulado de cada um dos times.
  (define desempenhos-acumulado (acumula-desempenhos desempenhos))

  (define classificacao (classifica desempenhos-acumulado))

  (map desempenho->string classificacao))
  
(display-lines (classifica-times (port->lines)))
  
  

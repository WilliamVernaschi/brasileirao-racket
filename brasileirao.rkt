#lang racket

(require "./auxiliares.rkt" examples)

(struct resultado (nome-anfitrião gols-anfitrião nome-visitante gols-visitante) #:transparent)
;; nome-anfitrião é uma String
;; gols-anfitrião é um inteiro não negativo
;; nome-visitante é uma String
;; gols-vitante é um inteiro não negativo

(struct desempenho (nome pontuacao num-vitorias saldo) #:transparent)
;; nome é uma String
;; pontuacao é um inteiro não negativo
;; num-vitorias é um inteiro não negativo
;; saldo é um inteiro não negativo


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
  (define saldo-t1 (- (resultado-gols-anfitrião resultado)
                      (resultado-gols-visitante resultado)))
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
     (define nome-times (list (resultado-nome-anfitrião (first resultados))
                              (resultado-nome-visitante (first resultados))))
     (define vitoria (list
                      (if (> (first saldo-times) 0) 1 0)
                      (if (> (second saldo-times) 0) 1 0)))
     (cons
      (desempenho (first nome-times) (first pontuacao-times) (first vitoria) (first saldo-times))
      (cons
       (desempenho (second nome-times) (second pontuacao-times) (second vitoria) (second saldo-times))
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
   (desempenho "Corinthians" 3 1 2)
   (desempenho "Palmeiras" 0 0 -2)
   (desempenho "Cuiabá" 0 0 -1)
   (desempenho "Bragantino" 3 1 1)
   (desempenho "Bragantino" 1 0 0)
   (desempenho "Corinthians" 1 0 0))))
 

;; listof listof desempenho -> listof desempenho
;; dado uma lista de desempenhos agrupados, retorna
;; uma lista de desempenhos acumulados.
(define (acumula-rec desempenhos)
  (map (lambda (desempenhos-agrupados)
         (foldr (lambda (d1 d2) (desempenho
                                     (desempenho-nome d1)
                                     (+ (desempenho-pontuacao d1) (desempenho-pontuacao d2))
                                     (+ (desempenho-num-vitorias d1) (desempenho-num-vitorias d2))
                                     (+ (desempenho-saldo d1) (desempenho-saldo d2))))
                    (desempenho "" 0 0 0)
                    desempenhos-agrupados))
       desempenhos))

(examples
 (check-equal? (acumula-rec (list (list (desempenho "a" 1 2 3) (desempenho "a" 3 2 1))))
                            (list (desempenho "a" 4 4 4)))
 (check-equal? (acumula-rec (list (list (desempenho "a" 1 2 3) (desempenho "a" 3 2 1))
                                  (list (desempenho "b" 1 4 3) (desempenho "b" 13 14 21)))) 
                            (list (desempenho "a" 4 4 4)
                                  (desempenho "b" 14 18 24))))


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
  (define desempenho-agrupado-por-time (group-by (lambda (x) (desempenho-nome x)) desempenhos-ordenados-por-nome))
  
  (acumula-rec desempenho-agrupado-por-time))

(examples
 (check-equal?
  (acumula-desempenhos
   (list
    (desempenho "Corinthians" 3 1 2)
    (desempenho "Palmeiras" 0 0 -2)
    (desempenho "Cuiabá" 0 0 -1)
    (desempenho "Bragantino" 3 1 1)
    (desempenho "Bragantino" 1 0 0)
    (desempenho "Corinthians" 1 0 0)))
  (list
   (desempenho "Bragantino" 4 1 1)
   (desempenho "Corinthians" 4 1 2)
   (desempenho "Cuiabá" 0 0 -1)
   (desempenho "Palmeiras" 0 0 -2))))


;; dados o desempenho de dois times, retorna #t se o primeiro
;; time deve estar antes do segundo, e #f caso contrário.
;; desempenho desempenho -> Bool
(define (compara-desempenho d1 d2)
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

(examples
 (check-equal? (compara-desempenho (desempenho "a" 30 20 10) (desempenho "b" 29 20 10)) #t)
 (check-equal? (compara-desempenho (desempenho "a" 30 25 10) (desempenho "b" 30 20 10)) #t)
 (check-equal? (compara-desempenho (desempenho "a" 30 20 12) (desempenho "b" 30 20 10)) #t)
 (check-equal? (compara-desempenho (desempenho "a" 30 20 10) (desempenho "b" 30 20 10)) #t)
 (check-equal? (compara-desempenho (desempenho "a" 6 20 10) (desempenho "b" 10 20 10)) #f))
               
(define (classifica desempenho-times)
  (merge-sort desempenho-times compara-desempenho))

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
  (define desempenhos-acumulados (acumula-desempenhos desempenhos))

  (define classificacao (classifica desempenhos-acumulados))

  (map desempenho->string classificacao))
  
(display-lines (classifica-times (port->lines)))
  
  

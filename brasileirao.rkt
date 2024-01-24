#lang racket

(require examples)

;; list(X) list(X) (X X -> Bool) -> list(X)
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

(examples
  (check-equal? (merge-lists (list 1 4 6) (list 1 2 3) <) (list 1 1 2 3 4 6))
  (check-equal? (merge-lists (list "a" "b" "z") (list "d") string<?) (list "a" "b" "d" "z")))

;; list(X) InteiroNãoNegativo -> list(X)
;; Retorna uma lista contendo os primeiros `n` elementos de `lst`, dado
;; que (<= n (length lst))
(define (take lst n)
  (cond
    [(= n 0) empty]
    [else
     (cons (first lst) (take (rest lst) (sub1 n)))]))

(examples
 (check-equal? (take empty 0) empty)
 (check-equal? (take (list "a" "b" "c") 0) empty)
 (check-equal? (take (list "a" "b" "c") 1) (list "a"))
 (check-equal? (take (list "a" "b" "c") 2) (list "a" "b"))
 (check-equal? (take (list "a" "b" "c") 3) (list "a" "b" "c")))

;; list(X) InteiroNãoNegativo -> list(X)
;; Remove os primeiros `n` elementos de `lst`, dado
;; que (<= n (length lst))
(define (drop lst n)
  (cond
    [(= n 0) lst]
    [else
     (drop (rest lst) (sub1 n))]))

(examples
 (check-equal? (drop empty 0) empty)
 (check-equal? (drop (list "a" "b" "c") 0) (list "a" "b" "c"))
 (check-equal? (drop (list "a" "b" "c") 1) (list "b" "c"))
 (check-equal? (drop (list "a" "b" "c") 2) (list "c"))
 (check-equal? (drop (list "a" "b" "c") 3) empty))
    
;; list(X) (X X -> bool) -> list(X)
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
;; retorna o maior valor de k tal que (f a1) = (f a2) = (f a3) = ... = (f ak)
;; (X -> Y) list(X) -> InteiroNãoNegativo
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

;; Agrupa elementos adjacentes que possuem o mesmo valor
;; quando a função f é aplicada sobre eles.
;; (X -> Y) list(X) -> list(list(X))
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


(struct resultado (nome-anfitrião gols-anfitrião nome-visitante gols-visitante) #:transparent)
;; Representa o resultado de um jogo.
;; nome-anfitrião : String - Nome do time anfitrião.
;; gols-anfitrião : InteiroNãoNegativo - Quantidade de gols marcados pelo time anfitrião.
;; nome-visitante : String - Nome do time visitante.
;; gols-visitante : InteiroNãoNegativo - Quantidade de gols marcados pelo time visitante.


(struct desempenho-jogo (nome saldo) #:transparent)
;; DesempenhoJogo representa o desempenho de um time em uma única partida.
;; nome : String - Nome do time.
;; saldo : Inteiro - (- Número-de-gols-marcados Número-de-gols-sofridos) no jogo atual


;; desempenho-jogo -> PontuaçãoJogo
;; PontuaçãoJogo pode ser um dos valores: 0, 1 ou 3.
;; Dado o desempenho de um time em um jogo, retorna quantos pontos ele
;; ganhou.
(define (desempenho-jogo->pontuação desempenho)
  (cond
    [(> (desempenho-jogo-saldo desempenho) 0) 3]
    [(< (desempenho-jogo-saldo desempenho) 0) 0]
    [else 1]))

;; desempenho-jogo -> Bool
;; Dado o desempenho de um time em um jogo, retorna #t se
;; ele ganhou, e #f caso contrário.
(define (desempenho-jogo->ganhou? desempenho)
  (> (desempenho-jogo-saldo desempenho) 0))


(struct desempenho-final (nome pontuacao num-vitorias saldo) #:transparent)
;; desempenho-final representa o desempenho do time no final do campeonato.
;; nome : String - Nome do time.
;; pontuacao : InteiroNãoNegativo - Quantidade de pontos ganhos pelo time em todo o campeonato
;; num-vitorias : InteiroNãoNegativo - número de vitórias do time em todo o campeonato
;; saldo : Inteiro - (- Número-de-gols-marcados Número-de-gols-sofridos) em todo o campeonato


;; String -> resultado
;; Dado uma string que representa o placar de um jogo, retorna
;; um resultado.
(define (string->resultado sresultado)
  (define split (string-split sresultado))
  (resultado (first split)
             (string->number (second split))
             (third split)
             (string->number (fourth split))))

(examples
 (check-equal? (string->resultado "Sao-Paulo 1 Atletico-MG 2")
               (resultado "Sao-Paulo" 1 "Atletico-MG" 2))
 (check-equal? (string->resultado "A    3     Time2    10 ")
               (resultado "A" 3 "Time2" 10)))


;; resultado -> list(Inteiro)
;; Dado o resultado de um jogo, retorna uma lista
;; contendo o saldo de gols dos dois times.
(define (calcula-saldos resultado)
  (define saldo-t1 (- (resultado-gols-anfitrião resultado)
                      (resultado-gols-visitante resultado)))
  (list saldo-t1 (- saldo-t1)))

(examples
 (check-equal? (calcula-saldos (resultado "Corinthians" 7 "Santos" 1)) (list 6 -6))
 (check-equal? (calcula-saldos (resultado "Sao-Paulo" 2 "Palmeiras" 2)) (list 0 0)))

;; list(resultado) -> list(desempenho-jogo)
;; Dado uma lista contendo o resultado de n jogos, retorna
;; uma lista contendo 2n desempenhos dos times.
(define (calcula-desempenhos resultados)
  (cond
    [(empty? resultados) empty]
    [else
     (define saldo-times (calcula-saldos (first resultados)))
     (define nome-times (list (resultado-nome-anfitrião (first resultados))
                              (resultado-nome-visitante (first resultados))))
     (cons
      (desempenho-jogo (first nome-times) (first saldo-times))
      (cons
       (desempenho-jogo (second nome-times) (second saldo-times))
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
   (desempenho-jogo "Corinthians" 2)
   (desempenho-jogo "Palmeiras" -2)
   (desempenho-jogo "Cuiabá" -1)
   (desempenho-jogo "Bragantino" 1)
   (desempenho-jogo "Bragantino"  0)
   (desempenho-jogo "Corinthians" 0))))
 



;; list(desempenho-jogo) -> list(desempenho-final)
;; Dado uma lista do desempenho dos times em cada um dos jogos, retorna
;; uma lista de desempenhos finais acumulados.
(define (acumula-desempenhos desempenhos)
  (define desempenhos-ordenados-por-nome
    (merge-sort desempenhos (lambda (d1 d2)
                              (string<? (desempenho-jogo-nome d1)
                                        (desempenho-jogo-nome d2)))))
  (define desempenho-agrupado-por-time (group-by (lambda (x) (desempenho-jogo-nome x)) desempenhos-ordenados-por-nome))
  
  (map (lambda (desempenhos-agrupados)
         (foldr (lambda (djogo dfinal)
                  (desempenho-final
                   (desempenho-jogo-nome djogo)
                   (+ (desempenho-jogo->pontuação djogo) (desempenho-final-pontuacao dfinal))
                   (+ (if (desempenho-jogo->ganhou? djogo) 1 0) (desempenho-final-num-vitorias dfinal))
                   (+ (desempenho-jogo-saldo djogo) (desempenho-final-saldo dfinal))))
                (desempenho-final "" 0 0 0)
                desempenhos-agrupados))
       desempenho-agrupado-por-time))

(examples
 (check-equal?
  (acumula-desempenhos
   (list
    (desempenho-jogo "Corinthians" 2)
    (desempenho-jogo "Palmeiras" -2)
    (desempenho-jogo "Cuiabá" -1)
    (desempenho-jogo "Bragantino" 1)
    (desempenho-jogo "Bragantino" 0)
    (desempenho-jogo "Corinthians" 0)))
  (list
   (desempenho-final "Bragantino" 4 1 1)
   (desempenho-final "Corinthians" 4 1 2)
   (desempenho-final "Cuiabá" 0 0 -1)
   (desempenho-final "Palmeiras" 0 0 -2))))



;; desempenho-final desempenho-final -> Bool
;; Dados os desempenhos de dois times, retorna #t se o primeiro
;; time deve se classificar melhor do que o segundo, e #f caso contrário.
;; Um time tem classificação melhor que outro se possui maior pontuação,
;; sendo desempatados pelo número de vitórias, saldo de gols e ordenação
;; lexicográfica dos nomes, respectivamente.
(define (compara-desempenho d1 d2)
  (cond
    [(not (equal?
           (desempenho-final-pontuacao d1) (desempenho-final-pontuacao d2)))
     (> (desempenho-final-pontuacao d1) (desempenho-final-pontuacao d2))]

    [(not (equal?
           (desempenho-final-num-vitorias d1) (desempenho-final-num-vitorias d2)))
     (> (desempenho-final-num-vitorias d1) (desempenho-final-num-vitorias d2))]
    
    [(not (equal?
           (desempenho-final-saldo d1) (desempenho-final-saldo d2)))
     (> (desempenho-final-saldo d1) (desempenho-final-saldo d2))]

    [else (string<? (desempenho-final-nome d1) (desempenho-final-nome d2))]))

(examples
 (check-equal? (compara-desempenho (desempenho-final "a" 30 20 10) (desempenho-final "b" 29 20 10)) #t)
 (check-equal? (compara-desempenho (desempenho-final "a" 30 25 10) (desempenho-final "b" 30 20 10)) #t)
 (check-equal? (compara-desempenho (desempenho-final "a" 30 20 12) (desempenho-final "b" 30 20 10)) #t)
 (check-equal? (compara-desempenho (desempenho-final "a" 30 20 10) (desempenho-final "b" 30 20 10)) #t)
 (check-equal? (compara-desempenho (desempenho-final "a" 6 20 10) (desempenho-final "b" 10 20 10)) #f))

;; list(desempenho-final) -> list(desempenho-final)
;; ordena os times conforme as regras
(define (classifica desempenho-times)
  (merge-sort desempenho-times compara-desempenho))

;; String InteiroNãoNegativo Alinhamento -> String
;; Alinhamento é "esquerda" ou "direita"
;; Reescreve `str` utilizando `tamanho` posições. As posições que sobraram
;; são compensadas por espaços a esquerda, caso `direção`
;; for "esquerda" ou espaços a direita caso `direção` seja "direita"
;; requer que (>= tamanho (string-length str))
(define (cria-espaçamento str tamanho direção)
  (define espaços (make-string (- tamanho (string-length str)) #\space))
  (cond
    [(equal? direção "esquerda") (string-append espaços str)]
    [(equal? direção "direita") (string-append str espaços)]))

(examples
 (check-equal? (cria-espaçamento "texto" 6 "esquerda") " texto")
 (check-equal? (cria-espaçamento "texto" 6 "direita") "texto ")
 (check-equal? (cria-espaçamento "" 8 "direita") "        "))

;; desempenho-final InteiroNãoNegativo -> String
;; Dado o desempenho de um time e o espaçamento que será utilizado
;; para imprimir o nome, retorna uma string formatada que representa
;; o desempenho do time no final do campeonato.
(define (desempenho-final->string desempenho espaçamento-nome)
  (string-append (cria-espaçamento (desempenho-final-nome desempenho) (+ espaçamento-nome 2) "direita") 
                 (cria-espaçamento (number->string (desempenho-final-pontuacao desempenho)) 2 "esquerda")
                 (cria-espaçamento (number->string (desempenho-final-num-vitorias desempenho)) 4 "esquerda")
                 (cria-espaçamento (number->string (desempenho-final-saldo desempenho)) 5 "esquerda") " "))

;; list(desempenho-final) -> InteiroPositivo
;; Calcula o tamanho do do nome do time com maior nome.
(define (calcula-maior-nome desempenhos)
  (define tamanhos (map (lambda (d) (string-length (desempenho-final-nome d))) desempenhos))
  (foldr (lambda (t1 t2) (max t1 t2))
         0
         tamanhos))

(examples
 (check-equal?
  (calcula-maior-nome (list (desempenho-final "12" 0 0 0)
                            (desempenho-final "123" 0 0 0)
                            (desempenho-final "1" 0 0 0)))
  3))
  

;; list(String) -> list(String)
;; Dado uma lista de strings que representam os resultados dos jogos do campeonato, retorna
;; uma lista contendo os desempenhos dos times ao final do campeonato ordenados segundo
;; as regras descritas em `compara-desempenho`.
(define (classifica-times sresultados)
  ;; Transforma a lista de strings da entrada em uma lista de resultados
  (define resultados (map string->resultado sresultados))

  ;; list(resultado) -> list(desempenho-jogo)
  ;; Transforma a lista de resultados em uma lista de desempenhos de jogo
  (define desempenhos (calcula-desempenhos resultados))

  ;; list(desempenho-jogo) -> list(desempenho-final)
  ;; Transforma a lista de desempenhos de cada jogo em uma lista
  ;; do desempenho final de cada um dos times.
  (define desempenhos-acumulados (acumula-desempenhos desempenhos))

  ;; list(desempenho-final) -> list(desempenho-final)
  ;; Ordena os times.
  (define classificacao (classifica desempenhos-acumulados))
  
  (define maior-nome (calcula-maior-nome classificacao))

  (map
   (lambda (c) (desempenho-final->string c maior-nome))
   classificacao))
  
(display-lines (classifica-times (port->lines)))
  
  

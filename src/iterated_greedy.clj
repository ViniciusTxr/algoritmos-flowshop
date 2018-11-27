;Algoritmo baseado no artigo:
;An iterated greedy algorithm with optimization of partial solutions for the makespan permutation flowshop problem [2017] Lacoste, Pagnozzi and Stutzle

(ns iterated-greedy
  (:require [NEH :as neh]
            [parser :as p]
            [clojure.set :as st]))


(def k_IG 2)
(def Tp 0.7)

(defn destruicao 
  "recebe um vetor de n posiçoes. Retorna dois vetores resultantes da destruição, um com (n-K_IG) e outro com k_IG trabalhos"
  ([ent] 
   (let [
         i 0
         vet ent
         posicao (take k_IG (distinct (repeatedly #(rand-int p/n_job))))
         elemento (vector (nth vet (nth posicao i)))
         vet_aux (p/remove-elem-vetor vet (nth posicao i))]
     (destruicao (inc i) vet_aux posicao elemento)))
  ([i ent pos elem]
   (let [
         vet ent
         elem_pos (if (<= (count ent) (nth pos i)) 
                    (- (nth pos i) i) (nth pos i))
         elemento (conj elem (nth vet elem_pos))
         vet_aux (p/remove-elem-vetor vet elem_pos)]
     (if (< i (- k_IG 1))
       (recur (inc i) vet_aux pos elemento)
       (conj (vector vet_aux) elemento)
       ))))


(defn permuta-parcial-trabalhos
  "rearranja uma solução com (n-k_IG) trabalhos escolhendo um trabalho para permutar entre todos, este processo se repete (n-k_IG) vezes e no final é retornada a melhor permutaçao"
  [i vet vet_aux]
  (let [
        pi (second vet)
        melhor (first vet)
        pivo (nth pi i)
        del_pivo (p/remove-elem-vetor pi i)
        vet_dif (into [] (seq (st/difference (set del_pivo) (set vet_aux))))
        elem_dif (rand-nth vet_dif)
        indice_elem (.indexOf del_pivo elem_dif)
        vet_aux (conj vet_aux elem_dif)
        del_k (p/remove-elem-vetor del_pivo indice_elem)
        insere_pivo (p/add-vec del_k pivo i)
        tam (- p/n_job k_IG)
        insere_k (for [j (range tam)]
                   (p/add-vec insere_pivo elem_dif (- (- tam 1) j)))
        makespan_permutacoes (map #(neh/calcula-makespan (neh/calc-temp-proc %)) insere_k)
        permutacoes (map vector makespan_permutacoes insere_k)
        menor (apply min-key first permutacoes)
        menor_make (first menor)
        melhor (if (< menor_make melhor) menor vet)
        ]
    (if (< i (- (- p/n_job k_IG) 2))
      (recur (inc i) melhor vet_aux)
      melhor)))


(defn buscaSolucaoParcial
  "realiza a busca parcial em uma solução enquanto nao atingir um otimo local"
  [vet flag]
  (if (= flag true) 
    (let [
          pi (second vet)
          melhor (first vet)
          flag false
          novo (permuta-parcial-trabalhos 0 vet [])
          flag (if (< (first novo) melhor) true false)
          ]
      (recur novo flag))
    vet))


(defn construcao
  "reinsere os elementos que foram retirados na fase de destruicao um a um, inserindo-os em todas as posiçoes de forma a buscar a melhor permutação"
  [vet trabs i]
  (let [
        elem (first trabs)
        insere_k (for [j (range (- p/n_job (- k_IG i)))]
                   (p/add-vec vet elem j))
        makespan_permutacoes (map #(neh/calcula-makespan (neh/calc-temp-proc %)) insere_k)
        permutacoes (map vector makespan_permutacoes insere_k)
        menor (apply min-key first permutacoes)
        remove (into [] (rest trabs))
        ]
    (if (< i (- k_IG 1))
      (recur (second menor) remove (inc i))
      menor)))


(defn permuta-completa-trabalhos
  "rearranja uma solução com (n) trabalhos escolhendo um trabalho para permutar entre todos, este processo se repete ate ser encontrado o primeiro melhoramento ou por (n) vezes e no final é retornada a melhor permutaçao"
  [i vet]
  (let [
        pi (second vet)
        melhor (first vet)
        pivo (nth pi i)
        del_pivo (p/remove-elem-vetor pi i)
        pos_k (rand-int (- p/n_job 1))
        k (nth del_pivo pos_k)
        del_k (p/remove-elem-vetor del_pivo pos_k)
        insere_pivo (p/add-vec del_k pivo i)
        insere_k (for [j (range p/n_job)]
                   (p/add-vec insere_pivo k j))
        makespan_permutacoes (map #(neh/calcula-makespan (neh/calc-temp-proc %)) insere_k)
        permutacoes (map vector makespan_permutacoes insere_k)
        menor (apply min-key first permutacoes)
        menor_make (first menor)
        menor_pi (second menor)
        i (if (< menor_make melhor) (- p/n_job 2) i)
        melhor (if (< menor_make melhor) menor vet)
        ]
    (if (< i (- p/n_job 2))
      (recur (inc i) melhor)
      melhor)))


(defn buscaSolucaoCompleta
  "realiza a busca completa em uma solução enquanto nao atingir um otimo local"
  [vet flag]
  (if (= flag true) 
    (let [
          pi (second vet)
          melhor (first vet)
          flag false
          novo (permuta-completa-trabalhos 0 vet)
          flag (if (< (first novo) melhor) true false)
          ]
      (recur novo flag))
    vet))


(defn temperatura 
  "calcula a temperatura, ou seja, um fator para o problema estudado"
  []
  (let [
        temp (reduce + (map #(reduce + %) p/ger-list))
        denominador (* (* p/n_job p/n_maq) 10)
        ]
    (/ (* Tp temp) denominador)))


(def temper (temperatura))


(defn prob-metropolis 
  "calcula a probabilidade minima que uma solução deve ter para ser aprovada"
  [pi pi_2]
  (Math/exp (* (/ (double (- pi pi_2)) temper) -1)))


(defn criterio-aceitacao
  "recebe a melhor solução local, a solução corrente e a solução global. Retorna a nova melhor soluçao local e a global com base no criterio de aceitação"
  [pi pi_2 pi_b]
  (if (<= (first pi_2) (first pi))
    (if (< (first pi_2) (first pi_b))
      [pi_2 pi_2]
      [pi_2 pi_b])
    (if (<= 
         (rand 1)(prob-metropolis (first pi_2)(first pi)))
      [pi_2 pi_b]
      [pi pi_b])))


;(def tempo (int (* p/n_job (* (double (/ p/n_maq 2)) 60))))
;(def tempo (int (* p/n_job (* (double (/ p/n_maq 2)) 120))))
(def tempo (int (* p/n_job (* (double (/ p/n_maq 2)) 240))))


(defn iterated-greedy
  "é a função principal"
  [i pi pi_b time]
  (let [
        destroi (destruicao (nth pi 1))
        destroi_vet (nth destroi 0)
        make_dest_vet (neh/calcula-makespan (neh/calc-temp-proc destroi_vet))
        destroi_resto (nth destroi 1)
        solucao_parcial (buscaSolucaoParcial [make_dest_vet destroi_vet] true)
        constroi (construcao (second solucao_parcial) destroi_resto 0)
        solucao_completa (buscaSolucaoCompleta constroi true)
        aceita (criterio-aceitacao pi solucao_completa pi_b)
        pi (first aceita)
        pi_b (second aceita)
        ]
    (if (<= (System/currentTimeMillis) time)
      (recur (inc i) pi pi_b time)
      [pi_b i])))
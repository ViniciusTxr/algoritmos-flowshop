(ns parser
  (:require [clojure.string :as s]))

(defn le_arquivo 
  "le o arquivo das instancias e os salva em uma string, salva a string em uma lista, onde os elementos estao separados de acordo com as linhas do aqruivo "
  []
  (let [
    string1 (slurp "src/Benchmarks/tai20_5.txt") ;;qual o conjunto de instancias estamos trabalhando
    ]
  (s/split-lines string1)))

(defn strInt
  "converte string para inteiro"
  [str]
  (Integer/parseInt str))

(def instancia 0);;qual instancia estamos trabalhando

(def n_maq (strInt ((let [inicio ((le_arquivo) 1) 
                               aux (s/split inicio #"\s+")
                               ] aux) 2)))

(def linha_inicial (+ 1 (* instancia (+ n_maq 3))))

(defn separa-arquivos 
  ""
  []
  (let [inicio ((le_arquivo) linha_inicial) 
        aux (s/split inicio #"\s+")]
    (hash-map :n_maq (strInt (aux 2))
              :n_trab (strInt (aux 1))
              :seed (strInt (aux 3))
              :ub (strInt (aux 4))
              :lb (strInt (aux 5)))))


(def n_job ((find (separa-arquivos) :n_trab) 1))

(def upper_bound ((find (separa-arquivos) :ub) 1)) 

(def lower_bound ((find (separa-arquivos) :lb) 1)) 

(def prim_linha (+ (* n_maq  instancia) (* 3 (+ instancia 1)))) ;;primeira linha do tempo de processamento da instancia a ser lida

(def ultim_linha (+ prim_linha n_maq));;ultima linha do tempo de processamento da instancia a ser lida


(defn pega-matriz
  "pega a matriz do arquivo, removendo os espaços nos caracteres"
  []
  (map #(subvec % 1 (+ n_job 1))
       (map #(s/split % #"\s+") 
            (subvec (le_arquivo) prim_linha ultim_linha))))


(defn transforma
  "transforma os elementos char pra inteiro"
 [matriz]
 (into [] (map #(Integer/parseInt %) matriz)))


(defn gera-lista
  "gera a lista final dos tempos de trabalho "
  [] 
  (into [] (map transforma (pega-matriz))))


(def ger-list (gera-lista))

(defn transposicao-matriz
  "pega a matriz e faz a transposição"
  []
  (into [] (apply map vector ger-list)))


(defn key-jobs
  "constroi a lista de keywords de acordo com o numero de trabalhos"
  []
  (into [] (map keyword (map #(str "j"%) (range 1 (+ n_job 1))))))

 
(defn mapeia-trabalhos
  "concatena as keywords com a matriz de tempo de processamento do trabalho"
  []
  (zipmap (key-jobs) (transposicao-matriz)))


(defn remove-elem-vetor
  "remove elem in coll"
  [vetor pos]
  (vec (concat (subvec vetor 0 pos) (subvec vetor (inc pos)))))


(defn add-vec
  "add um elemento (elem) a um vetor (vet) na posiçao (pos)"
  [vet elem pos]
  (let [
        tam-vet (count vet)
        novo_vet (if-let [_ (= pos 0)]
                    (into [] (cons elem vet))
                    (if-let [_ (= tam-vet pos)]
                        (conj vet elem)
                        (into [] (concat (conj (subvec vet 0 pos) elem) (subvec vet pos tam-vet)))))
      ]
      novo_vet))
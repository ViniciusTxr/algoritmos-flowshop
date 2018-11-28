(ns core
  (:require [NEH :as neh]
            [monkey-search :as ms]
            [iterated-greedy :as ig]
            [parser :as p])
  (:gen-class))

(def form-data (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm:ss"))

(def chamadas 10)


(defn monkey
  "Função que calcula o resultado para o algoritmo Busca do Macaco Híbrida (Monkey Search) e salva a resposta em arquivo"
  [i melhor pior media-make media-tempo]
    (let [
          data-inicio (java.util.Date.)
          tempo-inicio (System/currentTimeMillis)
          inicial (ms/solucao_inicial)
          resultado (time (ms/monkey-search 0 inicial (ms/encontra-melhor inicial)))
          tempo-fim (System/currentTimeMillis)
          data-fim (java.util.Date.)
          tempo-total (float(/ (- tempo-fim tempo-inicio) 1000))
          make-result (first resultado)
          melhor (if (< make-result melhor) make-result melhor)
          pior (if (> make-result pior) make-result pior)
          media-make (+ media-make make-result)
          media-tempo (+ tempo-total media-tempo)
          ]
      (with-open [w (clojure.java.io/writer "src/resultados_BMH.txt" :append true)]
        (.write w (str "Makespan: "make-result "\nPermutação: "(into [] (map #(Integer/parseInt %) (map #(subs % 1) (map name (map first (first (ms/smallest-position-value (vector (second resultado)))))))))"\nData Inicio: " (.format form-data data-inicio) " / Data Fim: " (.format form-data data-fim) "\nTempo: " tempo-total " segundos\n\n"))) 
      (if(= i chamadas)
        (with-open [w (clojure.java.io/writer "src/resultados_BMH.txt" :append true)]
           (.write w (str "Melhor: "melhor "\nPior: "pior "\nMedia makespan: "(/ media-make chamadas) "\nMedia tempo: "(/ (double (long (* (/ media-tempo chamadas) 100))) 100) " segundos\n\n\n"))))
      (if (< i chamadas)
          (recur (inc i) melhor pior media-make media-tempo))))



(defn iterated 
  "Função que calcula o resultado para o algoritmo Iteração Gulosa (Iterated Greedy) e salva a resposta em arquivo"
  [i melhor pior media-make media-tempo media-iter]
    (let [
          data-inicio (java.util.Date.)
          tempo-inicio (System/currentTimeMillis)
          resultado (time (ig/iterated-greedy 0 neh/NEH neh/NEH (+ ig/tempo (System/currentTimeMillis))))
          tempo-fim (System/currentTimeMillis)
          data-fim (java.util.Date.)
          tempo-total (float(/ (- tempo-fim tempo-inicio) 1000))
          make-result (first (first resultado))                        
          melhor (if (< make-result melhor) make-result melhor)
          pior (if (> make-result pior) make-result pior)
          media-make (+ media-make make-result)
          media-tempo (+ tempo-total media-tempo)
          iteracao (+ 1 (second resultado))
          media-iter (+ iteracao media-iter)
          ]
      (with-open [w (clojure.java.io/writer "src/resultados_IG.txt" :append true)] 
          (.write w (str "Makespan: "make-result "\nPermutação: "(into [] (map #(Integer/parseInt %) (map #(subs % 1) (map name (second (first resultado)))))) "\nIterações: " iteracao "\nData Inicio: " (.format form-data data-inicio) " / Data Fim: " (.format form-data data-fim) "\nTempo: " tempo-total " segundos\n\n")))
      (if (= i chamadas)
        (with-open [w (clojure.java.io/writer "src/resultados_IG.txt" :append true)]
           (.write w (str "Melhor: "melhor "\nPior: "pior "\nMedia makespan: "(/ media-make chamadas)"\nMedia de iteraçoes: "(/ media-iter chamadas) "\nMedia tempo: "(/ (double (long (* (/ media-tempo chamadas) 100))) 100) " segundos\n\n\n"))))
      (if (< i chamadas)
        (recur (inc i) melhor pior media-make media-tempo media-iter))))



(defn -main
  "opcao passada como parametro, se 1 entao chama o Busca do Macaco Híbrida (Monkey Search), se 2 entao chama o Iteração Gulosa (Iterated Greedy), se outra entao não faz nada."
  [opc]
    (let [
          opc (Integer/parseInt opc)
          resultado (case opc
                      1 "Busca do Macaco Híbrida - Monkey Search"
                      2 "Iteração Gulosa - Iterated Greedy"
                      "Opção Invalida!")
          ]
      (if(or (= opc 1) (= opc 2))   
        (do
          (println "Algoritmo: "resultado " | Instancia: "p/instancia) 
           (if (= opc 2) 
            (do
              (with-open [w (clojure.java.io/writer "src/resultados_IG.txt" :append true)]
                (.write w (str "-----------------------------------------------------------------------------------------\n" "Numero de trabalhos: " p/n_job "\nNumero de maquinas: " p/n_maq "\nInstancia: "p/instancia"\nLower bound: "p/lower_bound "\nUpper bound: "p/upper_bound "\n\n")))
              (iterated 1 999999999 0 0.0 0.0 0.0)) 
            (do
              (with-open [w (clojure.java.io/writer "src/resultados_BMH.txt" :append true)]
               (.write w (str "-----------------------------------------------------------------------------------------\n" "Numero de trabalhos: " p/n_job "\nNumero de maquinas: " p/n_maq "\nInstancia: "p/instancia"\nLower bound: "p/lower_bound "\nUpper bound: "p/upper_bound "\n\n")))
              (monkey 1 999999999 0 0.0 0.0)))
        )resultado)))

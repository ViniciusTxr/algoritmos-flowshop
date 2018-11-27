(ns NEH
  (:require [parser :as p]))


(def map-trab (p/mapeia-trabalhos))

(defn concatena-soma 
  "concatena os trabalhos com o seu devido tempo de processamento total"
  []
  (into {} (zipmap map-trab (map #(reduce + ^int %) (map #(% 1) map-trab)))))


(defn ordena-trabalhos
  "ordena os trabalhos pelo tempo de processamento total em ordem decrescente"
  []
  (into [] (apply concat
                  (map pop 
                       (map peek
                            (map pop 
                                 (reverse (sort-by last (concatena-soma)))))))))


(def ord-trab (ordena-trabalhos))

(defn pega-vetor 
  "retorna o vetor na posição i do vetor de tempos ordenados"
  [i]
  (map-trab (ord-trab i)))

(defn pega-vetor-trabalho 
  "retorna o vetor do trabalho t de vetor de tempos ordenados"
  [t]
  (map-trab t))


(defn pega-trabalho 
  "retorna o trabalho indicado na posição i do vetor de trabalhos ordenados"
  [i]
  (ord-trab i))


(defn elem-trab [vetor elem]
  (conj vetor elem)) 


(defn soma-prim-trab 
  ""
  [l]
  (into [] (for [i (range (count l))] 
             (reduce + ^int (subvec l 0 (+ i 1))))))


(defn calc-temp-proc
  "recebe uma lista de trabalhos e calcula o tempo de processamento de cada trabalho em cada maquina, retorna uma matriz com todos os tempos "
  [l] 
  (let [
        tam-l (count l) 
        vet [(soma-prim-trab (pega-vetor-trabalho (l 0)))]
        tempos (loop [inicio vet
                      i 0]
                      (if (= i (- tam-l 1))
                        inicio
                        (recur (let [vetor-atual (pega-vetor-trabalho (l (+ i 1)))
                                     tempos-parciais (loop [vet-aux []
                                                            j 0]
                                                           (if (= j p/n_maq)
                                                              vet-aux
                                                              (recur (if (= j 0)
                                                                        (conj vet-aux (+ ^int ((inicio i) 0) (vetor-atual 0)))
                                                                        (do  
                                                                          (if (> (vet-aux (- j 1)) ((inicio i) j)) 
                                                                            (conj vet-aux (+ ^int (vet-aux (- j 1)) (vetor-atual j)))
                                                                            (conj vet-aux (+ ^int ((inicio i) j) (vetor-atual j))) 
                                                                          ))
                                                                    ) (inc j)))) 
                                    ] (conj inicio tempos-parciais)
                                ) (inc i))))
        ] tempos))


(defn calcula-makespan 
  "recebe uma matriz com os tempos de processamento e retorna o makespan"
  [mat]
  (let [tam (count mat)
        makespan ((mat (- tam 1)) (- p/n_maq 1))]
    makespan))


(defn encontra-sequencia
  "retorna o melhor makespan e a melhor solução encontrada"
  []
  (let [
        vet-aux-enc [(pega-trabalho 0)]
        vet-aux-enc2 vet-aux-enc
        sequencia (loop [inicio [0 vet-aux-enc2]
                         i 0]
                    (if (= i  (- p/n_job 1))
                      inicio
                      (recur (let [makespan-melhor 9999999
                                   vet-enc (inicio 1)
                                   sequencia-parcial (loop [inicio-parcial [makespan-melhor vet-enc vet-enc]
                                                            j 0] 
                                                        (if (= j (+ i 2));;permuta o trabalho j em todas as posiçoes
                                                          inicio-parcial
                                                          (recur (let [makespan-melhor (inicio-parcial 0)
                                                                       vet-aux-enc (p/add-vec vet-enc (pega-trabalho (+ i 1)) j)
                                                                       makespan-aux (calcula-makespan (calc-temp-proc vet-aux-enc))
                                                                       nova-sequencia (if (< makespan-aux makespan-melhor)
                                                                                        (let [makespan-melhor makespan-aux
                                                                                              vet-aux-enc2 vet-aux-enc
                                                                                             ]
                                                                                          [makespan-melhor vet-aux-enc2]
                                                                                          )
                                                                                          inicio-parcial)
                                                                      ]
                                                                      nova-sequencia) (inc j))))
                                ]
                                sequencia-parcial)(inc i))))
        ]
        sequencia))


(def NEH (encontra-sequencia))
;Algoritmo baseado no artigo:
;Hybrid monkey search algorithm for flow shop scheduling problem under makespan and total flow time [2017] Marichelvam, Tosun and Geetha

(ns monkey-search
  (:require [NEH :as neh]
            [parser :as p]
            ))

(def M 20) ;20
(def v_max 4.0)
(def v_min -4.0)
(def c1 2)
(def c2 c1)

(def w0 0.9)
(def w_min 0.4)
(def beta 0.975)

(def mutacao 0.01) ;1%

(def visao 0.5)

(def salto_mortal_c -1)
(def salto_mortal_d 1)

(def iteracoes 200)
(def subidas 2000)

(def range_init 1) ;valores iniciais de 0 a 1
(def range_velo 1) ;velocidades de 0 a 1


(defn pega-elem-matriz 
  "retorna o elemento na posicao [i][j] da matriz "
  [mat i j]
  ((mat i) j))


(defn valores-iniciais
  [] 
  (take p/n_job (repeatedly ^double #(rand range_init))))


(defn solucoes-aleatorias
  "cria M-1 soluccoes aleatórias com de acordo com n_trab entre [0, 1]"
  [m]
  (vec (map #(zipmap (shuffle (p/key-jobs)) %) (vec (for [i (range m)]
             (vec (valores-iniciais)))))))


(defn ordena-trab-aleatorios
  "recebe um vetor de trabalhos aleatorios e os ordena de acordo com o valor associado ao trabalho em ordem crescente "
  [l]
  (let [vet l]
    (into (sorted-map-by 
            (fn [key1 key2]
              (compare [(vet key1) key1]
                       [(vet key2) key2])))
      vet)))


(defn NEH_MONKEY [] (conj [(neh/NEH 0)] (ordena-trab-aleatorios (zipmap (neh/NEH 1) (sort (valores-iniciais))))))


(defn smallest-position-value
  "aplica a funcao ordena-trab-aleatorios a todas as M solucoes geradas"
  [soluc]
  (map #(ordena-trab-aleatorios %) soluc))

(defn SPV_inicial [] (vec (smallest-position-value (solucoes-aleatorias (- M 1)))))

(defn solucao_inicial [] (conj (SPV_inicial) ((NEH_MONKEY) 1)))

(defn pega-trabalhos-SPV
  "recebe uma lista l de chaves-valores como parametro e retorna só as chaves"
 [h]
 (vec (map #(% 0) h)))


(defn trabalhos-aleatorios
  "pega cada chave-valor contido em SPV e retorna só os valores dos trabalhos de cada"
  [spv] 
  (vec (map pega-trabalhos-SPV spv)))


(defn makespan-SPV 
  "retorna o makespan de cada lista de trabalho do SPV"
  [spv]
  (vec (map #(neh/calcula-makespan (neh/calc-temp-proc %)) (trabalhos-aleatorios spv))))

(defn avalia-macaco
  "associa o makespan correspondende a sua lista de trabalho"
  [spv]
  (map vector (makespan-SPV spv) spv))


(defn encontra-melhor
  "retorna o melhor em uma pupoulaçao"
  [x]
    (apply min-key #(% 0) (avalia-macaco x)))



(defn velocidade-inicial
  "cria o vetor de velocidades iniciais para cada macaco no conjunto M"
  []
  (vec (for [x (range M)] 
             (vec 
                (take p/n_job 
                  (repeatedly #(+ ^double v_min (* ^double (- v_max v_min) (rand range_velo)))))))))


(defn prob-mutacao 
  "verifica a probabilidade de mutacao x"
  [x]
  (> x (rand)))


(defn Xt-posicao 
  "pega o vetor referente as posicoes dos macacos "
  [xt]
  (vec (for [i (range (count xt))] 
              (vec (map #(% 1) (xt i))))))

(defn Xt-trabalhos
  "pega o vetor referente aos trabalhos dos macacos"
  [xt]
  (vec (for [i (range (count xt))] 
              (vec (map #(% 0) (xt i))))))



(defn atualiza-peso-inercia
  "recebe como parametro um vetor com o peso inicial e a iteracao inicial e retorna um vetor com todos os pesos da inercia, com peso minimo 0.4, de acordo com o numero de iteracoes "
  [w1 i]
  (let [aux w1] 
    (if (< i subidas)
      (if (< (* ^double (aux i) beta) w_min)
        (recur (conj aux w_min) (inc i))
        (recur (conj aux (* ^double (aux i) beta))(inc i)))
      aux)))

(def vetor-pesos (atualiza-peso-inercia [w0] 0))


(defn atualiza-velocidades
  "atualiza a velocidade do macaco i na dimensão j"
  [wt vij xij localij globalj]
  (let [ y (+ 
            (+ (* ^double wt vij) (* ^double c1 (* ^double (rand range_velo) (- ^double localij xij))))
            (* ^double c2 (* ^double (rand range_velo) (- ^double globalj xij))))
        ] 
    (if (< y v_min)
      v_min
      (if (> y v_max)
        v_max
        y))))


(defn novas-velocidades
  "atualiza a velocidade de todos os macacos"
  [iter xt-p veloc local global]
  (vec (for [i (range M)]
            (vec (for [j (range p/n_job)]
                      (atualiza-velocidades 
                        (vetor-pesos iter) 
                        (pega-elem-matriz veloc i j)
                        (pega-elem-matriz xt-p i j)
                        (pega-elem-matriz local i j)
                        (global j)
                       ))))))



(defn novas-posicoes
  "atualiza as posicoes dos macacos de acordo com a velocidade e a posicao anterior"
  [xt-p veloc]
  (vec (for [i (range M)]
             (vec (for [j (range p/n_job)]
                        (+ ^double (pega-elem-matriz xt-p i j)
                            (pega-elem-matriz veloc i j)
                         ))))))


(defn concatena-chave-valor 
  "recebe um conjunto chave e um conjunto valor e concatena eles"
  [chave valor]
  (for [x (range M)]
    (zipmap (chave x) (valor x))))


(defn atualiza-melhor-local
  "dado os melhores locais e os locais atuais, verifica quais sao os menores posicao a posicao e atualiza o melhor local"
  [melhor atual]
  (vec (for [i (range M)]
             (if (< ((atual i) 0) ((melhor i) 0))
               (atual i)
               (melhor i)))))


(defn atualiza-melhor-global
  "dado o melhor global e o global atual, verifica qual é o menor e atualiza o melhor global"
  [melhor atual]
  (if (< (atual 0) (melhor 0))
    atual
    melhor))


(defn mutacao-deslocamento [vet pos1 pos2]
  "aplica uma mutacão no melhor global, removendo um trabalho da posicao pos1 e inserindo na pos2"
  (let [
        vet-aux (vet 1)
        vet-trab (vec (map #(% 0) vet-aux))
        vet-pos (vec (map #(% 1) vet-aux))
        elem (vet-trab pos1)
        vet-trab (p/add-vec (p/remove-elem-vetor vet-trab pos1) elem pos2)
        novo-vet (vec (avalia-macaco [(ordena-trab-aleatorios (zipmap vet-trab vet-pos))]))
        ]
     (novo-vet 0)
    ))


(defn verifica-mutacao 
  "verifica se a mutação vai ocorrer, se sim aplica a mutação, senao nao acontece nada"
  [global]
  (if (prob-mutacao mutacao) 
          (atualiza-melhor-global global (mutacao-deslocamento global (rand-int p/n_job) (rand-int p/n_job)))
          global))


(defn atualiza-global-local
  "atualiza o melhor local caso a mutação melhore o macaco"
  [global mutacao local indice ]
  (if (< (mutacao 0) (global 0))
    (p/add-vec (p/remove-elem-vetor local indice) mutacao indice)
    local))


(defn atualiza-local
  "atualiza o valor do local com o menor entre o atual e o que tinha antes"
  [anterior atual]
  (vec (for [i (range M)
              :let [
                      atual_i (atual i)
                      anterior_i (anterior i)
                    ]
            ]
          (if (< (atual_i 0) (anterior_i 0))
            atual_i
            anterior_i))))


(defn processo-subida-mutacao
  "recebe como parametros i=(iteracao atual), vij=(velocidade de cada macaco), xij=(posicao de cada macaco), pij=(melhor local de cada macaco), gj=(melhor global) e retorna as melhores posicoes locais aplicando o processo de mutacao"
  [i vij xij pij gj]
  (let [
        xij_pj (vec (map #(% 1) xij))
        xt_pos (Xt-posicao xij_pj) 
        nova_vij (novas-velocidades (- i 1) xt_pos vij (vec (Xt-posicao (vec (map #(% 1) pij)))) (vec (map #(% 1) (gj 1))));9%
        posicao (novas-posicoes xt_pos nova_vij)
        nova_xij (concatena-chave-valor (Xt-trabalhos xij_pj) posicao) 
        ordena_xij (smallest-position-value nova_xij) 
        local (vec (avalia-macaco ordena_xij)) 
        atual_local (vec (atualiza-local xij local))
        novo_pij (atualiza-melhor-local pij atual_local)
        indice_melhor_local (.indexOf novo_pij (apply min-key #(% 0) novo_pij))
        atual_global (novo_pij indice_melhor_local) 
        novo_gj (atualiza-melhor-global gj atual_global)
        mutacao_gj (verifica-mutacao novo_gj)
        novo_pij (vec (atualiza-global-local novo_gj mutacao_gj novo_pij indice_melhor_local))
        ]
    (if (< i subidas)
      (recur (inc i) nova_vij atual_local novo_pij mutacao_gj)
      novo_pij)))


(defn escolhe-aleatorio-observa
  "recebe a iteracão atual e um vetor. Para cada elem escolhe um numero aleatorio entre os valores de [elem-visao, elem+visao], retorna um vetor com estes novos elementos se o makespan deles for menor ou igual que o makespan do vetor original, senao retorna o vetor original depois de um certo numero de tentativas de melhoramento"
  [iter x]
  (let [
      make (x 0)
      vet (x 1)
      novo_vet (vec (map #(+ ^double (rand (- (+ ^double % visao) (- ^double % visao))) (- ^double % visao)) ^double ((Xt-posicao [vet]) 0)))
      ordena (smallest-position-value [(zipmap ((Xt-trabalhos [vet]) 0) novo_vet)])
      novo_x (vec (avalia-macaco ordena))
    ]
    (if (or (<= ((novo_x 0) 0) make) (> iter 1000))
      (if (> iter 1000)
        x
        (novo_x 0))
      (recur (inc iter) x))
  ))
  
(defn observa-salta
  "para cada macaco, chama a funcao escolhe-aleatorio-observa"
  [atual]
  (map #(escolhe-aleatorio-observa 0 (vec %)) atual))


(defn escolhe-aleat-saltomortal
  "escolhe aleatoriamente um valor entre salto_mortal_c e salto_mortal_d e monta um vetor com M desses valores"
  []
  (for [x (range M)] (+ ^double (rand (- ^double salto_mortal_d salto_mortal_c)) salto_mortal_c)))


(defn media-saltomortal
  "calcula a media de cada dimensão dos macacos"
  [xij]
  (vec (map #(/ ^double % M) (apply map + ^double xij)))
)

(defn saltomortal
  "cada macaco da um saltomortal para uma nova posicao, de acordo com uma funcao, esta nova posicao será passada como parametro para a nova itreracao"
  [xij]
  (let [aleat (vec (escolhe-aleat-saltomortal))
        pivo (media-saltomortal xij)
        ]
    (vec
       (for [i (range M)]
         (vec 
            (for [j (range p/n_job)]
               (+ ^double ((xij i) j) 
                  (* ^double (aleat i)
                     (- ^double (pivo j)((xij i) j))))
              ))))))


(defn monkey-search
  "função principal que ira chamar todas as outras funções"
  [i xij global]
  (let [
        y (println i "\n")
        avalia_xij (vec (avalia-macaco xij))
        climb (processo-subida-mutacao 1 (velocidade-inicial) avalia_xij avalia_xij (apply min-key #(% 0) avalia_xij))
        posicao (map #(% 1) climb)
        melhor (atualiza-melhor-global global (apply min-key #(% 0) climb))
        obs_salt (vec (observa-salta climb))
        melhor (atualiza-melhor-global melhor (apply min-key #(% 0) obs_salt))
        new_climb (processo-subida-mutacao 1 (velocidade-inicial) obs_salt obs_salt (apply min-key #(% 0) obs_salt))
        melhor (atualiza-melhor-global melhor (apply min-key #(% 0) new_climb))
        nova_posicao (vec (map #(% 1) new_climb))
        salto (saltomortal (Xt-posicao nova_posicao))
        concatena (smallest-position-value (concatena-chave-valor (Xt-trabalhos nova_posicao) salto))
        melhor (atualiza-melhor-global melhor (encontra-melhor concatena))
        y (println (melhor 0))
        y (println "\n----------\n")
        ]
    (if (< i (- iteracoes 1))
      (recur (inc i) concatena melhor)
      melhor)))
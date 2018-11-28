## Algoritmos e Resultados para o Problema Flow Shop Permutacional

Os algoritmos Iteração Gulosa com Busca Local em Soluções Parciai, Busca do Macaco Híbrida e a heurística NEH estão localizados dentro da pasta 'src/' nos arquivos 'iterated_greedy.clj', 'monkey_search.clj' e 'NEH.clj' respectivamente. 

Os resultados podem ser encontrados dentro da pasta 'resultados/'. Cada método gerou uma subpasta com seus respectivos resultados. Dentro da pasta 'resultados/IG' contém os resultados obtidos para os três criterios de parada, 'T60', 'T120' e 'T240'. Todos os resultados estão no formato de arquivo '.txt'.


### Executando os Algoritmos
Para executar os algoritmos deve-se ter instalado o Leiningen e o Java Develpment Kit (JDK).

#### Instalar o Leiningen
Os passos a segir ensinam a instalar o Leiniengin em uma máquina com Ubuntu 16.04 LTS rodando o Java 8.

1) Faça o download do arquivo de script:
     
     $ wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein

2) Faça do arquivo um executável:
    
    $ chmod a+x lein
    
3) Mova o arquivo para a pasta /usr/bin com o comando:
  
    $ sudo mv lein /usr/bin
 
4) Execute o script para que ele baixe o pacote de auto-instalação e
instale o Leiningen: 
  
    $ lein
    
#### Executar a partir do lein
Após a instalação do Leiningen ja se pode executar os algoritmos. Para isto siga os passos a seguir:

1) Entre na pasta do projeto pelo terminal:

    $ cd 'caminho-ate-projeto'/algoritmos-flowshop
    
2) Use o Clojure a partir do terminal com o comando: 

    $ lein repl
    
3) Para executar o algotirmo Iteração Gulosa digite:

    core=> (-main "1")
    
4) Para executar o algotirmo Busca do Macaco Híbrida digite:

    core=> (-main "2")
    
Obs: A saída produzida a partir da execução dos algoritmos pode ser encontada dentro da pasta 'src'.


#### Executar a partir do pacote jar
Para executar os algoritmos a partir do pacote 'jar' siga os passos a seguir:

1) Entre na pasta do projeto pelo terminal:

    $ cd 'caminho-ate-projeto'/algoritmos-flowshop
    
2) Gere o pacote 'jar' com o comando: 

    $ lein uberjar
    
3) Para executar o algotirmo Busca do Macaco Híbrida digite:

    $ java -jar target/uberjar/BMH-IG-PSFP-standalone.jar 1
    
4) Para executar o algotirmo Iteração Gulosa digite:

    $ java -jar target/uberjar/BMH-IG-PSFP-standalone.jar 2
    
Obs: A saída produzida a partir da execução dos algoritmos pode ser encontada dentro da pasta 'src'.



### Alterar o conjunto de instâncias e a instância que está sendo executada
Por padrão ambos algoritmos estão configurados para executar a instância 'tai1', ou seja, o conjunto 20x5 e a instância 1.
Para mudar esta configuração siga os passos:

1) Entre no arquivo 'parser.clj' presente na pasta 'src/' 

2) Na função 'le_arquivo' você encontrará algo como "src/Benchmarks/tai20_5.txt" presente na linha 8. Então troque 'tai20_5.txt' pelo conjunto de instâncias que você deseja executar. 
  Obs: Os 12 conjuntos de instâncias estudados neste trabalho estão contidos em 'src/Benchmarks/'

3) Para trocar a instancia executada vá até a linha 17, voce verá algo como '(def instancia 1)', então basta trocar o numeral '1' pelo numeral da instância desejada.
    Obs: As instâncias vão de 1 até 10.
    
4) Salve o arquivo e recompile o código para aplicar as alterações feitas.
    
    
##### Alterar o ctritério de parada do Itegação Gulosa com Busca Local em Soluções Parciais
O algoritmo IG_BLSP tem três críterios de parada possíveis chamados de: T60, T120 e T240. Por padrão este algoritmo está configurado com o critério de parada T240, para altera-lo siga os seguintes passos:

1) Entre no arquivo 'iterated_greedy.clj' presente na pasta 'src/' 

2) Vá até a linha 172, você verá algo como '(def tempo (int (* p/n_job (* (double (/ p/n_maq 2)) 240))))'. Agora comente esta linha.

3) As linhas 170, 171 e 172 contém os critérios de parada T60, T120 e T240 respectivamente. Escolha qual o criterio voce deseja utilizar e descomente a sua respectiva linha.

4) Salve o arquivo e recompile o código para aplicar as alterações feitas.


 
##### O artigo original deste trabalho com todas as explicações, analises de resultado entre os métodos e os detalhes da implementação podem ser encontrados em: (monografia em aprovação, link aqui em breve).     
   
    
###### Autor: Vinicius Lopes da Silva Teixeira
###### e-mail: viniciuslt@outlook.com

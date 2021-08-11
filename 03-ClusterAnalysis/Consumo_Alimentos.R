#**********************************************************
#AN?LISE DE CLUSTER - CONSUMO DE ALIMENTOS
#**********************************************************
#**********************************************************

#Mapear diret?rio de trabalho
#Aten??o: Alterar Diret?rio
setwd("/home/wlinhares/Documentos/GoogleDrive/Estudos/FIA/Estatística Aplicada/Aula 09 e 10/")
getwd()

#**********************************************************

#**********************************************************
#(a) Abra o banco de dados Consumo_Alimentos.txt no R.
#Leitura da Base de Consumo de Alimentos
consumo <- read.table("Consumo_Alimentos.txt", header = TRUE)
nrow(consumo)
ncol(consumo)

#**********************************************************
#(b) Fa?a uma an?lise explorat?ria completa da base de dados. Comente sobre a variabilidade dos dados.
#Medidas resumo principais de todas as vari?vei, exceto a primeira, que ? o pais
summary(consumo[,-1]) #Min, Q1, Q2, Q3 e Max
apply(consumo[,-1] , 2 , sd) #Desvio Padr?o
#Descritiva em formato de tabela #Frequencia, Min, Max, Media, Desvio Padra?, IIQ e CV

library(magrittr)
summarytools::dfSummary(consumo) %>% 
  summarytools::view()

#Matriz de boxplots
library(tidyverse)
library(ggplot2)
consumo %>%
  select(-Pais) %>% #Desconsiderando pa?s e selecionar as demais vari?veis 
  gather(var, valor) %>% 
  ggplot(aes(y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var) + # "facet_wrap(~var, scales = "free") +" para deixar escala livre
  theme(legend.position = "none")
#**********************************************************
#(c) Calcule a matriz de dist?ncias euclidianas entre os 25 pa?ses.

#C?lculo da dist?ncia euclidiana entre os elementos
distancia <- dist(consumo[,-1], method="euclidean") #C?lculo das dist?ncias euclidianas
distancia
#Vizualia??o por mapa de calor, usando pacote factoextra
library(factoextra)
fviz_dist(distancia)

#(d) Fa?a a an?lise de agrupamento usando o M?todo os 5 M?todos 
#apresentados e sugira a quantidade de grupos ap?s a an?lise do 
#Dendrograma, e comente seus aspectos.

clust_single <- hclust(distancia, method="single") 
fviz_dend(clust_single, main = "M?todo Single")
fviz_dend(clust_single, k = 4, main = "M?todo single")
print(clust_single$height)

#fviz_dend(clust_single, k = 4, main = "M?todo Single")
#print(clust_single$height)

clust_complete <- hclust(distancia, method="complete")
fviz_dend(clust_complete, main = "M?todo Complete")
fviz_dend(clust_complete, k = 4, main = "M?todo Complete")
print(clust_complete$height)

#(e)Analise as caracter?sticas de cada grupo.
#Atribui a cada pa?s o cluster a qual ele pertence pela vari?vel cluster
consumo$cluster <- as.factor(cutree(clust_complete, k=4))

#Tamanho dos Clusters
table(consumo$cluster)

#Faz BoxPlot para cada vari?vel e compara por cluster
#Distribui??o das vari?veis por cluster
consumo[,-1] %>% 
  gather(var, valor, -cluster) %>% 
  ggplot(aes(x = cluster, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")

#(f)Padronize as vari?veis pelo Z-escore, refa?a o item (d) e compare os resultados.
consumo <- read.table("Consumo_Alimentos.txt", header = TRUE)

#O comando Scale padroniza os dados
#Padronizar vari?veis para an?lise (Z-Score: (x-m?dia)/dp)
consumo_z<-as.data.frame(scale(consumo[,-1]))  ## -1 = Retira a primeira coluna
#Retirar nota??o cient?fica no R
options(scipen = 999)
#Verificar m?dia e desvio-padr?o das vari?veis padronizadas: (0,1)
apply(consumo_z,2,mean)
apply(consumo_z,2,sd)

#Descritiva em formato de tabela #Frequencia, Min, Max, Media, Desvio Padra?, IIQ e CV
summarytools::dfSummary(consumo_z) %>% 
  summarytools::view()

#Matriz de boxplots
library(tidyverse)
library(ggplot2)
# Matriz de gráficos de dimensão 3 linhas x 3 colunas
consumo_z %>% 
  gather(var, valor) %>% 
  ggplot(aes(y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var) +
  theme(legend.position = "none")

#Calcule a matriz de dist?ncias euclidianas entre os 25 pa?ses.
#C?lculo da dist?ncia euclidiana entre os elementos
rownames(consumo_z) <- consumo$Pais
distancia_z <- dist(consumo_z[,-1], method="euclidean") #C?lculo das dist?ncias euclidianas
distancia_z
#Vizualia??o por mapa de calor, usando pacote factoextra
library(factoextra)
fviz_dist(distancia_z)

#Fa?a a an?lise de agrupamento usando o M?todo os 5 M?todos 
#apresentados e sugira a quantidade de grupos ap?s a an?lise do 
#Dendrograma, e comente seus aspectos.

clust_single_z <- hclust(distancia_z, method="single") 
fviz_dend(clust_single_z, main = "M?todo Single")
fviz_dend(clust_single_z, k = 4, main = "M?todo Single")

#fviz_dend(clust_single, k = 4, main = "M?todo Single")
#print(clust_single$height)

clust_complete_z <- hclust(distancia_z, method="complete")
fviz_dend(clust_complete_z, main = "M?todo Complete")
fviz_dend(clust_complete_z, k = 4, main = "M?todo Complete")
print(clust_complete_z$height)

#Analise as caracter?sticas de cada grupo.
#Atribui a cada pa?s o cluster a qual ele pertence pela vari?vel cluster
consumo_z$cluster_z <- as.factor(cutree(clust_complete_z, k=4))

#Tamanho dos Clusters
table(consumo$cluster_z)

#Faz BoxPlot para cada vari?vel e compara por cluster
#Distribui??o das vari?veis por cluster
consumo[,-1] %>% 
  gather(var, valor, -cluster_z) %>% 
  ggplot(aes(x = cluster_z, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")


# Item g ------------------------------------------------------------------
# Rode o M?todo K-m?dias, utilizando o k encontrado no item (f).
rm(list=ls())

setwd("/home/wlinhares/Documentos/GoogleDrive/Estudos/FIA/Estatística Aplicada/Aula 11 - Exercícios/")
getwd()

consumo <- read.table("Consumo_Alimentos.txt", header = TRUE)
consumo_z <- as.data.frame(scale(consumo[,-1]))
row.names(consumo_z) <- consumo$Pais
set.seed(0) # Atribuir a semente para deixar o processo aleat?rio igual, todas as vezes que processarmos o programa
consumo_km <- kmeans(consumo_z, centers = 4) # Centers sao os k grupos 
consumo_km

# Gr?fico dos clusters
# install.packages("FactoMineR")
# library("FactoMineR")
fviz_cluster(consumo_km, data = consumo_z)

# Marcar todas as observa??es com os clusters gerados
consumo$cluster_km <- as.factor(consumo_km$cluster)

# Tamanho dos Clusters
table(consumo$cluster_km)

#Faz BoxPlot para cada vari?vel e compara por cluster
#Distribui??o das vari?veis por cluster
consumo[,-1] %>% 
  gather(var, valor, -cluster_km) %>% 
  ggplot(aes(x = cluster_km, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")

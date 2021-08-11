#**********************************************************
#AN?LISE DE CLUSTER - CART?O DE CR?DITO
#**********************************************************

#**********************************************************
#Mapear diret?rio de trabalho
getwd()
#Aten??o: Alterar Diret?rio
setwd("/Users/wilbe/OneDrive/Documentos/GoogleDrive/Estudos/FIA/Estatística Aplicada/Aula 19")
#**********************************************************
#Retirar nota??o cient?fica
options(scipen=999)

#Leitura da Base de cartao
library(readxl)
cartao = read_excel("Cartao_Credito_BusinessCase.xlsx", sheet="Base de Dados")

#Estutura da base de dados
nrow(cartao)
ncol(cartao)
str(cartao)

#(a)Realize a an?lise explorat?ria univariada. 
#Calcule as medidas resumos e construa boxplots e histogramas para todas as vari?veis. 
#Analise os resultados.

summary(cartao[,-1])
apply(cartao[,-1] , 2 , sd, na.rm = TRUE) #Desvio Padr?o

par(mfrow=c(2,6)) #Matriz de gr?ficos
#Boxplot
boxplot(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
boxplot(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
boxplot(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
boxplot(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_USO_CARTAO_12M")
boxplot(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
boxplot(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")
#Histograma
hist(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
hist(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
hist(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
hist(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_USO_CARTAO_12M")
hist(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
hist(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")

#(b) Avalie a presen?a e quantifique os missings para cada vari?vel. 
#Seria poss?vel tratar os missings?  
#Se sim, trate-os segundo o contexto do neg?cio.

#Os missings poderiam ser substitu?dos por ZERO uma vez que o sistema n?o registra
#quando n?o h? transa??es ou fatura de cart?o de cr?dito:
cartao$QTDE_TRANSACAO_T0 <- ifelse(is.na(cartao$QTDE_TRANSACAO_T0), 0,cartao$QTDE_TRANSACAO_T0)
cartao$VALOR_FATURA_T0 <- ifelse(is.na(cartao$VALOR_FATURA_T0), 0,cartao$VALOR_FATURA_T0)

#Analisando novamente as descritivas da base com o tratamento de missings
summary(cartao[,-1])
apply(cartao[,-1] , 2 , sd) #Desvio Padr?o

par(mfrow=c(2,6)) #Matriz de gr?ficos
#Boxplot
boxplot(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
boxplot(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
boxplot(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
boxplot(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_USO_CARTAO_12M")
boxplot(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
boxplot(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")
#Histograma
hist(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
hist(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
hist(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
hist(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_USO_CARTAO_12M")
hist(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
hist(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")

#(c)Avalie a presen?a e quantifique os outliers para cada vari?vel. 
#Seria poss?vel tratar os outliers?  
#Se sim, trate-os segundo o contexto do neg?cio.

#Percentil 1 e 99
round(apply(cartao[,-1] , 2 , quantile , probs = c(0.01,0.99)),2)

#Substituindo por P1
cartao$LIMITE_DISP_T0 <- ifelse(cartao$LIMITE_DISP_T0 < quantile(cartao$LIMITE_DISP_T0,.01), quantile(cartao$LIMITE_DISP_T0,.01),cartao$LIMITE_DISP_T0)
cartao$LIMITE_TOTAL_T0 <- ifelse(cartao$LIMITE_TOTAL_T0 < quantile(cartao$LIMITE_TOTAL_T0,.01), quantile(cartao$LIMITE_TOTAL_T0,.01),cartao$LIMITE_TOTAL_T0)
cartao$PERC_USO_LIMITE_T0 <- ifelse(cartao$PERC_USO_LIMITE_T0 < quantile(cartao$PERC_USO_LIMITE_T0,.01), quantile(cartao$PERC_USO_LIMITE_T0,.01),cartao$PERC_USO_LIMITE_T0)
cartao$PERC_FAT_CARTAO_12M <- ifelse(cartao$PERC_FAT_CARTAO_12M < quantile(cartao$PERC_FAT_CARTAO_12M,.01), quantile(cartao$PERC_FAT_CARTAO_12M,.01),cartao$PERC_FAT_CARTAO_12M)
cartao$QTDE_TRANSACAO_T0 <- ifelse(cartao$QTDE_TRANSACAO_T0 < quantile(cartao$QTDE_TRANSACAO_T0,.01), quantile(cartao$QTDE_TRANSACAO_T0,.01),cartao$QTDE_TRANSACAO_T0)
cartao$VALOR_FATURA_T0 <- ifelse(cartao$VALOR_FATURA_T0 < quantile(cartao$VALOR_FATURA_T0,.01), quantile(cartao$VALOR_FATURA_T0,.01),cartao$VALOR_FATURA_T0)

#Substituindo por P99
cartao$LIMITE_DISP_T0 <- ifelse(cartao$LIMITE_DISP_T0 > quantile(cartao$LIMITE_DISP_T0,.99), quantile(cartao$LIMITE_DISP_T0,.99),cartao$LIMITE_DISP_T0)
cartao$LIMITE_TOTAL_T0 <- ifelse(cartao$LIMITE_TOTAL_T0 > quantile(cartao$LIMITE_TOTAL_T0,.99), quantile(cartao$LIMITE_TOTAL_T0,.99),cartao$LIMITE_TOTAL_T0)
cartao$PERC_USO_LIMITE_T0 <- ifelse(cartao$PERC_USO_LIMITE_T0 > quantile(cartao$PERC_USO_LIMITE_T0,.99), quantile(cartao$PERC_USO_LIMITE_T0,.99),cartao$PERC_USO_LIMITE_T0)
cartao$PERC_FAT_CARTAO_12M <- ifelse(cartao$PERC_FAT_CARTAO_12M > quantile(cartao$PERC_FAT_CARTAO_12M,.99), quantile(cartao$PERC_FAT_CARTAO_12M,.99),cartao$PERC_FAT_CARTAO_12M)
cartao$QTDE_TRANSACAO_T0 <- ifelse(cartao$QTDE_TRANSACAO_T0 > quantile(cartao$QTDE_TRANSACAO_T0,.99), quantile(cartao$QTDE_TRANSACAO_T0,.99),cartao$QTDE_TRANSACAO_T0)
cartao$VALOR_FATURA_T0 <- ifelse(cartao$VALOR_FATURA_T0 > quantile(cartao$VALOR_FATURA_T0,.99), quantile(cartao$VALOR_FATURA_T0,.99),cartao$VALOR_FATURA_T0)

#Rever os boxplots e histogramas com valores extremos tratados
par(mfrow=c(2,6))

#Boxplot
boxplot(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
boxplot(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
boxplot(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
boxplot(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_FAT_CARTAO_12M")
boxplot(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
boxplot(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")
#Histograma
hist(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
hist(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
hist(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
hist(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_FAT_CARTAO_12M")
hist(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
hist(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")

#**********************************************************
#(d)Para a base "tratada" nos itens (b) e (c), realize a an?lise explorat?ria univariada 
#novamente. Calcule as medidas resumos e construa boxplots e histogramas para todas 
#as vari?veis. Analise os resultados.

#**********************************************************
#(e)Avalie a correla??o entre as vari?veis por meio Correla??o de Pearson. 
#Discuta a rela??o entre as vari?veis e decida quais vari?veis ser?o utilizadas 
#para agrupar os clientes. 
cor(cartao[,-1])

#Escolha das vari?veis: "PERC_USO_LIMITE_T0","QTDE_TRANSACAO_T0","VALOR_FATURA_T0"

#**********************************************************
#(f)Realize a padroniza??o das vari?veis.

#Avaliar variabilidade dos dados
round(apply(cartao[,-1],2,sd),2)

#Aplicar padroniza??o Z-Score das vari?veis (Z-Score: (x-m?dia)/dp)
aux_padr <- as.data.frame(apply(cartao[,-1],2,scale))
cartao_padr <- cbind(cartao[,"Cod_Cliente"],aux_padr)
colnames(cartao_padr)[1] <- "Cod_Cliente"

#Verificar m?dia e desvio-padr?o das vari?veis padronizadas: (0,1)
round(apply(cartao_padr[,-1],2,mean),2)
round(apply(cartao_padr[,-1],2,sd),2)

#Mantendo apenas as vari?veis de interesse:
cartao_padr <- cartao_padr[,c("Cod_Cliente","PERC_USO_LIMITE_T0",
                              "QTDE_TRANSACAO_T0","VALOR_FATURA_T0")]

#**********************************************************
#(g)Dado as caracter?sticas da base de dados, qual m?todo de agrupamento voc? adotaria?
#Discuta com a sala.

#**********************************************************
#(f) Realize a an?lise de agrupamento com os 5 m?todos hier?rquicos, 
#selecionando aleatoriamente 1.000 observa??es, pela an?lise do dendrograma, 
#escolha um dos m?todos e defina a quantidade de grupos.

#(Utilize os comando do R: cartao_h <- sample_n(cartao_padr, 1000), 
#sendo cart?o_h o objeto que guardar? a base amostral.
library(dplyr) #biblioteca para tirar amostra aleat?ria usando sample_n
set.seed(5) #para garantir sempre a mesma amostra 
cartao_h <- sample_n(cartao_padr, 1000) #amostra de 1000 casos

#C?lculo da dist?ncia entre os elementos: compara??o entre os m?todos
distancia <- dist(cartao_h[,-1], method="euclidean") #C?lculo das dist?ncias euclidianas

par(mfrow=c(1,2))
clust_h <- hclust(distancia, method="single") #Cria??o do cluster
plot(clust_h, main="Single", hang=-1,labels=FALSE) #Plot do Dendograma - representa??o visual do agrupamento
clust_h <- hclust(distancia, method="complete") #Cria??o do cluster
plot(clust_h, main="Complete", hang=-1,labels=FALSE) #Plot do Dendograma - representa??o visual do agrupamento

#Propondo 4 grupos para o cluster criado a partir do m?todo escolhido
rect.hclust(clust_h, k=3, border=1:5) #Plot do Dendograma, com aux?lio visual a partir da indica??o do n?mero de grupos

#(i)Realize a an?lise de agrupamento pelo m?todo hier?rquico K-m?dias e defina 
#a quantidade de grupos.
set.seed(4)
library(factoextra)
#M?todo Elbow (Crit?rio: Soma de quadrados dentro)
fviz_nbclust(cartao_h[,-1], kmeans, method = "wss")

#(j)Compare a quantidade de grupos encontrados pelos m?todos hier?rquicos e K-m?dias.

#(k)Considerando que a base de dados ? "grande", realize o agrupamento dos clientes 
#pelo K-m?dias utilizando k definido no item (j).
#Dado a escolha de k=5, realizar o K-m?dias
set.seed(5)
clust_km <- kmeans(cartao_padr[,-1], 4) #considerando o n?mero de clusters igual a 4

# Gr?fico dos clusters
fviz_cluster(clust_km , data = cartao_padr[,-1])

#Marcar toda as observa??es com os clusters gerados
#cartao$cartao_kmeans_agrup <- cbind(cartao, cluster_km = as.factor(clust_km$cluster))
cartao$cluster_kmeans <- as.factor(clust_km$cluster)

#Tamanho dos Clusters 
clust_km$size 
round(prop.table(clust_km$size),2)

#(l)Descreva as personas e justifique para ?rea de neg?cios porque o agrupamento
#formado ? adequado para implementar estrat?gias de atendimento e relacionamento 
#diferenciados.

#Matriz de boxplots
#library(tidyverse)
#library(ggplot2)
#Faz BoxPlot para cada vari?vel ORIGINAL e compara por cluster
#Distribui??o das vari?veis por cluster: 
cartao[,c(-1,-2,-3,-5)] %>% #s? com as vari?veis do cluster
  gather(var, valor, -cluster_kmeans) %>% 
  ggplot(aes(x = cluster_kmeans, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")

#Descritiva com todas as vari?veis
cartao[,-1] %>% 
  gather(var, valor, -cluster_kmeans) %>% 
  ggplot(aes(x = cluster_kmeans, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")

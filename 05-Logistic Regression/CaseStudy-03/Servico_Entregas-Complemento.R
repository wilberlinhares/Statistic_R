#install.packages("factoextra")
#**********************************************************
#AN?LISE DE CLUSTER - ENTREGAS
#**********************************************************
#**********************************************************
#Aten??o: Alterar Diret?rio
setwd("/Users/wilbe/OneDrive/Documentos/GoogleDrive/Estudos/FIA/Estatística Aplicada/Aula 19")

#Retirar nota??o cient?fica no R
options(scipen = 999)
#**********************************************************

#**********************************************************
#Leitura da Base 
library(readxl)
entregas <- read_excel("Servico_Entregas-Complemento.xlsx", sheet = 'Base de Dados')
nrow(entregas)
ncol(entregas)
#**********************************************************


#**********************************************************
# Fa?a uma an?lise explorat?ria da base de dados 
# (obtenha as medidas de posi??o e dispers?o). 
summary(entregas[,-1]) #Min, Q1, Q2, Q3 e Max
apply(entregas[,-1] , 2 , sd) #Desvio Padr?o

#**********************************************************
#Padronize as vari?veis.
entregas_z<-scale(entregas[,-1])
summary(entregas_z) #Min, Q1, Q2, Q3 e Max
apply(entregas_z, 2, sd) #Desvio Padr?o

set.seed(123)
library(factoextra)
#M?todo Elbow (Crit?rio: Soma de quadrados dentro)
fviz_nbclust(entregas_z, kmeans, method = "wss")

set.seed(123)
clust_km <- kmeans(entregas_z, 4) #considerando o n?mero de clusters igual a 4 ou 5
clust_km

# Gr?fico dos clusters
fviz_cluster(clust_km , data = entregas_z)

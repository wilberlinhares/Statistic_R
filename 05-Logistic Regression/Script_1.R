#Alterar o Diretorio
setwd("/Users/wilbe/OneDrive/Documentos/GoogleDrive/Estudos/FIA/Estat�stica Aplicada/Aula 14 a 15")

#Leitura da base de dados
library(readxl)
cosmeticos <- read_excel("Cosmeticos.xlsx", sheet ="Banco de Dados")

options(scipen=999) #Retira a notação científica

#Análise Exploratória da Base de Dados

#Variável Sexo
table(cosmeticos$Sexo) 

#Variável Idade
summary(cosmeticos$Idade)
boxplot(cosmeticos$Idade) 

#Variável Cidade
table(cosmeticos$Cidade) 

#Variável Sinistro
table(cosmeticos$Resposta) 
prop.table(table(cosmeticos$Resposta)) 

#Modelo de Regressão Logística
modelo <- glm(Resposta ~
                Sexo
              + Idade
              + Cidade,
              family=binomial(link='logit'),data=cosmeticos)
summary(modelo)

#**********************************************************
#Mapear diret?rio de trabalho
#Aten??o: Alterar Diret?rio
setwd("/Users/wilbe/OneDrive/Documentos/GoogleDrive/Estudos/FIA/Estatística Aplicada/Aula 05 a 07")
getwd()

#Leitura da Base de Dados
dados_lim_cred=read.table("Limite_Credito.txt", header = TRUE, sep = "\t", dec = ".")

#Verificar vari?veis
names(dados_lim_cred)

# Matriz de Gr?fico de Dispers?o
#Matriz de Scatter Plot
library(GGally)
ggpairs(dados_lim_cred, title="correlogram with ggpairs()") 

#Regress?o Linear M?ltipla
#Modelo de Regress?o Linear M?ltipla
options(scipen=999)
regressao <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Idade+Salario+LimitedeCreditoImediato)
summary(regressao)

#Modelo anterior sem Idade
regressao_1 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato)
summary(regressao_1)


#Modelo anterior Teste
regressao_2 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ RendimentoTotal+Salario+LimitedeCreditoImediato)
summary(regressao_2)

#Modelo anterior Teste
regressao_3 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ RendimentoTotal+LimitedeCreditoImediato)
summary(regressao_3)

#Leitura da Base de Dados
dados_lim_cred_esc=read.table("Limite_Credito_Escolaridade.txt", header = TRUE, sep = "\t", dec = ".")
names(dados_lim_cred_esc)

# Matriz de Gr?fico de Dispers?o
#Matriz de Scatter Plot
library(GGally)
ggpairs(dados_lim_cred_esc, title="correlogram with ggpairs()") 

regressao_2 <- lm(data=dados_lim_cred_esc, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato+Escolaridade)
summary(regressao_2)

regressao_3 <- lm(data=dados_lim_cred_esc, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato+Escolaridade)
summary(regressao_3)
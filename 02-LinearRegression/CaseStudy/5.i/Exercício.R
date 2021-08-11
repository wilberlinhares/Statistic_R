#**********************************************************
#Mapear diret?rio de trabalho
getwd()
#Aten??o: Alterar Diret?rio
setwd("/home/wilber/Documentos/OneDrive/Estudos/FIA/Estatística Aplicada/Aula 08 - Exercícios/5.i")
#Leitura da Base de Dados
dados_lim_cred=read.table("Limite_Credito_Escolaridade.txt", header = TRUE, sep = "\t", dec = ".")

#Verificar vari?veis
names(dados_lim_cred)
options(acipen=999) ## Retira a nota??o cient?fica

# Matriz de Gr?fico de Dispers?o
#Matriz de Scatter Plot
library(GGally)
ggpairs(dados_lim_cred, title="Exerc?cio 5i") 

#Regress?o Linear M?ltipla
#Modelo de Regress?o Linear M?ltipla
regressao_1 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Idade+Salario+RendimentoTotal+LimitedeCreditoImediato+Escolaridade)
summary(regressao_1)

regressao_2 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Salario+RendimentoTotal+LimitedeCreditoImediato+Escolaridade)
summary(regressao_2)

regressao_3 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato+Escolaridade)
summary(regressao_3)

regressao_4 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Salario+Escolaridade)
summary(regressao_4)

regressao_5 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato)
summary(regressao_5)



#Leitura da Base de Dados
dados_lim_cred_esc=read.table("Limite_Credito_Escolaridade.txt", header = TRUE, sep = "\t", dec = ".")
names(dados_lim_cred_esc)


# Matriz de Gr?fico de Dispers?o
#Matriz de Scatter Plot
library(GGally)
ggpairs(dados_lim_cred_esc, title="correlogram with ggpairs()") 

regressao_2 <- lm(data=dados_lim_cred_esc, LimitedoChequeEspecial ~ Idade+Salario+LimitedeCreditoImediato+Escolaridade)
summary(regressao_2)

regressao_3 <- lm(data=dados_lim_cred_esc, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato+Escolaridade)
summary(regressao_3)

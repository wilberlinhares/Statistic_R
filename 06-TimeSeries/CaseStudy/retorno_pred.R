#Apontar diretório de dados
setwd("/home/wilber/Documentos/Pessoais/Estudos/FIA/Estatística Aplicada/Aula 21 - Exercícios")
getwd()

#Leitura da base de dados
retorno<- read.table("retorno_pred.txt", header = FALSE )

#Análise Exploratória da Série
summary(retorno$V1)

#Gráfico temporal
ts.plot(retorno$V1)

#Teste de Estacionariedade
library(tseries)
adf.test(retorno$V1)

#Gráfico de Autocorrelação e Autocorrelação Parcial da série
par(mfrow=c(1,2))
print(acf(retorno$V1))
print(pacf(retorno$V1))

#Ajuste do modelo: 'order = c(X,0,0)' em que X é a ordem do AR e cada NA indica os parâmetros a serem estimados 'fixed = c(NA,NA,NA)'
(modelo <- arima(retorno$V1, order = c(5,0,0), 
                 fixed = c(NA,NA,NA,0,NA,NA)))

#Teste de hipótese dos parâmetros
library(lmtest)
coeftest(modelo)
par(mfrow=c(1,2))

#Sem intercepto
#Ajuste do modelo: 'order = c(X,0,0)' em que X é a ordem do AR e cada NA indica os parâmetros a serem estimados 'fixed = c(NA,NA,NA)'
(modelo <- arima(retorno$V1, order = c(5,0,0), 
                 fixed = c(NA,NA,NA,0,NA,0)))
coeftest(modelo)
par(mfrow=c(1,2))
#Gráfico de Autocorrelação e Autocorrelação Parcial dos resíduos
acf(residuals(modelo))
pacf(residuals(modelo))

#Projeção N passos para frente
library(forecast)
(projecao <- forecast(modelo, h=6))

#Dados observados da próxima semana
#0.103906254
#0.188157433
#-0.080349801
#0.11583838
#0.121133841

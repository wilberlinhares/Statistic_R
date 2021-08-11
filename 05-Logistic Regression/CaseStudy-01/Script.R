#**********************************************************
#REGRESSÃO LOGÍSTICA - CREDIT SCORE
#**********************************************************
#**********************************************************

#**********************************************************
#Mapear diretório de trabalho
getwd()
#Atenção: Alterar Diretório
setwd("C:\\Users\\wilbe\\OneDrive\\Estudos\\FIA\\Estatística Aplicada\\Aula 16 - Exercícios")
#**********************************************************

#**********************************************************
#Leitura da base de dados
credit = read.table("CreditScore_r.txt", header = TRUE, sep = "\t", dec = ".")
names(credit)
str(credit)
nrow(credit)


#(a) Faça a análise exploratória univariada e interprete todas as variáveis do banco de dados. Interprete os resultados na visão do negócio.
#Frequência de todas as variáveis, exceto a variável chave
summary(credit[,-1])

#(b)Faça uma análise do % de default. 
#Percentual da variável resposta
round(prop.table(table(credit$RESPOSTA)),4)
#**********************************************************
#(c) Faça a análise bivariada das variáveis explicativas (covariáveis) vs a variável resposta. Quais variáveis discriminam o evento resposta? Como você poderia tratar as categorias com missings values na análise bivariada?
#Vamos fazer a tabela cruzada entre as covariáveis e a resposta
table(credit$FX_IDADE,credit$RESPOSTA)
table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA)
table(credit$FX_RENDA,credit$RESPOSTA)
table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA)
table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA)

#Podemos gerar também as proporções, sumarizando na categoria de cada covariável
round(prop.table(table(credit$FX_IDADE,credit$RESPOSTA),1),2)
round(prop.table(table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$FX_RENDA,credit$RESPOSTA),1),2)
round(prop.table(table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA),1),2)
#**********************************************************
#(d)Rode o modelo de Regressão Logística. 
#Selecione um modelo final no qual a interpretação dos parâmetros 
#esteja de acordo com a análise bivariada.
modelo_full <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA +  
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_full)
#**********************************************************
#(e) Faça a análise de multicolinearidade entre as covariáveis. 
#Reajuste o modelo caso seja necessário, garantindo que as estimativas dos 
#parâmetros fiquem condizentes com a análise exploratória bivariada.
library(lsr)#biblioteca para o cálculo da estatística de Cramers'V
cramersV(table(credit$FX_IDADE,credit$CEP_GRUPO_RISCO))
cramersV(table(credit$FX_IDADE,credit$FX_RENDA))
cramersV(table(credit$FX_IDADE,credit$INDICADOR_RESTRITIVO))
cramersV(table(credit$FX_IDADE,credit$QTDE_CONSULTAS_CREDITO))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$FX_RENDA))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$INDICADOR_RESTRITIVO))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$QTDE_CONSULTAS_CREDITO))
cramersV(table(credit$FX_RENDA,credit$INDICADOR_RESTRITIVO))
cramersV(table(credit$FX_RENDA,credit$QTDE_CONSULTAS_CREDITO))
cramersV(table(credit$INDICADOR_RESTRITIVO,credit$QTDE_CONSULTAS_CREDITO))


#Modelo Completo
modelo_red1 <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA +  
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_red1)
credit$p1 <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$p1)
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em variável binária
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)

library(tidyverse)
credit %>%
  count(FX_IDADE,
        CEP_GRUPO_RISCO,
        INDICADOR_RESTRITIVO,
        QTDE_CONSULTAS_CREDITO,
        FX_RENDA,
        p1,
        resp_bin1) %>%
  write.table("clipboard-123", sep = "\t", dec = ",", row.names = FALSE)

#Modelo sem RENDA
modelo_red1 <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO +  
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_red1)
credit$p1 <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$p1)
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em variável binária
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)

#modelo Com RENDA e sem IDADE
modelo_red1 <- glm(RESPOSTA ~ CEP_GRUPO_RISCO + FX_RENDA +  
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_red1)
credit$p1 <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$p1)
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em variável binária
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)

#Modelo sem o CEP
modelo_red1 <- glm(RESPOSTA ~ FX_IDADE + FX_RENDA +  
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_red1)
credit$p1 <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$p1)
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em variável binária
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)

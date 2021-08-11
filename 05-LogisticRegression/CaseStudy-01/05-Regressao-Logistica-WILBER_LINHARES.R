#**********************************************************
#REGRESS?O LOG?STICA - CREDIT SCORE
#**********************************************************
#**********************************************************

#**********************************************************
#Mapear diret?rio de trabalho
getwd()
#Aten??o: Alterar Diret?rio
setwd("/home/wilber/Documentos/OneDrive/Estudos/FIA/Estatística Aplicada/Aula 16 - Exercícios")
#**********************************************************

#**********************************************************
#Leitura da base de dados
credit = read.table("CreditScore_r.txt", header = TRUE, sep = "\t", dec = ".")
names(credit)
str(credit)
nrow(credit)


#(a) Fa?a a an?lise explorat?ria univariada e interprete todas as vari?veis do banco de dados. Interprete os resultados na vis?o do neg?cio.
#Frequ?ncia de todas as vari?veis, exceto a vari?vel chave
summary(credit[,-1])

#(b)Fa?a uma an?lise do % de default. 
#Percentual da vari?vel resposta
round(prop.table(table(credit$RESPOSTA)),4)
#**********************************************************
#(c) Fa?a a an?lise bivariada das vari?veis explicativas (covari?veis) vs a vari?vel resposta. Quais vari?veis discriminam o evento resposta? Como voc? poderia tratar as categorias com missings values na an?lise bivariada?
#Vamos fazer a tabela cruzada entre as covari?veis e a resposta
table(credit$FX_IDADE,credit$RESPOSTA)
table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA)
table(credit$FX_RENDA,credit$RESPOSTA)
table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA)
table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA)

#Podemos gerar tamb?m as propor??es, sumarizando na categoria de cada covari?vel
round(prop.table(table(credit$FX_IDADE,credit$RESPOSTA),1),2)
round(prop.table(table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$FX_RENDA,credit$RESPOSTA),1),2)
round(prop.table(table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA),1),2)
#**********************************************************
#(d)Rode o modelo de Regress?o Log?stica. 
#Selecione um modelo final no qual a interpreta??o dos par?metros 
#esteja de acordo com a an?lise bivariada.
modelo_full <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA +  
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_full)
#**********************************************************
#(e) Fa?a a an?lise de multicolinearidade entre as covari?veis. 
#Reajuste o modelo caso seja necess?rio, garantindo que as estimativas dos 
#par?metros fiquem condizentes com a an?lise explorat?ria bivariada.
library(lsr)#biblioteca para o c?lculo da estat?stica de Cramers'V
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
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em vari?vel bin?ria
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
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em vari?vel bin?ria
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
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em vari?vel bin?ria
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



















#Modelo sem o CEP
modelo_red1 <- glm(RESPOSTA ~ FX_IDADE + FX_RENDA +  
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_red1)
credit$p1 <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$p1)
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em vari?vel bin?ria
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)





























#(g)Analise a sensibilidade, especificidade e acur?cia pela tabela de 
#classifica??o. 

#Para o modelo log?stico, com a fun??o 'predict', tendo como par?metro type = 'response' conseguimos obter as probabilidades do modelo para a classifica??o '1'
credit$p1 <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$p1)
#Cria vari?vel resposta predita com base na probabilidade predita pela ?rvore de Decis?o
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em vari?vel bin?ria

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur?cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)

#Representatividade
library(tidyverse)
credit %>%
  count(FX_IDADE,
        CEP_GRUPO_RISCO,
        INDICADOR_RESTRITIVO,
        QTDE_CONSULTAS_CREDITO,
        p1,
        resp_bin1) %>%
  write.table("clipboard-123", sep = "\t", dec = ",", row.names = FALSE)
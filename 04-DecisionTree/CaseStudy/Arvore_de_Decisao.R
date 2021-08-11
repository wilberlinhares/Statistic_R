#**********************************************************
#?RVORE DE DECIS?O - TELEFONIA FIXA
#**********************************************************

#Mapear diret?rio de trabalho
getwd()
#Aten??o: Alterar Diret?rio
setwd("/home/wilber/Documentos/OneDrive/Estudos/FIA/Estatística Aplicada/Aula 13 - Exercícios")

#(a) Utilize o banco de dados 'base_telefonia_reduzida.sav'.
telefonia <- read.table("Telefonia_AD.txt", header = TRUE, sep = "\t", dec = ".")

#(b) Fa?a a an?lise explorat?ria  univariada das vari?veis 'Idade' at? 'Resposta'.
#Use as medidas resumo b?sicas e tabela de freq??ncias.
#Medidas resumo principais de todas as vari?vei, exceto a primeira, que ? o c?digo do Cliente
summary(telefonia[,-1]) #Medidas resumo das vari?vel, menos a primeira coluna
#Tabela de frequ?ncia para a vari?vel 'resposta'
(resposta_a<-table(telefonia$resposta))
(resposta_p<-prop.table(resposta_a)*100) #0,8% de cancelamento volunt?rio

#Tratamento da vari?vel 'Minutos realizados'
telefonia$Minutos_realizados_T0 <- ifelse(is.na(telefonia$Minutos_realizados_T0),0,telefonia$Minutos_realizados_T0)
summary(telefonia$Minutos_realizados_T0)

#Para rodar a ?rvore pelo m?todo CHAID, a vari?vel resposta deve estar no formato fator (vari?vel qualitativa)
telefonia$resposta <- as.factor(telefonia$resposta)

#Como a vari?vel explicativa tamb?m deve ser categ?rica, vamos segmentar a Idade em quartil
#library(gtools) #pacote com a fun??o 'quantcut'
telefonia$Minutos_realizados_T0_q <- quantcut(telefonia$Minutos_realizados_T0,4)
telefonia$Tempo_casa_q <- quantcut(telefonia$Tempo_casa,4)
telefonia$Qtd_retencao_6meses_q <- quantcut(telefonia$Qtd_retencao_6meses,4)
telefonia$Qtd_prod_q <- quantcut(telefonia$Qtd_prod,4)

#Tabela Bidimensional: covari?vel x resposta
Minutos_table<-table(telefonia$Minutos_realizados_T0_q,telefonia$resposta)
Tempo_table<-table(telefonia$Tempo_casa_q,telefonia$resposta)
Qtd_retencao_table<-table(telefonia$Qtd_retencao_6meses_q,telefonia$resposta)
Qtd_prod_table<-table(telefonia$Qtd_prod_q,telefonia$resposta)
#Multiplicando por 100 para virar porcentagem e arredondamento para 2 casas decimais
round(prop.table(Minutos_table,1)*100,2) #par?metro 1 dentro de prop.table indica que ? a propor??o da linha
round(prop.table(Tempo_table,1)*100,2)
round(prop.table(Qtd_retencao_table,1)*100,2)
round(prop.table(Qtd_prod_table,1)*100,2)

#Teste Qui-quadrado
chisq.test(Minutos_table)
chisq.test(Tempo_table)
chisq.test(Qtd_retencao_table)
chisq.test(Qtd_prod_table)
#install.packages("CHAID", repos="http://R-Forge.R-project.org")
#Fun??o 'chaid' nos permite criar uma ?rvore de decis?o de acordo com o algoritmo CHAID
library(partykit)#pacote precisa ser instalado previamente para usar o CHAID
#install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID) #pacote com a fun??o 'chaid'

#Para a ?rvore n?o ficar muito grande, a dois n?veis
controle <- chaid_control(maxheight = 3) #A tarefa de casa dever? ser com 3 n?veis

#Fun??o 'chaid' nos permite criar uma ?rvore de decis?o de acordo com o algoritmo CHAID
(arvore <- chaid(resposta ~ 
                   Minutos_realizados_T0_q +
                   Tempo_casa_q +
                   Qtd_retencao_6meses_q +
                   Qtd_prod_q, data = telefonia, control=controle)) #indicando em qual base o modelo deve ser estimado
plot(arvore,gp = gpar(col = "black",cex=0.6),type='simple') #darkturquoise

#Vamos avaliar a propens?o dos n?s finais
telefonia$node <- predict(arvore, type = "node") #atribui o n?mero do n? para uma vari?vel nova a cada indiv?duo
#Frequ?ncia Absoluta dos n?s
table(telefonia$node)
#Percentual de representatividade de cada n?
round(prop.table(table(telefonia$node)),3)*100
#Cruza os n?s finais com a vari?vel resposta
pred_resp<-table(telefonia$node,telefonia$resposta) #cruza os n?s com a resposta
#Percetual de evento n?o resposta (0) e evento resposta (1)
round(prop.table(pred_resp,1)*100,2) #faz a tabela cruzada com 2 casa decimais

#Incluir na base de dados a probabilidade predita pela ?rvore de Decis?o
probs<- as.data.frame(predict(arvore, newdata = telefonia, type = "p")) #"p" salva o valor predito da probabilidade
summary(probs) #mostra o que ele criou no comando acima
names(probs) <- c("P_0","P_1")
telefonia <- cbind(telefonia,probs)#insere 2 colunas na base com as probabilidade preditas de 0 e 1
#Cria vari?vel resposta predita com base na probabilidade predita pela ?rvore de Decis?o
telefonia$predict <- as.factor(ifelse(telefonia$P_1 >= 0.008685467,1,0)) #transforma a probabilidade em vari?vel bin?ria

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(telefonia$resposta,telefonia$predict))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur?cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(telefonia))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)

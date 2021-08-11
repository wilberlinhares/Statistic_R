#**********************************************************
#ÁRVORE DE DECISÃO - TELEFONIA FIXA
#**********************************************************

#Atenção: Alterar Diretório
setwd("/Users/wilbe/OneDrive/Documentos/GoogleDrive/Estudos/FIA/Estat�stica Aplicada/Aula 12")

#Mapear diretório de trabalho
getwd()

#(a) Utilize o banco de dados 'base_telefonia_reduzida.sav'.
telefonia <- read.table("Telefonia_AD.txt", header = TRUE, sep = "\t", dec = ".")

#(b) Faça a análise exploratória  univariada das variáveis 'Idade' até 'Resposta'.
#Use as medidas resumo básicas e tabela de freqüências.
#Medidas resumo principais de todas as variávei, exceto a primeira, que é o código do Cliente
summary(telefonia[,-1]) #Medidas resumo das variável, menos a primeira coluna
#Tabela de frequência para a variável 'resposta'
(resposta_a<-table(telefonia$resposta))
(resposta_p<-prop.table(resposta_a)*100) #0,8% de cancelamento voluntário

#Tratamento da variável 'Minutos realizados'
telefonia$Minutos_realizados_T0 <- ifelse(is.na(telefonia$Minutos_realizados_T0),0,telefonia$Minutos_realizados_T0)

#Para rodar a árvore pelo método CHAID, a variável resposta deve estar no formato fator (variável qualitativa)
telefonia$resposta <- as.factor(telefonia$resposta)

#Como a variável explicativa também deve ser categórica, vamos segmentar a Idade em quartil
library(gtools) #pacote com a função 'quantcut'
telefonia$Minutos_realizados_T0_q <- quantcut(telefonia$Minutos_realizados_T0,4)
telefonia$Tempo_casa_q <- quantcut(telefonia$Tempo_casa,4)
telefonia$Qtd_retencao_6meses_q <- quantcut(telefonia$Qtd_retencao_6meses,4)
telefonia$Qtd_prod_q <- quantcut(telefonia$Qtd_prod,4)

#Tabela Bidimensional: covariável x resposta
Minutos_table<-table(telefonia$Minutos_realizados_T0_q,telefonia$resposta)
Tempo_table<-table(telefonia$Tempo_casa_q,telefonia$resposta)
Qtd_retencao_table<-table(telefonia$Qtd_retencao_6meses_q,telefonia$resposta)
Qtd_prod_table<-table(telefonia$Qtd_prod_q,telefonia$resposta)
#Multiplicando por 100 para virar porcentagem e arredondamento para 2 casas decimais
round(prop.table(Minutos_table,1)*100,2) #parâmetro 1 dentro de prop.table indica que é a proporção da linha
round(prop.table(Tempo_table,1)*100,2)
round(prop.table(Qtd_retencao_table,1)*100,2)
round(prop.table(Qtd_prod_table,1)*100,2)

#Teste Qui-quadrado
chisq.test(Minutos_table)
chisq.test(Tempo_table)
chisq.test(Qtd_retencao_table)
chisq.test(Qtd_prod_table)

#Função 'chaid' nos permite criar uma árvore de decisão de acordo com o algoritmo CHAID
library(partykit)#pacote precisa ser instalado previamente para usar o CHAID
#install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID) #pacote com a função 'chaid'
#Para a árvore não ficar muito grande, a dois níveis
controle <- chaid_control(maxheight = 2)

#Função 'chaid' nos permite criar uma árvore de decisão de acordo com o algoritmo CHAID
(arvore <- chaid(resposta ~ 
                   Minutos_realizados_T0_q +
                   Tempo_casa_q +
                   Qtd_retencao_6meses_q +
                   Qtd_prod_q, data = telefonia, control=controle)) #indicando em qual base o modelo deve ser estimado
plot(arvore,gp = gpar(col = "darkturquoise",cex=0.6),type='simple')

#Vamos avaliar a propensão dos nós finais
telefonia$node <- predict(arvore, type = "node") #atribui o número do nó para uma variável nova a cada indivíduo
#Frequência Absoluta dos nós
table(telefonia$node)
#Percentual de representatividade de cada nó
round(prop.table(table(telefonia$node)),3)*100
#Cruza os nós finais com a variável resposta
pred_resp<-table(telefonia$node,telefonia$resposta) #cruza os nós com a resposta
#Percetual de evento não resposta (0) e evento resposta (1)
round(prop.table(pred_resp,1)*100,2) #faz a tabela cruzada com 2 casa decimais

#Incluir na base de dados a probabilidade predita pela Árvore de Decisão
probs<- as.data.frame(predict(arvore, newdata = telefonia, type = "p")) #"p" salva o valor predito da probabilidade
summary(probs) #mostra o que ele criou no comando acima
names(probs) <- c("P_0","P_1")
telefonia <- cbind(telefonia,probs)#insere 2 colunas na base com as probabilidade preditas de 0 e 1
#Cria variável resposta predita com base na probabilidade predita pela Árvore de Decisão
telefonia$predict <- as.factor(ifelse(telefonia$P_1 >= 0.008685467,1,0)) #transforma a probabilidade em variável binária

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(telefonia$resposta,telefonia$predict))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(telefonia))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
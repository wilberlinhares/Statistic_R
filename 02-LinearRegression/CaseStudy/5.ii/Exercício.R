#Mapear diret?rio de trabalho
getwd()
#Aten??o: Alterar Diret?rio
setwd("/home/wilber/Documentos/OneDrive/Estudos/FIA/Estatística Aplicada/Aula 08 - Exercícios/5.ii")
#**********************************************************

#**********************************************************
#Leitura da Base de Dados
imobiliario <- read.table("Imobiliario.txt", header = TRUE, sep = "\t", dec = ".")
#Verificar vari?veis
names(imobiliario)

#An?lise Explorat?ria
summary(imobiliario)

#Gr?fico de dispers?o
plot(imobiliario$Distancia_metro_Km, imobiliario$Mil_reais_m2)
plot(imobiliario$Idade_imovel, imobiliario$Mil_reais_m2)
plot(imobiliario$Comercios_proximos, imobiliario$Mil_reais_m2)


#plot(imobiliario$ Distancia_metro_Km, imobiliario$Mil_reais_m2, ylab="Pre?o (Mil R$/m2)", xlab="Dist?ncia (km)", col='darkturquoise', xlim = c(0,3), ylim = c(0,20))
#abline(lm(data=imobiliario, Mil_reais_m2 ~ Distancia_metro_Km), col='blue')

#Correla??o Linear de Pearson
cor(imobiliario$ Distancia_metro_Km, imobiliario$Mil_reais_m2)
cor(imobiliario$ Idade_imovel, imobiliario$Mil_reais_m2)
cor(imobiliario$ Comercios_proximos, imobiliario$Mil_reais_m2)
summary(imobiliario)

#Regress?o Linear Simples
regressao <- lm(data=imobiliario, Mil_reais_m2 ~ Distancia_metro_Km)
summary(regressao)

regressao <- lm(data=imobiliario, Mil_reais_m2 ~ Idade_imovel)
summary(regressao)

regressao <- lm(data=imobiliario, Mil_reais_m2 ~ Comercios_proximos)
summary(regressao)

library(GGally)
ggpairs(imobiliario, title="correlogram with ggpairs()") 

######

regressao_1 <- lm(data=imobiliario, Mil_reais_m2 ~ Idade_imovel+Distancia_metro_Km+Comercios_proximos)
summary(regressao_1)

options(scipen=999)
regressao <- lm(data=imobiliario, LimitedoChequeEspecial ~ Idade+Salario+LimitedeCreditoImediato)
summary(regressao)

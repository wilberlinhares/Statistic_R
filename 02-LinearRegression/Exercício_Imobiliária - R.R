options(scipen=999)
setwd("/Users/wilbe/OneDrive/Documentos/GoogleDrive/Estudos/FIA/Estatística Aplicada/Aula 05 a 07")
getwd()
dados_rls=read.table("Imobiliario.txt", header = TRUE, sep = "\t", dec = ".")
#Modelo de Regress?o Linear Simples
imobiliario <- read.table("Imobiliario.txt", header = TRUE, sep = "\t", dec = ".")
#Verificar vari?veis
names(imobiliario)

#An?lise Explorat?ria
summary(imobiliario)


#calculando o valor do im?vel
y <- 18.8154+(-7.2166*2.6)
(y*1000)*(70)

#Gr?fico de dispers?o (Coeficiente de Correla??o de Pearson)
plot(imobiliario$Distancia_metro_Km, imobiliario$Mil_reais_m2)
#plot(imobiliario$ Distancia_metro_Km, imobiliario$Mil_reais_m2, ylab="Pre?o (Mil R$/m2)", xlab="Dist?ncia (km)", col='darkturquoise', xlim = c(0,3), ylim = c(0,20))
#abline(lm(data=imobiliario, Mil_reais_m2 ~ Distancia_metro_Km), col='blue')

#Correla??o Linear de Pearson
cor(imobiliario$ Distancia_metro_Km, imobiliario$Mil_reais_m2)

#Regress?o Linear Simples
regressao <- lm(data=imobiliario, Mil_reais_m2 ~ Distancia_metro_Km)
summary(regressao)
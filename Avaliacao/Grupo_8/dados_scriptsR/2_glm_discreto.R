#GLM PARA DADOS DISCRETOS

setwd("D:/Documentos/Doutorado/Disciplinas/GLMM/ECL0072-GLM_GAM-gh-pages")

library(ggplot2)
library(dplyr)
library(GGally) #pairsplot
library(MASS) #Para rodar modelo Binomial Negativo
library(corrplot) #multicolinearidade
library(DHARMa)
library(ggeffects) #para plotar os resultados
library(performance) #avaliaC'C#o dos residuos

benthos <- readRDS("Dados/benthos_exposure.rds")

str(benthos)
View(benthos)

#visualizando os dados

## DistribuiC'C#o dos dados (variC!vel y)
hist(benthos$Richness) #o dado parece meio zero inflacionado
boxplot(benthos$Richness)
summary(benthos$Richness)
dotchart(benthos$Richness) #aqui tambem parece

benthos[, c("Exposure", "Beach", "Sample")] <- lapply(benthos[, c("Exposure", "Beach", "Sample")], factor)

## exposicao a estresse
plot(benthos$Richness ~ benthos$Exposure) #menor riqueza quanto maior a exposicao

## altura da mare
plot(benthos$Richness ~benthos$NAP) #menor riqueza nas alturas maiores

##Praia
plot(benthos$Richness ~ benthos$Beach) #praias 1 e 2 parecem diferentes das outras

## Testando modelos
# Modelo 1: Vendo o efeito aditivo da exposi????o com a altura da mar?? sobre a riqueza de esp??cies
mod_benthos1 <- glm(Richness ~ Exposure + NAP,
                    data = benthos,
                    family = "poisson")

# Modelo 2: Adicionando o efeito da praia no modelo anterior
mod_benthos2 <- glm(Richness ~ Exposure + NAP + Beach,
                    data = benthos,
                    family = "poisson")


# Comparando os tr??s modelos com AIC
AIC(mod_benthos1, mod_benthos2)


summary(mod_benthos1) #resultados do modelo 1 que foi o mais ajustado
summary(mod_benthos2) #rodando esse aqui porque no grafico parece ser significativo a praia. fiquei confusa que apareceu NA em beach2 e 7
summary(mod_benthos3) #o modelo 3 foi o mais ajustado, mas ficou confuso de interpretar, acho que vou tirar esse 


#analisar o ajuste do modelo 1, que foi o melhor
par(mfrow=c(2,2))
plot(mod_benthos1) #sinceramente nao achei muito top
check_model(mod_benthos1)

par(mfrow=c(1,1)) #nao sei pra que serve essa linha

check_overdispersion(mod_benthos1) #tem sobredispersao. Sera que resolve usando binomial negativa?

# binomial negativa
mod_benthos3 <- glm.nb(Richness ~ Exposure + NAP,
                data = benthos)

AIC(mod_benthos1, mod_benthos2, mod_benthos3) #Comparando 

summary(mod_benthos3)

par(mfrow=c(2,2))
plot(mod_benthos3) #segue uma bomba
par(mfrow=c(1,1)) 

check_overdispersion(mod_benthos3) #Sem sobredispersao


simres_benthos3 <- simulateResiduals(mod_benthos3, n=1000)
plot(simres_benthos3) #ruim


check_model(mod_benthos3) #sinceramente nao sei

check_zeroinflation(mod_benthos3) #top


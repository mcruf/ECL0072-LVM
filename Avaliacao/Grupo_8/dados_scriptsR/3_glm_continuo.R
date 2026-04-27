#GLM PARA DADOS CONTC
NUOS

library(glmmTMB) #Para rodar modelo log-Normal
library(ggplot2)
library(MASS) #seleC'C#o automC!tica de modelos
library(dplyr)
library(DHARMa)
library(ggeffects) #para plotar os resultados
library(performance) #avaliaC'C#o dos residuos

setwd("D:/Documentos/Doutorado/Disciplinas/GLMM/ECL0072-GLM_GAM-gh-pages")

#abrir meu banco de dados
fago <- read.csv("Relatorio/dados_scriptsR/fagocitose.csv", header = T, sep =";", dec = ".")

str(fago)
View(fago)

fago[, c("origin", "ID", "health")] <- lapply(fago[, c("origin", "ID", "health")], factor)

hist(fago$phagocytosis)
dotchart(fago$phagocytosis)

summary(fago$phagocytosis)

## Dinamica espacial
plot(fago$phagocytosis ~ fago$origin)

ggplot(fago, aes(x = origin, y = phagocytosis)) +
  #ggplot(dados, aes(x = Mes, y = log1p(BPUE))) +
  geom_boxplot(size = 0.7, fill = 'cyan4',col = 'cyan4', alpha = 0.15) +
  stat_summary(fun = mean, geom="point", shape=19, size=1.5, color="cyan4") +
  theme_minimal()

## por estado de saude
plot(fago$phagocytosis ~ fago$health)

ggplot(fago, aes(x = health, y = phagocytosis, fill= health)) +
  #ggplot(dados, aes(x = Mes, y = log1p(BPUE))) +
  geom_boxplot(size = 0.7, alpha = 0.30) +
  stat_summary(fun = mean, geom="point", shape=19, size=1.5) +
  theme_minimal()

#fazendo o modelo
## Modelo Gama
mod_fago_gama <- glmmTMB(phagocytosis ~ origin + health,
                    family = Gamma(link = "log"), 
                    data = fago)
## Modelo lognormal
mod_fago_log <- glmmTMB(phagocytosis ~ origin + health,
                         family = lognormal(link = "log"), 
                         data = fago)

## AIC
AIC(mod_fago_gama, mod_fago_log)

res_gama <- simulateResiduals(mod_fago_gama, n=1000)
res_log <- simulateResiduals(mod_fago_log, n=1000)

plot(res_gama) #ficou top
plot(res_log) #ficou meio esquisito

check_model(mod_fago_gama) #esse aqui talvez tenha ficado um pouquinho melhor e vou escolher ele pelo AIC
check_model(mod_fago_log)

summary(mod_fago_gama) #o intercepto foi shallow e bleached
#foi diferente tide de shallow (tide teve mais)
#foi diferente healthy de bleached (healthy teve mais), mas pink foi igual a bleached 

ggpredict(mod_fago_gama) %>% plot() #pra que serve, eu nao sei

#OBS.: TEM UMA PARTE ADICIONAL NO SCRIPT PRA COMPARAR AS PREDICCES DOS MODELOS, MAS NAO SEI SE PRECISO FAZER
#OBS1.: NESSE SCRIPT EU USEI A FUNCAO GLMMMTMB QUE SUPORTA MEDIDAS, REPETIDAS, ENTAO DA PRA FAZER COM OS DADOS DOS EXPERIMENTOS SE SOBRAR TEMPO
#OBS.: EU TENHO DADO DE PAM DESSES CORAIS TAMBC	M, DA PRA USAR 
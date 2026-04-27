#####################################################
#                                                   #
#                   Aula prática 07                 #
#   - Distribuição Beta para dados de proporção -   #
#                                                   #
#####################################################



#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~

library(glmmTMB) #Para rodar modelo com distribuição beta
#library(betareg) #Pacote alternativo para rodar um modelo com dist. beta
#library(mgcv) #Pacote alternativo para rodar um modelo com dist. beta

library(ggplot2) #Visualização
library(patchwork) #Para visualização de gráficos
library(GGally) #Complemento ao ggplot2
library(ggpubr) #Visualização
library(dplyr) #Manipulação dos dados
library(DHARMa) #Avaliação de modelos
library(performance) #Avaliação de modelos

#install.packages("glmmTMB")
#install.packages("betareg")
#install.packages("mgcv")
#install.packages("patchwork")
#install.packages("GGally")
#install.packages("ggpubr)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo 'tema' geral para ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
theme_set(theme_minimal(base_size = 15))



#><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~

dados <- readRDS("Dados/mammal_sleep.rds")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

str(dados)

## Transformando variáveis para fator (quando necessário)
dados[, c("predation", "exposure", "danger")] <- lapply(dados[, c("predation", "exposure", "danger")], factor) 

head(dados)

## Calculando a proporção de tempo sonhando/dormindo
dados$prop_sonho <- dados$dream / dados$sleep


## Retirando NAs
idx <- which(is.na(dados$prop_sonho))
dados <- dados[-idx, ]

head(dados)



## Distribuição da variável resposta
p1 <- ggplot(dados, aes(x = prop_sonho)) +
      geom_histogram(size = 0.7, fill = 'darkorange',col = 'darkorange', alpha = 0.15,
                     bins = 10) 


p2 <- ggplot(dados, aes(y = prop_sonho)) +
      geom_boxplot(size = 0.7, fill = 'darkorange',col = 'darkorange', alpha = 0.15) 

(p1 + p2)


## Relação com outras covariáveis
tmp <- dados[, c("prop_sonho", "body", "brain", "lifespan", "gestation", "predation")]
  
ggpairs(tmp)



## Avaliando as variáveis 'body' e 'brain' 
body <- ggplot(tmp, aes(x = body, y=prop_sonho)) +
        geom_point(size=3) +
        geom_smooth(method = "lm", se = F, color = "blue") +# se=FALSE hides the confidence interval
        stat_cor(method = "pearson") 

body_log <- ggplot(tmp, aes(x = log(body), y=prop_sonho)) +
            geom_point(size=3) +
            geom_smooth(method = "lm", se = F, color = "blue") +# se=FALSE hides the confidence interval
            stat_cor(method = "pearson") 


brain <- ggplot(tmp, aes(x = brain, y=prop_sonho)) +
          geom_point(size=3) +
          geom_smooth(method = "lm", se = F, color = "blue") +# se=FALSE hides the confidence interval
          stat_cor(method = "pearson") 

brain_log <- ggplot(tmp, aes(x = log(brain), y=prop_sonho)) +
              geom_point(size=3) +
              geom_smooth(method = "lm", se = F, color = "blue") +# se=FALSE hides the confidence interval
              stat_cor(method = "pearson") 

(body + body_log)/(brain + brain_log)


## Qual animal com maior relação sonho/dormida
which.max(dados$body)
dados[2, ]


## Avaliando prop_sonho em relação ao risco de predação
ggplot(tmp, aes(x = predation, y = prop_sonho)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", shape=19, size=2, color="red") 




#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Modelando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
?glmmTMB


# Ajustando valores de prop_sonho (0 < y < 1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(tmp$prop_sonho)

tmp$prop_sonho2 <- (tmp$prop_sonho + (median(tmp$prop_sonho)*0.1))



## Modelo com variáveis naturais
mod1 <- glmmTMB(prop_sonho2 ~ brain + lifespan + gestation + predation,
                beta_family(link = "logit"),
                data = tmp)


### Usando pacote betareg
# mod1 <- betareg(prop_sonho ~ brain + lifespan + gestation + predation,
#                 link = "logit", #probit, cloglog, cauchit, log, loglog
#                 data = tmp)

### Usando pacote gam
#mod1b <- gam(prop_sonho ~ brain + lifespan + gestation + predation,
#             family=betar(link = "logit"), 
#             data = tmp)



## Modelo com variáveis transformada
mod2 <- glmmTMB(prop_sonho2 ~ log(brain) + lifespan + gestation + predation,
                beta_family(link = "logit"),
                data = tmp)



AIC(mod1, mod2)


## Diagnóstico visual 
simres1 <- simulateResiduals(mod1, n=1000)
simres2 <- simulateResiduals(mod2, n=1000)
plot(simres1)
plot(simres2)
check_model(simres1) #oops....
check_model(simres2) #oops....


# Embora o mod1 tenha resultado no menor AIC, o diagnóstico dos resíduos revelou
# que mod2 se ajustou melhor aos dados. 


### Interpretando os resultados
summary(mod2)




### Visualizando os resultados
library(ggeffects)


## Variável categórica
pred_predation <- ggpredict(mod2, terms = "predation")

ggplot(pred_predation, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Índice de predação", y = "Poporção (sonho/dormida)") +
  theme_minimal()


## Variável numérica
pred_lifespan <- ggpredict(mod2, terms = "lifespan")
ggplot(pred_lifespan, aes(x = x, y = predicted)) +
  geom_line(size = 1, color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Expecativa de vida", y = "Poporção (sonho/dormida)") +
  theme_minimal()

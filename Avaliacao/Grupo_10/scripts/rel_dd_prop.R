#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(glmmTMB) #Para rodar modelo log-Normal
library(ggplot2)
library(MASS) #seleção automática de modelos
library(dplyr)
library(DHARMa)
library(ggeffects) #para plotar os resultados
library(performance) #avaliação dos residuos
library(patchwork)


## Definindo diretório base
#setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



#~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("Dados/fisheries.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
str(dados)
View(dados)


# 2.1) Transformando as variáveis 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## character -> vector
dados[, c("Ano", "Mes", "Barco")] <- lapply(dados[, c("Ano", "Mes", "Barco")], factor)



# 2.2) Calculando a abundância de espécies não-alvo (bycatch)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Espécies alvo (atuns)
especies_alvo <- c("Thunnus_albacares",
                   "Thunnus_atlanticus",
                   "Katsuwonus_pelamis")

## Espécies não-alvo (todo o restante)
especies_nalvo <- setdiff(colnames(dados)[8:27], especies_alvo)
length(especies_nalvo) #No. de espécies que não são alvo da pesca


dotchart(dados$N_anzol) #Há pelo menos 2 outliers bem marcantes


### Tirando o outlier
dados2 <- filter(dados, N_anzol <= 1500) 


dotchart(dados2$N_anzol) 


## Calculando a abundância total de espécies não-alvo
### Precisamos também padronizar a abundância pelo esforço amostral (BPUE - bycatch per unit of effort)
dados2 <- dados2 %>%
  rowwise() %>%  # analisa linha por linha
  mutate(catch = sum(c_across(all_of(especies_alvo))),
         bycatch = sum(c_across(all_of(especies_nalvo))),
         t_catch = catch + bycatch,
         r_bycatch = bycatch/t_catch) %>%
  data.frame()

summary(dados2)


## Distribuição da variável resposta
p1 <- ggplot(dados2, aes(x = r_bycatch)) +
  geom_histogram(size = 0.7, fill = 'darkorange',col = 'darkorange', alpha = 0.15,
                 bins = 10) 


p2 <- ggplot(dados2, aes(y = r_bycatch)) +
  geom_boxplot(size = 0.7, fill = 'darkorange',col = 'darkorange', alpha = 0.15) 

(p1 + p2)



dados2 <- dados2 %>%
  rowwise() %>%  # analisa linha por linha
  mutate(r_bycatch_adj = case_when(
    r_bycatch == 1 ~ 0.99,
    r_bycatch < 1 ~ r_bycatch
  )) %>%
  data.frame()


modelo_bt <- glmmTMB(r_bycatch_adj ~ Mes + Lat + Long,
                     family = beta_family(link = "logit"), 
                     data = dados2)



### Avaliando o comportamento geral dos resíduos
check_model(modelo_bt2)

summary(modelo_bt)

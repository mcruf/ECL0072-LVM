######################################
#                                    #
#         Aula prática 06            #
#   - GLM para dados categóricos -   #
#                                    #
######################################

# O script abordará o modelo multinomial.
# Para tal, será utilizado os dados de pinguim de palmer como exemplo prático.

# A principal questão a ser avaliada é se a preferência alimentar muda ao longo
# do crescimento do pinguim, bem como se há distinção na preferência alimentar entre as diferentes espécies, sexo e estágios de desenvolvimento.
# Portanto, temos:
# y = tipo de dieta (4 categorias)
# X = massa corporal (e/ou espécie, sexo, estagio de desenvolvimento,....)


#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(VGAM) #Modelo multinomial
library(nnet) #Modelo multinomial (pacote alternativo)
library(ggplot2)
library(dplyr)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando funções auxiliares
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#source("Scripts/funcoes_auxiliares.R")


#><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("Dados/palmerpenguins_extended.rds")


# 1.1) Transformando as variáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# chr -> factor

cols <- c("species", "island", "sex", "diet", "life_stage", "health_metrics")
dados[, cols] <- lapply(dados[, cols], factor)



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~

## Qual a variação da saúde entre as espécies de pinguins?



## E por espécies?
ggplot(dados, aes(x = species, fill = health_metrics)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#d6ccc2", "cyan4", "darkorange", "gray50")) +
  labs(y = "Proporção", x = "") +
  theme_minimal(base_size = 15) 



## E entre sexos?
ggplot(dados, aes(x = sex, fill = health_metrics)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#d6ccc2", "cyan4", "darkorange", "gray50")) +
  labs(y = "Proporção", x = "") +
  facet_wrap(species ~ .) +
  theme_minimal(base_size = 15) 


## E estágios de vida?
ggplot(dados, aes(x = life_stage, fill = health_metrics)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#d6ccc2", "cyan4", "darkorange", "gray50")) +
  labs(y = "Proporção", x = "") +
  facet_wrap(species ~ .) +
  theme_minimal(base_size = 15)  



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Modelando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# CUIDADO: definir o nível de referência
levels(dados$health_metrics) 



# Modelo 1: health_metrics ~ life_stage
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

modelo1 <- vglm(health_metrics ~ life_stage, 
                family = multinomial(refLevel = 1),  # define a categoria de referência
                data = dados)

summary(modelo1)


## Visualizando os resultados ##

## Predições das probabilidades para cada espécie
dados_pred1 <- data.frame(life_stage = levels(dados$life_stage)) #Dados para predições; certifica-se dos níveis; primeiro nível tem que coincidir com o primeiro nível do modelo
preds1 <- predict(modelo1,
                  newdata = dados_pred1,
                  type = "response") %>% data.frame()



### Formatar os resultados para gerar o gráfico
preds1$life_stage <- dados_pred1$life_stage

colnames(preds1) <- c("Healthy", "Overweight", "Underweight", "life_stage") 



### Transformar dado para o formato longo (necessário para o ggplot2)
preds1l <- preds1 %>%
  tidyr::pivot_longer(cols = c("Healthy", "Overweight", "Underweight"),
                      names_to = "Health",
                      values_to = "Probability")



### Plotando.... 
ggplot(preds1l, aes(x = life_stage, y = Probability, fill = Health)) +
  geom_bar(stat = "identity", position = "stack",  alpha = 0.8) +
  scale_fill_manual(values = c("Healthy" = "darkorange", "Overweight" = "#d6ccc2", "Underweight" = "cyan4")) +
  labs(y = "Probabilidade",
       x = "",
       fill = "Life Stage") +
  theme_minimal() +
  theme(legend.position = "bottom")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Modelo 2: health_metrics ~ species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
modelo2 <- vglm(health_metrics ~ species, 
                family = multinomial(refLevel = 1),  # define a categoria de referência
                data = dados)

summary(modelo2)


### Visualizando as probabilidades ### 

## Predições das probabilidades para cada espécie
dados_pred2 <- data.frame(species = levels(dados$species)) #Dados para predições; certifica-se dos níveis; primeiro nível tem que coincidir com o primeiro nível do modelo
preds2 <- predict(modelo2,
                  newdata = dados_pred2,
                  type = "response") %>% data.frame()



### Formatar os resultados para gerar o gráfico
preds2$species <- dados_pred2$species

colnames(preds2) <- c("Healthy", "Overweight", "Underweight", "Species") 



### Transformar dado para o formato longo (necessário para o ggplot2)
preds2l <- preds2 %>%
  tidyr::pivot_longer(cols = c("Healthy", "Overweight", "Underweight"),
                      names_to = "Health",
                      values_to = "Probability")



### Plotando.... 
ggplot(preds2l, aes(x = Species, y = Probability, fill = Health)) +
  geom_bar(stat = "identity", position = "stack",  alpha = 0.8) +
  scale_fill_manual(values = c("Healthy" = "darkorange", "Overweight" = "#d6ccc2", "Underweight" = "cyan4")) +
  labs(y = "Probabilidade",
       x = "",
       fill = "Health metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

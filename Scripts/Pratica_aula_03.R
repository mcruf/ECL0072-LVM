####################################
#                                  #
#         Aula prática 03          #
#    - GLM para dados binários -   #
#                                  #
####################################


#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2) # para plotar
library(dplyr) #manipular os dados
library(DHARMa) #avaliação dos residuos
library(performance) #avaliação dos residuos


## Definindo diretório base
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Importando o banco de dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("../Dados/palmerpenguins_extended.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Análise exploratória 
#~~~~~~~~~~~~~~~~~~~~~~~~~~
str(dados)



# 2.1) Convertendo as variáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cols <- c("species", "island", "sex", "diet", "life_stage", "health_metrics", "year")

dados[, cols] <- lapply(dados[, cols], factor)



# 2.2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(dados$health_metrics)



## Filtrando os dados ##
library(dplyr)

dados2 <- filter(dados, !(health_metrics == "underweight"))


dados2 %>%
  group_by(health_metrics) %>% 
  summarise(Npinguins = n()) %>%
  mutate(Proporcao = Npinguins / sum(Npinguins)) #43% dos pinguins são obesos!



library(ggplot2)

## Relação com o peso individual ##
# Precisamos converter as categorias de peso em formato binário

dados2$categoria <- ifelse(dados2$health_metrics == "overweight",1, 0) %>%                       as.factor()

ggplot(dados2, aes(x = body_mass_g, y = categoria)) +
  geom_point(size = 4, col = 'gray50', alpha = 0.25) +
  theme_minimal()



## Incidência de sobrepeso vs. espécie ##
ggplot(dados2, aes(x = species, fill = as.factor(categoria))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#35978f", "#dfc27d"))+
  theme_minimal()



## Incidência de sobrepeso vs. sexo ##
ggplot(dados2, aes(x = sex, fill = as.factor(categoria))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#35978f", "#dfc27d"))+
  theme_minimal()




## Incidência de sobrepeso vs. sexo & espécie ##
ggplot(dados2, aes(x = sex, fill = as.factor(categoria))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#35978f", "#dfc27d"))+
  facet_wrap(species ~ .) +
  theme_minimal()



## Incidência de sobrepeso vs. dieta ##
ggplot(dados2, aes(x = diet, fill = as.factor(categoria))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#35978f", "#dfc27d"))+
  theme_minimal()



#~~~~~~~~~~~~~~~~~~~~~~
# 3) Testando modelos
#~~~~~~~~~~~~~~~~~~~~~~


## Modelo Nulo
mod0 <- glm(categoria ~ 1,
            family = binomial(link = "logit"),
            data = dados2)

## Modelo 1
mod1 <- glm(categoria ~ species,
            family = binomial(link = "logit"),
            data = dados2)

## Modelo 2
mod2 <- glm(categoria ~ species + sex,
            family = binomial(link = "logit"),
            data = dados2)

## Modelo 3
mod3 <- glm(categoria ~ species + sex + diet,
            family = binomial(link = "logit"),
            data = dados2)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Comparando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Calcular os valores de AIC ##
aic <- AIC(mod0, mod1, mod2, mod3)


## Ordenar dos menores aos maiores valores ##
aic %>% arrange(-AIC)


## Teste de razão de verossimilhança ##

#lmtest::lrtest(mod3, mod2) # mesma coisa
anova(mod3, mod2, test = "Chisq")


#~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Validação do modelo
#~~~~~~~~~~~~~~~~~~~~~~~~
library(DHARMa)

## Simulando os resíduos a partir do modelo escolhido ##
resid_sim <- simulateResiduals(mod3, n=1000)

plot(resid_sim)


library(performance)
check_model(mod3)


## Teste de autocorrelação
check_autocorrelation(mod3)
acf(residuals(mod3))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6) Analisando os resultados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(mod3)

#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(dplyr)
library(GGally) #pairsplot
library(MASS) #Para rodar modelo Binomial Negativo
library(corrplot) #multicolinearidade
library(DHARMa)
library(ggeffects) #para plotar os resultados
library(performance) #avaliação dos residuos


dados <- readRDS("Dados/koalas.rds")

# Define a primeira linha como cabeçalho
colnames(dados) <- dados[1, ]  # usa a primeira linha como nomes das colunas
dados <- dados[-1, ]  

dados <- dados |>
  mutate(across(where(is.character), as.numeric))

summary(dados)


ggplot(dados, aes(presence)) +
  geom_bar()



## Modelo Nulo
mod0 <- glm(presence ~ 1,
            family = binomial(link = "logit"),
            data = dados)

## Modelo 1
mod1 <- glm(presence ~ pprim_ssite,
            family = binomial(link = "logit"),
            data = dados)

## Modelo 2
mod2 <- glm(presence ~ pprim_ssite + psec_ssite,
            family = binomial(link = "logit"),
            data = dados)

## Calcular os valores de AIC ##
aic <- AIC(mod0, mod1, mod2)


## Ordenar dos menores aos maiores valores ##
aic %>% arrange(-AIC)


## Simulando os resíduos a partir do modelo escolhido ##
resid_sim <- simulateResiduals(mod1, n=1000)

plot(resid_sim)

check_model(mod1)

check_autocorrelation(mod1)
acf(residuals(mod1))

summary(mod1)

ggplot(dados, aes(x = pprim_ssite, y = presence)) +
  geom_point(alpha = 0.6) +  # Scatter plot of actual data
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),
              col="red", lty=2) +  # Fitted curve
  labs(title = " ", x = "Porcentagem de árvores primárias",
       y = "Probabilidade de presença") +
  theme_minimal()

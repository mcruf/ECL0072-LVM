#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(dplyr)
library(GGally) #pairsplot
library(MASS) #Para rodar modelo Binomial Negativo
library(corrplot) #multicolinearidade
library(DHARMa)
library(ggeffects) #para plotar os resultados
library(performance) #avaliação dos residuos

dados <- readRDS("Dados/benthos_exposure.rds")
#-----------------

hist(dados$Richness)
boxplot(dados$Richness)
summary(dados$Richness)
dotchart(dados$Richness)


plot(dados$Richness ~ dados$NAP)




mod_1 <- glm(Richness ~ NAP,
             data = dados,
             family = "poisson")

summary(mod_1)

mod_1$deviance/mod_1$df.residual #Não há sobredispersão -> phi > 1

sum(residuals(mod_1, type = "pearson")^2) / df.residual(mod_1)

mod_2 <- glm(Richness ~ NAP + Exposure,
             data = dados,
             family = "poisson")

AIC(mod_1, mod_2) #Comparando 


mod_3 <- glm.nb(Richness ~ NAP + Exposure,
                data = dados)

AIC(mod_2, mod_3)


simres2 <- simulateResiduals(mod_3, n=1000)
plot(simres2)


check_model(mod_1)

check_zeroinflation(mod_1)

summary(mod_3)



exp(coef(mod_3))




library(ggplot2)
library(dplyr)
library(MASS) # para glm.nb
library(patchwork) # para combinar gráficos

# --- Gráfico efeito de NAP ---

# grade de valores
newdat_NAP <- expand.grid(
  NAP = seq(min(dados$NAP), max(dados$NAP), length.out = 100),
  Exposure = mean(dados$Exposure)
)

# predição + IC 95%
pred <- predict(mod_3, newdata = newdat_NAP, type = "link", se.fit = TRUE)
newdat_NAP <- newdat_NAP %>%
  mutate(
    fit = exp(pred$fit),  # previsão na escala da contagem
    lwr = exp(pred$fit - 1.96*pred$se.fit),
    upr = exp(pred$fit + 1.96*pred$se.fit)
  )

# plot
p1 <- ggplot() +
  geom_point(data = dados, aes(x = NAP, y = Richness), alpha = 0.5) +
  geom_line(data = newdat_NAP, aes(x = NAP, y = fit), color = "steelblue", size = 1.2) +
  geom_ribbon(data = newdat_NAP, aes(x = NAP, ymin = lwr, ymax = upr), fill = "steelblue", alpha = 0.2) +
  labs(x = "NAP (altura relativa ao nível do mar)",
       y = "Riqueza de espécies",
       title = "A) Efeito de NAP sobre a riqueza"
       ) +
  theme_classic(base_size = 13)


# --- Gráfico efeito de Exposição ---

# grade de valores
newdat_Exp <- expand.grid(
  Exposure = seq(min(dados$Exposure), max(dados$Exposure), length.out = 100),
  NAP = mean(dados$NAP)
)

# predição + IC 95%
pred <- predict(mod_3, newdata = newdat_Exp, type = "link", se.fit = TRUE)
newdat_Exp <- newdat_Exp %>%
  mutate(
    fit = exp(pred$fit),
    lwr = exp(pred$fit - 1.96*pred$se.fit),
    upr = exp(pred$fit + 1.96*pred$se.fit)
  )

# plot
p2 <- ggplot() +
  geom_point(data = dados, aes(x = Exposure, y = Richness), alpha = 0.5) +
  geom_line(data = newdat_Exp, aes(x = Exposure, y = fit), color = "darkorange3", size = 1.2) +
  geom_ribbon(data = newdat_Exp, aes(x = Exposure, ymin = lwr, ymax = upr), fill = "darkorange3", alpha = 0.2) +
  labs(x = "Exposição (índice composto)",
       y = "Riqueza de espécies",
       title = "B) Efeito da Exposição sobre a riqueza"
       ) +
  theme_classic(base_size = 13)

# --- combinar gráficos lado a lado ---
p1 + p2



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



dotchart(dados$N_anzol) #Há pelo menos 2 outliers bem marcantes


### Tirando o outlier
dados2 <- filter(dados, N_anzol <= 1500) 


dotchart(dados2$N_anzol) 


## Calculando a abundância total de espécies não-alvo
### Precisamos também padronizar a abundância pelo esforço amostral (BPUE - bycatch per unit of effort)
dados2 <- dados2 %>%
  rowwise() %>%  # analisa linha por linha
  mutate(catch = sum(c_across(all_of(especies_alvo))),
         CPUE = catch/N_anzol) %>%
  data.frame()


# 2.3) Visualizando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Distribuição dos dados (variável y)
hist(dados2$CPUE)
summary(dados2$CPUE)

#dotchart(dados$BPUE)



## Dinâmica temporal
plot(dados2$CPUE ~ dados2$Mes)

ggplot(dados2, aes(x = Mes, y = CPUE)) +
  #ggplot(dados, aes(x = Mes, y = log1p(CPUE))) +
  geom_boxplot(size = 0.7, fill = 'cyan4',col = 'cyan4', alpha = 0.15) +
  stat_summary(fun = mean, geom="point", shape=19, size=1.5, color="black") +
  theme_minimal()



## Dinâmica espacial

### Latitude
ggplot(dados2, aes(x = Lat, y = CPUE)) +
  #ggplot(dados, aes(x = Lat, y = log1p(BPUE))) +
  geom_point(size = 3, col = 'darkorange', alpha = 0.4) +
  geom_smooth(method = "lm", col = 'cyan4') +
  theme_minimal()



### Longitude
ggplot(dados2, aes(x = Long, y = CPUE)) +
  #ggplot(dados, aes(x = Long, y = log1p(BPUE))) +
  geom_point(size = 3, col = 'darkorange', alpha = 0.4) +
  geom_smooth(method = "lm", col = 'cyan4') +
  theme_minimal()



# ggplot(dados, aes(x = Long, y = log1p(BPUE))) +
#   geom_point(size = 3, col = 'darkorange', alpha = 0.4) +
#   geom_smooth(method = "lm", col = 'cyan4', formula = y ~ poly(x,2)) +
#   theme_minimal()


#~~~~~~~~~~~~~~~~~~~~
# 3) Modelando BPUE
#~~~~~~~~~~~~~~~~~~~~


# 3.1) Preparando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Lembrando que a distribuição Gama e log-Normal são para variáveis estritamente positivas.
# Isso implica que a variável resposta não pode conter o valor 0.
# Truqe matemátcio: adicionar uma pequena constante (e.g., 10% da mediana)


# Usando 10% da mediana do BPUE
dados2$CPUE2 <- dados2$CPUE + (0.1 * median(dados2$CPUE))


# Transformando para log
#dados$BPUE2_log <- log(dados$BPUE2)



# 3.2) Rodando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Modelo Gama ##
modelo_G <- glmmTMB(CPUE2 ~ Mes + Lat + Long,
                    family = Gamma(link = "log"), 
                    data = dados2)



## Modelo log-Normal ##
modelo_LN <- glmmTMB(CPUE2 ~ Mes + Lat + Long,
                     family = lognormal(link = "log"), 
                     data = dados2)



# 3.3) Avaliando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

## AIC
AIC(modelo_G, modelo_LN)


## simulando os residuos
resG <- simulateResiduals(modelo_G, n=1000)
resLN <- simulateResiduals(modelo_LN, n=1000)


### Avaliando o comportamento geral dos resíduos
check_model(modelo_G)
check_model(modelo_LN)


### Avaliando correlação espacial
testSpatialAutocorrelation(resG, x = dados2$Long, y = dados2$Lat) #autocorrelação espacial detectada; problema não pode ser ignorado!

### Avaliando a correlação temporal
res <- recalculateResiduals(resG, group = dados2$Mes)
testTemporalAutocorrelation(res, time = unique(dados2$Mes))


# Embora ambos os modelos apresentam um bom ajuste visual, escolhe-se o modelo com
# distribuição Gama como o melhor modelo, dado ao menor valor de AIC.


## Avaliando os resultados
summary(modelo_LN)


### Plotando valores ajustados
ggpredict(modelo_LN) %>% plot()

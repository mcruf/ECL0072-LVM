######################################
#                                    #
#         Aula prática 08            #
#             - GAM -                #
#                                    #
######################################



#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(mgcv) #rodar gam
library(mgcViz) #visualizar resultados do gam
library(gratia) #visualizar resultados do gam
library(ggplot2) #plotar
library(dplyr) #manipular dados
library(DHARMa) #avaliação dos residuos
library(performance) #avaliação dos residuos
library(ggeffects) #para plotar os resultados



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



#><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando & manipulando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1.1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("Dados/bioluminescence.rds")
str(dados)


# 1.2) Transformando as variáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cols <- c("Station", "Time", "Month", "Year", "Season")
dados[, cols] <- lapply(dados[, cols], factor) #Transformando para fator


# 1.3) Filtrando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Análises prévias mostraram que nem todas as estações de amostragem
# apresentaram o mesmo padrão de relação entre bioluminescencia ~ profundidade.
# Para explorar melhor o poder dos modelos GAM, os dados serão filtrados
# para conter apenas o conjunto de estações que apresentaram uma relação altaramente não linear

## Vejamos...
dados %>%
  ggplot(aes(x=SampleDepth, y = Sources)) +
  geom_point(col = 'gray40', pch = 19, alpha =0.5, size = 3) +
  facet_wrap(Station ~ . , scales = "free_y") +
  geom_smooth(method = "gam", fill = 'gray60', col = 'darkorange') +
  labs(x = "Profundidade (m)",
       y = "Bioluminescência") +
  theme_minimal(base_size = 14) 


dados2 <- filter(dados, Station %in% c(6,7,8,9,11,16,18,19))



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~

## Explorando a variável resposta (Sources - bioluminescência)
ggplot(dados2, aes(x = Sources)) +
  geom_histogram(bins = 35, fill = 'cyan4', col = 'white', alpha = 0.6)

# O dado apresenta uma forte assimetria à direita


## Explorando a relação bioluminescência vs. profundidade
ggplot(dados2, aes(x = SampleDepth, y = Sources)) +
  geom_point(pch = 19, alpha =0.5, size = 3,
             col = 'cyan4') +
  #geom_smooth(method = "gam",col = 'darkorange', fill = '#b6ad90') +
  theme_minimal(base_size = 14) +
  #facet_wrap(Station ~ .) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'))


## Bioluminescência vs. lat/long
ggplot(dados2, aes(x = Longitude, y = Latitude, size = Sources)) +
  geom_point(pch = 19, col = 'cyan4') +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'))


## Relação bioluminescência vs. ano
ggplot(dados2, aes(x = Year, y = Sources)) +
  geom_boxplot(fill = 'cyan4', alpha = 0.5) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'))




#~~~~~~~~~~~~~~~~~~~~~~~
# 3) Modelando os dados
#~~~~~~~~~~~~~~~~~~~~~~~

## Vamos começar com um GLM e aumentar sequencialmente o 
## nível de complexidade do modelo


# 3.1) GLM com distribuição Guassiana (Normal)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Modelo com relação puramente linear
m_glm <- glm(Sources ~ SampleDepth,
             data = dados2,
             family = gaussian)


## Modelo com termo quadrático
m_glm2 <- glm(Sources ~ poly(SampleDepth, 2),
             data = dados2,
             family = gaussian)


## Modelo com termo cúbico
m_glm3 <- glm(Sources ~ poly(SampleDepth, 3),
              data = dados2,
              family = gaussian)


## Modelo com termo à quarta
m_glm4 <- glm(Sources ~ poly(SampleDepth, 4),
              data = dados2,
              family = gaussian)


AIC(m_glm, m_glm2, m_glm3, m_glm4) # A adição de termos polinomiais melhora consideravelmente o ajuste


## Análise dos pressupostos
simres1 <- simulateResiduals(m_glm, n=1000) #modelo mais simples
simres4 <- simulateResiduals(m_glm4, n=1000) #modelo mais complexo


check_model(simres1) #Modelo mais simples
check_model(simres4) #Modelo mais complexp


# Será que usar uma distribuição gama melhora o ajuste?

## Modelo com termo à quarta & distribuição gama
### Lembrando: distribuição gama é estritamente positiva (y > 0)
summary(dados2$Sources)
dados2$Sources2 <- dados2$Sources + median(dados2$Sources)*0.1 #Adicionando 10% da mediana para evitar o valor zero


m_glm5 <- glm(Sources2 ~ poly(SampleDepth, 4),
              data = dados2,
              family = Gamma(link = 'log'))

AIC(m_glm4, m_glm5)

simres5 <- simulateResiduals(m_glm5, n=1000) #modelo mais complexo
check_model(simres5) #Modelo mais simples



## Os resíduos ainda mostram um padrão - vamos tentar um GAM!


# 3.2) Modelos GAM 
#~~~~~~~~~~~~~~~~~~

### GAM com distribuição Normal
m_gam1 <- gam(Sources2 ~ s(SampleDepth, bs = "cr"), #outros suavizadores: tp, ts, cc
              data = dados2,
              family = gaussian())

AIC(m_glm5, m_gam1) #glm5 usa distribuição GAMA, m_gam1 usa distribuição normal
  

## Comparando GLM & GAM
ggplot(dados2, aes(SampleDepth, Sources2)) +
  geom_point(alpha = 0.5, col="gray40") +
  geom_smooth(method = "lm", formula = y~poly(x,4), col = "darkorange", se = FALSE) +
  geom_smooth(method = "gam", formula = y~s(x), col="cyan4", se = FALSE) +
  labs(y = "Bioluminescência", x = "Profundidade (m)") +
  theme_minimal()



### GAM com distribuição Gama
m_gam2 <- gam(Sources2 ~ s(SampleDepth, bs = "cr"),
              data = dados2,
              family = Gamma(link = "log"))

AIC(m_gam1, m_gam2) #O GAM fornece um melhor ajuste comparado ao modelo GLM


## Avaliando os pressupostos
par(mfrow = c(2,2))
gam.check(m_gam2) #versão com pacote mgcv
par(mfrow = c(1,1))

check_model(m_gam2)


### Outra forma de avaliar com o pacote gratia
gratia::appraise(m_gam2,
                 method = 'simulate',
                 seed = 123) 


### Interpretando um GAM
summary(m_gam2) # edf >>> 1 (efeito não-linear)



### Visualizando os resultados
plot(m_gam2)
gratia::draw(m_gam2) #versão com pacote gratia


### Avaliando o efeito de 'k' (nós) (default é k = 10)
m_gam2b <- gam(Sources2 ~ s(SampleDepth, k = 3), data=dados2, family=Gamma(link="log"))
m_gam2c <- gam(Sources2 ~ s(SampleDepth, k = 8), data=dados2, family=Gamma(link="log"))
m_gam2d <- gam(Sources2 ~ s(SampleDepth, k = 15), data=dados2, family=Gamma(link="log"))

par(mfrow=c(1,3))
plot(m_gam2b)
plot(m_gam2c)
plot(m_gam2d)
par(mfrow=c(1,1))

gam.check(m_gam2b) #Verificar se o número de nós (k) escolhido foi suficiente
# O teste compara a suavização ajustada com os resíduos. 
# Se houver estrutura residual que poderia ser explicada com uma base maior, o teste indica isso com p < 0.05 e k-index < 1.



####################
### Curiosidades ###
####################


## Modelo com efeito fixo e suavizador
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m_gam3 <- gam(Sources2 ~ s(SampleDepth) + Station, 
              data = dados2,
              family = Gamma(link = "log")) #Interpretação análoga a um termo de interação 

summary(m_gam3)


## Modelo com profundidade por estação
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m_gam4 <- gam(Sources2 ~ s(SampleDepth, bs = "cr", by = Station), 
              data = dados2,
              family = Gamma(link = "log")) #Interpretação análoga a um termo de interação 

summary(m_gam4)
draw(m_gam4)


## Modelo com interação long/lat (superfície bidimensional)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m_gam5 <- gam(Sources2 ~ s(SampleDepth, bs = "cr") + s(Longitude, Latitude, k=8),
              data = dados2,
              family = Gamma(link = "log"))

summary(m_gam5)
draw(m_gam5)


## Visualização alternativa
sm <- smooth_estimates(m_gam5, select = "s(Longitude,Latitude)") # Extrair efeito suavizador
draw(sm) +
  scale_fill_viridis_c(option = "D") +
  labs(x = "Longitude", 
       y = "Latitude", 
       fill = "Efeito parcial")



## Modelo com profundidade variando na long/lat 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m_gam5 <- gam(Sources2 ~ te(SampleDepth, Longitude, Latitude),
              data = dados2, 
              family = Gamma(link="log"))

draw(m_gam5)

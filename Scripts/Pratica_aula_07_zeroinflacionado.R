#######################################################
#                                                     #
#                   Aula prática 07                   #
#   - Distribuições para dados zero-inflacionados -   #
#                                                     #
#######################################################



#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~

library(glmmTMB) #Para rodar modelos ZIP/ZINB e ZAP/ZANB
#library(pscl) #Alternativa para modelos ZIP/ZINB e ZAP/ZANB
library(dplyr) #Manipulação dos dados


library(ggplot2) #Visualização
library(patchwork) #Para visualização de gráficos
#library(GGally) #Complemento ao ggplot2
#library(ggpubr) #Visualização
library(DHARMa) #Avaliação de modelos
library(performance) #Avaliação de modelos





#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo 'tema' geral para ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
theme_set(theme_minimal(base_size = 15))



#><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando & manipulando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dados disponibilizados por Moreno et al. (2019). https://doi.org/10.1111/2041-210X.13185
# O estudo baseia-se na hipótese da Liberação do Inimigo, a qual postula que plantas exóticas sofrerão 
# uma redução em seus níveis de herbivoria em comparação com as espécies nativas na nova área de distribuição, 
# ao deixar para trás os inimigos de sua área de origem. 

# Essa hipótese prevê um grande número de plantas intactas (ausência de herbivoria) no habitat invadido,
# e, consequentemente, os dados podem estar inflacionados em zeros e provavelmente sobredispersos.

dados <- readRDS("Dados/plant_herbivore.rds")



# 1.2) Manipulando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Das 4 espécies de plantas abordadas no estudo de Moreno et al. (2019),
# 2 são classificadas como nativas (Senecio vulgaris, S. lividus), e as outras 2 como exóticas/invasoras (S. inaequidens, S. pterophorus).
# Vamos criar uma coluna para criar uma covariável exótica vs. nativa

table(dados$Species)

dados$tipo_planta <- ifelse(dados$Species == "SI", "invasora",
                            ifelse(dados$Species == "SL", "nativa",
                                   ifelse(dados$Species == "SP", "invasora", "nativa"))) %>% factor()

table(dados$tipo_planta)


## Transformando character para factor
dados[, c("Subject","Location", "Species")] <- lapply(dados[, c("Subject","Location", "Species")], factor)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A hipótese prevê um grande número de plantas intactas 
# (ausência de herbivoria) no habitat invadido.

# Qual a frequência de zeros na variável resposta (damage)?
plot(table(dados$Damaged))
table(dados$Damaged == 0) # ~55% de zeros


# Será que conseguimos identificar um padrão entre os 2 tipos de plantas?
dados %>%
  ggplot(aes(x = Damaged)) +
  geom_histogram(fill = "cyan4", alpha = 0.6, col = 'white') +
  facet_wrap(. ~ tipo_planta, ncol = 1) +    
  labs(x = "No. de cabeças predadas", y = "Frequência") 


# Houve algum efeito do local de coleta, i.e., area em que se registrou mais predação? 
dados %>%
  ggplot(aes(x = Damaged)) +
  geom_histogram(fill = "cyan4", alpha = 0.6, col = 'white') +
  facet_grid(Location ~ tipo_planta) +    
  labs(x = "No. de cabeças predadas", y = "Frequência")


dados %>%
  mutate(Damaged2 = Damaged + (mean(Damaged) * 0.10)) %>%
  ggplot(aes(x = tipo_planta, y = Damaged2, fill = tipo_planta)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("#ee9b00", "gray50")) 


dados %>%
  group_by(tipo_planta) %>%
  summarize(N_total = n(), #No. de plantas de cada tipo
            N_zeros = sum(Damaged == 0), #Frequência de zeros
            Proporcao_zeros = N_zeros / N_total, #Proporção de zeros
            Pred_tot = sum(Damaged),
            Pred_medio = mean(Damaged))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Modelando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Modelo ZIP
#~~~~~~~~~~~~~~~~
# Vamos assumir que o número de cabeças de plantas predadas segue uma distribução ZIP,
# Começaremos com um modelo simples, em que se assume apenas covariáveis no processo de contagem (i.e, \lambda da distribuição poisson)


## Modelo 1 - covariável apenas para o processo de contagem
ZIP1 <- glmmTMB(Damaged ~ tipo_planta,
                ziformula = ~ 1,
                data = dados,
                family = poisson(link = "log"))


## Modelo 2 - covariável apenas para o processo de contagem
ZIP2 <- glmmTMB(Damaged ~ tipo_planta + Location,
                ziformula = ~ 1,
                data = dados,
                family = poisson(link = "log"))


## Modelo 3 - covariáveis para o processo de contagem e binário
ZIP3 <- glmmTMB(Damaged ~ tipo_planta + Location,
                ziformula = ~ Species, #Testa a hipótese da liberação do inimigo (i.e., espécies invasoras (SI, SP) deveriam ter uma probabilidade maior de terem zeros)
                data = dados,
                family = poisson(link = "log"))


### Comparando os modelos
AIC(ZIP1, ZIP2, ZIP3)


### Validando o modelo
simres <- simulateResiduals(ZIP3, n=1000)
plot(simres)
check_model(simres) #Os dados apresentam sobredispersão

testResiduals(simres) #Avaliando a sobredispersão


### Há sobredispersão além da dispersão causada pelo excesso de zeros.
### Podemos testar um modelo com distribuição Binomial Negativa


# 3.2) Modelo ZINB
#~~~~~~~~~~~~~~~~~~

ZINB1 <- glmmTMB(Damaged ~ tipo_planta + Location,
                ziformula = ~ Species, #Testa a hipótese da liberação do inimigo (i.e., espécies invasoras (SI, SP) deveriam ter uma probabilidade maior de terem zeros)
                data = dados,
                family = nbinom2(link = "log")) #nbinom1 = variancia aumenta linearmente com a media; nbino2 = variância aumenta quadraticamente com a media (formulacao mais comum)


# Comparando os modelos
AIC(ZIP3, ZINB1) #Eita!


simres2 <- simulateResiduals(ZINB1, n=1000)
plot(simres2)
check_model(simres2) #Os dados ainda apresentam sobredispersão

testResiduals(simres2) #Avaliando a sobredispersão


##Vamos avaliar o ajuste do ZINB1 (menor AIC)
summary(ZINB1)




# 3.3) Modelo ZAP
#~~~~~~~~~~~~~~~~~
# Agora vamos considerar casos em que não se consegue discernir os tipos de zeros.
ZAP1 <- glmmTMB(Damaged ~ tipo_planta + Location,
                ziformula = ~ Species, 
                data = dados,
                family = truncated_poisson(link = "log"))


AIC(ZINB1, ZAP1) #Comparando com o último modelo

simres4 <- simulateResiduals(ZAP1, n=1000)
check_model(simres4) #Os dados ainda apresentam sobredispersão
check_overdispersion(simres4) # Avaliando a sobredispersão


# 3.4) Modelo ZANB
#~~~~~~~~~~~~~~~~~
# Especificando uma binomial negativa zero-truncada
ZANB1 <- glmmTMB(Damaged ~ tipo_planta + Location,
                ziformula = ~ Species, 
                data = dados,
                family = truncated_nbinom2(link = "log"))


AIC(ZINB1, ZAP1, ZANB1) #Comparando os modelos...


simres5 <- simulateResiduals(ZANB1, n=1000)
check_model(simres5) #Os dados ainda apresentam sobredispersão
check_overdispersion(simres5) # Avaliando a sobredispersão


ggpredict(ZANB1) %>% plot()



# 3.4) Modelo Tweedie
#~~~~~~~~~~~~~~~~~~~~~~
tweedie <- glmmTMB(Damaged ~ tipo_planta + Location,
                   data = dados,
                   family = tweedie(link = "log"))


AIC(ZANB1, tweedie) #Comparando os modelos...


simres6 <- simulateResiduals(tweedie, n=1000)
check_model(simres6) #Os dados ainda apresentam sobredispersão
check_overdispersion(simres6) # Avaliando a sobredispersão
summary(tweedie)



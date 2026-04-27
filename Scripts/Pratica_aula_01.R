####################################
#                                  #
#         Aula prática: LM         #
#                                  #
####################################


# O script abordará:

# 1 - LM com preditor numérico 
# 2 - LM com preditor categórico (teste-t)
# 3 - LM com preditor categórico (ANOVA)



#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)


#~~~~~~~~~~~~~~~~~~~~~
# Carregando os dados
#~~~~~~~~~~~~~~~~~~~~~

## Definindo diretório base
setwd("~/OneDrive/Arbeit/Lectures_and_talks/UFRN/Lectures/ECL0072-GLM_GAM/")


## Carregando os dados
dados <- readRDS("Dados/palmerpenguins_extended.rds") #Pinguins de Palmer


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Análise exploratória
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# Resumo geral dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~
str(dados)
summary(dados) 
#dplyr::glimpse(dados)



# Transformando os dados 
#~~~~~~~~~~~~~~~~~~~~~~~~
# Há variáveis classificadas como 'character'.
# Precisamos transformar elas para 'factor', de modo que sejam
# reconhecidas como categorias e assim permitir a realização de comparações estatísticas.

## Selecionando as colunas a serem transformadas
cols <- c("species", "island","sex", "diet", "life_stage", "health_metrics")

## Transformando para fator
dados[, cols] <- lapply(dados[, cols], factor)

#glimpse(dados)


### Variável resposta
par(mfrow = c(1,3))
hist(dados$body_mass_g, main = "Histograma")
boxplot(dados$body_mass_g, main = "Boxplot")
dotchart(dados$body_mass_g, main = "Dotchart")
par(mfrow = c(1,1))


### Variável preditora (numérica)
par(mfrow = c(1,3))
hist(dados$bill_length_mm, main = "Histograma")
boxplot(dados$bill_length_mm, main = "Boxplot")
dotchart(dados$bill_length_mm, main = "Dotchart")
par(mfrow = c(1,1))


### Correlação entre y-x
plot(dados$body_mass_g ~ dados$bill_length_mm) #Gráfico de dispserão
cor(dados$body_mass_g, dados$bill_length_mm) #Correlação de pearson
?cor



### Variável preditora (categórica)
plot(dados$body_mass_g~dados$species)
plot(dados$body_mass_g~dados$sex)



#~~~~~~~~~~~~~
# 2) Modelos
#~~~~~~~~~~~~~


# 2.1) Modelo linear (LM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Avaliando a relação entre peso vs. comprimento do bico

## Rodando o modelo linear
mod1 <- lm(body_mass_g ~ bill_length_mm, dados)
#mod1b <- lm(body_mass_g/1000 ~ bill_length_mm, dados) #Alternativa para quando as variáveis são números muito grandes (não é o caso daqui)



## O que tem dentro do objeto mod1?
names(mod1)


## E a matriz de design (i.e., preditores + intercepto)
head(model.matrix(dados$body_mass_g ~ dados$bill_length_mm))


## Resultado numérico do modelo
summary(mod1) 

# - Intercept: Massa corporal estimada quando o comprimento do bico (x) é zero;
# - bill_length_mm: Para um aumento de 1mm no comprimento do bico, espera-se um aumento médio de 65.8 gramas na massa corporal do pinguim
# - Res. std. error: os valores individuais de massa corporal se desviam ~984 g da reta ajustada.
#


## Ajuste do modelo
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))




# 2.2) Teste-t
#~~~~~~~~~~~~~~
# Há diferenças de peso entre pinguins femeas e machos?

mod2 <- lm(body_mass_g ~ sex, data = dados)
summary(mod2)


## Comparando com a função padrão
t.test(body_mass_g ~ sex, data = dados)



# 2.3) ANOVA (1 fator)
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Há diferenças de peso entre as diferentes espécies?

mod3 <- lm(body_mass_g ~ species, data = dados)

head(model.matrix(dados$body_mass_g ~ dados$species)) #Matriz 

summary(mod3)


## ANOVA tradicional
summary(aov(body_mass_g ~ species, data = dados))


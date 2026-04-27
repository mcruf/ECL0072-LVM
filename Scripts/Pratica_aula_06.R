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
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando funções auxiliares
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("Scripts/funcoes_auxiliares.R")


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

## Há preferência alimentar de acordo com o tamanho dos pinguins? (usando peso como proxy de tamanho)
ggplot(dados, aes(y = body_mass_g, x = as.factor(diet))) +
  geom_boxplot(size = 0.7, fill = 'darkorange',col = 'darkorange', alpha = 0.15) +
  stat_summary(fun = mean, geom="point", shape=19, size=1.5, color="black") +
  theme_minimal(base_size = 15)



## E por espécies?
ggplot(dados, aes(x = species, fill = diet)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#d6ccc2", "cyan4", "darkorange", "gray50")) +
  labs(y = "Proporção", x = "") +
  theme_minimal(base_size = 15) 



## E entre sexos?
ggplot(dados, aes(x = sex, fill = diet)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#d6ccc2", "cyan4", "darkorange", "gray50")) +
  labs(y = "Proporção", x = "") +
  facet_wrap(species ~ .) +
  theme_minimal(base_size = 15) 


## E estágios de vida?
ggplot(dados, aes(x = life_stage, fill = diet)) +
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
levels(dados$diet) 



# Modelo 1: dieta ~ massa corporal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## O modelo avaliará apenas a relação da composição da dieta e a massa corporal.
modelo1 <- vglm(diet ~ body_mass_g, 
                family = multinomial(refLevel = 1),  # define a categoria de referência
                data = dados)

summary(modelo1)

#### Mesma coisa, porém com pacote diferente e outputs em outro formato
# library(nnet)
# modelo1b <- multinom(diet ~ body_mass_g, data = dados)
# summary(modelo1b)


## Visualizando os resultados ##

## Criando dados para predizer
dados_pred <- data.frame(body_mass_g = seq(min(dados$body_mass_g),
                                           max(dados$body_mass_g),
                                           length.out = 100))

## Predizendo...
preds <- predict(modelo1, 
                 newdata = dados_pred, 
                 type = "link", 
                 se.fit = TRUE)


## Calculando probabilidades por categoria & ICs
n_cat <- ncol(preds$fit) + 1 #no. de categorias
n_obs <- nrow(preds$fit) #no. de observações
categorias <- levels(dados$diet) #nome das categorias
cat_ref <- categorias[1] #categoria de referência
cat_outras <- categorias[-1]


preds_full <- lapply(1:n_obs, function(i) {
  
  eta <- preds$fit[i, ]
  se_eta <- preds$se.fit[i, ]
  
  probs <- softmax(eta)
  
  # Matriz de covariância dos preditores lineares (diagonal com variâncias)
  V_eta <- diag(se_eta^2)
  
  # Jacobiano da softmax
  J_soft <- softmax_jacobian(probs)
  
  # Método Delta: variância aproximada das probabilidades
  var_probs <- diag(J_soft %*% V_eta %*% t(J_soft))
  se_probs <- sqrt(var_probs)
  
  tibble(
    body_mass_g = dados_pred$body_mass_g[i],
    dieta = categorias,
    Prob = probs,
    SE = se_probs,
    IC_lower = pmax(0, probs - 1.96 * se_probs),
    IC_upper = pmin(1, probs + 1.96 * se_probs)
  )
})

preds_full <- do.call("rbind", preds_full)


## Plotando...
ggplot(preds_full, aes(x = body_mass_g, y = Prob, color = dieta, fill = dieta)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = IC_lower, ymax = IC_upper), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "Massa Corporal (g)",
       y = "Probabilidade p",
       color = "Dieta",
       fill = "Dieta") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("fish" = "darkorange", "squid" = "#d6ccc2", "parental" = "cyan4", "krill"="gray50")) +
  scale_colour_manual(values = c("fish" = "darkorange", "squid" = "#d6ccc2", "parental" = "cyan4", "krill"="gray50")) +
  theme(plot.title = element_text(hjust = 0.5))




# Modelo 2: dieta ~ espécie (nicho alimentar)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
modelo2 <- vglm(diet ~ species, 
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

colnames(preds2) <- c("Fish", "Krill", "Parental", "Squid", "Species") #Renomeando as colunas de dieta, assumindo a mesma ordem do modelo (i.e, Fish, Krill, Parental, Squid)



### Transformar dado para o formato longo (necessário para o ggplot2)
preds2l <- preds2 %>%
                tidyr::pivot_longer(cols = c("Fish", "Krill", "Parental", "Squid"),
                                    names_to = "Diet",
                                    values_to = "Probability")



### Plotando.... 
ggplot(preds2l, aes(x = Species, y = Probability, fill = Diet)) +
  geom_bar(stat = "identity", position = "stack",  alpha = 0.8) +
  scale_fill_manual(values = c("Fish" = "darkorange", "Squid" = "#d6ccc2", "Parental" = "cyan4", "Krill"="gray50")) +
  labs(y = "Probabilidade",
       x = "",
      fill = "Dieta") +
  theme_minimal() +
  theme(legend.position = "bottom")



### Visualizando as razões de chances (log odds) ### 

### Extraíndo os coeficientes do modelo
tmp <- summary(modelo2)
coefs <- tmp@coef3[, "Estimate"]
se <- tmp@coef3[, "Std. Error"]

results <- data.frame(Coef = coefs,
                      SE = se) #Savlando em um df


### Excluindo os interceptos (para melhor visualização)
results <- results[-c(1:3),]



### Calculando razão de chances & ICs a 95%
df_or <- results %>%
            mutate(OR = exp(Coef), #odds ratio
                   LCI = exp(Coef - (1.96 * SE)), #limite inferior do IC 95%
                   UCI = exp(Coef + (1.96 * SE)), #limite superior do IC 95%
                   Comparison = rownames(results)) #reavendo os níveis de comparação
                  


### Formatando os dados para plotagem
df_or <- df_or %>%
          mutate(Species = gsub("^species", "", Comparison), #tirando 'species'
                 Diet = gsub(".*:", "", Comparison)) %>% #ficando apenas com os números
          mutate(Species = gsub(":[0-9]+$", "", Species), #tirando os números
                 Diet = ifelse(Diet == 1, "Krill", 
                               ifelse(Diet == 2, "Parental", 'Squid'))) #reclassificando os números de acordo com as categorias alimentares



### Plotando...
ggplot(df_or, aes(y = Diet, x = OR, color = Species)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2, position = position_dodge(width = 0.5), size = 1.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +
  scale_x_log10() + # Escala log para melhor visualização do OR
  scale_colour_manual(values = c("darkorange","cyan4")) +
  labs(x = "Razão de chances (log)",
       y = "Dieta",
       color = "Espécie") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")






################
#  ADICIONAL 
###############



# Modelo 3: regressão multinomial ordinal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Quando a ordem dos níveis é importante

levels(dados$life_stage)
dados$life_stage <- factor(dados$life_stage, levels = c("chick", "juvenile", "adult"), ordered = T)
#dados$life_stage <- factor(dados$life_stage, levels = c("adult", "juvenile", "chick"), ordered = T)



modelo3 <- vglm(life_stage ~ body_mass_g,
                family = cumulative(parallel = TRUE, link = "logitlink"),
                data = dados)
summary(modelo3)



## Criando dados para predizer
dados_pred <- data.frame(body_mass_g = seq(min(dados$body_mass_g),
                                           max(dados$body_mass_g),
                                           length.out = 100))

## Predizendo...
preds <- predict(modelo3, 
                 newdata = dados_pred, 
                 type = "link", 
                 se.fit = TRUE)


## Calculando probabilidades por categoria & ICs
n_cat <- ncol(preds$fit) + 1 #no. de categorias
n_obs <- nrow(preds$fit) #no. de observações
categorias <- levels(dados$life_stage) #nome das categorias
cat_ref <- categorias[1] #categoria de referência
cat_outras <- categorias[-1]


preds_full <- lapply(1:n_obs, function(i) {
  
  eta <- preds$fit[i, ]
  se_eta <- preds$se.fit[i, ]
  
  probs <- softmax(eta)
  
  # Matriz de covariância dos preditores lineares (diagonal com variâncias)
  V_eta <- diag(se_eta^2)
  
  # Jacobiano da softmax
  J_soft <- softmax_jacobian(probs)
  
  # Método Delta: variância aproximada das probabilidades
  var_probs <- diag(J_soft %*% V_eta %*% t(J_soft))
  se_probs <- sqrt(var_probs)
  
  tibble(
    body_mass_g = dados_pred$body_mass_g[i],
    dieta = categorias,
    Prob = probs,
    SE = se_probs,
    IC_lower = pmax(0, probs - 1.96 * se_probs),
    IC_upper = pmin(1, probs + 1.96 * se_probs)
  )
})

preds_full <- do.call("rbind", preds_full)


## Plotando...
ggplot(preds_full, aes(x = body_mass_g, y = Prob, color = dieta, fill = dieta)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = IC_lower, ymax = IC_upper), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "Massa Corporal (g)",
       y = "Probabilidade p",
       color = "Estágio \n de vida",
       fill = "Estágio \n de vida") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("chick" = "darkorange", "juvenile" = "#d6ccc2", "adult" = "cyan4")) +
  scale_colour_manual(values = c("chick" = "darkorange", "juvenile" = "#d6ccc2", "adult" = "cyan4")) +
  theme(plot.title = element_text(hjust = 0.5))

install.packages("sjPlot")
library(sjPlot)
plot_model(modelo3, type = 'pred')

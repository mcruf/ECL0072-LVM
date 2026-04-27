#GLM PARA DADOS CATEGORICOS

setwd("D:/Documentos/Doutorado/Disciplinas/GLMM/ECL0072-GLM_GAM-gh-pages")

library(VGAM) #Modelo multinomial
library(nnet) #Modelo multinomial (pacote alternativo)
library(ggplot2)
library(dplyr)

source("Scripts/funcoes_auxiliares.R")

pinguins <- readRDS("Dados/palmerpenguins_extended.rds")

View(pinguins)
str(pinguins)

colss <- c("species", "island", "sex", "diet", "life_stage", "health_metrics")
pinguins[, colss] <- lapply(pinguins[, colss], factor)


## metrica de saude dos pinguins comparando especies
ggplot(pinguins, aes(x = species, fill = health_metrics)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("cyan4", "red", "darkorange")) +
  labs(y = "Proporção", x = "") +
  theme_minimal(base_size = 15)

## metrica de saude dos pinguins comparando estagios de vida
ggplot(pinguins, aes(x = life_stage, fill = health_metrics)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("cyan4", "red", "darkorange")) +
  labs(y = "Proporção", x = "") +
  theme_minimal(base_size = 15)
# parece ter mais adultos obesos

## metrica de saude dos pinguins comparando sexo
ggplot(pinguins, aes(x = sex, fill = health_metrics)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("limegreen", "purple","skyblue")) +
  labs(y = "Proporção", x = "") +
  theme_minimal(base_size = 15)

#juntando tudo num grafico so pra visualizar melhor
ggplot(pinguins, aes(x = life_stage, fill = health_metrics)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("cyan4", "red", "darkorange")) +
  labs(y = "Proporção", x = "") +
  facet_wrap(species ~ sex)+
  theme_minimal(base_size = 15)

#FAZENDO O MODELO

#checando os niveis do fator health_metrics para ver qual vai ser o nível de referencia
levels(pinguins$health_metrics) #vou escolher a referencia como healthy (1)
#obs: 1 = healthy, 2 = overweight, 3 = underweight

#MODELO 1: apenas saude e especie
mod_pin1 <- vglm(health_metrics ~ species, 
family = multinomial(refLevel = 1),  # coloquei 1, entao o modelo vai comparar tudo com healthy
data = pinguins)

summary(mod_pin1) #a probabilidade de chinstrap ter sobrepeso  é menor que a de ser saudavel
#a probabilidade de gentoo ter sobrepeso é maior que a de ser saudavel
# a probabilidade de gentoo ter subpeso é menor que a de ser saudavel
#pra Adelie, a probabilidade de ter sobre e subpeso é menor que a de ser saudavel

#criar dados para predizer
## Predições das probabilidades para cada espécie
dados_pred_pin <- data.frame(species = levels(pinguins$species)) #Dados para predições; certifica-se dos níveis; primeiro nível tem que coincidir com o primeiro nível do modelo
preds_pin <- predict(mod_pin1,
                  newdata = dados_pred_pin,
                  type = "response") %>% data.frame()

preds_pin$species <- dados_pred_pin$species
colnames(preds_pin) <- c("healthy", "overweight", "underweight", "species") #Renomeando as colunas de dieta, assumindo a mesma ordem do modelo
View(preds_pin)

preds_pinl <- preds_pin %>%
  tidyr::pivot_longer(cols = c("healthy","overweight", "underweight"),
                      names_to = "health_metrics",
                      values_to = "Probability")

### Plotando.... 
ggplot(preds_pinl, aes(x = species, y = Probability, fill = health_metrics)) +
  geom_bar(stat = "identity", position = "stack",  alpha = 0.8) +
  scale_fill_manual(values = c("healthy" = "darkorange", "overweight" = "#d6ccc2", "underweight" = "cyan4")) +
  labs(y = "Probabilidade",
       x = "",
       fill = "Saude") +
  theme_minimal() +
  theme(legend.position = "bottom")


#visualizando as razoes de chances
### Extraíndo os coeficientes do modelo
tmp <- summary(mod_pin1)
coefs <- tmp@coef3[, "Estimate"]
se <- tmp@coef3[, "Std. Error"]

results <- data.frame(Coef = coefs,
                      SE = se) #Savlando em um df
results <- results[-c(1:2),] ### Excluindo os interceptos (para melhor visualização)

### Calculando razão de chances & ICs a 95%
df_or <- results %>%
  mutate(OR = exp(Coef), #odds ratio
         LCI = exp(Coef - (1.96 * SE)), #limite inferior do IC 95%
         UCI = exp(Coef + (1.96 * SE)), #limite superior do IC 95%
         Comparison = rownames(results)) #reavendo os níveis de comparação

#formatando os dados pra plotar
df_or <- df_or %>%
  mutate(Species = gsub("^species", "", Comparison), #tirando 'species'
         Health = gsub(".*:", "", Comparison)) %>% #ficando apenas com os números
  mutate(Species = gsub(":[0-9]+$", "", Species), #tirando os números
         Health = ifelse(Health == 1, "overweight", "underweight")) #reclassificando os números de acordo com as categorias alimentares

### Plotando...
ggplot(df_or, aes(y = Health, x = OR, color = Species)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2, position = position_dodge(width = 0.5), size = 1.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +
  scale_x_log10() + # Escala log para melhor visualização do OR
  scale_colour_manual(values = c("darkorange","cyan4")) +
  labs(x = "Razão de chances (log)",
       y = "Saude",
       color = "Espécie") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


#apenas saude e estagio de vida

mod_pin2 <- vglm(health_metrics ~ life_stage, 
                 family = multinomial(refLevel = 1),  # coloquei 1, entao o modelo vai comparar tudo com healthy
                 data = pinguins)

summary(mod_pin2) #a probabilidade de um pinguim adulto ter sobrepeso é maior que a de ser saudavel. e a probabilidade de ter subpeso é menor
#pra bebes, as probs de ter sobre e subpeso sao menores que a de ser saudavel
#pra juvenis, a prob de sobrepeso é menor que a de ser saudavel (esse ta esquisito)



#############################################################################
#                                                                           #
#                            Prática da aula 01                             #
#                        - Modelos Lineares Gerais -                        #
#                                                                           #
#############################################################################


# CONTEXTO:
# CONTEXTO:
# Modelos lineares (LMs) constituem a base de diversos modelos estatísticos mais complexos (GLM, GAM, GLMM/GAMM, ...).
# Nesta prática, utilizaremos dados morfométricos dos pinguins de Palmer para explorar o ajuste e a interpretação de modelos lineares no R,
# tendo a massa corporal (body_mass_g) como variável resposta e características como sexo, comprimento do bico e espécie como variáveis explicativas. 
# A partir desses dados, veremos como teste-t, regressão linear, ANOVA e ANCOVA podem ser formulados dentro de um mesmo framework:
# y = Xβ + ε.


# OBJETIVOS:
# - Explorar o banco de dados 
# - Compreender a estrutura geral de um modelo linear e sua matriz de design
# - Ajustar modelos lineares gerais via função lm()
#    - Teste-t ............ preditor categórico (2 níveis)
#    - LM ................. preditor contínuo
#    - ANOVA .............. preditor categórico (> 2 níveis)
#    - ANCOVA ............. preditor contínuo + categórico (retas paralelas)
#    - ANCOVA c/ interação. preditor contínuo + categórico (retas não paralelas)
# Interpretar os coeficientes no contexto de cada modelo testado
# Comparar os diferentes modelos e avaliar os seus pressupostos


#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) # Manipulação de dados
library(ggplot2) # Visualização de dados
library(patchwork) # Visualização de dados
library(performance) # Avaliação de modelos


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



#~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/palmerpenguins_extended.rds")
head(df)
str(df)



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Tratamento dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Há variáveis classificadas como 'character'.Precisamos transformar elas para 'factor', de modo que sejam
# reconhecidas como categorias e assim permitir a realização de comparações estatísticas.

# Selecionando as colunas a serem transformadas
cols <- c("species", "island","sex", "diet", "life_stage", "health_metrics")

# Transformando para fator
df[, cols] <- lapply(df[, cols], factor)
str(df) # Sanity check




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Análise exploratória dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table(is.na(df)) # Há NA nos dados? Se sim, a análise exploratória deverá ser feita com mais cautela.
colSums(is.na(df))


# 3.1) Variável resposta
#~~~~~~~~~~~~~~~~~~~~~~~~~
summary(df$body_mass_g)


## Histograma
phist <- ggplot(df, aes(x = body_mass_g)) +
         geom_histogram(fill = '#3e5c76', col = 'white', alpha = 0.8, linewidth = 1) +
         coord_cartesian(expand = F) +
         labs(x = 'Body mass (g)', y = 'Frequency') +
         theme_bw(base_size = 12)


## Boxplot
pbox <- ggplot(df, aes(y = body_mass_g)) +
        geom_boxplot(fill = 'white', col = '#3e5c76', alpha = 0.8, linewidth = 1.2, outlier.size = 3) +
        labs(y = 'Body mass (g)') +
        theme_bw(base_size = 12) +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())


## Dotchart
pdot <- df %>% 
        mutate(row_id = row_number()) %>% 
        ggplot(aes(x = body_mass_g, y = row_id)) +
        geom_point(color = '#3e5c76', size = 2.5, alpha = 0.4) +
        labs(x = "Body mass (g)", y = NULL) +
        scale_x_continuous(breaks = seq(2000, 11000, by = 1000),
                           minor_breaks = seq(2000, 11000, by = 100)) +
        theme_classic(base_size = 12) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              panel.grid.major.x = element_line(color = "gray85", linewidth = 0.4),
              panel.grid.minor.x = element_line(color = "gray92", linewidth = 0.2))


phist + pbox + pdot



# 3.2) Relação peso corporal e sexo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ps <- ggplot(df, aes(y = body_mass_g, x = sex, fill = sex, col = sex)) +
      geom_boxplot(linewidth = 1, alpha = 0.3) +
      scale_fill_manual(values = c("#846267", "#f6bd60")) +
      scale_color_manual(values = c("#846267", "#f6bd60")) +
      labs(x = '', y = 'Body mass (g)') +
      theme_light(base_size = 14) 

ps2 <- ggplot(df, aes(x = body_mass_g, fill = sex, col = sex)) +
        geom_density(linewidth = 1, alpha = 0.3) +
        scale_fill_manual(values = c("#846267", "#f6bd60")) +
        scale_color_manual(values = c("#846267", "#f6bd60")) +
        labs(x = 'Body mass (g)', y = 'Density') +
        theme_light(base_size = 14) 

ps + ps2

tapply(df$body_mass_g, df$sex, mean)



# 3.3) Relação peso corporal vs. tamanho do bico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(df, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(size = 3, alpha = 0.3, col = '#3e5c76') +
  geom_smooth(method = 'lm', fill = "gray20", col = 'gray20') +
  labs(x = 'Bill length (mm)', y = 'Body mass (g)') +
  theme_light(base_size = 14) 



## A relação difere entre sexos?
ggplot(df, aes(x = bill_length_mm, y = body_mass_g, col = sex)) +
  geom_point(size = 3, alpha = 0.2) +
  scale_fill_manual(values = c("#846267", "#f6bd60")) +
  scale_color_manual(values = c("#846267", "#f6bd60")) +
  geom_smooth(method = 'lm') +
  labs(x = 'Bill length (mm)', y = 'Body mass (g)') +
  theme_light(base_size = 14) 


## E entre as espécies, essa relação muda?
ggplot(df, aes(x = bill_length_mm, y = body_mass_g, col = species)) +
  geom_point(size = 3, alpha = 0.2) +
  scale_fill_manual(values = c("#846267", "#f6bd60", "#606c38")) +
  scale_color_manual(values = c("#846267", "#f6bd60", "#606c38")) +
  geom_smooth(method = 'lm') +
  labs(x = 'Bill length (mm)', y = 'Body mass (g)') +
  theme_light(base_size = 14) 

# ggplot(df, aes(x = bill_length_mm, y = body_mass_g, col = species, group = species)) +
#   geom_point(size = 3, alpha = 0.3) +
#   scale_fill_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
#   scale_color_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
#   geom_smooth(method = 'lm', fill = "gray30", col = 'gray30') +
#   facet_wrap(species ~ .) +
#   labs(x = 'Bill length (mm)', y = 'Body mass (g)') +
#   theme_light(base_size = 14) 

table(df$species)


# Complicando um pouco mais...
ggplot(df, aes(x = bill_length_mm, y = body_mass_g, col = species, group = sex)) +
  geom_point(size = 3, alpha = 0.3) +
  scale_fill_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
  scale_color_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
  geom_smooth(aes(group = sex, linetype = sex),method = 'lm', fill = "gray30", col = 'gray30') +
  facet_wrap(species ~ .) +
  labs(x = 'Bill length (mm)', y = 'Body mass (g)', linetype = "Sex") +
  theme_light(base_size = 14) 

table(df$species, df$sex)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Modelos Lineares Gerais 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~
# 4.1) Teste-t: 
#~~~~~~~~~~~~~~~~~
# Machos e fêmeas diferem quanto ao peso corporal?

mod_t <- lm(body_mass_g ~ sex, data = df)
#t.test(body_mass_g ~ sex, data = df, var.equal = TRUE) # Mesma coisa


## O que tem dentro do objeto mod_t?
names(mod_t)


## Matriz de design
head(model.matrix(mod_t))


## Resultado do modelo
summary(mod_t)

# - Intercept: peso das fêmeas
# - sexmale: diferença no peso médio entre femeas e machos. Peso do macho: Intercept + sexmale



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.2) Modelo linear (LM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Relação peso corporal - comprimento do bico

mod_lm <- lm(body_mass_g ~ bill_length_mm, data = df)
head(model.matrix(mod_lm)) # Matriz de design


## Resultado do modelo
summary(mod_lm)

# - Intercept: peso médio esperado de um pinguim quando o comprimento do bico = 0 mm
# - bill_length_mm: Aumento médio da massa corporal do pinguim para cada aumento de 1 mm no comprimento do bico


coef(mod_lm)
confint(mod_lm)


## Ajuste do modelo
par(mfrow=c(2,2))
plot(mod_lm)
par(mfrow=c(1,1))

hist(mod_lm$residuals)
check_model(mod_lm)
plot(df$body_mass_g ~ mod_lm$fitted.values)
abline(a = 0, b = 1, col = 2)


## Centralizando a covariável
df$bill_length_mm_c <- df$bill_length_mm - mean(df$bill_length_mm, na.rm = T)

mod_lm2 <- lm(body_mass_g ~ bill_length_mm_c, data = df)
summary(mod_lm2) 

# - Intercept: peso médio esperado de um pinguim cujo comprimento do bico = comprimento médio do bico
# - bill_length_mm: Aumento médio da massa corporal do pinguim para cada aumento de 1 mm no comprimento do bico
# ATENÇÃO: centralizar uma variável é diferente de padronizar - mudança esperada no peso associada a um aumento de 1 desvio-padrão no comprimento do bico


## Visualizando o ajuste: intervalo de confiança vs. intervalo de predição
lmin <- min(df$bill_length_mm)
lmax <- max(df$bill_length_mm)


pred_df <- data.frame(bill_length_mm = seq(lmin, lmax, length.out = 100))
head(pred_df)


### incerteza sobre a resposta média esperada ( mu_i(x) = beta_0_i + beta_1_i*x )
pred_ci <- predict(mod_lm, newdata = pred_df, interval = "confidence")  


### incerteza sobre uma nova observação  ( y_i(x) = beta_0 + beta_1*x_i + epsilon_i)
pred_pi <- predict(mod_lm, newdata = pred_df, interval = "prediction")  


#### Armazenando predições no data frame
pred_df$fit <- pred_ci[, "fit"] # Ou pred_pi - mesmo resultado!
pred_df$ci_lwr <- pred_ci[, "lwr"] # IC-95 inferior de pred_ci
pred_df$ci_upr <- pred_ci[, "upr"] # IC-95 superior de pred_ci
pred_df$pi_lwr <- pred_pi[, "lwr"] # IP-95 inferior de pred_pi
pred_df$pi_upr <- pred_pi[, "upr"] # IP-95 superior de pred_pi



#### Visualizando...
p_ci <- ggplot() +
        geom_point(data = df, aes(x = bill_length_mm, y = body_mass_g),
                   size = 3, alpha = 0.2, col = '#f6bd60') +
        geom_ribbon(data = pred_df, aes(x = bill_length_mm, ymin = ci_lwr, ymax = ci_upr),
                    fill = '#846267', alpha = 0.4) +
        geom_line(data = pred_df, aes(x = bill_length_mm, y = fit),
                  col = '#846267') +
        labs(x = 'Bill length (mm)', y = 'Body mass (g)', title = 'Incerteza sobre a resposta média') +
        theme_light(base_size = 14)


p_pi <- ggplot() +
        geom_point(data = df, aes(x = bill_length_mm, y = body_mass_g),
                   size = 3, alpha = 0.2, col = '#f6bd60') +
        geom_ribbon(data = pred_df, aes(x = bill_length_mm, ymin = pi_lwr, ymax = pi_upr),
                    fill = '#846267', alpha = 0.4) +
        geom_line(data = pred_df, aes(x = bill_length_mm, y = fit),
                  col = '#846267') +
        labs(x = 'Bill length (mm)', y = 'Body mass (g)', title = 'Incerteza sobre uma nova observação') +
        theme_light(base_size = 14)


p_ci + p_pi



#~~~~~~~~~~~~~~~~~
# 4.3) ANOVA
#~~~~~~~~~~~~~~~~~
# O peso médio difere entre as espécies?

mod_anova <- lm(body_mass_g ~ species, data = df)
head(model.matrix(mod_anova)) # Matriz de design

summary(mod_anova)
# - Intercept: peso médio da espécie de referência (Adelie)
# - speciesChinstrap: diferença no peso médio entre Chinstrap e Adelie; Peso Chinstrap = 4445.48 + 157.06
# - speciesGentoo: diferença no peso médio entre Gentoo e Adelie; Peso Gentoo = 4445.48 + 992.17

anova(mod_anova) # O peso difere entre as espécies?


## Visualizando a média estimada por espécie com IC 95%
df_pred <- data.frame(species = levels(df$species))


### Armazenando tudo em um data frame
pred_anova <- predict(mod_anova, newdata = df_pred, interval = "confidence")
df_pred <- cbind(df_pred, pred_anova)


#### Visualizando....
ggplot() +
  geom_jitter(data = df, aes(x = species, y = body_mass_g, col = species),
              width = 0.15, size = 2, alpha = 0.15) +
  geom_pointrange(data = df_pred, aes(x = species, y = fit, ymin = lwr, ymax = upr),
                  size = 1, linewidth = 1, col = 'gray20') +
  scale_color_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
  labs(x = NULL, y = 'Body mass (g)') +
  theme_light(base_size = 14) +
  theme(legend.position = 'none')



# 4.4) ANCOVA
#~~~~~~~~~~~~~~
# O peso varia entre espécies depois de controlar pelo comprimento do bico?
# Cada espécie tem seu próprio intercepto, porém compartilham a mesma inclinação

mod_ancova <- lm(body_mass_g ~ bill_length_mm + species, data = df)
head(model.matrix(mod_ancova)) # Matriz de design
anova(mod_ancova)

summary(mod_ancova)
# - Intercept: peso médio da espécie de referência (Adelie) quando o comprimento do bico = 0
# - bill_length_mm: efeito do comprimento do bico sobre o peso do pinguim
# - speciesChinstrap: diferença no peso médio entre Chinstrap e Adelie quando comprimento do bico = 0
# - speciesGentoo: diferença no peso médio entre Gentoo e Adelie quando comprimento do bico = 0


## A adição do efeito 'comprimento do bico' melhora o ajuste do modelo?
anova(mod_anova, mod_ancova)
#anova(mod_anova, mod_ancova, test = 'LRT') # Likelihood Ratio Test

## O efeito das espécies muda quando controlamos pelo tamanho do bico?
cbind(ANOVA  = coef(mod_anova)[c("speciesChinstrap", "speciesGentoo")], # Diferença 'bruta' no peso médio entre cada espécie 
      ANCOVA = coef(mod_ancova)[c("speciesChinstrap", "speciesGentoo")]) # Diferença de peso médio entre as espécies com o mesmo comprimento do bico


## Visualizando os valores ajustados
pred_df <- df %>%
           group_by(species) %>%
           reframe(bill_length_mm = seq(min(bill_length_mm), max(bill_length_mm), length.out = 100))

pred_df$fit <- predict(mod_ancova, newdata = pred_df) # Retorna apenas os valores ajustados

p_ancova <- ggplot() +
            geom_point(data = df, aes(x = bill_length_mm, y = body_mass_g, col = species),
                       size = 3, alpha = 0.15) +
            geom_line(data = pred_df, aes(x = bill_length_mm, y = fit, col = species), linewidth = 1.5) +
            scale_color_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
            labs(x = 'Bill length (mm)', y = 'Body mass (g)', col = 'Species') +
            theme_light(base_size = 14)

p_ancova



# 4.5) ANCOVA com interação
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A relação peso-comprimento é a mesma para todas as espécies? 
# Cada espécie tem seu próprio intercepto e inclinação

mod_ancova2 <- lm(body_mass_g ~ bill_length_mm * species, data = df)
head(model.matrix(mod_ancova2)) # Matriz de design

summary(mod_ancova2)

# - Intercept: peso médio da espécie de referência (Adelie) quando o comprimento do bico = 0
# - bill_length_mm: efeito do comprimento do bico sobre o peso do pinguim
# - speciesChinstrap: diferença no peso médio entre Chinstrap e Adelie quando comprimento do bico = 0
# - speciesGentoo: diferença no peso médio entre Gentoo e Adelie quando comprimento do bico = 0
# - Diferença de inclinação de Chinstrap em relação à espécie de referência (Adelie)
# - Diferença de inclinação de Gentoo em relação à espécie de referência (Adelie)


## A interação é significativa? 
anova(mod_ancova, mod_ancova2)
#anova(mod_ancova, mod_ancova2, test = 'LRT') # Likelihood Ratio Test

anova(mod_ancova2)



# 4.6) Comparando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

models <- list(mod_t, mod_lm, mod_anova, mod_ancova, mod_ancova2)
names(models) <- c("t-test", "lm", "anova", "ancova", "ancova-interaction")


tab_modelos <- data.frame(modelo = names(models),
                          k = sapply(models, function(m) attr(logLik(m), "df")),   # nº de parâmetros (inclui sigma)
                          sigma  = sapply(models, sigma),
                          R2_adj = sapply(models, function(m) summary(m)$adj.r.squared),
                          AIC = sapply(models, AIC)) %>%
                mutate(dAIC = AIC - min(AIC)) %>%
                arrange(AIC)

tab_modelos

# ATENÇÃO: modelos aninhados podem ser comparados via teste de razão de verossimilhança (Likelihood Ratio Teste - via função anova());
# modelos não aninhados (ex.: t-test por sexo vs. ANOVA por espécie) só podem ser comparados via critérios de informação (e.x., AIC)



# 4.7) Diagnóstico do modelo selecionado
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_model(mod_ancova2)

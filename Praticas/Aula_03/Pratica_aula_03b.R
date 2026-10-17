#############################################################################
#                                                                           #
#                         Prática das aulas 2 & 3                           #
#          - Estimação do tamanho de primeira maturação (L50) -             #
#                                                                           #
#############################################################################


# CONTEXTO:
# Os dados utilizados nesta prática são provenientes de amostragens realizadas por meio de pesca experimental ao longo da costa do Rio Grande do Norte, 
# entre os anos de 2012 e 2014. Para esta análise, foram selecionados os dados biológicos de Lutjanus synagris (ariocó), um importante recurso pesqueiro na região Nordeste do Brasil.
# O conjunto de dados compreende informações de 89 indivíduos, incluindo comprimento corporal, sexo e estágio de maturação gonadal.
# A partir dessas informações, iremos investigar a relação entre o comprimento corporal e a maturidade sexual e
# estimar o comprimento de primeira maturação (L50). O L50 é definido como o comprimento no qual um indivíduo apresenta 50% de probabilidade de estar 
# sexualmente maduro. Para isso, utilizaremos modelos de regressão logística Bayesianos para descrever a relação entre comprimento corporal,
# sexo e maturidade sexual.


# OBJETIVOS:
# - Entender o efeito do comprimento e sexo sobre a probabilidade do peixe estar maturo;
# - Estimar o comprimento no qual a maturidade esperada é de 50% (L50)


# ESTRUTURA DOS DADOS
# $Length = comprimento corporal (cm)
# $Sex = sexo (F = fêmea; M = macho)
# $Maturity_stage = estágio de maturação (A-D)
# Posteriormente, os estágios de maturação serão convertidos em uma variável binária:
#   0 = imaturo (A/B)
#   1 = maturo   (C/D)


# ABORDAGEM ANALÍTICA:
# O estado de maturação de cada indivíduo é uma variável binária (imaturo/maturo) e pode
# ser descrito de acordo com uma distribuição de probabilidade de Bernoulli, conforme:
#         Y_i ~ Bernoulli(p_i)
#     logit(p_i) = beta_0 + beta_1*x_1
# onde p_i representa a probabilidade de o indivíduo i estar maturo.

# Para mais informações, acesse o artigo: https://www.sciencedirect.com/science/article/abs/pii/S030438001730073X



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) # Manipulação de dados
library(tidyr) # Manipulação de dados
library(brms) # Modelos Bayesianos
library(ggplot2) # Visualização de dados
library(patchwork) # Visualização de dados (figuras com +1 painel)



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~

df <- readRDS("Dados/maturity.rds")
head(df)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Tratamento dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Classificando os indivíduos como maturos (1) e imaturos (0)
# Maturos: estágios C, D
# Imaturos: estágios A, B

df <- df %>% 
      mutate(Maturity = ifelse(Maturity_stage %in% c("A", "B"), 0, 1))

table(df$Maturity_stage)
table(df$Maturity)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Breve análise exploratória
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Comprimento por sexo e estado de maturação
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df %>%
  group_by(Sex, Maturity) %>%
  summarise(n = n(),
            min = min(Length),
            mean = mean(Length),
            max = max(Length))

table(df$Sex, df$Maturity)



# 3.2) Relação entre comprimento e maturidade
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(df, aes(x = Length, y = Maturity, col = Sex, fill = Sex)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.4, size = 4) +
  #facet_wrap(~ Sex) +
  scale_colour_manual(values = c("#e07a5f", "gray40")) +
  scale_fill_manual(values = c("#e07a5f", "gray40")) +
  stat_smooth(method = "glm", method.args = list(family = binomial(link = "logit")), se = TRUE, alpha = 0.2) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  theme_default()



#~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Rodando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~


# 4.1) Centralizando os comprimentos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Centralizamos o comprimento em torno de sua média para facilitar a interpretação do intercepto. 
# Assim, Length_c = 0 corresponde ao comprimento médio da amostra e beta_0 representa o log-odds de maturidade nesse comprimento.
df$Length_c <- df$Length - mean(df$Length)



# 4.2) Rodando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(mc.cores = 4, brms.backend = "cmdstanr") # Para acelerar a computação

# MCMC inputs
CHAINS <- 3 # Número de cadeias
ITER <- 5000 # Número de iterações (amostragens)
WARMUP <- 1000 # Número de iterações para descartar
THIN <- 5 # Reter cada 5o valor



## Prioris default
get_prior(Maturity ~ Length_c * Sex,
          data = df,
          family = bernoulli(link = "logit"))



## Modelo 1: efeito apenas do comprimento sobre a maturidade 
# Considera uma única relação comprimento-maturidade para ambos os sexos (mesmo intercepto e mesmo efeito do comprimento)
mod1 <- brm(Maturity ~ Length_c,
            data = df,
            family = bernoulli(link = 'logit'),
            chains = CHAINS,
            iter = ITER,
            warmup = WARMUP,
            thin = THIN)



## Modelo 2: efeito do comprimento & diferença entre sexos (i.e., machos e fêmeas diferem quanto ao tamanho de maturação)
# Considera que machos e fêmeas podem diferir na probabilidade de maturidade, porém o efeito do comprimento é o mesmo para ambos os sexos (slopes iguais)
mod2 <- brm(Maturity ~ Length_c + Sex,
            data = df,
            family = bernoulli(link = 'logit'),
            chains = CHAINS,
            iter = ITER,
            warmup = WARMUP,
            thin = THIN)



## Modelo 3: efeito do comprimento, diferença entre sexos & interação
# Considera que machos e fêmeas podem diferir na probabilidade de maturidade e também no efeito do comprimetno (interceptos e slopes diferentes)
mod3 <- brm(Maturity ~ Length_c*Sex,
            data = df,
            family = bernoulli(link = 'logit'),
            chains = CHAINS,
            iter = ITER,
            warmup = WARMUP,
            thin = THIN)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Comparação & diagnóstico dos modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 5.1) Comparação de modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loo1 <- loo(mod1)
loo2 <- loo(mod2)
loo3 <- loo(mod3)

loo_compare(loo1, loo2, loo3) # Não há evidência de ganho substancial na capacidade preditiva ao adicionar sexo e sua interação com comprimento

# Esse resultado sugere que o comprimento contém a maior parte da informação preditiva relevante para descrever a maturidade de L. synagris.
# Adicionar sexo, portanto, não melhora a predição, nem quando se permite que machos e fêmeas tenham relações comprimento–maturidade diferentes.


# 5.2) Diagnóstico dos modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(mod1)
plot(mod3)

pp_check(mod1, type = "bars", ndraws = 100)
pp_check(mod3, type = "bars", ndraws = 100)


summary(mod3)
# Intercepto: log-odds de maturidade de uma fêmea com comprimento médio. Para uma fêmea de comprimento médio, a probabilidade esperada de estar madura é aproximadamente 57%.
# Length_c: efeito do comprimento para as fêmeas. Como todo o IC-95% > 0, os dados dão suporte a uma relação positiva entre comprimento e probabilidade de maturidade nas fêmeas.
# SexM: para um peixe de comprimento médio, o modelo estima probabilidades de maturidade muito semelhantes entre os sexos
# Length_c:SexM:  diferença entre o efeito do comprimento (slope) de machos e fêmeas. Valores próximos de zero indicam pouca evidência de que a relação comprimento-maturidade apresente inclinações distintas entre os sexos.


# Em tese, não há distinção entre os L50%, mas vamos rodar os modelos separadamente e avaliar
# se há diferença entre os L50 das fêmeas e dos machos (teste de hipótese bayesiano!).



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6) Estimando o L50 separadamente para machos e fêmeas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Apesar dos resultados anteriores não sugerirem diferenças entre sexos, iremos
# estimar e explorar separadamente as distribuições posteriores de L50 para machos e fêmeas.


# 6.1) Filtrando o banco de dados por sexo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tapply(df$Length, df$Sex, mean)

df_f <- subset(df, Sex == 'F')
df_m <- subset(df, Sex == 'M')


## Re-centralizando os comprimentos
df_f$Length_c <- df_f$Length - mean(df_f$Length) # Femeas
df_m$Length_c <- df_m$Length - mean(df_m$Length) # Machos



# 6.2) Modelo para as fêmeas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_f <- brm(Maturity ~ Length_c,
             data = df_f,
             family = bernoulli(link = 'logit'),
             chains = CHAINS,
             iter = ITER,
             warmup = WARMUP,
             thin = THIN)


# 6.3) Modelo para os machos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_m <- brm(Maturity ~ Length_c,
             data = df_m,
             family = bernoulli(link = 'logit'),
             chains = CHAINS,
             iter = ITER,
             warmup = WARMUP,
             thin = THIN)



# 6.4) Diagnóstico dos modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(mod_f)
plot(mod_m)

summary(mod_f) # Os dados fornecem evidência de que, para as fêmeas, a probabilidade de estar madura aumenta com o comprimento
summary(mod_m) # Menos suporte para os machos (IC-95% inclui valor zero)


# 6.5) Extraindo as distribuições posteriores
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Fêmeas
post_f <- as_draws_df(mod_f)
names(post_f)
dim(post_f)
head(post_f)


## Machos
post_m <- as_draws_df(mod_m)


## Visualizando os parâmetros conjuntamente 
pf <- ggplot(post_f, aes(x = b_Intercept, y = b_Length_c)) +
      geom_point(alpha = 0.3, size = 4, col = "#e07a5f") +
      geom_density2d(col = 'black') + 
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      labs(title = 'Fêmeas', x = expression(beta[0]), y = expression(beta[1])) +
      theme_default() +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      

pm <- ggplot(post_m, aes(x = b_Intercept, y = b_Length_c)) +
      geom_point(alpha = 0.3, size = 4, col = "gray40") +
      geom_density2d(col = 'black') + 
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      labs(title = 'Machos', x = expression(beta[0]), y = expression(beta[1])) +
      theme_default() +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

pf + pm



# Para as fêmeas, a posterior de beta_1 encontra-se predominantemente acima de zero, indicando suporte para uma relação positiva entre 
# comprimento e probabilidade de maturação. 
# Para os machos, beta_1 também se concentra principalmente em valores positivos, porém a posterior se aproxima e ultrapassa zero, indicando maior
# incerteza quanto à magnitude e até mesmo à direção desse efeito.


## Visualizando os parâmetros separadamente (beta_1)

### Fêmeas
beta_f <- ggplot(post_f, aes(x = b_Length_c)) +
          geom_density(col = "#e07a5f", fill = "#e07a5f", alpha = 0.4) +
          geom_vline(xintercept = 0, linetype = "dashed") +
          labs(title = "Fêmeas",
               x = expression(beta[1]),
               y = "Densidade posterior") +
          theme_default() +
          theme(plot.title = element_text(hjust = 0.5, face = 'bold'))


### Machos
beta_m <- ggplot(post_m, aes(x = b_Length_c)) +
          geom_density(col = "gray40", fill = "gray40", alpha = 0.4) +
          geom_vline(xintercept = 0, linetype = "dashed") +
          labs(title = "Machos",
               x = expression(beta[1]),
               y = "Densidade posterior") +
          theme_default() +
          theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

beta_f + beta_m


# Probabilidade posterior de o efeito do comprimento ser negativo nos machos
mean(post_m$b_Length_c < 0)

# Equivalentemente, probabilidade posterior de o efeito ser positivo:
mean(post_m$b_Length_c > 0)

     
     
     
# 6.6) Calculando a distribuição posterior de L50
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# O modelo logístico pode ser escrito como: logit(p) = beta_0 + beta_1 * Length_c
# Por definição, L50 corresponde ao comprimento em que p = 0.5, já que logit(0.5) = 0.
# Assim, 0 = beta_0 + beta_1 * L50_c
# Portanto, L50_c = - beta_0 / beta_1
# Como Length_c representa o comprimento centralizado, precisamos adicionar novamente o comprimento médio (Lbar)
# para retornar à escala original (cm).
# Portanto, L50 = (- beta_0 / beta_1) + Lbar


# Comprimentos médios por sexo
(mean_f <- mean(df_f$Length))
(mean_m <- mean(df_m$Length))


# Calculando o L50 para CADA draw da posterior (..e propagação da incerteza!)
post_f$L50 <-  (-post_f$b_Intercept/post_f$b_Length_c) + mean_f
post_m$L50 <-  (-post_m$b_Intercept/post_m$b_Length_c) + mean_m



# 6.7) Analisando a distribuição posterior de L50
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
quantile(post_f$L50,c(0.001,.025,0.25,.5,0.75,.975,0.999))
quantile(post_m$L50,c(0.001,.025,0.25,.5,0.75,.975,0.999)) # IC-95% bem mais amplo do que nas fêmeas


par(mfrow = c(1,2))
hist(post_f$L50[post_f$L50 > 20 & post_f$L50 < 29], main="Fêmeas", ylab="Frequência", xlab="L50")
abline(v = quantile(post_f$L50, 0.5), col = 'red')
hist(post_m$L50[post_m$L50 > 9 & post_m$L50 < 37],  main="Machos", ylab="Frequência", xlab="L50")
abline(v = quantile(post_m$L50, 0.5), col = 'red')
par(mfrow = c(1,1))


# Organizando as posteriores em um único df (para visualização via ggplot)
df_l50 <- data.frame(L50 = c(post_f$L50, post_m$L50),
                     Sex = rep(c("Fêmea", "Macho"), c(nrow(post_f), nrow(post_m))))


# Visualizando no ggplot (mais bonitinho...)
df_l50 %>% 
  group_by(Sex) %>% 
  filter(L50 >= quantile(L50, 0.025), L50 <= quantile(L50, 0.975)) %>% 
  ggplot(aes(x = L50, fill = Sex, colour = Sex)) +
  geom_density(alpha = 0.25) +
  scale_fill_manual(values = c("#e07a5f", "gray40")) +
  scale_colour_manual(values = c("#e07a5f", "gray40")) +
  labs(x = expression(L[50]~"(cm)"),
       y = "Densidade posterior",
       fill = "Sexo",
       colour = "Sexo") +
  theme_default() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.1, 0.85)) 




# 6.8) Comparação posterior do L50 entre os sexos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pergunta: há evidência de diferença entre o L50 de fêmeas e machos?
# Na abordagem Bayesiana, essa comparação pode ser realizada diretamente a partir das
# distribuições posteriores de L50. Para cada draw, calculamos a diferença entre o L50
# estimado para fêmeas e machos: diff = L50_fêmeas - L50_machos
# - diff > 0 -> L50 maior nas fêmeas
# - diff < 0 -> L50 maior nos machos

# A proporção de draws acima e abaixo de zero fornece as probabilidades de o L50 ser 
# maior nas fêmeas ou nos machos, respectivamente. Para avaliar a força da evidência
# em favor de uma dessas possibilidades, calculamos a razão entre essas probabilidades conforme:
# prob_ratio = p(diff >= 0) / p(diff < 0)

# - prob_ratio > 5    -> evidência em favor de L50_fêmeas > L50_machos
# - prob_ratio < 1/5  -> evidência em favor de L50_fêmeas < L50_machos
# Valores entre 1/5 e 5 indicam que  a distribuição posterior não fornece evidência 
# suficientemente forte em favor de uma das duas possibilidades.


# Calculando a diferença
diff_L50 <- post_f$L50 - post_m$L50

quantile(diff_L50, c(0.001,.025,0.25,.5,0.75,.975,0.999)) # A distribuição posterior permite quantificar diretamente a magnitude e a incerteza associadas à diferença entre os L50.
hist(diff_L50[diff_L50 >- 14 & diff_L50 < 18], nclass = 80, main = "", ylab = "Frequência", xlab = "Diferença entre os valores simulados")
abline(v=0,col=2,lwd=2)


# Probabilidades posteriores
p_female <- mean(diff_L50 >= 0) # Probabilidade de L50 ser maior nas fêmeas do que nos machos
p_male <- mean(diff_L50 < 0) # Probabilidade de L50 ser maior nos machos

p_female
p_male # 1-p_female


# Razão entre as probabilidades posteriores
prob_ratio <- p_female / p_male
prob_ratio # Não sustenta a hipótese de que L50 seja distinto entre os sexos
# Para os dados em questão, temos evidências para suportar a ideia de que, para fins de manejo pesqueiro,
# podemos considerar um único L50 para a espécie em questão.

# IMPORTANTE: ausência de evidência de diferença não implica evidência de igualdade.


# 6.9) Curvas de maturação (ajustadas)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Definir os dados para predição

## Fêmeas 
new_f <- data.frame(Length_c = seq(min(df_f$Length_c), 
                                   max(df_f$Length_c), length.out = 100))

## Machos 
new_m <- data.frame(Length_c = seq(min(df_m$Length_c), 
                                   max(df_m$Length_c), length.out = 100))



# Calcular os valores preditos (da distribuição posterior)
## Fornece draws da distribuição posterior da probabilidade esperada de maturação para cada comprimento.

## Fêmeas 
pred_f <- posterior_epred(object = mod_f,
                          newdata = new_f)



## Machos 
pred_m <- posterior_epred(object = mod_m,
                          newdata = new_m)



# Calculando as estatísticas

## Fêmeas
new_f$mean <- apply(pred_f, 2, mean)
new_f$lower <- apply(pred_f, 2, quantile, 0.025)
new_f$upper <- apply(pred_f, 2, quantile, 0.975)

## Machos
new_m$mean  <- apply(pred_m, 2, mean)
new_m$lower <- apply(pred_m, 2, quantile, 0.025)
new_m$upper <- apply(pred_m, 2, quantile, 0.975)


# Retornando os comprimentos para a escala original
new_f$Length <- new_f$Length_c + mean_f
new_m$Length <- new_m$Length_c + mean_m


# Juntando os dados
new_f$Sex <- "Fêmeas"
new_m$Sex <- "Machos"

preds <- bind_rows(new_f, new_m)


# Visualizando as curvas de maturação preditas
ggplot(preds, aes(x = Length, y = mean, colour = Sex, fill = Sex)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_colour_manual(values = c("#e07a5f", "gray40")) +
  scale_fill_manual(values = c("#e07a5f", "gray40")) +
  labs(x = "Comprimento (cm)",
       y = "Probabilidade de maturação",
       colour = "Sexo",
       fill = "Sexo") +
  theme_default() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.1, 0.85)) 

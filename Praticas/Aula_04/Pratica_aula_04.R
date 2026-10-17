#############################################################################
#                                                                           #
#                            Prática da aula 04                             #
#                       - Modelo Linear Misto (LMM) -                       #
#                                                                           #
#############################################################################


# CONTEXTO:
# Nesta prática, serão utilizados dados que simulam o monitoramento da ictiofauna recifal para explorar o ajuste
# e a interpretação de Modelos Lineares Mistos (LMMs) em uma abordagem Bayesiana. 
# A prática será dividida em duas partes: 
# - (1) introdução aos LMMs 
# - (2) estrutura dos efeitos aleatórios.

# DADOS:
# - Variável resposta: biomassa de peixes (biomassa_kg_m2; contínua, em kg/m2)
# - Covariáveis: temperatura superficial do mar (sst; contínua) e fatores de agrupamento com medidas repetidas:
#   recife (n = 5), ano (n = 3), mês (n = 12), transecto e mergulhador (n = 3)

# OBJETIVOS:
# - Compreender a lógica dos Modelos Lineares Mistos (LMMs) e como eles lidam com a dependência entre observações (medidas repetidas)
# - Interpretar os efeitos fixos e aleatórios, incluindo a partição da variância entre os níveis de agrupamento
# - Avaliar como diferentes estruturas de efeitos aleatórios influenciam a inferência e a capacidade preditiva dos modelos

# ABORDAGEM ANALÍTICA:
# - Modelos Bayesianos ajustados via brms (distribuição Gaussiana)

# > PARTE 1 - Introdução aos LMMs:
# 1) LM com fator (ANOVA)
#     - A biomassa média difere entre os recifes?
# 2) LM com fator & var. numérica (ANCOVA)
#     - A relação entre biomassa e SST difere entre os recifes?
# 3) LMM com intercepto aleatório
#     - Quanto da variabilidade da biomassa está associada às diferenças
#      entre recifes?
# 4) LMM com intercepto e inclinação aleatórios
#     - A relação entre biomassa e SST varia entre os recifes?

# > PARTE 2 - Estrutura dos efeitos aleatórios
# - Explorar diferentes estruturas de efeitos aleatórios para representar as fontes de dependência presentes no desenho amostral



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) # Manipulação de dados
library(brms) # Modelos Bayesianos
library(bayesplot) # Visualização dos resultados Bayesianos (extra)
library(tidybayes) # Facilita a extração e visualização da distribuição posterior
library(emmeans) # Computar médias marginais
library(ggplot2) # Visualização dos dados/resultados
library(GGally) # Gráfico de multicolinearidade
library(patchwork) # Visualização de dados (figuras com +1 painel)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Customizando o tema do ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_theme <- theme_bw(base_size = 12) + 
            theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                  axis.text.x = element_text(colour = "grey20", margin = margin(t = 10)),
                  axis.text.y = element_text(colour = "grey20", margin = margin(r = 10)),
                  axis.title = element_text(face = "bold", colour = "black", size = 12),
                  strip.background = element_rect(fill = 'white'),
                  legend.background = element_rect(fill = "transparent", colour = NA),
                  legend.box.background = element_rect(fill = "transparent", colour = NA),
                  plot.background   = element_rect(fill = "transparent", color = NA),
                  panel.background  = element_rect(fill = "transparent", color = NA),
                  panel.grid.major.y = element_line(linetype = 'dotted'),
                  panel.grid.major.x = element_line(linetype = 'dotted'),
                  axis.line = element_line(color = "black"),
                  plot.margin = margin(5, 10, 5, 5))


# Paleta de cores
my_cols <- c("#846267", "#588157", "#e07a5f", "#457b9d", "#f6bd60") 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando funções auxiliares
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("Praticas/auxiliary_functions.R")



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/reef_biomass.rds")
str(df)


## Convertendo variáveis para fator
df[, c("id_transecto", "recife", "ano", "mes", "transecto", "mergulhador")] <- lapply(df[, c("id_transecto", "recife", "ano", "mes", "transecto", "mergulhador")], factor)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Análise exploratória dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 2.1) Cobertura espaço-temporal das amostragens
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(df$ano, df$mes)
table(df$ano, df$recife)
table(df$mes, df$recife)



# 2.2) Variável-resposta (biomassa)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Histograma
phist <- ggplot(df, aes(x = biomassa_kg_m2)) +
         geom_histogram(bins = 10,fill = '#457b9d', col = 'white', alpha = 0.8, linewidth = 1) +
         coord_cartesian(expand = c(0,0)) +
         labs(x = 'Biomassa (Kg/m2)', y = 'Frequência') +
         my_theme

## Boxplot
pbox <- ggplot(df, aes(y = biomassa_kg_m2)) +
        geom_boxplot(col = '#457b9d', fill = 'white', linewidth = 1, outlier.size = 3) +
        labs(y = 'Biomassa (Kg/m2)') +
        my_theme


## Visualizando...
phist + pbox 



# 2.3) Variável-resposta vs. preditores
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.3.1) Tendências temporais
## Anual
ggplot(df, aes(y = biomassa_kg_m2, x = ano)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 1, outlier.size = 3) +
  labs(y = 'Biomassa (Kg/m2)') +
  my_theme

## Mensal
ggplot(df, aes(y = biomassa_kg_m2, x = mes)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 1, outlier.size = 3) +
  labs(y = 'Biomassa (Kg/m2)') +
  my_theme

## Mes-Ano
ggplot(df, aes(y = biomassa_kg_m2, x = mes)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 0.6) +
  labs(y = 'Biomassa (Kg/m2)') +
  facet_wrap(ano ~ .) +
  my_theme



# 2.3.2) Tendências espaciais
ggplot(df, aes(y = biomassa_kg_m2, x = recife)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 0.6) +
  labs(y = 'Biomassa (Kg/m2)') +
  #facet_wrap(ano ~ .) +
  my_theme



# 2.3.3) Variabilidade entre mergulhadores
ggplot(df, aes(y = biomassa_kg_m2, x = mergulhador)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 0.6) +
  labs(y = 'Biomassa (Kg/m2)') +
  my_theme



# 2.3.4) Variabilidade das pseudo-réplicas
ggplot(df, aes(y = biomassa_kg_m2, x = transecto)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 0.6) +
  labs(y = 'Biomassa (Kg/m2)') +
  facet_wrap(recife ~ ., nrow = 1) +
  my_theme


# 2.3.5) Influência da temperatura superficial do mar (sst)
ggplot(df, aes(y = biomassa_kg_m2, x = sst)) +
  geom_point(col = '#457b9d', size = 3, alpha = 0.5) +
  #geom_smooth(method = 'lm', col = 'black') +
  labs(y = 'Biomassa (Kg/m2)', x = "SST (°C)") +
  my_theme


## A relação biomassa~sst muda entre os recifes?
ggplot(df, aes(y = biomassa_kg_m2, x = sst)) +
  geom_point(col = '#457b9d', size = 3, alpha = 0.5) +
  geom_smooth(method = 'lm', col = 1) +
  labs(y = 'Biomassa (Kg/m2)', x = "SST (°C)") +
  my_theme


ggplot(df, aes(y = biomassa_kg_m2, x = sst, col = recife, fill = recife)) +
  geom_point( size = 3, alpha = 0.5) +
  scale_color_manual(values = my_cols) +
  scale_fill_manual(values = my_cols) +
  geom_smooth(method = 'lm') +
  labs(y = 'Biomassa (Kg/m2)', x = "SST (°C)") +
  my_theme



#~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Modelagem - parte 1
#~~~~~~~~~~~~~~~~~~~~~~~~~
options(mc.cores = 4, brms.backend = "cmdstanr") # Para acelerar a computação



# 3.1) ANOVA
#~~~~~~~~~~~~~~
# Pergunta: Os recifes apresentam diferentes biomassas médias?
# Recife é considerado como efeito fixo; portanto, estimamos diretamente as diferenças entre os recifes amostrados.
# Os efeitos dos recifes são estimados separadamente, sem compartilhamento de informação entre eles (no pooling).

mod_anova <- brm(biomassa_kg_m2 ~ recife,
                 data = df,
                 family = gaussian())



## Avaliando convergência e ajuste
mcmc_trace(mod_anova)

p1 <- pp_check(mod_anova)
p2 <- pp_check(mod_anova, "scatter_avg")
p3 <- pp_check(mod_anova, type = 'stat', stat = 'mean')

(p1 + p2 + p3)

performance::check_model(mod_anova) 


## Avaliando os resultados
summary(mod_anova)
conditional_effects(mod_anova)
#mcmc_dens(mod_anova)


## Os recifes diferem quanto às suas biomassas médias?
### Contraste par-a-par entre as médias dos recifes ('Tipo' teste post-hoc de Tukey)

emm_recife <- emmeans(mod_anova, ~ recife)
emm_recife

contrastes <- pairs(emm_recife) # Comparações par-a-par
contrastes


contrastes %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value,y = contrast)) +
  stat_halfeye(.width = c(0.80, 0.95), point_interval = median_qi, col = 'darkred') +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs( x = "Diferença posterior na biomassa média (kg/m²)", y = NULL) +
  my_theme



# 3.2) ANCOVA
#~~~~~~~~~~~~~~
# Pergunta: a relação entre biomassa e SST difere entre os recifes?
# A interação recife:SST permite que a relação entre biomassa e SST varie entre os recifes.
# Como recife é considerado efeito fixo, são estimadas diferenças de intercepto e inclinação entre os recifes amostrados.
# Os efeitos dos recifes são estimados sem compartilhamento de informação entre eles (no pooling).


## Centralizando a covariável (intercepto expresso na SST média)
df$sst_c <- df$sst - mean(df$sst, na.rm = T)

mod_ancova <- brm(biomassa_kg_m2 ~ recife*sst_c,
                  data = df,
                  family = gaussian())


## Avaliando convergência/ajuste
mcmc_trace(mod_ancova)
performance::check_model(mod_ancova) 

summary(mod_ancova)
conditional_effects(mod_ancova, effects = "sst_c:recife")



# 3.3) LMM com intercepto aleatório
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pergunta: quanto da variabilidade da biomassa pode ser associada às diferenças entre recifes?
# Recife é considerado como efeito aleatório; cada recife apresenta um desvio próprio em relação ao intercepto populacional (i.e., média global)
# Há compartilhamento parcial de informação (partial pooling), i.e., as estimativas específicas dos recifes compartilham informação por meio de uma distribuição comum dos efeitos de recife.
# Tipo ANOVA

mod_lmm1 <- brm(biomassa_kg_m2 ~  (1 | recife),
                data = df,
                #control = list(adapt_delta = 0.99),
                #seed = 1234,
                family = gaussian())


## Avaliando convergência/ajuste
mcmc_trace(mod_lmm1)
performance::check_model(mod_lmm1) 


## Avaliando os resultados
summary(mod_lmm1)
# há uma variação considerável entre os níveis médios de biomassa dos diferentes recifes. Repare, contudo, no IC-95%!


### Calculando o coeficiente de correlação intraclasse (ICC)
post_df <- as_draws_df(mod_lmm1) # Extraindo a distribuição posterior dos parâmetros
var_entre  <- post_df$sd_recife__Intercept^2 # Variância entre recifes (sigma_a^2)
var_dentro <- post_df$sigma^2 # Variância residual (sigma^2)

icc <- var_entre / (var_entre + var_dentro)
median(icc) # Proporção da variância atribuível à diferença entre recifes
quantile(icc, probs = c(0.025, 0.975))



### Visualização do efeito aleatório dos recifes (desvios em relação à média global)
mod_lmm1 %>%
  spread_draws(r_recife[recife, parameter]) %>% 
  ggplot(aes(x = r_recife, y = recife)) +
  stat_pointinterval(.width = c(0.80, 0.95), color = "#2b5c8f") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Desvio em relação à média", y = "Recife") +
  my_theme


### Visualização do efeito médio de cada recife (intercepto_recife = \beta_0 + b_1j )
mod_lmm1 %>%
  spread_draws(b_Intercept, r_recife[recife, parameter]) %>% 
  mutate(intercepto_recife = b_Intercept + r_recife) %>% 
  ggplot(aes(x = intercepto_recife,
             y = recife)) +
  #stat_pointinterval(.width = c(0.80, 0.95),  color = "#2b5c8f") +
  stat_halfeye(.width = c(0.80, 0.95), color = 1, fill = "#2b5c8f", alpha = 0.7) +
  geom_vline(xintercept = fixef(mod_lmm1)["Intercept", "Estimate"], linetype = "dashed", color = "gray50") +
  labs( x = "Biomassa média estimada (kg/m²)", y = "Recife") +
  my_theme



# 3.4) LMM com intercepto e inclinação aleatórios
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pergunta: a relação entre biomassa e SST varia entre os recifes?
# Cada recife apresenta seu próprio intercepto e sua própria inclinação para SST.
# Há compartilhamento parcial de informação (partial pooling) entre os recifes.
# Estrutura análoga à ANCOVA, mas com interceptos e inclinações variando aleatoriamente entre recifes.

mod_lmm2 <- brm(biomassa_kg_m2 ~ sst_c + (1 + sst_c | recife),
                data = df,
                #control = list(adapt_delta = 0.98),
                #seed = 1234,
                family = gaussian())


## Avaliando convergência/ajuste
mcmc_trace(mod_lmm2)
performance::check_model(mod_lmm2) 


## Avaliando os resultados
summary(mod_lmm2)



# Visualização dos efeitos aleatórios 
mod_lmm2 %>%
  spread_draws(r_recife[recife, parameter]) %>% 
  ggplot(aes(x = r_recife, y = recife)) +
  stat_halfeye(.width = c(0.80, 0.95), point_interval = median_qi, fill = "#2b5c8f", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ parameter, scales = "free" ) +
  labs(x = "Efeito aleatório", y = "Recife") +
  my_theme




# Visualizando as retas ajustadas

## Criando dados para predição

### Nível de grupo
pred_dfg <- expand.grid(sst_c = seq(min(df$sst_c),max(df$sst_c), length.out = 100),
                        recife = levels(df$recife))

### Nível populacional
pred_dfp <- expand.grid(sst_c = seq(min(df$sst_c),max(df$sst_c), length.out = 100),
                        recife = levels(df$recife)[1])




### Predições - nível de grupo (com efeitos aleatórios)
pred_group <- posterior_epred(mod_lmm2, newdata = pred_dfg, re_formula = NULL)
pred_group <- pred_dfg %>%
                mutate(Estimate = apply(pred_group, 2, median),
                       Q2.5 = apply(pred_group, 2, quantile, 0.025),
                       Q97.5 = apply(pred_group, 2, quantile, 0.975)) # Resumindo a distribuição posterior
head(pred_group)


### Predições - nível de população (sem efeitos aleatórios)
pred_pop <- posterior_epred(mod_lmm2, newdata = pred_dfp, re_formula = NA)
pred_pop <- pred_dfp %>%
  mutate(Estimate = apply(pred_pop, 2, median),
         Q2.5 = apply(pred_pop, 2, quantile, 0.025),
         Q97.5 = apply(pred_pop, 2, quantile, 0.975)) # Resumindo a distribuição posterior

head(pred_pop)


### Visualizando...
ggplot() +
  geom_ribbon(data = pred_group, aes(x = sst_c, ymin = Q2.5, ymax = Q97.5, fill = recife), alpha = 0.20, show.legend = FALSE) +
  geom_point(data = df, aes(x = sst_c, y = biomassa_kg_m2, color = recife), alpha = 0.3, size = 2) +
  geom_line(data = pred_group, aes(x = sst_c, y = Estimate, color = recife), linewidth = 1) +
  geom_line(data = pred_pop, aes(x = sst_c, y = Estimate), linewidth = 1.2, linetype = "dashed", color = "black") +
  scale_color_manual(values = my_cols) +
  scale_fill_manual(values = my_cols) +
  labs(x = "SST centralizada (°C)", y = expression("Biomassa (kg/m"^2*")"), color = "Recife") +
  my_theme




#~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Modelagem - parte 2
#~~~~~~~~~~~~~~~~~~~~~~~~~

# Até então, foi considerado apenas a variabilidade associada aos recifes.
# O desenho amostral, contudo, apresenta outras fontes potenciais de dependência e heterogeneidade (e.g. ano, mês, transecto e mergulhador)

# Nesta seção, iremos testar modelos com diferentes estruturas de efeitos aleatórios e avaliar como essas escolhas afetam:
# - a decomposição da variância
# - as estimativas dos efeitos fixos
# - a incerteza das estimativas
# - a capacidade preditiva e o ajuste dos modelos

# ATENÇÃO: a inclusão de efeitos aleatórios deve, também, refletir a estrutura do desenho amostral e as hipóteses do estudo; não apenas melhorar uma métrica de ajuste.


# 4.1) Modelos
#~~~~~~~~~~~~~~~~


# 4.1.0) Modelo nulo
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Para contraste

mod_r0 <- brm(biomassa_kg_m2 ~ 1,
              data = df,
              family = gaussian())



# 4.1.1) Variação espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Efeitos aleatórios: recife 
# Permite que o nível médio de biomassa varie entre recifes 

mod_r1 <- brm(biomassa_kg_m2 ~ sst_c + (1 | recife),
              data = df,
              family = gaussian())



# 4.1.2) Variação espaço-temporal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Efeitos aleatórios: recife e ano 
# - fatores cruzados: todos os recifes foram amostrados em diferentes anos
# Permite que o nível médio de biomassa varie entre anos e recifes

mod_r2 <- brm(biomassa_kg_m2 ~ sst_c + (1 | recife) + (1 | ano),
              data = df,
              family = gaussian())



# 4.1.3) Variação espaço-temporal II
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Efeitos aleatórios: recife, ano e mês (efeitos cruzados)
# O efeito de cada mês é assumido como recorrente entre os anos.
# Por exemplo, março de 2021, 2022 e 2023 compartilham o mesmo efeito.


mod_r3 <- brm(biomassa_kg_m2 ~ sst_c + (1 | recife) + (1 | ano) + (1 | mes),
              data = df,
              family = gaussian())



# 4.1.4) Variação espaço-temporal III
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Efeitos aleatórios: recife e mês aninhado no ano 
# Assume que 

mod_r4 <- brm(biomassa_kg_m2 ~ sst_c + (1 | recife) + (1 | ano / mes),
              data = df,
              family = gaussian())




## Qual estrutura temporal apresenta melhor capacidade preditiva?
mod_r2 <- add_criterion(mod_r2, "loo")
mod_r3 <- add_criterion(mod_r3, "loo")
mod_r4 <- add_criterion(mod_r4, "loo")

loo_compare(mod_r2, mod_r3, mod_r4)



# 4.1.5) Variação espaço-temporal & desenho amostral
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Efeitos aleatórios: recife, ano & mergulhador

# Permite representar a heterogeneidade associada aos diferentes mergulhadores
mod_r5 <- brm(biomassa_kg_m2 ~ sst_c + (1 | recife) + (1 | ano) + (1 | mergulhador),
              data = df,
              family = gaussian())


## Mergulhador como efeito fixo (nlevels < 5)
# mod_r5b <- brm(biomassa_kg_m2 ~ sst_c + mergulhador + (1 | recife) + (1 | ano), 
#               data = df,
#               family = gaussian())



# 4.2) Comparando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_r0 <- add_criterion(mod_r0, "loo")
mod_r1 <- add_criterion(mod_r1, "loo")
mod_r5 <- add_criterion(mod_r5, "loo")


comp_loo <- loo_compare(mod_r0, # modelo nulo
                        mod_r1, # efeito espacial
                        mod_r2, # efeito espaço-temporal
                        mod_r5) # efeito espaço-temporal & amostragem

print(comp_loo, simplify = T)





# 4.3) Avaliando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(mod_r5)

mcmc_trace(mod_r5)

# p1 <- pp_check(mod_r5)
# p2 <- pp_check(mod_r5, "scatter_avg")
# p3 <- pp_check(mod_r5, type = 'stat', stat = 'mean')
# 
# (p1 + p2 + p3)

performance::check_model(mod_anova) 


### Calculando o ICC
mlist <- list(mod_r1, mod_r2, mod_r5)
names(mlist) <- c("mod1", "mod2", "mod5")


df_var <- lapply(names(mlist), function(n) variance_partition(mlist[[n]], n))
df_var <- bind_rows(df_var)


## Gráfico da distribuição posterior do ICC por componente
ggplot(df_var, aes(x = Component, y = Proportion, fill = Component)) +
  stat_summary(fun = median, geom = "bar", position = "dodge", alpha = 0.85) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.1f%%", after_stat(y) * 100)), 
               vjust = -0.5,       
               size = 3.5,         
               fontface = "bold") +
  facet_wrap(~ model, scales = "free_x") +
  scale_fill_manual(values = my_cols) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Componentes de Variância", y = "Coeficiente de correlação \nintra-classe (ICC %)") +
  my_theme +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1))



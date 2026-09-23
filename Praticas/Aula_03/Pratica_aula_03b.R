#########################################
#                                       #
#         Prática da aula 02            #
#          - L50 bayesiano -            #
#                                       #
#########################################

# FIXME: melhorar o contexto - introduzir os dados do Lutjanus Synagris
# Contexto:
# O comprimento de primeira maturação (L50) corresponde ao comprimento no qual um indivíduo apresenta 50% de probabilidade de estar sexualmente
# maduro. Nesta prática, utilizaremos modelos de regressão logística Bayesianos para descrever a relação entre comprimento corporal,
# sexo e maturidade sexual.


# Objetivo: 
#   - 1) Entender o efeito do comprimento e sexo sobre a probabilidade do peixe estar maturo;
#   - 2) Estimar o comprimento no qual a maturidade esperada é de 50% (L50)

# Estrutura dos dados:
# $Length         = comprimento corporal (cm)
# $Sex            = sexo (F = fêmea; M = macho)
# $Maturity_stage = estágio de maturação (A-D)
# Posteriormente, os estágios de maturação serão convertidos em uma variável binária:
#   0 = imaturo (A/B)
#   1 = maturo   (C/D)


# Abordagem analítica:
# O estado de maturação de cada indivíduo é uma variável binária (imaturo/maturo) e pode
# ser descrito de acordo com uma distribuição de probabilidade de Bernoulli, conforme:
#         Y_i ~ Bernoulli(p_i)
#     logit(p_i) = beta_0 + beta_1*x_1
# onde p_i representa a probabilidade de o indivíduo i estar maturo.



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)
library(patchwork)


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
# Facilita interpretação: representa o log-odds de maturidade de um indivíduo com comprimento igual ao
# comprimento médio da população.
df$Length_c <- df$Length - mean(df$Length)



# 4.2) Rodando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(mc.cores = 4, brms.backend = "cmdstanr") # Para acelerar a computação

## Prioris default
get_prior(Maturity ~ Length_c * Sex,
          data = df,
          family = bernoulli(link = "logit"))



## Modelo 1: efeito apenas do comprimento sobre a maturidade 
# Considera uma única relação comprimento-maturidade para ambos os sexos (mesmo intercepto e mesmo efeito do comprimento)
mod1 <- brm(Maturity ~ Length_s,
            data = df,
            family = bernoulli(link = 'logit'))



## Modelo 2: efeito do comprimento & diferença entre sexos (i.e., machos e fêmeas diferem quanto ao tamanho de maturação)
# Considera que machos e fêmeas podem diferir na probabilidade de maturidade, porém o efeito do comprimento é o mesmo para ambos os sexos (slopes iguais)
mod2 <- brm(Maturity ~ Length_s + Sex,
            data = df,
            family = bernoulli(link = 'logit'))



## Modelo 3: efeito do comprimento, diferença entre sexos & interação
# Considera que machos e fêmeas podem diferir na probabilidade de maturidade e também no efeito do comprimetno (interceptos e slopes diferentes)
mod3 <- brm(Maturity ~ Length_s*Sex,
            data = df,
            family = bernoulli(link = 'logit'))





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
# Length_s: efeito do comprimento para as fêmeas. Como todo o IC-95% > 0, os dados dão suporte a uma relação positiva entre comprimento e probabilidade de maturidade nas fêmeas.
# SexM: para um peixe de comprimento médio, o modelo estima probabilidades de maturidade muito semelhantes entre os sexos
# Length_s:SexM: 


# Em tese, não há distinção entre os L50%, mas vamos rodar os modelos separadamente



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
df_f$Length_s <- df_f$Length - mean(df_f$Length) # Femeas
df_m$Length_s <- df_m$Length - mean(df_m$Length) # Machos



# 6.2) Modelo para as fêmeas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_f <- brm(Maturity ~ Length_s,
             data = df_f,
             family = bernoulli(link = 'logit'))


# 6.3) Modelo para os machos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_m <- brm(Maturity ~ Length_s,
             data = df_m,
             family = bernoulli(link = 'logit'))


# 6.4) Diagnóstico dos modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(mod_f)
plot(mod_m)

summary(mod_f)
summary(mod_m)



post_f <- as_draws_df(mod_f)
names(post_f)
head(post_f)


post_m <- as_draws_df(mod_m)


## Visualizando os parâmetros
pf <- ggplot(post_f, aes(x = b_Intercept, y = b_Length_s)) +
      geom_point(alpha = 0.3, size = 4, col = "#e07a5f") +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      labs(title = 'Fêmeas') +
      theme_bw()
      

pm <- ggplot(post_m, aes(x = b_Intercept, y = b_Length_s)) +
      geom_point(alpha = 0.3, size = 4, col = "gray40") +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      labs(title = 'Machos') +
      theme_bw()

pf + pm


post_f$L50 <- (- post_f$b_Intercept / post_f$b_Length_s) + mean(df_f$Length)
post_m$L50 <- (-post_m$b_Intercept / post_m$b_Length_s) + mean(df_m$Length)


quantile(post_f$L50, probs = c(0.025, 0.5, 0.975))
quantile(post_m$L50, probs = c(0.025, 0.5, 0.975))

par(mfrow = c(1,2))
hist(post_f$L50, nclass = 80)
hist(post_m$L50, nclass = 80)
par(mfrow = c(1,1))

L50_post <- data.frame(L50 = c(post_f$L50, post_m$L50),
                       sexo = rep(c("Fêmea", "Macho"), 
                                  c(nrow(post_f), nrow(post_m))))



ggplot(L50_post, aes(x = L50, fill = sexo)) +
  geom_density(alpha = 0.4) +
  labs(x = expression(L[50]~"(cm)"),
    y = "Densidade posterior",
    fill = "Sexo") +
  theme_minimal()


delta_L50 <- post_f$L50 - post_m$L50
median(delta_L50)

hist(delta_L50)

quantile(delta_L50,c(0.001,.025,0.25,.5,0.75,.975,0.999))
hist(delta_L50[delta_L50>-12 & delta_L50<17],nclass=80,main="",ylab="Frequ?ncia",xlab="Diferen?a entre os valores simulados")
abline(v=0,col=2,lwd=2)

legend("topright",legend=expression(P(H[0]==0.63)),bty="n")
legend("topleft",legend=expression(P(H[1]==0.36)),bty="n")
#sum(delta_L50>0)/length(delta_L50)
mean(delta_L50 > 0)
1-mean(delta_L50 > 0)






new_f <- data.frame(
  Length_s = seq(
    min(df_f$Length_s),
    max(df_f$Length_s),
    length.out = 100
  )
)

epred_f <- posterior_epred(
  mod_f,
  newdata = new_f
)


new_f$mean <- apply(epred_f, 2, mean)
new_f$lower <- apply(epred_f, 2, quantile, 0.025)
new_f$upper <- apply(epred_f, 2, quantile, 0.975)


new_f$Length <- new_f$Length_s + mean(df_f$Length)


ggplot(new_f,
       aes(x = Length,
           y = mean)) +
  
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.2
  ) +
  
  geom_line(linewidth = 1) +
  
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed"
  ) +
  
  labs(
    x = "Comprimento (cm)",
    y = "Probabilidade de maturidade"
  ) +
  
  theme_minimal()


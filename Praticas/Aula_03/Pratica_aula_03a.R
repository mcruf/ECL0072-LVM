#############################################################################
#                                                                           #
#                         Prática das aulas 2 & 3                           #
#                       - Modelo Linear Bayesiano -                         #
#                                                                           #
#############################################################################


# CONTEXTO:
# Esta prática irá utilizar dados de riqueza de espécies de plantas nas ilhas Galápagos para introduzir os principais componentes de uma
# regressão linear Bayesiana utilizando o pacote brms.

# DADOS:
# O conjunto de dados reúne informações sobre a riqueza de espécies de plantas e diferentes características das ilhas, incluindo área, elevação
# e medidas relacionadas ao seu grau de isolamento.
# Fontes dos dados:
# - Johnson & Raven (1973). Species frequency and island area. https://www.science.org/doi/10.1126/science.179.4076.893
# - Fieberg, J. Data4Ecologists: https://github.com/jfieberg/Data4Ecologists

# OBJETIVOS:
# Percorrer as principais etapas de um fluxo de trabalho Bayesiano (Bayesian workflow), incluindo:
# - especificação do modelo estatístico
# - definição e interpretação das distribuições a priori (priors)
# - realização de prior predictive checks
# - ajuste do modelo e obtenção da distribuição posterior via MCMC
# - diagnóstico da convergência das cadeias
# - avaliação do ajuste por meio de posterior predictive checks
# - interpretação das distribuições posteriores dos parâmetros
# - realização de inferências e predições a partir do modelo ajustado

# Ao final, iremos explorar brevemente como diferentes hipóteses ecológicas podem ser 
# contrastadas e interpretadas sob o paradigma Bayesiano.


# ABORDAGEM ANALÍTICA:
# Nesta prática investigaremos a relação entre a riqueza de espécies de plantas e a área das ilhas.
# A relação espécie-área (SAR) é tradicionalmente modelada por uma função potência:
#   Species = c * Area^z

# Ao logaritmizar ambos as variáveis (x, y), linearizamos a relação:
#   log(Species) = log(c) + z * log(Area)

# Podemos, dessa forma, relacionar os parâmetros da relação espécie-área aos parâmetros de um modelo linear (beta0 e beta1), conforme:
#   log(Species) = beta_0 + beta_1 * log(Area)

# O parâmetro beta_1 (ou z) será de especial interesse ao longo da prática, uma vez que representa a magnitude e a direção da associação 
# entre a área dailha e a riqueza de espécies (na escala log-log!).
# Assumindo que o erro na escala logarítmica segue uma distribuição Normal, podemos formular o modelo conforme:
#   y_i ~ Normal(mu_i, sigma)
#   mu_i = beta_0 + beta_1 * x_i
# Lembrando que y_i e x_i estão na escala log!



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) # Manipulação de dados
library(brms) # Modelos Bayesianos
library(bayesplot) # Visualização dos resultados Bayesianos (extra)
library(ggplot2) # Visualização dos dados/resultados
library(GGally) # Gráfico de multicolinearidade
library(patchwork) # Visualização de dados (figuras com +1 painel)




#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~


# 1.1) Carregando os dados (gala)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#devtools::install_github("jfieberg/Data4Ecologists") # Rodar apenas quando instalar pela 1a vez
#df <- readRDS("Dados/galapagos.rds") # Bkup

df <- gala 
str(df)
head(df)
summary(df)



# 1.2) Tratando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~
df$Island <- as.factor(df$Island) # Convertendo para fator 
df$Species_log <- log(df$Species) # Log da variável resposta
df$Area_log <- log(df$Area) # Log da área (preditor)
df$Adj_isl_log <- log(df$Adjacent.isl.area) # Log da área adjacente (preditor)
df$Elevation_log <- log(df$Elevation) # Log da elevação
df$Nearest.dist_log <- log(df$Nearest.dist) # Log da distância à ilha mais próxima
df$Santacruz.dist_log <- log1p(df$Santacruz.dist) # Log(x+1) da distância à Santa Cruz (contém valor 0 - log(0): -Inf)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Análise exploratória dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 2.1) Variável-resposta (riqueza de plantas)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(df$Species)


## Histograma
phist <- ggplot(df, aes(x = Species)) +
         geom_histogram(bins = 5,fill = '#e07a5f', col = 'white', alpha = 0.8, linewidth = 1) +
         coord_cartesian(expand = c(0,0)) +
         labs(x = 'Riqueza', y = 'Frequência') +
         theme_bw(base_size = 12)

## Boxplot
pbox <- ggplot(df, aes(y = Species)) +
        geom_boxplot(fill = 'white', col = '#e07a5f', alpha = 0.8, linewidth = 1.2, outlier.size = 3) +
        labs(y = 'Riqueza') +
        theme_bw(base_size = 12) +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())


## Dotchart
pdot <- df %>% 
        mutate(row_id = row_number()) %>% 
        ggplot(aes(x = Species, y = row_id)) +
        geom_point(color = '#e07a5f', size = 3, alpha = 0.6) +
        labs(x = "Riqueza", y = NULL) +
        scale_x_continuous(breaks = seq(0, 450, by = 100),
                           minor_breaks = seq(0, 450, by = 10)) +
        theme_classic(base_size = 12) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              panel.grid.major.x = element_line(color = "gray85", linewidth = 0.4),
              panel.grid.minor.x = element_line(color = "gray92", linewidth = 0.2))


## Visualizando...
phist + pbox + pdot


## Curiando...
filter(df, Species > 100)



# 2.2) Relação espécie-área
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Variáveis na escala original
ggplot(df, aes(x = Area, y = Species)) +
  geom_point(size = 4, alpha = 0.7, color = '#e07a5f') +
  labs(x = expression(Area~(km^2)),
       y = "Riqueza de espécies") +
  #geom_smooth(method = 'lm', col = 'black') +
  theme_classic(base_size = 14)


## Variáveis na escala transformada (log)
ggplot(df, aes(x = Area_log, y = Species_log)) +
  geom_point(size = 4, alpha = 0.7, color = '#e07a5f') +
  labs(x = expression(log(Area~(km^2))),
       y = expression(log("Riqueza de espécies"))) +
  #geom_smooth(method = 'lm', col = 'black') +
  theme_classic(base_size = 14)
 


# 2.3) Relação espécie & demais preditores
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Analisando multicolinearidade
df %>% 
  #select("Area", "Elevation","Nearest.dist", "Santacruz.dist", "Adjacent.isl.area") %>%  
  select(Species_log, Area_log, Elevation_log, Nearest.dist_log, Santacruz.dist_log, Adj_isl_log) %>% 
  ggpairs(lower = list(continuous = wrap("smooth", color = "#e07a5f", alpha = 0.5, se = FALSE)),
          diag = list(continuous = wrap("densityDiag", fill = "#e07a5f", alpha = 0.6)),
          upper = list(continuous = wrap("cor", size = 4, color = "black"))) +
  theme_bw() + 
  theme(strip.text = element_text(size = 10, face = "bold"), 
        panel.grid.major = element_blank())

# ATENÇÃO: alta colinearidade entre área e elevação!






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Modelos lineares Bayesianos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inicialmente será modelado apenas a relação entre riqueza de plantas e área das ilhas.
# O processo de ajuste do modelo seguirá a lógica do 'Bayesian workflow', sendo aqui dividido nas seguintes etapas:
# 1 - Definir o modelo estatístico
# 2 - Definir as prioris 
# 3 - Realizar o prior predictive check
# 4 - Ajuste do modelo via MCMC
# 5 - Diagnóstico da convergência das cadeias MCMC
# 6 - Diagnóstico do ajuste do modelo (posterior predictive check)
# 7 - Explorar os resultados da distribuição posterior dos parâmetros
# 8 - Inferências sobre hipóteses levantadas


## Input brms (para acelerar a computação)
options(mc.cores = 4, brms.backend = "cmdstanr") 
?brm


# 3.1) Definindo o modelo estatístico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mformula <- Species_log ~ Area_log

mod <- brm(formula = mformula, 
           data = df,
           family = gaussian())




# 3.2) Definindo as prioris 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Antes de ajustar o modelo, podemos verificar quais prioris são utilizadas como default pelo brms
default_prior(mformula,
              data = df,
              family = gaussian())

#get_prior(mformula,data = df,family = gaussian())

# Como default, o brms especifica prioris não informativas para os parâmetros dos efeitos fixos (flat);
# Há, contudo, prioris vagamente informativas sobre o intercepto e sigma - tomam por base os próprios dados.
# ATENÇÃO: Internamente, o pacote brms utiliza preditores centralizados na média. 
# O parâmetro do intercepto (e sua priori), portanto, baseia-se em variáveis centralizadas na média. 
# O que é exibido no resumo do modelo é o parâmetro do intercepto transformado para a escala original.


# Para vias de simplificação, iremos definir apenas uma priori para o parâmetro relacionado ao efeito da área (coef = Area_log).
# Quais os valores plausíveis para este parâmetro? Espera-se que a relação espécie-área seja sempre positiva?
# CUIDADO: estamos interpretando o parâmetro considerando o modelo na escala log-log.
# S = cA^z -> log(S) = log(c) + z*log(A)
# o efeito da área, portanto, é mensurado pelo coeficiente 'z' (beta_1; beta_logarea)

# O efeito de \beta_1 pode ser interpretado como \beta_logarea = delta log(Species) / delta log(Area) - onde delta = diff entre máximo e mínimo,
# Isto nos fornece uma ideia (inicial) da escala do coeficiente \beta_area, mas cuidado, não devemos nos basear apeans nos dados!

diff(range(df$Species_log)) /diff(range(df$Area_log)) # Está na escala do parâmetro estimado (logArea = 0.4)


## Intuições sobre o parâmetro 'z' da relação espécie-área
efeito_z <- function(z, aumento_area = 2) {
  
  riqueza_relativa <- round(aumento_area^z, 3)
  
  data.frame(z = z,
             mudanca_percentual = (riqueza_relativa - 1) * 100)
}

efeito_z(z = c(-1, -0.5, 0, 0.1, 0.3, 0.5, 1), aumento_area = 2) #Efeito de z sobre riqueza quando duplicamos a área
# Assumindo z = -1, faz sentido que a riqueza diminua pela metade quando duplicamos a área?



### Priori fracamente informativa ###
weak_prior <- prior(normal(0, 0.5), class = "b", coef = "Area_log") # Esta priori não permite definir direção da relação, i.e, a relação espécie~área pode ser tanto positiva quanto negativa.
qnorm(c(0.025, 0.975), mean = 0, sd = 0.5) # Aproximadamente 95% da probabilidade de z está entre [-0.97, 0.97]



### Priori informativa ###
informative_prior <- prior(normal(0.3, 0.2), class = "b", coef = "Area_log")  # Esta priori atribui maior plausibilidade a uma relação espécie-área positiva (porém ainda permita valores negativos de z).
qnorm(c(0.025, 0.975), mean = 0.3, sd = 0.2) # Aproximadamente 95% da probabilidade de z está entre [-0.09, 0.69]

# ATENÇÃO: a priori informativa é meramente ilustrativa a título do exemplo aqui abordado. 
# Uma priori informativa sólida deve ser baseada na literatura, experimentos prévios ou conhecimento especializado.



# 3.3) Prior predictive check
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Antes de ajustar o modelo aos dados, devemos avaliar quais tipos de observações são gerados pelas distribuições a priori escolhidas.
# Essa etapa é denominada prior predictive check e permite verificar se as prioris resultam em valores plausíveis para a variável-resposta.


## Modelo com priori fracamente informativa
mod_wprior <- brm(mformula,
                  data = df,
                  family = gaussian(),
                  prior = weak_prior,
                  sample_prior = "only") 


## Modelo com priori informativa
mod_iprior <- brm(mformula,
                  data = df,
                  family = gaussian(),
                  prior = informative_prior,
                  sample_prior = "only")


# Quais intervalos de valores parecem mais plausíveis para 'z' (i.e., \beta_logarea)?
p1 <- mcmc_hist(mod_wprior, pars = "b_Area_log") + ggtitle("Weakly informative prior")
p2 <- mcmc_hist(mod_iprior, pars = "b_Area_log") + ggtitle("Informative prior")

## Visualizando...
p1 / p2 


# Uma distribuição a priori não implica apenas valores plausíveis para os parâmetros. 
# Ela também implica valores plausíveis para a variável-resposta!

# Durante a etapa do 'prior predictive check', a pergunta central é: que tipos de dados o modelo seria capaz de gerar ANTES de
# utilizar os dados observados (likelihood) para atualizar os parâmetros?
# Podemos avaliar isso via função pp_check():


## PP-check do modelo com priori fracamente informativa
ppw <- pp_check(mod_wprior, ndraws = 100) + ggtitle("Weakly informative prior")


## PP-check do modelo com priori informativa
ppi <- pp_check(mod_iprior, ndraws = 100) + ggtitle("Informative prior")


## Visualizando...
ppw / ppi


# Também podemos visualizar apenas a relação média esperada para diferentes draws dos parâmetros, via função conditional_effects():
ppw2 <- plot(conditional_effects(mod_wprior, spaghetti = TRUE, ndraws = 200), points = TRUE, plot = FALSE)
#ppw2 <- plot(conditional_effects(mod_wprior, ndraws = 200), points = TRUE, plot = FALSE)
ppi2 <- plot(conditional_effects(mod_iprior, spaghetti = TRUE, ndraws = 200), points = TRUE, plot = FALSE)


## Visualizando...
ppw2$Area_log + ppi2$Area_log



# 3.4) Ajustando o modelo via MCMC
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Vamos re-rodar o modelo inicial com as duas prioris e avaliar a distribuição posterior dos parâmetros


## Modelo com priori fracamente informativa
mod_wprior <- brm(mformula,
                  data = df,
                  family = gaussian(),
                  prior = weak_prior) 


## Modelo com priori informativa
mod_iprior <- brm(mformula,
                  data = df,
                  family = gaussian(),
                  prior = informative_prior) 



# 3.5) Diagnóstico da convergência das cadeias MCMC
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Antes de interpretar o modelo, devemos verificar se o algoritmo MCMC explorou adequadamente a distribuição posterior.
# Principais aspectos para atentar:
# - Rhat próximo de 1
# - tamanho efetivo da amostra (ESS)
# - boa mistura das cadeias
# - ausência de problemas de convergência


plot(mod_wprior)
#plot(mod_iprior, variable = c("b_Intercept", "b_Area_log")) # Selecionando apenas um conjunto de parâmetros
summary(mod_wprior) # Olhar para o valor Rhat - tem que estar próximo de 1

plot(mod_iprior) 
summary(mod_iprior) 


## Curiosidades...
color_scheme_set("teal") # Just for fun...(https://mc-stan.org/bayesplot/reference/bayesplot-colors.html)

mcmc_trace(mod_iprior, pars = c("b_Intercept", "b_Area_log", "sigma")) # Cadeias MCMC
mcmc_hist(mod_iprior, pars = c("b_Intercept", "b_Area_log", "sigma")) # Histograma da distribuição posterior dos parâmetros
mcmc_dens(mod_iprior, pars = c("b_Intercept", "b_Area_log", "sigma")) # Densidade da distribuição posterior dos parâmetros
pairs(mod_iprior, variable = c("b_Intercept", "b_Area_log", "sigma"), off_diag_fun="hex") # A distribuição posterior é multivariada -> parâmetros estão correlacionados (caso não tenham sido padronizados)




# 3.6) Diagnóstico do ajuste do modelo (posterior predictive check)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Após verificar a convergência da MCMC, devemos avaliar se o modelo é capaz de reproduzir os dados.


## Comparando a distribuição dos dados com as distribuições (posteriores) do modelo
pp_check(mod_iprior, ndraws = 200)
#pp_check(mod_wprior, ndraws = 100)


## Comparando valores observados vs. preditos
pp_check(mod_iprior, "scatter_avg")


## Comparando a média dos dados com as médias simuladas
pp_check(mod_iprior, type = 'stat', stat = 'mean')


## Coeficiente de determinação
bayes_R2(mod_iprior)


## Avaliando os resíduos
performance::check_model(mod_iprior) 
# ATENÇÃO: a interpretação dos diferentes diagnósticos depende da família e da estrutura do modelo. 
# Nem todos os pressupostos dos modelos lineares clássicos são avaliados da mesma maneira em modelos Bayesianos mais complexos (GLM, GLMMs, ...).



# 3.7) Avaliando o resultado do modelo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lembrando que no paradigma Bayesiano, todo o processo inferencial é realizado através da distribuição posterior.

summary(mod_iprior)
summary(mod_iprior, prob = 0.90) # Sumário com intervalo de credibilidade adaptável


fixef(mod_iprior) # Extraindo apenas os resultados dos efeitos fixos


mcmc_dens(mod_iprior, pars = c("b_Intercept", "b_Area_log")) # Distribuição posterior dos parâmetros
mcmc_hist(mod_iprior, pars = c("b_Intercept", "b_Area_log")) # Mesma coisa, mas no formato de histograma...


## Predições
# ATENÇÃO: as predições podem ser realizadas usando:
# (i) distribuição posterior da média esperada:
#     mu = beta0 + beta1*x
# -> representa a incerteza sobre a resposta média esperada

# (ii) distribuição preditiva posterior de uma nova observação:
#     y_new ~ Normal(mu, sigma)
# -> incorpora tanto a incerteza dos parâmetros quanto a variabilidade residual

### Predições da média
plot(conditional_effects(mod_iprior, spaghetti = TRUE, ndraws = 200),points = TRUE)
head(fitted(mod_iprior))
fitted(mod_iprior, newdata = data.frame(Area_log = 8))  %>%  round(3) # Predizendo para uma ilha com log(area) = 8 


### Predições da variável-resposta
plot(conditional_effects(mod_iprior, method = "posterior_predict"), points=TRUE) #IC-95% deve cobrir ~95% dos pontos
head(predict(mod_iprior))
predict(mod_iprior, newdata = data.frame(Area_log = 8))  %>%  round(3) # Predizendo para uma ilha com log(area) = 8 



## Extraindo amostras da distribuição posterior
post <- as_draws_df(mod_iprior) # as_draws_matrix() - extrai como matriz
str(post)

head(as.data.frame(post)) # Cada linha corresponde a uma amostra da distribuição posterior (multivariada)



# 3.8) Inferências sobre hipóteses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sob prisma Bayesiano, podemos avaliar a palusibilidade das hipóteses.
# No contexto do presente exemplo, podemos nos perguntar qual é a probabilidade
# da riqueza aumentar com a área, ou seja, p(\beta_area > 0).
# Podemos usar a função hypothesis() para testar qualquer tipo de hipótese.

hypothesis(mod_iprior, "Area_log > 0") # Repare no Post.Prob!
#hypothesis(mod_iprior, "Area_log > 0.4") # Repare no Post.Prob!


# Visualizando...
ph <- plot(hypothesis(mod_iprior, "Area_log > 0"),  plot = FALSE)
ph[[1]] + geom_vline(xintercept=0)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Contrastando os resultados com abordagem frequentista
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_lm <- lm(mformula, data = df)

summary(mod_lm)
fixef(mod_iprior) 

# A semelhança entre as estimativas pontuais não significa que as abordagens sejam equivalentes!
# No modelo Bayesiano, todo o processo inferencial é baseado na distribuição posterior e não apenas em uma estimativa pontual.
# Isso, por consequência, permite interpretar os resultados probabilisticamente



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Comparando hipóteses ecológicas alternativas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Até agora, investigamos apenas a relação espécie-área, assumindo que a área da ilha está associada
# à variação na riqueza de espécies. No entanto, outros fatores também podem contribuir para explicar os padrões observados.
# O banco de dados contém informações sobre características das ilhas que nos permitem representar diferentes hipóteses 
# ecológicas por meio de modelos estatísticos alternativos.

# Vamos considerar 5 hipóteses:

# H0: a riqueza varia aleatoriamente (hipótese nula)
# H1: a riqueza de espécie é influenciada pela área
# H2: a riqueza de espécie é influenciada pela elevação
# H3: a riqueza de espécie depende do seu grau de isolamento
# H4: a riqueza de espécie depende da área da ilha e do seu grau de isolamento

# OBS.: Para vias de simplificação, não iremos realizar um processo detalhado de elicitação das prioris para cada modelo. 
# Antes de interpretar uma comparação Bayesiana entre modelos, entretanto, é importante verificar as prioris utilizadas.




# 5.1) Rodando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~

## H0: Hipótese nula
m0 <- Species_log ~ 1
mod0 <- brm(m0,
            data = df,
            family = gaussian()) 


## H1: Hipótese da área
m1 <- Species_log ~ Area_log
mod1 <- brm(m1,
            data = df,
            family = gaussian()) 


## H2: Hipótese da elevação
m2 <- Species_log ~ Elevation_log
mod2 <- brm(m2,
            data = df,
            family = gaussian()) 


## H3: Hipótese do isolamento
m3 <- Species_log ~ Nearest.dist_log + Santacruz.dist_log
mod3 <- brm(m3,
            data = df,
            family = gaussian()) 


## H4: Hipótese da área + isolamento
m4 <- Species_log ~ Area_log + Nearest.dist_log + Santacruz.dist_log
mod4 <- brm(m4,
            data = df,
            family = gaussian()) 



# 5.2) Comparando os modelos (capacidade preditiva)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Podemos comparar os modelos utilizando o leave-one-out cross-validation (LOO), uma abordagem de validação cruzada que 
# avalia a capacidade PREDITIVA do modelo para observações que não foram utilizadas no ajuste.

# Nessa metodologia, cada observação é, sucessivamente, tratada como se estivesse fora do conjunto de dados. 
# Avalia-se, então, quão bem o modelo seria capaz de predizer a observação que ficou de fora.

# O desempenho preditivo é resumido pela métrica ELPD, sendo maiores valores indicativos de melhor capacidade preditiva.
# A comparação entre os modelos é feita em relação ao modelo com maior ELPD, com os demais modelos apresentando a sua diferença de ELPD em relação ao melhor modelo (elpd_diff).
# Assim, valores negativos de elpd_diff indicam menor desempenho preditivo.

# ATENÇÃO: A magnitude de elpd_diff não deve ser interpretada isoladamente.
# Deve-se, também, considerar o erro-padrão associado à diferença de ELPD entre cada modelo e o modelo de referência  (se_diff) .

# ATENÇÃO-2: O LOO compara a capacidade preditiva dos modelos utilizados para representarem as nossas hipóteses ecológicas. 
# Ele não calcula a probabilidade de uma hipótese ser verdadeira ou falsa!

loo0 <- loo(mod0)
loo1 <- loo(mod1)
loo2 <- loo(mod2)
loo3 <- loo(mod3)
loo4 <- loo(mod4)

loo_compare(loo0, loo1, loo2, loo3, loo4) # Modelo com maior ELPD é usado como referência; todos os demais modelos são comparados ao modelo de referência
# mod1 apresentou maior desempenho preditivo, sendo praticamente indistinguível do mod4.



# 5.3) Comparando as hipóteses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Após avaliar a capacidade preditiva de cada modelo, podemos nos perguntar qual o suporte que os dados fornecem em relação às hipóteses testadas.
# Para isso, podemos utilizar o Bayes factor (BF): BF = p(y | model A) / p(y | model B) 
# O BF indica quantas vezes os dados alteram as razões de chances relativas (odds) entre os dois modelos.

# Qual o suporte do mod1 em relação ao mod4?
bayes_factor(mod1, mod4) # mod1 é 4x mais plausível do que mod4
#bayes_factor(mod4, mod1) # Cuidado com a direção.


# Qual o suporte do mod1 em relação ao mod0?
bayes_factor(mod1, mod0) # Eita!!



#~~~~~~~~~~~~~~~~~
# 6) Curiosidades
#~~~~~~~~~~~~~~~~~
# O pacote bayestestR possui várias funcionalidades interessantes e que podem ser exploradas:
# https://easystats.github.io/bayestestR/
library(bayestestR)


# 6.1) HDI (highest density interval)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# O HDI representa o intervalo que contém uma determinada proporção da distribuição posterior, priorizando os valores de maior densidade.
hdi(mod1, ci = c(0.5, 0.80, 0.95))
plot(hdi(mod1, ci = c(0.5, 0.80, 0.95)), show_intercept = T)



# 6.2) Probabilidade da direção do efeito
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Determina a probabilidade de um parâmetro ser estritamente positivo ou negativo.
p_direction(mod1) 
plot(p_direction(mod1))


# 6.3) ROPE (region of practical equivalence)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define uma região ao redor de zero correspondente a efeitos considerados pequenos o suficiente para serem praticamente irrelevantes.
# Assim, avalia-se o quanto da distribuição posterior se encontra dentro dessa região, permitindo investigar não apenas se o efeito difere de zero,
# mas também se sua magnitude é relevante na prática.
rope(mod1)
plot(rope(mod1))


#####################################################################
#                                                                   #
#                       Prática da aula 07                          #                            
#                  - Breve introdução ao INLA  -                    #
#                                                                   #
#####################################################################

# Este script apresenta uma introdução prática ao pacote R-INLA para modelagem bayesiana. 
# Para tal, serão utilizados o mesmo conjunto de dados pesqueiros das práticas anteriores (Pratica_aula_06a.R e Pratica_aula_06b.R).

# O objetivo principal é promover a familiarização com o fluxo de trabalho
# e a sintaxe do INLA, abordando as seguintes etapas:
#   (i)   Ajustar modelos utilizando a função inla()
#   (ii)  Acessar e interpretar os principais outputs do modelo
#   (iii) Explorar distribuições marginais posteriores
#   (iv)  Comparar os modelos (DIC, WAIC e LCPO)
#   (v)   Realizar o diagnóstico visual do ajuste (PIT e QQ-plot)

# Nota: A especificação completa de modelos geoestatísticos com campos aleatórios contínuos (SPDE/Matérn) será abordada na próxima prática (Pratica_aula_07b.R)




#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(INLA) # Modelos bayesianos
library(dplyr) # Manipulação dos dados
library(ggplot2) # Visualização dos dados
library(sp) # Visualizar residuos espaciais



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo tema geral do ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_theme <- theme_bw(base_size = 14) + 
            theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                  axis.text.x = element_text(colour = "grey20", margin = margin(t = 10)),
                  axis.text.y = element_text(colour = "grey20", margin = margin(r = 10)),
                  axis.title = element_text(face = "bold", colour = "black", size = 16),
                  strip.background = element_rect(fill = 'white'),
                  legend.background = element_rect(fill = "transparent", colour = NA),
                  legend.box.background = element_rect(fill = "transparent", colour = NA),
                  panel.grid.major.y = element_line(linetype = 'dotted'),
                  panel.grid.major.x = element_line(linetype = 'dotted'),
                  axis.line = element_line(color = "black"),
                  plot.margin = margin(5, 10, 5, 5))


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando os dados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1) Dados pesqueiros
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/fisheries_covars.rds")
str(df)
df$Month <- as.factor(df$Month)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Rodando um modelo no INLA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 2.1) Modelo incluindo apenas características das operações pesqueiras
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f1 <- Prionace_glauca ~ 1 + Year + Month + Flag + Lat + Long + offset(log(Nhooks)) 

mod1 <- inla(f1,
             family = 'poisson',
             data = df,
             #offset = log(Nhooks), # Termo entra no preditor linear 
             #E = Nhooks, # Termo entra na função verossimilhança
             control.predictor = list(compute = T), # Calcula as distribuições marginais posteriores dos valores preditors (demora mais para rodar); default do inla é 'FALSE'
             control.compute = list(dic = T, waic = T, cpo = T, 
                                    return.marginals.predictor = T)) # Armazena as distribuições marginais posteriores



# 2.2) Modelo incluindo apeans covariáveis ambientais (abióticas)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f2 <- Prionace_glauca ~ 1 + thetao + zos + mlotst + nppv + bathy + offset(log(Nhooks)) # Modelo incluindo apeans covariáveis ambientais (abióticas)

mod2 <- inla(f2,
             family = 'poisson',
             data = df,
             control.predictor = list(compute = T), 
             control.compute = list(dic = T, waic = T, cpo = T,
                                    return.marginals.predictor = T))



# 2.3) Modelo incluindo as operações pesqueiras & covariáveis ambientais 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f3 <- Prionace_glauca ~ 1 + Year + Month + Flag + Lat + Long + thetao + zos + mlotst + nppv + bathy + offset(log(Nhooks)) # Modelo completo

mod3 <- inla(f3,
             family = 'poisson',
             data = df,
             control.predictor = list(compute = T), 
             control.compute = list(dic = T, waic = T, cpo = T,
                                    return.marginals.predictor = T))



# 2.4) Modelo incluindo efeito aleatório das embarcações
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f4 <- Prionace_glauca ~ 1 + Year + Month + Flag + Lat + Long + f(Vessel, model = 'iid') + thetao + zos + mlotst + nppv + bathy + offset(log(Nhooks)) 

mod4 <- inla(f4,
             family = 'poisson',
             data = df,
             control.predictor = list(compute = T), 
             control.compute = list(dic = T, waic = T, cpo = T,
                                    return.marginals.predictor = T))




# 2.5) Modelo incluindo efeitos suavizadores sobre Lat/Long (GAM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f5 <- Prionace_glauca ~ 1 + Year + Month + Flag + f(Lat, model = 'rw1') + f(Long, model = 'rw1') + f(Vessel, model = 'iid') + thetao + zos + mlotst + nppv + bathy + offset(log(Nhooks)) 
#f5 <- Prionace_glauca ~ 1 + Year + Month + Flag + f(inla.group(Lat, n = 50), model = 'rw1') + f(inla.group(Long, n = 50), model = 'rw1') + f(Vessel, model = 'iid') + thetao + zos + mlotst + nppv + bathy + offset(log(Nhooks)) 

mod5 <- inla(f5,
             family = 'poisson',
             data = df,
             control.predictor = list(compute = T), 
             control.compute = list(dic = T, waic = T, cpo = T,
                                    return.marginals.predictor = T))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Extraindo os resultados do INLA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Sumário estatístico geral
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(mod1) # mod2, mod3, ....



# 3.2) Acessando outros componentes dos resultados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
names(mod1) # Tudo o que fica salvo no objeto 'inla'


## Estatísticas dos valores ajustados
head(mod1$summary.fitted.values)


## Estatísticas do preditor linear
head(mod1$summary.linear.predictor)
# 'kld' representa a divergência de Kullback-Leibler, i.e., mede o quanto a distribuição
# posterior estimada pelo INLA difere de uma aproximação Gaussiana. 
# Valores próximos de zero indicam que a aproximação Normal é excelente.


## Estatísticas dos efeitos fixos
head(mod1$summary.fixed)


## Estatísticas dos efeitos aleatórios
head(mod1$summary.random) # Não temos efeitos aleatórios em mod1
names(mod5$summary.random)
head(mod5$summary.random[[1]])
head(mod5$summary.random[[2]])

# EXTRA: Visualizando os efeitos aleatórios
dat2 <- mod5$summary.random$Vessel[,c("ID","mean", "0.025quant", "0.975quant")]
colnames(dat2) <- c("Vessel", "mean", "lower", "upper")

ggplot(dat2, aes(x = mean, y = Vessel)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'red') +
  my_theme



## Estatísticas dos hiperparâmetros
head(mod5$summary.hyperpar)



## Distribuições marginais posteriores dos valores ajustados
names(mod1$marginals.fitted.values)
head(mod1$marginals.fitted.values$fitted.Predictor.001)


### Distribuição marginais posteriores dos efeitos fixos
names(mod1$marginals.fixed)
head(mod1$marginals.fixed[[1]])


### Distribuição marginais efeitos aleatórios
names(mod1$marginals.fixed)
head(mod1$marginals.fixed[[2]])



# O R-INLA possui diversas funções para manipular as distribuições marginais posteriores
# - inla.emarginal()
# - inla.qmarginal()
# - inla.smarginal()
# - inla.hpdmarginal ()
# - inla.tmarginal()
# - inla.zmarginal()


### Visualizando a distribuição posterior
beta <- mod5$marginals.fixed$thetao # Dist. posterior da covariável 'temperatura do mar'

ggplot(data = data.frame(inla.smarginal(beta))) + # Usamos a função inla.smarginal() para suavizar as curvas da distribuição posterior
  geom_line(aes(x = x, y = y), col = '#e07a5f', linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab(expression(beta)) +
  ylab(expression(tilde(p)(paste(beta, "|", y)))) +
  my_theme


### Podemos calcular a probabilidade da variável ser menor do que zero - P(beta < 0)
prob_zero <- inla.pmarginal(0, beta)
prob_zero # Existe uma probabilidadede 90,8% de que o efeito da temperatura seja negativo
1 - prob_zero # Probabiliade da temperatura ser maior que zero


hpd <- inla.hpdmarginal(0.95, beta)
hpd # Inclui o valor zero -> não há forte evidência de que o efeito seja estritamente diferente de zero ao nível de 95% de credibilidade.

#### Visualizando em um gráfico
beta_df <- as.data.frame(inla.smarginal(beta)) # Suavizando a distribuição marginal
beta_df$HPD <- beta_df$x >= hpd[1] & beta_df$x <= hpd[2] # Filtrando os valores

ggplot(beta_df, aes(x = x, y = y)) +
    geom_area(data = subset(beta_df, HPD),
              aes(x = x, y = y), fill = "#e07a5f", alpha = 0.4) +
    geom_line(col = "#e07a5f", linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) +
    labs(x = expression(beta), y = expression(paste("p(", beta, "|y)"))) +
    my_theme



# 3.3) Comparando modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. DIC (Deviance Information Criterion): 
#     - métrica penalizada pela complexidade do modelo
#     - baseado na média da posteriori -> problemático para modelos hierárquicos complexos cujas posterioris podem ser assimétricas/multimodais
#     - Quanto menor valor, melhor

# 2. WAIC (Watanabe-Akaike Information Criterion):  
#     - Similar ao DIC, porém mais robusto para modelos complexos
#     - Quanto menor valor, melhor

# 3. LCPO (Mean Logarithmic Conditional Predictive Ordinate):
#     - Métrica baseada em validação cruzada do tipo Leave-One-Out (LOO-CV).
#     - Mede a capacidade preditiva do modelo
#     - Quanto menor o valor, maior o poder preditivo fora da amostra (out-of-sample).


## As três métricas podem ser acessadas facilmente conforme
mod1$dic$dic # dic
mod1$waic$waic # waic
-mean(log(mod1$cpo$cpo), na.rm = T) # lcpo


### Comparando os modelos...

#### Funçãozinha para automatizar...
fit_metrics <- function(model){
  dic <- model$dic$dic
  waic <- model$waic$waic
  lcpo <- (-mean(log(model$cpo$cpo), na.rm = T))
  tmp <- data.frame(DIC = dic,
                    WAIC = waic,
                    LCPO = lcpo)
  return(tmp)
}

modelos <- list(mod1, mod2, mod3, mod4, mod5)
fits <- lapply(modelos, fit_metrics)
do.call("rbind", fits) # Fortes indícios de que o modelo Poisson não é adequado para os dados (instabilidade numérica)


# 3.4) Avaliando a qualidade de ajuste do modelo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Homocedasticidade
# Independência
#


## Selecionando um modelo
modelo <- modelos[[3]] #Modelos 1-5


## Montando um df com valores preditos, observados, IC 95%, residuos e coordenadas
df_fit <- data.frame(predicted = modelo$summary.fitted.values[ ,'mean'], 
                     lower = modelo$summary.fitted.values[ , '0.025quant'], 
                     upper = modelo$summary.fitted.values[ , '0.975quant'], 
                     observed = df$Prionace_glauca,
                     Long = df$Long,
                     Lat = df$Lat,
                     res_dev = modelo$residuals$deviance.residuals, # Residuo deviance
                     pit = modelo$cpo$pit,
                     res_quant = qnorm(modelo$cpo$pit)) # Resíduo quantílico normalizado)





# 3.4.1) Homocedasticidade
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(df_fit, aes(x = predicted, y = res_dev)) + 
  geom_point() + 
  labs(y = "Residuals", 
       x = "Fitted") + 
  geom_smooth(se = T) + 
  geom_hline(yintercept = 0, linetype = 2, col = "red") + 
  my_theme



# 3.4.2) Independência
#~~~~~~~~~~~~~~~~~~~~~~~

## Independência temporal
acf(df_fit$res_dev)


## Independência espacial
df_fit2 <- df_fit
coordinates(df_fit2) <- c("Long", "Lat") # Convertendo df para SPDF
bubble(df_fit2, "res_dev")
rm(df_fit2)


# 3.4.3) Observado vs. predito
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(df_fit, aes(x = observed, y = predicted)) + 
  geom_point() + 
  labs(y = "Predicted", x = "Observed") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_smooth(method = "lm") +
  my_theme



# 3.4.4) Calibração Preditiva via PIT (Probability Integral Transform)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Se o modelo estiver bem calibrado - > PIT deve ter distribuição Uniforme U(0,1)
hist(df_fit$pit, breaks = 15, col = "lightgrey")



# 3.4.5) Normalidade dos Resíduos Quantílicos (QQ-Plot)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Avalia se a distribuição de probabilidade escolhida (Poisson) capturou corretamente a variação dos dados.
# Os pontos devem se alinhar sobre a linha pontilhada azul.

ggplot(df_fit, aes(sample = res_quant)) +
  stat_qq_line(col = 'blue', linetype = 'dashed', linewidth = 1) +
  stat_qq(size = 2, alpha = 0.6) +
  my_theme







#                                 #
#          - Figuras -            #
#            Aula 03              #
#                                 #
###################################


# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(dplyr)


# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



##################################
# Exemplo de regressão logística #
##################################

# Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~
dados <- read.csv("Dados/palmerpenguins_extended.csv")


# Reclassificando a variável "métrica de saúde"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(dados$health_metrics)


## Vamos reclassificar: sobrepeso/normal
dados$health_metrics2 <- ifelse(dados$health_metrics == "overweight", "overweight", "healthy")
table(dados$health_metrics2)

## E agora codificar para o estado binário
dados$estado_saude <- ifelse(dados$health_metrics2 == "overweight", 1, 0)


#plot(dados$estado_saude~dados$body_mass_g)


# Ajustando um modelo logístico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

modelo <- glm(estado_saude ~ body_mass_g,
              data = dados,
              family = binomial(link = "logit"))
summary(modelo)


# Predizendo valores para incluir a curva sigmoide
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Criando 100 valores de body_mass_g que vão do menor ao maior peso
body_mass_seq <- seq(min(dados$body_mass_g, na.rm = TRUE),
                     max(dados$body_mass_g, na.rm = TRUE),
                     length.out = 100)

## Predizendo a probabilidade de "sobrepeso"
pred <- predict(modelo,
                newdata = data.frame(body_mass_g = body_mass_seq),
                type = "response")  # type = "response" retorna probabilidade

df_pred <- data.frame(body_mass_g = body_mass_seq,
                      prob_overweight = pred)

## Plotando o gráfico
p1 <- ggplot(dados, aes(x = body_mass_g, y = estado_saude)) +
      geom_jitter(height = 0.025, size = 3, alpha = 0.15, col = 'cyan4') +  # pontos dispersos para visualização
      geom_line(data = df_pred, aes(x = body_mass_g, y = prob_overweight), color = "gray40", size = 1.2) +
      labs(x = "Peso (g)",
           y = "Probabilidade de sobrepeso") +
      #theme_minimal(base_size = 16) +
      ggpubr::theme_pubclean(base_size = 16)+
      theme(axis.title.y = element_text(vjust = 3),
            axis.title.x = element_text(vjust = -3),
            plot.title = element_text(hjust = 0.5),
            plot.margin = margin(20, 20, 20, 20))


ggsave(plot = p1,
       filename ="Figuras/Curva_sigmoide.png", 
       width = 8,
       height = 5,
       dpi = 350)



##########################
#   Transformação logit  #
##########################

# Função logit
#~~~~~~~~~~~~~~
logit <- function(p){
  odds <- p / (1-p)
  log_odds <- log(odds)
  return(log_odds)
}


# Criando uma sequência de valores p_i
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pi <- seq(0, 1, by = 0.01) # lembrando: 0 < p_i < 1

#logit(pi)
#plot(pi~logit(pi))

# Calculando o logit(p_i)
#~~~~~~~~~~~~~~~~~~~~~~~~~
logit_pi <- logit(pi)


# Plotando o gráfico
#~~~~~~~~~~~~~~~~~~~~

## Armazenando os valores em um data frame
dat <- data.frame(pi = pi, logit = logit_pi)

p2 <- dat %>% 
      ggplot(aes(x = logit_pi, y = pi)) +
      geom_point(size = 4, col ='darkorange', alpha = 0.5) +
      theme_classic(base_size = 16) +
      labs( x = bquote("logit" ~ (p[i])),
            y = expression(p[i]))


ggsave(plot = p2,
       filename ="Figuras/Curva_sigmoide2.png", 
       width = 8,
       height = 5,
       dpi = 350)



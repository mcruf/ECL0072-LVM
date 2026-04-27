
###################################
#                                 #
#          - Figuras -            #
#            Aula 01              #
#                                 #
###################################

library(ggplot2)
library(dplyr)
library(patchwork)


# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Library/CloudStorage/OneDrive-Personal/Arbeit/Lectures_and_talks/UFRN/Lectures/ECL0072-GLM_GAM/")


# Importando o banco de dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dados <- read.csv("Dados/palmerpenguins_extended.csv")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Scatter plot & histograma pinguins  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Scatter plot
p1 <- dados %>%
  slice_head(n=100) %>%
  ggplot(aes(y = body_mass_g, x = bill_length_mm)) +
  geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
  geom_smooth(method = 'lm', col = 'gray50', fill = 'gray80') +
  theme_classic(base_size = 16) +
  labs(x = "Tamanho do bico (mm)", y = "Peso (g)") +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.margin = margin(20,20,20,20))


## Histograma
p2 <- dados %>%
  slice_head(n=100) %>%
  ggplot(aes(x = body_mass_g)) +
  geom_histogram(bins = 13, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  #geom_histogram(bins = 13, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  #geom_density(size=1, col = 'gray40') +
  theme_classic(base_size = 16) +
  labs(y = "Frequência", x = "Peso (g)") +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.margin = margin(20,20,20,20))

pf <- (p1 + p2)

ggsave(plot = pf,
       filename ="Figuras/relacao_y_x_variabilidade.png", 
       width = 9,
       height = 4,
       dpi = 350)


#################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Exemplos de correlação   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Para garantir reprodutibilidade dos resultados
set.seed(987)


## Simulando os dados
x <- rnorm(100)
y_pos <- x + rnorm(100, sd = 0.5)
y_neg <- -x + rnorm(100, sd = 0.5)
y_sem <- rnorm(100)

## Salvando tudo em um data frame
df <- data.frame(x = x,
                 y_pos = y_pos,
                 y_neg = y_neg,
                 y_sem = y_sem)


## Transformando do formato largo para longo (para usar no ggplot)
dfl <- tidyr::pivot_longer(data = df, 
                           cols=y_pos:y_sem,
                           names_to = "Correlacao",
                           values_to = "y")

## Reclassificando a coluna 'Correlação' para melhorar visualização
dfl$Relacoes <- ifelse(dfl$Correlacao == "y_pos", "Positiva",
                       ifelse(dfl$Correlacao == "y_neg", "Negativa","Ausente"))

dfl$Relacoes <- factor(dfl$Relacoes, levels = c("Positiva", "Negativa", "Ausente"))

cp <- ggplot(dfl, aes(x = x, y = y)) +
      geom_point(alpha = 0.5, size = 3, colour = "darkorange") +
      geom_smooth(method = "lm", 
                  se = TRUE, 
                  col = 'gray50',
                  fill = 'gray80') +
      facet_wrap(Relacoes ~.) +
      theme_bw(base_size = 16) +
      theme(axis.title.y = element_text(vjust = 0.5, angle = 360),
            axis.title.x = element_text(vjust = -3),
            plot.margin = margin(20,20,20,20),
            panel.grid = element_blank(),
            strip.background = element_rect(fill = "white", linewidth = 1))
      

ggsave(plot = cp,
       filename ="Figuras/tipos_de_correlacoes.png", 
       width = 8,
       height = 4,
       dpi = 350)

    

#################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Exemplos de Francis Galton     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Carregando os dados
df <- read.csv("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/Dados/Galton_Family_Heights.csv")

## Calculando altura média dos pais
df$Parent_height <- (df$Father_height + (1.08*df$Mother_height))/2 # O valor de 1.08 representa a razão de mulheres/homens, conforme calculado por Galton baseado em sua amostra

## Convertendo df para o formato longo
df2 <- pivot_longer(data=df, 
                    cols = c(Child_height,Parent_height),
                    names_to = "Familia",
                    values_to = "Alturas")


## Categorizando a família (para vias de ilustração)
df2$Familia <- as.factor(df2$Familia)
#df2$Familia2 <- ifelse(df2$Familia == "Child_height", "Criança", "Pais")
df2$Familia2 <- ifelse(df2$Familia == "Child_height", "Crianças", "Pais")
df2$Familia2 <- as.factor(df2$Familia2)


## Calculando alturas médias
# alturas_medias <- df2 %>%
#   group_by(Familia2) %>%
#   summarise(media_altura = mean(Alturas, na.rm = TRUE))
# 

## Plotando o gráfico
pg <- df2 %>%
      mutate(Familia2 = factor(Familia2, levels = c("Pais", "Crianças"))) %>%
      ggplot(aes(x = 2.54*Alturas, fill = Familia2)) +
      geom_density(position = "identity", alpha = 0.5,
                   col = 'white', linewidth = 0.7) +
      # geom_histogram(position = "identity", alpha = 0.5, bins = 20,
      #                col = 'white') +
      #facet_wrap(Familia2 ~ .) +
      scale_fill_manual(values =c("darkorange", "cyan4")) +
      ggpubr::theme_pubr(base_size = 16) +
      labs(x = "Altura (cm)") +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y.left = element_blank(),
            axis.line.y.left = element_blank(),
            axis.title.x = element_text(vjust = -3),
            legend.title = element_blank(),
            plot.margin = margin(20,20,20,20))


ggsave(plot = pg,
       filename ="Figuras/Exemplo_Gauss_alturas.png", 
       width = 5,
       height = 4,
       dpi = 350)


#################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Exemplos de regressão linear   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Simulando dados não lineares
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Baseado em uma regressão polinomial de 3o grau (função cúbica)

set.seed(123) # Para garantir a reprodutibilidade

n <- 70
x <- seq(-3, 3, length.out = n)
y <- 1 + 0.5*x - 1.2*x^2 + 0.3*x^3 + rnorm(n, sd = 2)

data <- data.frame(x, y)

## regressão linear
p1 <- data %>%
      slice_head(n=100) %>%
      ggplot(aes(y = y, x = x)) +
      geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
      geom_smooth(method = 'lm', 
                  formula = y~x,
                  col = 'gray50',
                  fill = 'gray80') +
      theme_classic(base_size = 16) +
      labs(title = "Regressão linear", subtitle = expression(y[i] == beta[0] + beta[1]*x[i] + epsilon[i])) +
      theme(axis.title.y = element_text(vjust = 3),
            axis.title.x = element_text(vjust = -3),
            plot.title = element_text(hjust = 0.5, face='bold'),
            plot.subtitle = element_text(hjust = 0.5),
            plot.margin = margin(20,20,20,20))



## regressão polinomial
p2 <- data %>%
  slice_head(n=100) %>%
  ggplot(aes(y = y, x = x)) +
  geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
  geom_smooth(method = 'lm', 
              formula = y ~ poly(x, 3, raw=TRUE),
              col = 'gray50', 
              fill = 'gray80') +
  theme_classic(base_size = 16) +
  labs(title = "Regressão polinomial", subtitle = expression(y[i] == beta[0] + beta[1]*x[i] + beta[2]*x[i]^2 + beta[3]*x[i]^3 + epsilon[i])) +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.title = element_text(hjust = 0.5, face='bold'),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(20,20,20,20))


# Combine them
full_p <- p1 + p2

ggsave(plot = full_p,
       filename ="Figuras/exemplo_regressoes.png", 
       width = 8,
       height = 4,
       dpi = 350)


#################################################

#~~~~~~~~~~~~~~~~~#
# Desvio residual #
#~~~~~~~~~~~~~~~~~#


set.seed(123)

## Selecionar 100 amostragens para ajustar o modelo linear
dados_100 <- dados %>%
  slice_head(n = 100) %>%
  mutate(
    pred = predict(lm(body_mass_g ~ bill_length_mm, data = .)),
    id = row_number(),
    grupo = if_else(id %in% sample(id, 10), "highlight", "fade")
  )


## Figura
ggplot(dados_100, aes(x = bill_length_mm, y = body_mass_g)) +
  # Plot all points with appropriate alpha
  geom_point(aes(alpha = grupo), size = 3.5, color = 'darkorange') +
  
  # Plot residual segments ONLY for highlighted points
  geom_segment(
    data = filter(dados_100, grupo == "highlight"),
    aes(xend = bill_length_mm, yend = pred),
    color = "black", size = 0.6
  ) +
  
  
# Regression line with deviations
geom_smooth(method = 'lm', color = 'gray50', fill = 'gray80') +
  
  # Transparency levels
  scale_alpha_manual(values = c("highlight" = 1, "fade" = 0.20)) +
  
  # Theme and labels
  theme_classic(base_size = 16) +
  labs(x = "Tamanho do bico (mm)", y = "Peso (g)") +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )


#################################################


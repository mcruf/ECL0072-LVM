#                                 #
#          - Figuras -            #
#            Aula 02              #
#                                 #
###################################



# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(dplyr)
library(patchwork)


# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")




#~~~~~~~~~~~~~~~~~~~~~~~~~#
# Modelo linear ajustado  #
#~~~~~~~~~~~~~~~~~~~~~~~~~#
dados <- read.csv("~/Library/CloudStorage/OneDrive-Personal/Arbeit/Lectures_and_talks/UFRN/Lectures/ECL0072-GLM_GAM/Dados/palmerpenguins_extended.csv")

p1 <- dados %>%
      ggplot(aes(y = body_mass_g, x = bill_length_mm)) +
      geom_point(size = 3.5, col ='darkorange', alpha = 0.2) +
      geom_smooth(method = 'lm', col = 'gray20', fill = 'gray40', se = TRUE) +
      theme_classic(base_size = 16) +
      labs(
        x = "Tamanho do bico (mm)",
        y = "Peso (g)",
        title = expression(Peso[i] == 2299.7 + 65.8 * Bico[i])) +
      theme(axis.title.y = element_text(vjust = 3),
            axis.title.x = element_text(vjust = -3),
            plot.title = element_text(hjust = 0.5),
            plot.margin = margin(20, 20, 20, 20))


ggsave(plot = p1,
       filename ="Figuras/LM_ajustado.png", 
       width = 8,
       height = 5,
       dpi = 350)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Curvas de distiribuição Normal  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Criando a variável x (comum a todos os y's simulados)
x <- seq(-5, 5, length.out = 200)


## Simulando a variável y (com diferentes parâmetros)
set.seed(452)
df <- rbind(
  data.frame(x = x, y = dnorm(x, mean = 0, sd = 0.5), 
             dist = "mu==0 ~ ',' ~ sigma^2==0.5"),
  data.frame(x = x, y = dnorm(x, mean = -2, sd = 0.5), 
             dist = "mu==-2 ~ ',' ~ sigma^2==0.5"),
  data.frame(x = x, y = dnorm(x, mean = 0, sd = 2), 
             dist = "mu==0 ~ ',' ~ sigma^2==2")
)

## Convertendo para fator 
df$dist <- factor(df$dist, 
                  levels = c("mu==-2 ~ ',' ~ sigma^2==0.5",
                             "mu==0 ~ ',' ~ sigma^2==0.5",
                             "mu==0 ~ ',' ~ sigma^2==2"))

## Figura
p2 <- ggplot(df, aes(x = x, y = y, colour = dist)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("cyan4", "gray50", "darkorange"),
    labels = c(expression(mu==-2 ~ "," ~ sigma^2==0.5),
               expression(mu==0 ~ "," ~ sigma^2==0.5),
               expression(mu==0 ~ "," ~ sigma^2==2))
  ) +
  labs(x = "x", y = "Densidade", color = "Parâmetros") +
  theme_classic(base_size = 14) +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = c(0.8, 0.8),         # Inside plot (x, y) coords
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA))

ggsave(plot = p2,
       filename ="Figuras/Dist_normal.png", 
       width = 8,
       height = 4,
       dpi = 350)



###################################
#    Histograma peso pinguins     #
###################################
#dados <- read.csv("~/Library/CloudStorage/OneDrive-Personal/Arbeit/Lectures_and_talks/UFRN/Lectures/ECL0072-GLM_GAM/Dados/palmerpenguins_extended.csv")

## Histograma
ph <- dados %>%
  ggplot(aes(x = body_mass_g)) +
  geom_histogram(bins = 13, size=1.2, col = 'white', fill = 'darkorange',alpha = 0.5) +
  #geom_histogram(bins = 13, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  #geom_density(size=1, col = 'gray40') +
  theme_classic(base_size = 16) +
  labs(y = "Frequência", x = "Peso (g)") +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.margin = margin(20,20,20,20))


ggsave(plot = ph,
       filename ="Figuras/histograma_pinguins.png", 
       width = 5,
       height = 4,
       dpi = 350)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Análise residual: homocedasticidade  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### Simulando os dados
set.seed(42)
n <- 100

x <- runif(n, 0, 10) #variável preditora


## Pressuposto é adequado

epsilon_bom <- rnorm(n, mean = 0, sd = 1) #Simulando a variável resposta
y_bom <- 5 + 2 * x + epsilon_bom #Equação da reta
mod_bom <- lm(y_bom ~ x) #Ajustando o modelo

df_bom <- data.frame(Predito = fitted(mod_bom), 
                     Residuo = rstudent(mod_bom)) #Criando o df

# Gráfico valores preditos vs. residuos
homo_p1 <- ggplot(df_bom, aes(x = Predito, y = Residuo)) +
  geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "cyan4", size = 0.9) +
  labs(title = "Homocedasticidade") +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.title = element_text(hjust = 0.5, face='bold'),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )



## Pressuposto é violado
epsilon_ruim <- rnorm(n, mean = 0, sd = 0.5 * x)
y_ruim <- 5 + 2 * x + epsilon_ruim


mod_ruim <-lm(y_ruim ~ x) #Ajustando o modelo

df_ruim <- data.frame(Predito = fitted(mod_ruim), 
                      Residuo = rstudent(mod_ruim)) #Criando o df


# Gráfico valores preditos vs. residuos
homo_p2 <- ggplot(df_ruim, aes(x = Predito, y = Residuo)) +
  geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "cyan4", size = 0.9) +
  labs(title = "Heterocedasticidade") +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.title = element_text(hjust = 0.5, face='bold'),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )


full_p3 <- (homo_p1 + homo_p2)

ggsave(plot = full_p3,
       filename ="Figuras/heterocedasticidade_plots.png", 
       width = 10,
       height = 5,
       dpi = 350)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#    Análise residual: correlação espacial    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

set.seed(456) #Para reprodutibilidade


## Definindo coordenadas geográficas ##
n <- 200 #No. de observações
x <- runif(n, 0, 100) #=longitude
y <- runif(n, 0, 100) #=latitude
coords <- data.frame(x, y)



## Dado com efeito espacial
efeito_s <- exp(-((x - 50)^2 + (y - 50)^2) / (2 * 30^2)) #cria hotspot na região mais central
lambda_s <- exp(1 + 2 * efeito_s) #log link
contagem_s <- rpois(n, lambda_s)
df_s <- data.frame(coords, contagem = contagem_s)


## Dado sem efeito espacial
lambda_n <- exp(1) 
contagem_n <- rpois(n, lambda_n)
df_n <- data.frame(coords, contagem = contagem_n)


## Ajustando um modelo Poisson ##
mods <- glm(contagem ~ 1, family = poisson, data = df_s) #com efeito espacial
modn <- glm(contagem ~ 1, family = poisson, data = df_n) #sem efeito espacial


## Extraíndo resíduos ##
df_s$residuals <- residuals(mods, type = "pearson") #agregando os res. ao banco dedados
df_n$residuals <- residuals(modn, type = "pearson") #agregando os res. ao banco dedados


## Plot com efeito espacial
p3 <- ggplot(df_s, aes(x = x, y = y)) +
  geom_point(aes(size = abs(residuals),
                 color = residuals > 0),
             alpha = 0.6) +
  scale_color_manual(values = c("cyan4", "darkorange"),
                     labels = c("Negativo", "Positivo")) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  labs(x = "X", y = "Y",
       title = "Com correlação espacial",
       color = "Resíduo", size = "") +
  coord_fixed() +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, face='bold'),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.margin = margin(20, 20, 20, 20))


## Plot sem efeito espacial
p4 <- ggplot(df_n, aes(x = x, y = y)) +
  geom_point(aes(size = abs(residuals),
                 color = residuals > 0),
             alpha = 0.6) +
  scale_color_manual(values = c("cyan4", "darkorange"),
                     labels = c("Negativo", "Positivo")) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  labs(x = "X", y = "Y",
       title = "Sem correlação espacial",
       color = "Resíduo", size = "") +
  coord_fixed() +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face='bold'),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.margin = margin(20, 20, 20, 20))

fp <- p3  + plot_spacer() + p4 + plot_layout(widths = c(4, 1, 4))
fp <- (p3 + p4)


ggsave(plot = fp,
       filename ="Figuras/corr_espacial.png", 
       width = 12,
       height = 7,
       dpi = 350)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#    Análise residual: correlação temporal    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

set.seed(567)
## Simulando um processo AR(1) ##
n <- 200 #No. de observacoes
time <- 1:n # No. de passos de tempo

phi <- 0.8 #parâmetro de correlação. Varia de 0 a 1; quanto maior valor, maior dependência residual
ar_errors <- rep(0, n)
white_noise <- rnorm(n, mean = 0, sd = 5)

for (t in 2:n) {
  ar_errors[t] <- phi * ar_errors[t-1] + white_noise[t]
}


## Criando a serie temporal ##
y <- 5 + 0.5 * time + ar_errors


## Ajustando o modelo linear ##
mod <- lm(y ~ time)


## Calculando valores de ACF ##
acf_data <- acf(residuals(mod), lag.max = 20, plot = FALSE)
acf_df <- with(acf_data, data.frame(lag, acf)) # convertendo para df


## Calculando intervalo de confiança ##
ci <- qnorm(0.975) / sqrt(n)

ci_df <- data.frame(x = c(-Inf, Inf), ymin = rep(-ci,2), ymax = rep(ci,2))


# Figura
acf_p1 <- ggplot(data = acf_df, aes(x = lag, y = acf)) +
  # Add the ACF bars
  geom_ribbon(data = ci_df,
              aes(x = x,ymin = ymin, ymax = ymax),
              inherit.aes = FALSE,
              fill = "darkorange", alpha = 0.2) +
  geom_segment(aes(xend = lag, yend = 0), color = "cyan4", size = 1.2) +
  # Add the horizontal zero line
  geom_hline(yintercept = 0, color = "cyan4", size = 0.9) +
  # Add the confidence interval lines (dashed blue)
  geom_hline(yintercept = ci, linetype = "dashed", color = "darkorange", size = 0.7) +
  geom_hline(yintercept = -ci, linetype = "dashed", color = "darkorange", size = 0.7) +

  # Customize labels and title
  labs(
    title = "Com dependência residual",
    x = "Lag",
    y = "ACF"
  ) +
  # Set the theme for a clean look
  theme_minimal(base_size = 16) +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, face='bold'),
    legend.position = "none"
  )



## Pressuposto adequado ##

# Criando o modelo simples
y2 <- 5 + 0.5 * time + rnorm(n, mean = 0, sd = 10)

# Ajustando o modelo
mod2 <- lm(y2 ~ time)


# Calculando valores de ACF
acf_data2 <- acf(residuals(mod2), lag.max = 20, plot = FALSE)
acf_df2 <- with(acf_data2, data.frame(lag, acf)) # convertendo para df

# Calculando intervalo de confiança
ci2 <- qnorm(0.975) / sqrt(n)

ci_df2 <- data.frame(x = c(-Inf, Inf), ymin = rep(-ci2,2), ymax = rep(ci2,2))



# Figura
acf_p2 <- ggplot(data = acf_df2, aes(x = lag, y = acf)) +
  geom_ribbon(data = ci_df,
              aes(x = x,ymin = ymin, ymax = ymax),
              inherit.aes = FALSE,
              fill = "darkorange", alpha = 0.2) +
  # Add the ACF bars
  geom_segment(aes(xend = lag, yend = 0), color = "cyan4", size = 1.2) +
  # Add the horizontal zero line
  geom_hline(yintercept = 0, color = "cyan4", size = 0.9) +
  # Add the confidence interval lines (dashed blue)
  geom_hline(yintercept = ci, linetype = "dashed", color = "darkorange", size = 0.7) +
  geom_hline(yintercept = -ci, linetype = "dashed", color = "darkorange", size = 0.7) +
  # Customize labels and title
  labs(
    title = "Sem dependência residual",
    x = "Lag",
    y = "ACF"
  ) +
  # Set the theme for a clean look
  theme_minimal(base_size = 16) +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.title = element_text(hjust = 0.5, face='bold'),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )

full_p2 <- (acf_p1 + acf_p2)

ggsave(plot = full_p2,
       filename ="Figuras/corr_temporal.png", 
       width = 10,
       height = 5,
       dpi = 350)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#    Análise residual: Normalidade    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


## Simulando um dado não-normal
set.seed(333)
#epsilon_ruim_norm <- rchisq(n, df = 1) - 20 # mean of chi-squared with df=3 is 3
#y_ruim_norm <- 5 + 2 * x + epsilon_ruim_norm
epsilon_ruim_norm <- rgamma(n, shape = 1, scale = 15) - 2
y_ruim_norm <- 5 + 2 * x + epsilon_ruim_norm
# epsilon_ruim_norm <- rt(n, df = 2)
# y_ruim_norm <- 5 + 2 * x + epsilon_ruim_norm

# Ajustando o modelo
mod_ruim_norm <- lm(y_ruim_norm ~ x)

#qqnorm(resid(mod_ruim_norm))
#qqline(resid(mod_ruim_norm))


# Criando o df
df_ruim_norm <- data.frame(Predito = fitted(mod_ruim_norm), 
                           Residuo = residuals(mod_ruim_norm)) #Criando o df


## Histograma (violação de normalidade)
hist_p1 <- ggplot(df_ruim_norm, aes(x = Residuo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  theme_minimal(base_size = 16) +
  labs(y = "Densidade", x = "Resíduo", title = "Violação de normalidade") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))+
  theme(  axis.title.y = element_text(vjust = 3),
          axis.title.x = element_text(vjust = -3),
          plot.title = element_text(hjust = 0.5, face='bold'),
          plot.margin = margin(20,20,20,20))


## QQplot (violação de normalidade)
qq_p1 <- ggplot(df_ruim_norm, aes(sample = Residuo)) +
  stat_qq(size = 3.5, col ='darkorange', alpha = 0.5) +
  stat_qq_line(linetype = "dashed", color = "cyan4", size = 0.9) +
  labs(y ="Quantis observados", x = "Quantis teóricos", title = "Violação de normalidade") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.title = element_text(hjust = 0.5, face='bold'),
        plot.margin = margin(20,20,20,20))


## Histograma (Normalidade)
hist_p2 <- ggplot(df_bom, aes(x = Residuo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  theme_minimal(base_size = 16) +
  labs(y = "Densidade", x = "Resíduo", title = "Normalidade") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))+
  theme(  axis.title.y = element_text(vjust = 3),
          axis.title.x = element_text(vjust = -3),
          plot.title = element_text(hjust = 0.5, face='bold'),
          plot.margin = margin(20,20,20,20))


## QQplot (normalidade)
qq_p2 <- ggplot(df_bom, aes(sample = Residuo)) +
  stat_qq(size = 3.5, col ='darkorange', alpha = 0.5) +
  stat_qq_line(linetype = "dashed", color = "cyan4", size = 0.9) +
  labs(y ="Quantis observados", x = "Quantis teóricos", title = "Normalidade") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.title = element_text(hjust = 0.5, face='bold'),
        plot.margin = margin(20,20,20,20))



#full_p4 <- (hist_p2 + hist_p1) / (qq_p2 + qq_p1)
full_p4 <- (qq_p2 + qq_p1)

full_p4


ggsave(plot = full_p4,
       filename ="Figuras/normalidade_plots.png", 
       width = 10,
       height = 5,
       dpi = 350)



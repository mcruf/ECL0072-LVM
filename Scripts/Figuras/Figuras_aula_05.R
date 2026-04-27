#                                 #
#          - Figuras -            #
#            Aula 05              #
#                                 #
###################################


# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(dplyr)
library(tidyr)


# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



######################
# Distribuição Gama #
#####################

# Parâmetros expressos em termos de alpha (shape) e beta (rate)
# ao invés de k e theta


alpha_vals <- c(1, 2, 4)   # diferentes formatos
beta <- 4                  # taxa fixa (beta = 1/theta)


# Sequência de x
x <- seq(0, 3, length.out = 100)



# Densidades
dens <- expand.grid(x = x, alpha = alpha_vals) |>
  mutate(dens = dgamma(x, shape = alpha, rate = beta),
         curva = paste0("α=", alpha, ";"," β=4", sep =""))


# Figura
p1 <- ggplot(dens, aes(x, dens, color = curva)) +
      geom_line(linewidth = 1.2) +
      #geom_point() +
      # linha vertical em x=1
      coord_cartesian(xlim = c(0, 3)) +
      theme_minimal(base_size = 16) +
      #scale_color_brewer(palette = "Spectral") +
      scale_color_manual(values = c("#0a9396", "#bf4342", "#ee9b00")) +
      labs(x = "y", y = "Densidade", color = "Parâmetro") 

  
ggsave(plot = p1,
         filename ="Figuras/Distribuicao_Gamma.png", 
         width = 10,
         height = 4,
         dpi = 350)



###########################
# Distribuição log-Normal #
###########################


## Definindo parâmetros da log-Normal (mu fixo) ##
log_mu <- 0  #mu na escala log (exp(0) = 1)
log_sigma <- c(0.2, 0.5,1.5)  # sd na escala log


## Criando a variável y ##
y_max <- 10
y <- seq(1e-4, y_max, length.out = 1000)


## Simulando a log-Normal ##
dens_df <- expand.grid(x = y, mu = log_mu, sigma = log_sigma) %>%
           mutate(density = dlnorm(x, meanlog = mu, sdlog = sigma),
                  label = paste0("mu=", mu, ", sigma=", sigma))


## Plot ##

p2 <- ggplot(dens_df, aes(x = x, y = density, color = label)) +
  geom_line(size = 1.05) +
  coord_cartesian(xlim = c(0, y_max)) +
  labs(x = "y (escala original)", y = "Densidade", color = "Parâmetros", fill = "") +
  scale_color_manual(values = c("#0a9396", "#bf4342", "#ee9b00")) +
  theme_minimal(base_size = 16)




ggsave(plot = p2,
       filename ="Figuras/Distribuicao_logNormal.png", 
       width = 10,
       height = 4,
       dpi = 350)


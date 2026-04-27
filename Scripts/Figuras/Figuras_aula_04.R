#                                 #
#          - Figuras -            #
#            Aula 04              #
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



########################
# Distribuição Poisson #
########################


## Definindo o parâmetro (lambda) ##
#lambdas <- c(4, 10, 15, 30)
lambdas <- c(4, 10, 30)


## Simulando os dados ##
df_pois <- lapply(lambdas, function(lambda) {
  x <- 0:qpois(0.999, lambda) # include 99.9% of mass
  data.frame(x = x,
             y = dpois(x, lambda),
             lambda = factor(lambda))}) %>%
  bind_rows()


## Plot
p1 <- ggplot(df_pois, aes(x = x, y = y, fill = lambda)) +
  #geom_line(linewidth = 1.2) +
  geom_col(position = "identity",col = 'white', width = 1) +
  theme_minimal(base_size = 14) +
  #scale_fill_brewer(palette = "Spectral",direction = -1) +
  scale_fill_manual(values = c("#0a9396", "#bf4342", "#ee9b00")) +
  facet_wrap(lambda ~ .) +
  theme_minimal(base_size = 15) +
  labs( x = "Contagem",
        y = "Probabilidade",
        fill = expression(lambda)) +
  theme(strip.text = element_blank())
p1

# ## Simulando os dados ##
# simu1 <- data.frame(x = 0:10, 
#                     y = dpois(0:10, lambda=3), 
#                     media = "E[Y] = 3")
# 
# simu2 <- data.frame(x = 0:10, 
#                     y = dpois(0:10, lambda=5),
#                     media = "E[Y] = 5")
# 
# simu3 <- data.frame(x = 0:40, 
#                     y = dpois(0:40, lambda=10), 
#                     media = "E[Y] = 10")
# 
# 
# simu4 <- data.frame(x = 60:140, 
#                     y = dpois(60:140, lambda=100), 
#                     media = "E[Y] = 100")
# 
# allsim <- rbind(simu1,simu2,simu3,simu4)
# allsim$media <- as.factor(allsim$media)
# 
# 
# ## Plot ##
# p1 <- ggplot(allsim, aes(x, y)) +
#   geom_segment(aes(x=x, xend=x, y=0, yend=y), linewidth=1.1, col = 'cyan4') +
#   geom_point(size=1.7,col = 'cyan4') +
#   facet_wrap(as.factor(media) ~ ., scales="free") +
#   ggpubr::theme_pubclean(base_size = 16) +
#   theme_bw(base_size = 16) +
#   theme(strip.background = element_rect(fill = "white"),
#         strip.text = element_text(face = "bold", size = 18),
#         panel.grid.major = element_line(linetype = "dotted"),
#         panel.grid.minor = element_line(linetype = "dotted"),
#         panel.border = element_blank(),
#         panel.spacing = unit(4, "lines"),
#         strip.placement = "outside")
# #p1

ggsave(plot = p1,
       filename ="Figuras/Distribuicao_Poisson.png", 
       width = 6,
       height = 2,
       dpi = 350)



##################
# Sobredispersão #
##################

# Para ilustrar o exemplo, usaremos a distribuição Poisson
# para representar os dados preditos.
# Para representar os dados observados, usaremos uma distribuição
# Binomial Negativa, que acomoda sobredispersão.


set.seed(987) #Para garantir a reprodutibilidade da simulação

## Definindo os parâmetros ##
nsim <- 1000 #No. de simulacoes
lambda <- 10 #Média de eventps
phi <- 4 #Sobredispersão

## Simulando os dados ##
predito <- rpois(nsim, lambda)
observado <- rnbinom(nsim, mu = lambda, size = phi) 

# Combine into dataframe with counts per value
df <- data.frame(
  valor = c(predito, observado),
  tipo = rep(c("Predito", "Observado"), each = nsim)
)


# Histogram bins
breaks <- seq(0, max(df$valor), by = 1)


## Plot ##
p2 <- ggplot(df, aes(x = valor, fill = tipo, color = tipo)) +
      geom_histogram(position = "stack",
                     bins = length(breaks),
                     alpha = 0.3) +
      scale_fill_manual(values = c("darkorange", "cyan4")) +
      scale_colour_manual(values = c("darkorange", "cyan4")) +
      labs(x = "Contagem",
           y = "Frequência") +
      theme_minimal(base_size = 16) +
      theme(#legend.position = "top",
            legend.position = c(0.95, 0.95),  # x and y coordinates (0 to 1 scale)
            legend.justification = c("right", "top"), # Align legend's right-top corner with the coordinates
            legend.title= element_blank())


ggsave(plot = p2,
       filename ="Figuras/Overdispersion.png", 
       width = 8,
       height = 5,
       dpi = 350)




#############################
# Sobredispersão - resíduos #
#############################

## Ajustar um modelo sobredisperso ##
set.seed(123)

n_obs <- 1000

x1 <- rnorm(n_obs)
y <- rpois(n_obs, lambda = exp(1 + 0.5 * x1))

mod <- glm(y ~ x1, family = "poisson")


## Calculando os resíduos de Pearson ##
df_res <- augment(mod, type.residuals = "pearson")


## Plot ##
p3 <- ggplot(df_res, aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0.4, size = 4, col = 'darkorange') +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", size =1.2) +
      labs(x = "Valores Ajustados",
           y = "Resíduos de Pearson") +
      theme_minimal(base_size = 16)


ggsave(plot = p3,
       filename ="Figuras/Overdispersion_residuals.png", 
       width = 8,
       height = 5,
       dpi = 350)



##################################
# Distribuição Binomial Negativa #
##################################


## Função para simular a distribuição BN ##
nb_data <- function(mu, sizes, prob_cutoff = 0.995) {
  df <- do.call(rbind, lapply(sizes, function(s) {
    # Dynamic cutoff for each size
    max_x <- qnbinom(prob_cutoff, mu = mu, size = s)
    x <- 0:max_x
    data.frame(x = x, y = dnbinom(x, mu = mu, size = s), size = factor(s))
  }))
  df
}

## Definindo os parâmetros ##
mu <- 10
#alpha <- c(0.5, 1, 2, 5, 20, 1000)
alpha <- c(0.5, 2, 20)



# Gerando os dados
df <- nb_data(mu, alpha, prob_cutoff = 0.99) # cut tail at 99% mass

## Plot
df$size <- as.factor(df$size)
p4 <- ggplot(df, aes(x = x, y = y, fill = size)) +
      geom_col(position = "identity") +
      theme_minimal(base_size = 14) +
      #scale_fill_brewer(palette = "Spectral",direction = -1) +
      scale_fill_manual(values = c("#0a9396", "#bf4342", "#ee9b00")) +
      facet_wrap(size ~ . ) +
      theme_minimal(base_size = 13) +
      labs( x = "Contagem",
            y = "Probabilidade",
            fill = expression(size)) +
      theme(strip.text = element_blank())


ggsave(plot = p4,
       filename ="Figuras/Distribuicao_BN.png", 
       width = 6,
       height = 2,
       dpi = 350)

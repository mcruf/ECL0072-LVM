#                                 #
#          - Figuras -            #
#            Aula 07              #
#                                 #
###################################


# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)


# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



######################
# Distribuição beta #
#####################


## Sequência de valores entre 0 e 1 (x 100)
x <- seq(0, 1, length.out = 100)


## Lista com diferentes combinações de parâmetros
parametros <- list(c(1, 1), 
                   c(2, 2),
                   c(2, 5),
                   c(5, 2))


## Dataframe com densidades
dados <- data.frame()

for (p in parametros) {
  #print(p)
  alfa <- p[1]; beta <- p[2]
  dens <- dbeta(x, alfa, beta)
  dados <- rbind(dados,
                 data.frame(x = x,
                            densidade = dens,
                            parametro = paste0("α=", alfa, "; β=", beta)))
}



# Plot
p1 <- ggplot(dados, aes(x = x, y = densidade, color = parametro)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c('gray50', "#bf4342","#ee9b00","#0a9396" )) +
      labs(title = "",
           x = "x",
           y = "Densidade",
           color = "Parâmetro") +
      theme_minimal(base_size = 16) 


ggsave(plot = p1,
       filename ="Figuras/Distribuicao_Beta.png", 
       width = 10,
       height = 4,
       dpi = 350)




##########################
# Dado zero-inflacionado #
##########################

dados <- readRDS("Dados/plant_herbivore.rds")


tmp <- as.data.frame(table(dados$Damaged))

p2 <- ggplot(tmp, aes(x=Var1, y = Freq)) + 
      geom_bar(stat="identity", fill='darkorange',width = 0.5) +
      #geom_rug(alpha = 0.5) +
      labs(y = "Frequência", 
           x = "Número de cabeças de flores predadas") +
      scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(breaks = seq(from = 0,to = max(tmp$Freq),by = 2)) +
      ggpubr::theme_pubr(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p2

# p2 <- ggplot(dados, aes(x =Damaged)) +
#   geom_histogram(bins = 20, 
#                  fill = 'darkorange', 
#                  col = 'white', 
#                  alpha = 0.7) +
#   labs(y = "Frequência", 
#        x = "Número de cabeças de flores predadas") +
#   theme_minimal(base_size = 16)

ggsave(plot = p2,
       filename ="Figuras/Dado_zeroinflacionado.png", 
       width = 12,
       height = 4,
       dpi = 350)




########################
# Distribuição Tweedie #
########################


# Pacotes
library(tweedie)
library(ggplot2)
library(dplyr)


## Função para simular de acordo com p
sim_tweedie <- function(p, n = 1000, mu = 2, phi = 1) {
  
  ## A função rtweedie não aceita valores de p < 1;
  ## Precisamos reparametrizar as distribuições tradicionais
  if (p == 0) {
    y <- rnorm(n, mean = mu, sd = sqrt(phi))
  } else if (p == 1) {
    y <- rpois(n, lambda = mu)
  } else if (p == 2) {
    y <- rgamma(n, shape = mu/phi, scale = phi)
  } else if (p > 1 & p < 2) {
    y <- rtweedie(n, mu = mu, phi = phi, power = p)
  } else {
    stop("Este exemplo só cobre p = 0, 1, 1<p<2, 2")
  }
  tibble(y = y, p = as.factor(p))
}

## Simulando
set.seed(123)

dados <- bind_rows(sim_tweedie(p = 0), #Gaussiana
                   sim_tweedie(p = 1), #Poisson
                   sim_tweedie(p = 1.2), #Tweedie (entre 1 e 2)
                   sim_tweedie(p = 2))  #Gamma)

# Gráfico
facet_names <- c(`0` = "Normal (p=0)",
                 `1` = "Poisson (p=1)",
                 `1.2` = "Tweedie (p=1.2)",
                 `2` = "Gamma (p=2)")

p3 <- ggplot(dados, aes(x = y)) +
      geom_histogram(bins = 40, fill = "darkorange", color = "white", alpha = .7) +
      facet_wrap(~p, scales = "free", labeller = as_labeller(facet_names)) +
      theme_minimal(base_size = 16) +
      labs(x = "Valores simulados de Y", y = "Frequência")



ggsave(plot = p3,
       filename ="Figuras/Distribuição_tweedie.png", 
       width = 9,
       height = 4,
       dpi = 350)

#                                 #
#          - Figuras -            #
#            Aula 06              #
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


############################
# Distribuição multinomial #
############################

## Parâmetros
n_obs <- 1000 #Número de simulações
n_items <- 25 #Número total de itens em cada tentativa
probs <- c(0.65, 0.25, 0.10) # Probabilidades para Categoria 1, 2 e 3

## Simular a distribuição multinomial
set.seed(123) #Para reprodutibilidade dos resultados
multinom_data <- rmultinom(n = n_obs, size = n_items, prob = probs)

## Converter a matriz em um data.frame
df <- as.data.frame(t(multinom_data))
colnames(df) <- c("Categoria 1", "Categoria 2", "Categoria 3")

## Transformar o data.frame para o formato "long" (necessário para o ggplot)
df_long <- df %>%
  pivot_longer(everything(), names_to = "Categoria", values_to = "Numero_de_Ocorrencias")


## Plots
p1 <- ggplot(df_long, aes(x = Numero_de_Ocorrencias, fill = Categoria)) +
  geom_histogram(col = 'white',
    aes(y = after_stat(count) / sum(after_stat(count))),
    position = "identity",
    alpha = 0.5,
    binwidth = 1) + # Define a largura das barras
  labs(x = "Número de ocorrências",y = "Probabilidade") +
  scale_fill_manual(values = c("Categoria 1" = "darkorange", "Categoria 2" = "gray50", "Categoria 3" = "cyan4")) +
  theme_minimal(base_size = 16) +
  theme(legend.position = 'top',
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank())

p1

ggsave(plot = p1,
       filename ="Figuras/Distribuicao_Multinomial.png", 
       width = 10,
       height = 4,
       dpi = 350)

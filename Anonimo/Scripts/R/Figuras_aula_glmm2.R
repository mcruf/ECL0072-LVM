#                                 #
#          - Figuras -            #
#         Aula GLMM II (5)        #
#                                 #
###################################


# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(ggpubr)
library(patchwork)
library(dplyr)
library(tidyr)



# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-GLM_GAM/")



###########################
#  Exemplo introdutório   #
###########################

## Importando os dados
dados <- readRDS("Dados/bioluminescence.rds")


## Plotando...
dados %>%
  filter(Station %in% c(6,7,8,9,11,16,18,19)) %>% #Filtrando apenas as estações com o mesmo padrão de bioluminescencia
  ggplot(aes(x=SampleDepth, y = Sources)) +
  geom_point(col = '#d5bdaf', pch = 19, alpha =0.5, size = 3) +
  labs(x = "Profundidade (m)",
       y = "Bioluminescência") +
  theme_light(base_size = 14) +
  my_theme


ggsave(plot = p1,
       filename ="Figuras/Exemplo_GAM.png", 
       width = 8,
       height = 5,
       dpi = 350) 

#                                 #
#          - Figuras -            #
#            Aula 08              #
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
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



###########################
#  Exemplo introdutório   #
###########################

## Importando os dados
dados <- readRDS("Dados/bioluminescence.rds")


## Plotando...
p1 <- dados %>%
      filter(Station %in% c(6,7,8,9,11,16,18,19)) %>% #Filtrando apenas as estações com o mesmo padrão de bioluminescencia
        ggplot(aes(x=SampleDepth, y = Sources)) +
        geom_point(col = 'darkorange', pch = 19, alpha =0.5, size = 3) +
        labs(x = "Profundidade (m)",
             y = "Bioluminescência") +
        theme_minimal(base_size = 16) 


ggsave(plot = p1,
       filename ="Figuras/Exemplo_GAM.png", 
       width = 8,
       height = 5,
       dpi = 350) 


##########################################
#  LM com linhas de ajuste polinomiais   #
#########################################


## Ajuste LM simples
p2 <- dados %>%
        filter(Station %in% c(6,7,8,9,11,16,18,19)) %>% #Filtrando apenas as estações com o mesmo padrão de bioluminescencia
        ggplot(aes(x=SampleDepth, y = Sources)) +
        geom_point(col = 'darkorange', pch = 19, alpha =0.5, size = 3) +
        geom_smooth(method = "lm", formula = y~x, col = 'cyan4', fill = 'gray60') +
        labs(x = "Profundidade (m)",
             y = "Bioluminescência",
             title = "LM simples") +
        theme_minimal(base_size = 14) +
        theme(#plot.margin = unit(c(1, 35, 1, 1), "pt"),
              plot.title = element_text(hjust = 0.5, face = 'bold'))


## Ajuste LM com função quadrática
p3 <- dados %>%
  filter(Station %in% c(6,7,8,9,11,16,18,19)) %>% #Filtrando apenas as estações com o mesmo padrão de bioluminescencia
  ggplot(aes(x=SampleDepth, y = Sources)) +
  geom_point(col = 'darkorange', pch = 19, alpha =0.5, size = 3) +
  geom_smooth(method = "lm", formula = y~poly(x,2), col = 'cyan4', fill = 'gray60') +
  labs(x = "Profundidade (m)",
       y = "Bioluminescência",
       title = "LM com termo quadrático") +
  theme_minimal(base_size = 14) +
  theme(#plot.margin = unit(c(1,1,1,35), "pt"),
        plot.title = element_text(hjust = 0.5, face = 'bold'))


## Ajuste LM com função cúbica
p4 <- dados %>%
  filter(Station %in% c(6,7,8,9,11,16,18,19)) %>% #Filtrando apenas as estações com o mesmo padrão de bioluminescencia
  ggplot(aes(x=SampleDepth, y = Sources)) +
  geom_point(col = 'darkorange', pch = 19, alpha =0.5, size = 3) +
  geom_smooth(method = "lm", formula = y~poly(x,3), col = 'cyan4', fill = 'gray60') +
  labs(x = "Profundidade (m)",
       y = "Bioluminescência",
       title = "LM com termo cúbico") +
  theme_minimal(base_size = 14) +
  theme(#plot.margin = unit(c(30, 35, 1, 1), "pt"),
        plot.title = element_text(hjust = 0.5, face = 'bold'))



## Ajuste LM com termo à quarta
p5 <- dados %>%
  filter(Station %in% c(6,7,8,9,11,16,18,19)) %>% #Filtrando apenas as estações com o mesmo padrão de bioluminescencia
  ggplot(aes(x=SampleDepth, y = Sources)) +
  geom_point(col = 'darkorange', pch = 19, alpha =0.5, size = 3) +
  geom_smooth(method = "lm", formula = y~poly(x,4), col = 'cyan4', fill = 'gray60') +
  labs(x = "Profundidade (m)",
       y = "Bioluminescência",
       title = "LM com termo à quarta") +
  theme_minimal(base_size = 14) +
  theme( #plot.margin = unit(c(30, 0, 0, 35), "pt"),
        plot.title = element_text(hjust = 0.5, face = 'bold'))

# p_final <- ggarrange(p2, p3, p4, p5, 
#                      ncol = 2, nrow = 2)
#p_final <- (p2+ plot_spacer() + p3)/(p4+plot_spacer() +p5)


p_final <- (p2 + p3)
p_final2 <- (p4 + p5)


ggsave(plot = p_final2,
       filename ="Figuras/Exemplo_GAM2b.png", 
       width = 9,
       height = 4,
       dpi = 350) 


#########
#  GAM  #
#########

p6 <- dados %>%
      filter(Station %in% c(6,7,8,9,11,16,18,19)) %>% #Filtrando apenas as estações com o mesmo padrão de bioluminescencia
      ggplot(aes(x=SampleDepth, y = Sources)) +
      geom_point(col = 'darkorange', pch = 19, alpha =0.5, size = 3) +
      geom_smooth(method = "gam", col = 'cyan4', fill = 'gray60') +
      labs(x = "Profundidade (m)",
           y = "Bioluminescência") +
      theme_minimal(base_size = 16) 


ggsave(plot = p6,
       filename ="Figuras/Exemplo_GAM2c.png", 
       width = 8,
       height = 5,
       dpi = 350) 



###################################
#  Decomposição da função cúbica  #
###################################

# Decompondo a função em suas funções base

## Figura alinhada com o exemplo da bioluminescencia vs. profundidade


## Montando o banco de dados

### Coeficientes para simular padrão ecológico
b1 <- 10.79     # intercepto (média da bioluminescencia - tirada do dado)
b2 <- -0.02   # termo linear
b3 <- 0.00001 # termo quadrático
b4 <- -0.000000001 # termo cúbico


### Criando sequência de profundidade
depth <- seq(0, 5000, length.out = 200)


### Calculando cada termo da função cúbica e salvando em data frame
df <- data.frame(depth = depth,
                 intercept = b1,
                 linear = b2 * depth,
                 quad = b3 * depth^2,
                 cubic = b4 * depth^3)


### Função final (bioluminescência prevista)
df <- df %>%
      mutate(biolum = intercept + linear + quad + cubic)


### Reorganizar o df em formato longo (para ggplot)
df_long <- df %>%
  pivot_longer(-depth, names_to = "term", values_to = "value") %>%
  mutate(term = factor(term, levels = c("intercept", "linear", "quad", "cubic", "biolum")))


### Plotando...

p7 <- ggplot(df_long, aes(x = depth, y = value)) +
        geom_line(aes(color = term, size = term)) +
        scale_size_manual(values=c(rep(1.5, 5))) +
        scale_colour_manual(values = c('cyan4', 'cyan4', 'cyan4', 'cyan4','darkorange')) + 
        facet_wrap(~term, 
                   scales = "free_y",
                   ncol = 3,
                   labeller = as_labeller(c(intercept = "Intercepto",
                                            linear    = "x",
                                            quad      = "x²",
                                            cubic     = "x³",
                                            biolum    = "f(x)"))) +
        labs(x = "Profundidade (m)", y = "Bioluminescência") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none",
              panel.grid.major = element_blank(),
              strip.text = element_text(face="bold", size=15),
              strip.background = element_rect(fill = 'gray95', color = 'gray95')) 

ggsave(plot = p7,
       filename ="Figuras/GAM_funcoes_base.png", 
       width = 10,
       height = 5,
       dpi = 350) 





#############################
#  Definindo número de nós  #
#############################

## Filtrando os dados para manter estações com mesmo padrão
dados2 <- filter(Station %in% c(6,7,8,9,11,16,18,19)) #Filtrando apenas as estações com o mesmo padrão de bioluminescencia
  

table(cut(dados2$SampleDepth, 3))

k3 <- ggplot(dados2, aes(x = SampleDepth, y = Sources)) +
        geom_point(pch = 19, alpha =0.5, size = 3,
                   col = '#efb366') +
        geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 3),
                    col = 'cyan4', fill = '#6eac99') +
        geom_vline(xintercept = c(1970, 3420, 4.87e+03), linetype = 2) +
        labs(x = "Profundidade (m)", y = "Bioluminescência",
             title = "k = 3") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none",
              panel.grid.major = element_blank(),
              plot.title = element_text(hjust = 0.5, face = 'bold'))



table(cut(dados2$SampleDepth, 5))

k5 <- ggplot(dados2, aes(x = SampleDepth, y = Sources)) +
        geom_point(pch = 19, alpha =0.5, size = 3,
                   col = '#efb366') +
        geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5),
                    col = 'cyan4', fill = '#6eac99') +
        geom_vline(xintercept = c(1.39e+03, 2.26e+03, 3.13e+03, 4e+03, 4.87e+03), linetype = 2) + 
        labs(x = "Profundidade (m)", y = "Bioluminescência",
             title = "k = 5") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none",
              panel.grid.major = element_blank(),
              plot.title = element_text(hjust = 0.5, face = 'bold'))

table(cut(dados2$SampleDepth, 8))

k8 <- ggplot(dados2, aes(x = SampleDepth, y = Sources)) +
  geom_point(pch = 19, alpha =0.5, size = 3,
             col = '#efb366') +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 8),
              col = 'cyan4', fill = '#6eac99') +
  geom_vline(xintercept = c(1.06e+03, 1.6e+03, 2.15e+03, 2.69e+03, 3.23e+03, 3.78e+03, 4.32e+03, 4.87e+03), linetype = 2) +
  labs(x = "Profundidade (m)", y = "Bioluminescência",
       title = "k = 8") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'))


p9 <- (k3 + k5 + k8)
p9


ggsave(plot = p9,
       filename ="Figuras/GAM_knots.png", 
       width = 10,
       height = 4,
       dpi = 350) 


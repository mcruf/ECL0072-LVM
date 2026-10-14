#############################################################################
#                                                                           #
#                            Prática da aula 01                             #
#                           - Regressão linear -                            #
#                                                                           #
#############################################################################


# CONTEXTO:


# OBJETIVOS:


# ESTRUTURA DOS DADOS



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(patchwork)



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



#~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~

df <- readRDS("Dados/palmerpenguins_extended.rds")
head(df)
str(df)



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Tratamento dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Há variáveis classificadas como 'character'.Precisamos transformar elas para 'factor', de modo que sejam
# reconhecidas como categorias e assim permitir a realização de comparações estatísticas.

# Selecionando as colunas a serem transformadas
cols <- c("species", "island","sex", "diet", "life_stage", "health_metrics")

# Transformando para fator
df[, cols] <- lapply(df[, cols], factor)
str(df) # Sanity check




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Análise exploratória dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table(is.na(df)) # Há NA nos dados? Se sim, a análise exploratória deverá ser feita com mais cautela.



# 3.1) Variável resposta
#~~~~~~~~~~~~~~~~~~~~~~~~~
summary(df$body_mass_g)


## Histograma
phist <- ggplot(df, aes(x = body_mass_g)) +
         geom_histogram(fill = '#3e5c76', col = 'white', alpha = 0.8, linewidth = 1) +
         coord_cartesian(expand = c(0,0)) +
         labs(x = 'Body mass (g)', y = 'Frequency') +
         theme_bw(base_size = 12)


## Boxplot
pbox <- ggplot(df, aes(y = body_mass_g)) +
        geom_boxplot(fill = 'white', col = '#3e5c76', alpha = 0.8, linewidth = 1.2, outlier.size = 3) +
        labs(y = 'Body mass (g)') +
        theme_bw(base_size = 12) +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())


## Dotchart
pdot <- df %>% 
        mutate(row_id = row_number()) %>% 
        ggplot(aes(x = body_mass_g, y = row_id)) +
        geom_point(color = '#3e5c76', size = 2.5, alpha = 0.4) +
        labs(x = "Body mass (g)", y = NULL) +
        scale_x_continuous(breaks = seq(2000, 11000, by = 1000),
                           minor_breaks = seq(2000, 11000, by = 100)) +
        theme_classic(base_size = 12) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              panel.grid.major.x = element_line(color = "gray85", linewidth = 0.4),
              panel.grid.minor.x = element_line(color = "gray92", linewidth = 0.2))


phist + pbox + pdot




# 3.2) Relação peso-corporal vs. tamanho do bico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(df, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(size = 3, alpha = 0.3, col = '#3e5c76') +
  geom_smooth(method = 'lm', fill = "gray20", col = 'gray20') +
  labs(x = 'Bill length (mm)', y = 'Body mass (g)') +
  theme_light(base_size = 14) 


# Há distinção na relação entre os sexos?
ggplot(df, aes(x = bill_length_mm, y = body_mass_g, col = sex)) +
  geom_point(size = 3, alpha = 0.3) +
  scale_fill_manual(values = c("#846267", "#f6bd60")) +
  scale_color_manual(values = c("#846267", "#f6bd60")) +
  geom_smooth(method = 'lm', fill = "gray30", col = 'gray30') +
  labs(x = 'Bill length (mm)', y = 'Body mass (g)') +
  theme_light(base_size = 14) 



# E entre as espécies, essa relação muda?
ggplot(df, aes(x = bill_length_mm, y = body_mass_g, col = species, group = species)) +
  geom_point(size = 3, alpha = 0.3) +
  scale_fill_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
  scale_color_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
  geom_smooth(method = 'lm', fill = "gray30", col = 'gray30') +
  facet_wrap(species ~ .) +
  labs(x = 'Bill length (mm)', y = 'Body mass (g)') +
  theme_light(base_size = 14) 

table(df$species)


# Complicando um pouco mais...
ggplot(df, aes(x = bill_length_mm, y = body_mass_g, col = species, group = species)) +
  geom_point(size = 3, alpha = 0.3) +
  scale_fill_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
  scale_color_manual(values = c("#3e5c76", "#bc6c25","#606c38")) +
  geom_smooth(method = 'lm', fill = "gray30", col = 'gray30') +
  facet_grid(species ~ sex) +
  labs(x = 'Bill length (mm)', y = 'Body mass (g)') +
  theme_light(base_size = 14) 

table(df$species, df$sex)
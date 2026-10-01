#############################################################################
#                                                                           #
#                            Prática da aula 04                             #
#                       - Modelo linear misto (LMM) -                       #
#                                                                           #
#############################################################################



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) # Manipulação de dados
library(brms) # Modelos Bayesianos
library(bayesplot) # Visualização dos resultados Bayesianos (extra)
library(ggplot2) # Visualização dos dados/resultados
library(GGally) # Gráfico de multicolinearidade
library(patchwork) # Visualização de dados (figuras com +1 painel)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Customizando o tema do ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_theme <- theme_bw(base_size = 12) + 
            theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                  axis.text.x = element_text(colour = "grey20", margin = margin(t = 10)),
                  axis.text.y = element_text(colour = "grey20", margin = margin(r = 10)),
                  axis.title = element_text(face = "bold", colour = "black", size = 12),
                  strip.background = element_rect(fill = 'white'),
                  legend.background = element_rect(fill = "transparent", colour = NA),
                  legend.box.background = element_rect(fill = "transparent", colour = NA),
                  plot.background   = element_rect(fill = "transparent", color = NA),
                  panel.background  = element_rect(fill = "transparent", color = NA),
                  panel.grid.major.y = element_line(linetype = 'dotted'),
                  panel.grid.major.x = element_line(linetype = 'dotted'),
                  axis.line = element_line(color = "black"),
                  plot.margin = margin(5, 10, 5, 5))
          
my_cols <- c("#846267", "#588157", "#e07a5f", "#457b9d", "#f6bd60") 




#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/reef_biomass.rds")
str(df)


## Convertendo variáveis para fator
df[, c("id_transecto", "recife", "ano", "mes", "transecto", "mergulhador")] <- lapply(df[, c("id_transecto", "recife", "ano", "mes", "transecto", "mergulhador")], factor)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Análise exploratória dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 2.1) Cobertura espaço-temporal das amostragens
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(df$ano, df$mes)
table(df$ano, df$recife)
table(df$mes, df$recife)



# 2.2) Variável-resposta (biomassa)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Histograma
phist <- ggplot(df, aes(x = biomassa_kg_m2)) +
         geom_histogram(bins = 10,fill = '#457b9d', col = 'white', alpha = 0.8, linewidth = 1) +
         coord_cartesian(expand = c(0,0)) +
         labs(x = 'Biomassa (Kg/m2)', y = 'Frequência') +
         my_theme

## Boxplot
pbox <- ggplot(df, aes(y = biomassa_kg_m2)) +
        geom_boxplot(col = '#457b9d', fill = 'white', linewidth = 1, outlier.size = 3) +
        labs(y = 'Biomassa (Kg/m2)') +
        my_theme


## Visualizando...
phist + pbox 



# 2.3) Variável-resposta vs. preditores
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.3.1) Tendências temporais
ggplot(df, aes(y = biomassa_kg_m2, x = ano)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 1, outlier.size = 3) +
  labs(y = 'Biomassa (Kg/m2)') +
  my_theme


ggplot(df, aes(y = biomassa_kg_m2, x = mes)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 1, outlier.size = 3) +
  labs(y = 'Biomassa (Kg/m2)') +
  my_theme


ggplot(df, aes(y = biomassa_kg_m2, x = mes)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 0.6) +
  labs(y = 'Biomassa (Kg/m2)') +
  facet_wrap(ano ~ .) +
  my_theme


# 2.3.2) Tendências espaciais
ggplot(df, aes(y = biomassa_kg_m2, x = recife)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 0.6) +
  labs(y = 'Biomassa (Kg/m2)') +
  facet_wrap(ano ~ .) +
  my_theme



# 2.3.3) Influência do mergulhador
ggplot(df, aes(y = biomassa_kg_m2, x = mergulhador)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 0.6) +
  labs(y = 'Biomassa (Kg/m2)') +
  my_theme


# 2.3.4) Variabilidade das pseudo-réplicas
ggplot(df, aes(y = biomassa_kg_m2, x = transecto)) +
  geom_boxplot(col = '#457b9d', fill = 'white',  linewidth = 0.6) +
  labs(y = 'Biomassa (Kg/m2)') +
  facet_wrap(recife ~ ., nrow = 1) +
  my_theme


# 2.3.5) Influência da temperatura superficial do mar (sst)
ggplot(df, aes(y = biomassa_kg_m2, x = sst)) +
  geom_point(col = '#457b9d', size = 3, alpha = 0.5) +
  #geom_smooth(method = 'lm', col = 'black') +
  labs(y = 'Biomassa (Kg/m2)', x = "SST (°C)") +
  my_theme


## A relação biomassa~sst muda entre os recifes?
ggplot(df, aes(y = biomassa_kg_m2, x = sst)) +
  geom_point(col = '#457b9d', size = 3, alpha = 0.5) +
  geom_smooth(method = 'lm') +
  labs(y = 'Biomassa (Kg/m2)', x = "SST (°C)") +
  my_theme


ggplot(df, aes(y = biomassa_kg_m2, x = sst, col = recife, fill = recife)) +
  geom_point( size = 3, alpha = 0.5) +
  scale_color_manual(values = my_cols) +
  scale_fill_manual(values = my_cols) +
  geom_smooth(method = 'lm') +
  labs(y = 'Biomassa (Kg/m2)', x = "SST (°C)") +
  my_theme

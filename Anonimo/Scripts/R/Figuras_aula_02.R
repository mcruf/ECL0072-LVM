###################################
#                                 #
#         Figuras aula 02         #
#                                 #
###################################



#~~~~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(dplyr)
library(patchwork)
library(gganimate)
library(purrr)


#~~~~~~~~~~~~~~~~~
# Default input
#~~~~~~~~~~~~~~~~~

# Colors
#~~~~~~~~~~
col_prior      <- "gray70"  # cinza
col_likelihood <- "#e07a5f"  # laranja
col_posterior  <- "#8C6D83"  # Roxo

txt_prior      <- "gray55"
txt_likelihood <- "#e07a5f"
txt_posterior  <- "#701A33"


# Main theme
#~~~~~~~~~~~~~~
tema_clean <- theme_classic(base_size = 13) +
              theme(plot.background   = element_rect(fill = "transparent", color = NA),
                    panel.background  = element_rect(fill = "transparent", color = NA),
                    axis.text         = element_text(color = "gray30", size = 12),
                    axis.title.x        = element_text(color = "gray20", size = 16, face = "bold"),
                    axis.title.y        = element_text(color = "gray20", size = 12),
                    axis.ticks        = element_line(color = "gray40", size = 0.4),
                    #panel.grid.major.y = element_line(linetype = 'dotted'),
                    #panel.grid.major.x = element_line(linetype = 'dotted'),
                    plot.margin       = margin(15, 15, 15, 15))

# my_theme <- theme_minimal(base_size = 14) +
#   theme(plot.title = element_text(hjust = 0.5, face = 'bold', margin = margin(b = 15), size = 13),
#         axis.text.x = element_text(colour = "grey20", margin = margin(t = 5)),
#         axis.text.y = element_text(colour = "grey20", margin = margin(r = 5)),
#         axis.title = element_text(face = "bold", colour = "black"),
#         axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
#         axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         strip.background = element_rect(fill = 'white'),
#         legend.background = element_rect(fill = "transparent", colour = NA),
#         legend.box.background = element_rect(fill = "transparent", colour = NA),
#         legend.title = element_text(hjust = 0.5, face = 'bold', size = 10),
#         legend.text = element_text(size = 9),
#         legend.position = "bottom",
#         legend.box = "horizontal",
#         panel.grid.major.y = element_line(linetype = 'dotted'),
#         panel.grid.major.x = element_line(linetype = 'dotted'),
#         plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) # Espaçamento lateral por painel





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Função dados beta-binomial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gera_dados <- function(a, b, k, n) {
  theta <- seq(0, 1, length.out = 500)
  
  # Priori: Beta(1,1) = Uniforme(0,1)
  prior <- dbeta(theta, a, b)
  
  # Verossimilhança reescalada como densidade Beta
  like  <- dbeta(theta, k + 1, n - k + 1)
  
  # Posterior: Beta(a + k, b + n - k)
  post  <- dbeta(theta, a + k, b + n - k)
  
  data.frame(
    theta = rep(theta, 3),
    densidade = c(prior, like, post),
    curva = factor(rep(c("Prior", "Likelihood", "Posterior"), each = 500),
                   levels = c("Prior", "Likelihood", "Posterior"))
  )
}




#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exemplo: REGRESSÃO LINEAR PINGUINS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Importando o banco de dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-GLM_GAM/Dados/palmerpenguins_extended.rds")



p1 <- dados %>%
  ggplot(aes(y = body_mass_g, x = bill_length_mm)) +
  geom_point(size = 3.5, col ='#e07a5f', alpha = 0.2) +
  geom_smooth(method = 'lm', col = 'gray20', fill = 'gray40', se = TRUE) +
  my_theme +
  labs(
    x = "Tamanho do bico (mm)",
    y = "Peso (g)",
    title = expression(Peso[i] == 2299.7 + 65.8 * Bico[i])) +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(20, 20, 20, 20))
p1


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exemplo: FREQUÊNCIA RELATIVA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulando o lançamento de uma moeda não viciada 1000 vezes e contando a prporção de caras


set.seed(37) 
N <- 1000
#N <- 500


# Simulação de lançamentos (1 = Cara/Sucesso, 0 = Coroa/Fracasso)
flips <- rbinom(N, size = 1, prob = 0.5)

# Cálculo da proporção acumulada (Running Proportion)
running_prop <- cumsum(flips) / (1:N)

# Sequência textual dos primeiros lançamentos para anotação
seq_text <- paste(ifelse(flips[1:10] == 1, "H", "T"), collapse = "")

df_running <- data.frame(
  Flip = 1:N,
  Proportion = running_prop
)

# Valor final
end_prop <- round(running_prop[N], 3)

# --- Cores e Estilo Lux ---
col_line <- "#e07a5f" # roxo
col_point <- "#e07a5f"

# --- Gráfico ggplot2 ---
p_running <- ggplot(df_running, aes(x = Flip, y = Proportion)) +
  # Linha de referência em 0.5
  geom_hline(yintercept = 0.5, color = "gray40", size = 0.4) +
  # Trajetória da proporção
  geom_line(color = col_line, size = 0.8, alpha = 0.8) +
  geom_point(shape = 21, color = col_point, fill = 'white', size = 1.5, stroke = 1) +
  # Escala logarítmica no eixo X (essencial para ver detalhes do início e convergência no fim)
  scale_x_log10(breaks = c(1, 5, 10, 50, 200, 500, 1000), labels = c("1", "5", "10",  "50", "200", "500", "1000")) +
  scale_y_continuous(limits = c(0, 1.05),
                     breaks = seq(0, 1, 0.2),
                     labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
  
  # Rótulos dos eixos e Título
  labs( x = "N", y = "Proporção de cara") +
  tema_clean +
  theme(axis.title.x = element_text(color = "gray20", size = 14, face = "bold",  margin = margin(t = 25)),
        axis.title.y = element_text(color = "gray20", size = 14, face = "bold", margin = margin(r = 20)))

p_running




# Salvando a figura para o RMarkdown/Slide
ggsave(filename = "Figuras/relative_frequency.png",
       plot = p_running,
       width = 8,
       height = 4.5,
       dpi = 400,
       bg = "transparent")







#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exemplo: REALOCACAO DE CREDIBILIDADE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# --- Configuração de Cores (Paleta Lux) ---
col_bar <- "#e07a5f"  # Azul pastel das barras
col_bg  <- "transparent"  # Off-white

# --- Dados dos 3 Passos da Inferência (B como Vencedor Final) ---
# Passo 1: A é descartado -> Credibilidade de B, C, D sobe para 1/3 (0.333)
df_p1_prior <- data.frame(Poss = c("A", "B", "C", "D"), Cred = c(0.25, 0.25, 0.25, 0.25))
df_p1_post  <- data.frame(Poss = c("A", "B", "C", "D"), Cred = c(0, 1/3, 1/3, 1/3))

# Passo 2: D é descartado -> Credibilidade de B, C sobe para 1/2 (0.50)
df_p2_prior <- df_p1_post
df_p2_post  <- data.frame(Poss = c("A", "B", "C", "D"), Cred = c(0, 0.5, 0.5, 0))

# Passo 3: C é descartado -> Credibilidade de B sobe para 1.0
df_p3_prior <- df_p2_post
df_p3_post  <- data.frame(Poss = c("A", "B", "C", "D"), Cred = c(0, 1.0, 0, 0))




# --- Função Auxiliar para Criar os Gráficos ---
cria_grafico <- function(df, titulo, texto_seta = NULL, pos_x_seta = NULL) {
  p <- ggplot(df, aes(x = Poss, y = Cred)) +
    geom_col(fill = col_bar, width = 0.8) +
    coord_cartesian(expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
    labs(title = titulo, x = "Possibilidades", y = "Credibilidade") +
    tema_sherlock
  
  if (!is.null(texto_seta)) {
    p <- p + 
      # Seta apontando para a barra zerada
      annotate("segment", x = pos_x_seta, xend = pos_x_seta, y = 0.60, yend = 0.03,
               arrow = arrow(length = unit(0.2, "cm")), color = "black", linewidth = 0.5) +
      # Texto explicativo da eliminação
      annotate("text", x = pos_x_seta, y = 0.72, label = texto_seta, 
               color = "black", size = 3.5, hjust = 0.5)
  }
  return(p)
}

# --- Construção dos Painéis ---
# Coluna 1: A é descartado (posição x = 1)
p1_top <- cria_grafico(df_p1_prior, "") +  theme(axis.text = element_text(size = 13),
                                                 axis.title.x = element_text(color = "gray20", size = 14, face = "bold",  margin = margin(t = 25)),
                                                 axis.title.y = element_text(color = "gray20", size = 14, face = "bold", margin = margin(r = 20)))

p1_bot <- cria_grafico(df_p1_post,  "Posterior", "A is\nimprobable", 1)

# Coluna 2: D é descartado (posição x = 4)
p2_top <- cria_grafico(df_p2_prior, "") +  theme(axis.text = element_text(size = 13),
                                                 axis.title.x = element_text(color = "gray20", size = 14, face = "bold",  margin = margin(t = 25)),
                                                 axis.title.y = element_text(color = "gray20", size = 14, face = "bold", margin = margin(r = 20)))

p2_bot <- cria_grafico(df_p2_post,  "Posterior", "D is\nimprobable", 4)

# Coluna 3: C é descartado (posição x = 3)
p3_top <- cria_grafico(df_p3_prior, "") +  theme(axis.text = element_text(size = 13),
                                                 axis.title.x = element_text(color = "gray20", size = 14, face = "bold",  margin = margin(t = 25)),
                                                 axis.title.y = element_text(color = "gray20", size = 14, face = "bold", margin = margin(r = 20)))

#p3_bot <- cria_grafico(df_p3_post,  "Posterior", "C is\nimprobable", 3)
p3_bot <- cria_grafico(df_p3_post,  "") +  theme(axis.text = element_text(size = 13),
                                                 axis.title.x = element_text(color = "gray20", size = 14, face = "bold",  margin = margin(t = 25)),
                                                 axis.title.y = element_text(color = "gray20", size = 14, face = "bold", margin = margin(r = 20)))


# --- Montagem Final com Patchwork ---
# col1 <- (p1_top / p1_bot)
# col2 <- (p2_top / p2_bot)
# col3 <- (p3_top / p3_bot)

p1_top + p2_top + p3_top + p3_bot


figura_sherlock_B <- (p1_top | p2_top | p3_top | p3_bot) +
  plot_layout(widths = c(1, 1, 1, 1)) &
  tema_clean +
  theme(axis.text = element_text(size = 13),
        axis.title.x = element_text(color = "gray20", size = 14, face = "bold",  margin = margin(t = 25)),
        axis.title.y = element_text(color = "gray20", size = 14, face = "bold", margin = margin(r = 20)))


figura_sherlock_B


# --- Salvando a Figura ---
ggsave(filename = "Figuras/reallocation_of_credibility_4.png", 
       plot = p3_bot, 
       width = 4, 
       height = 4, 
       dpi = 400, 
       bg = 'transparent')






#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exemplo: TIPOS DE PRIORIS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Priori não-informativa
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# beta(a = 1, b = 1)
# Likelihood: k = 3 detecções em n = 10 visitas (theta = 0.3)
d_natalensis <- gera_dados(a = 1, b = 1, k = 3, n = 10)


# Go for the plot....
difuse_prior <- d_natalensis %>% 
                #filter(curva != "Posterior") %>% 
                ggplot(aes(x = theta, y = densidade, fill = curva)) +
                geom_area(alpha = 0.45, position = "identity") +
                scale_fill_manual(values = c("Prior" = col_prior, 
                                             "Likelihood" = col_likelihood, 
                                             "Posterior" = col_posterior)) +
                scale_x_continuous(breaks = seq(0, 1, 0.2), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
                annotate("text", x = 0.88, y = 0.85, label = "Prior: Beta(1,1)", color = txt_prior, size = 3.8, fontface = "bold") +
                #annotate("text", x = 0.13, y = 2.5, label = "Likelihood", color = txt_likelihood, size = 4, fontface = "bold") +
                annotate("text", x = 0.55, y = 2.5, label = "Posterior = Likelihood", color = txt_posterior, size = 4, fontface = "bold") +
                labs(x = expression(theta),
                     y = "Density") +
                tema_clean +
                coord_cartesian(expand = c(0, 0)) +
                theme(legend.position = "none")

difuse_prior



# Salvando a imagem
ggsave(filename = "Figuras/diffuse_prior_likelihood_posterior.png", 
        plot = difuse_prior, 
        width = 6, 
        height = 4, 
        dpi = 400, 
        bg = "transparent")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Priori vagamente-informativa
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# beta(a = 2, b = 2)
# Likelihood: k = 3 detecções em n = 10 visitas (theta = 0.3)
d_natalensis <- gera_dados(a = 2, b = 2, k = 3, n = 10)


# Go for the plot....
winfo_prior <- d_natalensis %>% 
  filter(curva != "Posterior") %>% 
  ggplot(aes(x = theta, y = densidade, fill = curva)) +
  geom_area(alpha = 0.45, position = "identity") +
  scale_fill_manual(values = c("Prior" = col_prior, 
                               "Likelihood" = col_likelihood, 
                               "Posterior" = col_posterior)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
  annotate("text", x = 0.88, y = 1.2, label = "Prior: Beta(2,2)", color = txt_prior, size = 3.8, fontface = "bold") +
  annotate("text", x = 0.13, y = 2.5, label = "Likelihood", color = txt_likelihood, size = 4, fontface = "bold") +
  #annotate("text", x = 0.47, y = 2.9, label = "Posterior", color = txt_posterior, size = 4, fontface = "bold") +
  labs(x = expression(theta),
       y = "Density") +
  tema_clean +
  coord_cartesian(expand = c(0, 0)) +
  theme(legend.position = "none")

winfo_prior



# Salvando a imagem
ggsave(filename = "Figuras/weakly_informative_prior_likelihood.png", 
       plot = winfo_prior, 
       width = 6, 
       height = 4, 
       dpi = 400, 
       bg = "transparent")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Priori informativa
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# beta(a = 7, b = 3)
# Likelihood: k = 3 detecções em n = 10 visitas (theta = 0.3)
# Segundo Jaqueiuto, a deteção gira em torno de 7/10 (logo, beta(7,3))
# pois a média da beta é dada por (a / a+b)
d_natalensis <- gera_dados(a = 7, b = 3, k = 3, n = 10)


# Go for the plot....
info_prior <- d_natalensis %>% 
  #filter(curva != "Posterior") %>% 
  ggplot(aes(x = theta, y = densidade, fill = curva)) +
  geom_area(alpha = 0.45, position = "identity") +
  scale_fill_manual(values = c("Prior" = col_prior, 
                               "Likelihood" = col_likelihood, 
                               "Posterior" = col_posterior)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
  annotate("text", x = 0.91, y = 2.6, label = "Prior: Beta(7,3)", color = txt_prior, size = 3.8, fontface = "bold") +
  annotate("text", x = 0.13, y = 2.5, label = "Likelihood", color = txt_likelihood, size = 4, fontface = "bold") +
  annotate("text", x = 0.65, y = 3.2, label = "Posterior", color = txt_posterior, size = 4, fontface = "bold") +
  labs(x = expression(theta),
       y = "Density") +
  tema_clean +
  coord_cartesian(expand = c(0, 0)) +
  theme(legend.position = "none")

info_prior



# Salvando a imagem
ggsave(filename = "Figuras/informative_prior_likelihood_posterior.png", 
       plot = info_prior, 
       width = 6, 
       height = 4, 
       dpi = 400, 
       bg = "transparent")



###########################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exemplo: INFLUENCIA DO NAMOSTRAL SOBRE A POSTERIORI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# --- CENÁRIO 1: Amostra Pequena (N = 10, k = 4) ---
d_pequeno <- gera_dados(a = 7, b = 3, k = 4, n = 10)

p1 <- ggplot(d_pequeno, aes(x = theta, y = densidade, fill = curva)) +
        geom_area(alpha = 0.45, position = "identity") +
        scale_fill_manual(values = c("Prior" = col_prior, "Likelihood" = col_likelihood, "Posterior" = col_posterior)) +
        scale_x_continuous(breaks = seq(0, 1, 0.2), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
        annotate("text", x = 0.82, y = 2.8, label = "Prior", color = txt_prior, size = 3.8, fontface = "bold") +
        annotate("text", x = 0.25, y = 2.46, label = "Likelihood \n(N=10)", color = txt_likelihood, size = 3.8, fontface = "bold") +
        annotate("text", x = 0.42, y = 3.2, label = "Posterior", color = txt_posterior, size = 3.8, fontface = "bold") +
        labs(x = expression(theta),
             y = "Density") +
        tema_clean +
        coord_cartesian(expand = c(0, 0)) +
        theme(legend.position = "none")
p1


# --- CENÁRIO 2: Amostra Grande (N = 100, k = 40) ---
d_grande <- gera_dados(a = 7, b = 3, k = 40, n = 100)

p2 <- ggplot(d_grande, aes(x = theta, y = densidade, fill = curva)) +
  geom_area(alpha = 0.45, position = "identity") +
  scale_fill_manual(values = c("Prior" = col_prior, "Likelihood" = col_likelihood, "Posterior" = col_posterior)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
  annotate("text", x = 0.85, y = 2.8, label = "Prior", color = txt_prior, size = 3.8, fontface = "bold") +
  annotate("text", x = 0.28, y = 6.5, label = "Likelihood \n(N=100)", color = txt_likelihood, size = 3.8, fontface = "bold") +
  annotate("text", x = 0.53, y = 7.8, label = "Posterior", color = txt_posterior, size = 3.8, fontface = "bold") +
  labs(x = expression(theta),
       y = "Density") +
  tema_clean +
  coord_cartesian(expand = c(0, 0)) +
  theme(legend.position = "none")

p2




# --- COMBINAÇÃO COM PATCHWORK ---
figura_tamanho_dado <- (p1 + plot_spacer() + p2) +
  plot_layout(ncol = 3, widths = c(1, 0.25, 1)) &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

# Exibe
figura_tamanho_dado



# Salvando a figura para o RMarkdown/Overleaf
ggsave(filename = "Figuras/SampleSize_impact_posterior.png", 
  plot = figura_tamanho_dado, 
  width = 10, 
  height = 3.5, 
  dpi = 400, 
  bg = "transparent"
)


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exemplo: REGRESSÃO LINEAR COM DESVIOS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# --- 1. Preparação dos Dados ---
dados <- readRDS("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-GLM_GAM/Dados/palmerpenguins_extended.rds")

set.seed(125)
dados_100 <- dados %>%
  slice_head(n = 150) %>%
  mutate(id = row_number())

# IDs dos pontos destacados
ids_destaque <- sample(dados_100$id, 15)

# --- 2. Ajuste do Modelo MQO (para obter a reta ideal como referência) ---
mod_lm <- lm(body_mass_g ~ bill_length_mm, data = dados_100)
a_mqo <- coef(mod_lm)[1]
b_mqo <- coef(mod_lm)[2]

# --- 3. Criando uma Grade de Várias Inclinações (Frames da Animação) ---
# Vamos variar a inclinação (b) de -40 até 120 (passando pelo valor ideal ~87)
slopes <- seq(-40, 120, by = 4)

# Para cada inclinação, fazemos a reta girar ao redor do ponto médio (x_mean, y_mean)
x_mean <- mean(dados_100$bill_length_mm)
y_mean <- mean(dados_100$body_mass_g)

# Gerando a base de dados expandida para todos os frames
dados_anim <- map_df(seq_along(slopes), function(f) {
  b_curr <- slopes[f]
  a_curr <- y_mean - b_curr * x_mean # Mantém a reta girando no centro dos dados
  
  dados_100 %>%
    mutate(
      frame = f,
      slope = b_curr,
      intercept = a_curr,
      pred = a_curr + b_curr * bill_length_mm,
      sqr = sum((body_mass_g - pred)^2), # Soma dos Quadrados dos Resíduos
      grupo = if_else(id %in% ids_destaque, "highlight", "fade")
    )
})


# --- 4. Construção do Gráfico Base ---
p_anim <- ggplot(dados_anim, aes(x = bill_length_mm, y = body_mass_g)) +
  # Segmentos de resíduos para os pontos em destaque
  geom_segment(data = filter(dados_anim, grupo == "highlight"),
               aes(xend = bill_length_mm, yend = pred),
               color = "#e07a5f", linewidth = 0.8) +
  
  # Pontos
  geom_point(aes(alpha = grupo, color = grupo), size = 3.5) +
  
  # Reta com a inclinação do frame atual
  geom_abline(aes(intercept = intercept, slope = slope), color = "#2b5c8f", linewidth = 1.2) +
  
  # Escalas de Cores e Transparência
  scale_color_manual(values = c("fade" = "gray50", "highlight" = "#e07a5f")) +
  scale_alpha_manual(values = c("highlight" = 1, "fade" = 0.20)) +
  
  # Rótulo Dinâmico mostrando a Inclinação e a Soma dos Quadrados dos Resíduos (SQR)
  labs(x = "Tamanho do bico (mm)", 
       y = "Peso (g)") +
  
  tema_clean +
  theme(axis.text = element_text(size = 13),
        axis.title.x = element_text(color = "gray20", size = 14, face = "bold", margin = margin(t = 25)),
        axis.title.y = element_text(color = "gray20", size = 14, face = "bold", margin = margin(r = 20)),
        legend.position = "none") +
  
# --- 5. Adicionando Animação ---
transition_manual(frame)

# --- 6. Renderizando o GIF ---
gif_residuos <- animate(p_anim, 
                        nframes = length(slopes), 
                        fps = 6, 
                        width = 700, 
                        height = 500, 
                        res = 100, 
                        dpi = 400)

# Visualizar no RStudio
gif_residuos

# Salvar o arquivo em disco
anim_save("Figuras/MMQ_animacao.gif", animation = gif_residuos)



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exemplo: CURVAS DE DISTRIBUIÇÃO NORMAL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Criando a variável x (comum a todos os y's simulados)
x <- seq(-5, 5, length.out = 200)


## Simulando a variável y (com diferentes parâmetros)
set.seed(452)
df <- rbind(data.frame(x = x, y = dnorm(x, mean = 0, sd = 0.5), 
             dist = "mu==0 ~ ',' ~ sigma^2==0.5"),
            data.frame(x = x, y = dnorm(x, mean = -2, sd = 0.5), 
                       dist = "mu==-2 ~ ',' ~ sigma^2==0.5"),
            data.frame(x = x, y = dnorm(x, mean = 0, sd = 2), 
                       dist = "mu==0 ~ ',' ~ sigma^2==2"))

## Convertendo para fator 
df$dist <- factor(df$dist, 
                  levels = c("mu==-2 ~ ',' ~ sigma^2==0.5",
                             "mu==0 ~ ',' ~ sigma^2==0.5",
                             "mu==0 ~ ',' ~ sigma^2==2"))

## Figura
p2 <- ggplot(df, aes(x = x, y = y, colour = dist)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#457b9d", "#f6bd60", "#e07a5f"),
                     labels = c(expression(mu==-2 ~ "," ~ sigma^2 == 0.5),
                                expression(mu==0 ~ "," ~ sigma^2 == 0.5),
                                expression(mu==0 ~ "," ~ sigma^2 == 2))) +
  labs(x = "x", y = "Densidade", color = "Parâmetros") +
  tema_clean +
  theme(axis.text = element_text(size = 13),
        axis.title.x = element_text(color = "gray20", size = 14, face = "bold", margin = margin(t = 25)),
        axis.title.y = element_text(color = "gray20", size = 14, face = "bold", margin = margin(r = 20)),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(20, 20, 20, 20),
        legend.position = c(0.8, 0.8),         # Inside plot (x, y) coords
        legend.background = element_rect(fill = 'transparent', colour = NA))

p2



ggsave(plot = p2,
       filename ="Figuras/Dist_normal.png", 
       width = 6,
       height = 4,
       dpi = 400)




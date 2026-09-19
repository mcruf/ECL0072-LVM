###################################
#                                 #
#         Figuras aula 02         #
#                                 #
###################################



#~~~~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~~~~
library(ggplot2)


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
                    plot.margin       = margin(15, 15, 15, 15))



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

library(ggplot2)
library(patchwork)


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


#########################################

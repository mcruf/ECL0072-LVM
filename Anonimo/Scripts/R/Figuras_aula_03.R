###################################
#                                 #
#         Figuras aula 03         #
#                                 #
###################################

#~~~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(ggExtra)
library(brms)
library(tidybayes)
library(dplyr)
library(patchwork)



#~~~~~~~~~~~~~~~
# Default inputs
#~~~~~~~~~~~~~~~
my_theme <- theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', margin = margin(b = 15), size = 13),
        axis.text.x = element_text(colour = "grey20", margin = margin(t = 5)),
        axis.text.y = element_text(colour = "grey20", margin = margin(r = 5)),
        axis.title = element_text(face = "bold", colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        strip.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(hjust = 0.5, face = 'bold', size = 10),
        legend.text = element_text(size = 9),
        legend.position = "bottom",
        legend.box = "horizontal",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2),
        panel.grid.major.y = element_line(linetype = 'dotted'),
        panel.grid.major.x = element_line(linetype = 'dotted'),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
        plot.background   = element_rect(fill = "transparent", color = NA),
        panel.background  = element_rect(fill = "transparent", color = NA)) # Espaçamento lateral por painel


######################################################


# 1 - Gráfico distribuição posterior conjunta dos parâmetros
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Importando os resultados de um modelo bayesiano
mod <- readRDS("Anonimo/modelo_bayesiano.rds")
post <- as_draws_df(mod) # as_draws_matrix() - extrai resultados como matriz

head(as.data.frame(post[,c(1,2,7:8)]))

dfp <- as.data.frame(post[,c(1,2,7:8)])
colnames(dfp)[1:2] <- c("beta_0", "beta_1")


# Criando o gráfico de dispersão
p1 <- ggplot(dfp, aes(x = b_Intercept, y = b_Area_log)) +
      geom_point(color = "#e59c82",
                 alpha = 0.3,
                 size = 1.8) +
      labs(x = expression(beta[0]),
        y = expression(beta[1])) +
      my_theme

p1

# 3. Adicionando os histogramas marginais
p2 <- ggMarginal(p,
            type = "histogram",
            margins = "both",
            size = 5,
            bins = 30,
            fill = "#e59c82",
            colour = "white")
p2

ggsave(filename = "Figuras/multivariate_posterior.png",
       plot = p2,
       width = 5,
       height = 5,
       dpi = 400,
       bg = "transparent")



###################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2 - Gráfico: Linhas de regressão
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.1) Gráfico estático
#~~~~~~~~~~~~~~~~~~~~~~~~~
idx <- sample(seq_len(nrow(dfp)), size = 20)

dfp_f <- dfp[idx, ]  

dados <- mod$data

p3 <- ggplot(dados, aes(x = Area_log, y = Species_log)) +
      geom_abline(data = dfp_f[1:10,], aes(intercept = beta_0, slope = beta_1),
                  color = "#e59c82", alpha = 0.7, linewidth = 0.6) +
      geom_point(color = "grey20", size = 2.5, alpha = 0.8) +
      labs(x = expression(beta[0]),
           y = expression(beta[1])) +
      my_theme


ggsave(filename = "Figuras/posterior_regression.png",
       plot = p3,
       width = 5,
       height = 5,
       dpi = 400,
       bg = "transparent")





# Gráfico dinâmico (gif)
#~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(12345)
ndraws <- 50

# Selecionando 50 amostras da posterior
post_sel <- post %>%
  as.data.frame() %>%
  select(.draw, b_Intercept, b_Area_log) %>%
  slice_sample(n = ndraws) %>%
  mutate(quadro = row_number())

# Combinando cada amostra com uma sequência de valores de área
pred <- expand_grid(quadro = post_sel$quadro,
                    Area_log = seq(min(dados$Area_log), max(dados$Area_log), length.out = 100)) %>%
  left_join(post_sel, by = "quadro") %>%
  mutate(mu = b_Intercept + b_Area_log * Area_log)

# Criando a animação
p <- ggplot() +
  geom_line(data = pred, aes(x = Area_log, y = mu, group = .draw), color = "#e59c82", linewidth = 1) +
  geom_point(data = dados, aes(x = Area_log, y = Species_log), size = 2) +
  labs(x = "β0",
       y = "β1") +
  my_theme +
  transition_states(quadro,
                    transition_length = 0,
                    state_length = 1,
                    wrap = TRUE) +
  shadow_mark(past = TRUE,
              future = TRUE,
              alpha = 1 / 20,
              color = "gray50",
              exclude_layer = 2)

p <- p + theme( plot.background   = element_rect(fill = "white", colour = NA),
  panel.background  = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.key        = element_rect(fill = "transparent", colour = NA))


gif <- animate(p,
               nframes = ndraws,
               fps = 2.5,
               width = 960,     
               height = 1000,    
               units = "px",
               res = 200,       
               dev = "ragg_png",
               background = "transparent",
               renderer = gifski_renderer()) 


anim_save("Figuras/posterior_regression2.gif", animation = gif)



#############################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3 - Gráfico: Linhas de regressão & histograma
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Histograma
#~~~~~~~~~~~~~~~~~~~~
# Valor do preditor onde faremos o "corte" nas retas
x0 <- 0.5

# Média esperada para cada amostra da posterior
# Usamos todas as amostras para construir o histograma
post_mu <- post %>%
  as.data.frame() %>%
  mutate(mu = b_Intercept + b_Area_log * x0)

# Mediana e intervalo de credibilidade de 95% (caudas iguais)
resumo <- post_mu %>%
  summarise(inferior = quantile(mu, 0.025),
            mediana  = median(mu),
            superior = quantile(mu, 0.975))

# Posição do intervalo abaixo das barras
h <- hist(post_mu$mu, breaks = 35, plot = FALSE)
y_intervalo <- -0.06 * max(h$counts)



ph <- ggplot(post_mu, aes(x = mu)) +
  geom_histogram(bins = 35, fill = "#e59c82", color = "white", linewidth = 0.5) +
  geom_segment(data = resumo,
    aes(x = inferior, xend = superior, y = y_intervalo, yend = y_intervalo),
        inherit.aes = FALSE,
        color = "darkred",
        linewidth = 1.5) +
  geom_point(data = resumo, aes(x = mediana, y = y_intervalo),
            inherit.aes = FALSE,
            color = "darkred",
            size = 3) + 
  labs( x = expression(mu(beta[0] == 0.5)),
    y = "Frequency") +
  my_theme 

ph

# 3.2) Linhas de regressão
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Centro e largura do retângulo
x0 <- 0.5
largura <- 0.15

# Altura das retas no ponto escolhido
mu_x0 <- dfp_f$beta_0 + dfp_f$beta_1 * x0

# Limites verticais, com uma pequena margem
limites <- range(mu_x0, na.rm = TRUE)
margem <- 0.1 * diff(limites)



pr <- ggplot(dados, aes(x = Area_log, y = Species_log)) +
  geom_abline(data = dfp_f, aes(intercept = beta_0, slope = beta_1),
    color = "#e59c82", alpha = 0.2, linewidth = 0.6) +
  geom_point(color = "grey20", size = 2.5, alpha = 0.8) +
  annotate("rect",
    xmin = x0 - largura / 2,
    xmax = x0 + largura / 2,
    ymin = limites[1] - margem,
    ymax = limites[2] + margem,
    fill = NA,
    color = "darkred",
    linewidth = 1) +
  labs(x = expression(beta[0]),
       y = expression(beta[1])) +
  scale_x_continuous(breaks = scales::extended_breaks(n = 10)) +
  my_theme



pp <- (pr + ph) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", colour = NA)))

# pp <- (pr / ph) +
#   plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", colour = NA)))


ggsave(filename = "Figuras/posterior_ic95.png",
       plot = pp,
       width = 12,
       height = 4,
       # width = 5,
       # height = 7,
       dpi = 400,
       bg = "transparent")



# 3.3) preditor linear 
#~~~~~~~~~~~~~~~~~~~~~~~

# Sequência de valores do preditor
x_seq <- seq(min(dados$Area_log, na.rm = TRUE), max(dados$Area_log, na.rm = TRUE),length.out = 200)

# Cada coluna contém as médias esperadas em um valor de Area_log
# Cada linha corresponde a uma amostra da posterior
mu <- vapply(x_seq,
  function(x) post$b_Intercept + post$b_Area_log * x,
  numeric(nrow(post)))

# Resumindo a posterior em cada valor do preditor
pred <- data.frame(Area_log = x_seq,
                   media    = colMeans(mu),
                   inferior = apply(mu, 2, quantile, probs = 0.025),
                   superior = apply(mu, 2, quantile, probs = 0.975))

# Gráfico com faixa de credibilidade
pl <- ggplot() +
  geom_ribbon(data = pred, aes(x = Area_log, ymin = inferior, ymax = superior),
              fill = "#e59c82",
              alpha = 0.25) +
  geom_line(data = pred, aes(x = Area_log, y = media),
            color = "#e59c82",
            linewidth = 1) +
  geom_point(data = dados, aes(x = Area_log, y = Species_log),
              color = "grey20",
              size = 2.5,
              alpha = 0.8) +
  labs(x = expression(beta[0]),
       y = expression(beta[1])) +
  scale_x_continuous(breaks = scales::extended_breaks(n = 10)) +
  my_theme

pl

ggsave(filename = "Figuras/posterior_mu_ic95.png",
       plot = pl,
       width = 8,
       height = 5,
       dpi = 400,
       bg = "transparent")


###################


# Histogramas mu e y_hat
#~~~~~~~~~~~~~~~~~~~~~~~~

# Extraindo todas as amostras da posterior
post <- as.data.frame(as_draws_df(mod))

# Valor de log da área para as predições
x0 <- 0.5

# 1) Posterior da média esperada de log da riqueza
post$mu <- post$b_Intercept + post$b_Area_log * x0

# 2) Posterior preditiva de log da riqueza de uma nova ilha
set.seed(12345)

post$y_novo <- rnorm(
  n = nrow(post),
  mean = post$mu,
  sd = post$sigma
)

# Mesmos limites e classes para facilitar a comparação
limites <- range(c(post$mu, post$y_novo))
classes <- seq(limites[1], limites[2], length.out = 51)

# Posterior da média
p_media <- ggplot(post, aes(x = mu)) +
  geom_histogram(breaks = classes,
                 fill = "#e59c82",
                 color = "white",
                 linewidth = 0.5,
                 alpha = 0.8) +
  labs(x = expression(mu(beta[0] == 0.5)),
       y = "Frequency") +
  coord_cartesian(xlim = limites) +
  my_theme

p_media


# Posterior preditiva
p_pred <- ggplot(post, aes(x = y_novo)) +
  geom_histogram(breaks = classes,
                 fill = "#846267",
                 color = "white",
                 linewidth = 0.5,
                 alpha = 0.8) +
  labs(x = expression(hat(y)(beta[0] == 0.5)),
       y = "Frequency") +
  coord_cartesian(xlim = limites) +
  my_theme

p_pred

# Combinando os gráficos
fig <- p_media | p_pred

fig <- fig + 
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", colour = NA)))


p_media
ggsave(filename = "Figuras/posterior_mu_predictions.png",
       plot = p_media,
       width = 6,
       height = 4,
       # width = 5,
       # height = 7,
       dpi = 400,
       bg = "transparent")


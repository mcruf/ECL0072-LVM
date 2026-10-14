###################################
#                                 #
#         Figuras aula 04         #
#                                 #
###################################







library(ggplot2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Parâmetros ilustrativos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mu_pop    <- 15    # Média populacional
sigma_entre <- 3   # DP entre médias dos recifes
sigma_dentro <- 1  # Mesmo DP residual para todos os recifes

grupos <- data.frame(
  recife = LETTERS[1:5],
  media  = c(11, 13, 14.5, 16, 19)
)

cores <- c(
  A = "#846267",
  B = "#588157",
  C = "#e07a5f",
  D = "#457b9d",
  E = "#f6bd60"
)

x <- seq(2, 28, length.out = 2000)

# Deslocamento vertical para separar os dois níveis
base_pop <- 0.65

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Curvas normais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

curvas_grupos <- do.call(
  rbind,
  lapply(seq_len(nrow(grupos)), function(i) {
    data.frame(
      recife = grupos$recife[i],
      x = x,
      densidade = dnorm(
        x,
        mean = grupos$media[i],
        sd = sigma_dentro
      )
    )
  })
)

curva_pop <- data.frame(
  x = x,
  densidade = dnorm(x, mean = mu_pop, sd = sigma_entre)
)

# Ampliação vertical apenas para apresentação
fator_altura <- 2

curva_pop$altura <- curva_pop$densidade * fator_altura

pico_pop <- dnorm(mu_pop, mean = mu_pop, sd = sigma_entre) *
  fator_altura


# Mesma escala de densidade nos dois níveis
grupos$pico <- dnorm(
  grupos$media,
  mean = grupos$media,
  sd = sigma_dentro
)

# Setas ilustrativas: intervalo de ±2 desvios-padrão
grupos$seta_y <- seq(-0.045, -0.165, length.out = nrow(grupos))
grupos$xmin <- grupos$media - 2 * sigma_dentro
grupos$xmax <- grupos$media + 2 * sigma_dentro
grupos$rotulo <- paste0("mu[", grupos$recife, "]")

seta_dupla <- grid::arrow(
  ends = "both",
  type = "closed",
  length = grid::unit(0.10, "inches")
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Figura
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p <- ggplot() +
  
  # Distribuição populacional das médias
  geom_ribbon(
    data = curva_pop,
    aes(x = x, ymin = base_pop, ymax = base_pop + altura),
    fill = "grey90",
    colour = NA
  ) +
  geom_line(
    data = curva_pop,
    aes(x = x, y = base_pop + altura),
    colour = "grey65",
    linewidth = 1
  ) +
  
  # Distribuições das observações dentro de cada recife
  geom_ribbon(
    data = curvas_grupos,
    aes(x = x, ymin = 0, ymax = densidade,
        fill = recife, group = recife),
    alpha = 0.28
  ) +
  geom_line(
    data = curvas_grupos,
    aes(x = x, y = densidade, colour = recife),
    linewidth = 0.9
  ) +
  
  # Linhas de base
  annotate(
    "segment",
    x = 2, xend = 28,
    y = 0, yend = 0,
    colour = "grey25", linewidth = 0.6
  ) +
  annotate(
    "segment",
    x = 2, xend = 28,
    y = base_pop, yend = base_pop,
    colour = "grey25", linewidth = 0.6
  ) +
  
  # Médias: linhas tracejadas
  annotate(
    "segment",
    x = mu_pop, xend = mu_pop,
    y = base_pop,
    yend = 0.92,
    linetype = "dashed",
    colour = "grey25",
    linewidth = 0.7
  ) +
  geom_segment(
    data = grupos,
    aes(x = media, xend = media, y = 0, yend = pico,
        colour = recife),
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  # Rótulo da média populacional
  annotate(
    "text",
    x = mu_pop + 0.4, y = base_pop + 0.025,
    label = "mu", parse = TRUE,
    size = 6, hjust = 0
  ) +
  
  # Dispersão entre médias: ±2 sigma_alpha
  annotate(
    "segment",
    x = mu_pop - 2 * sigma_entre,
    xend = mu_pop + 2 * sigma_entre,
    y = base_pop - 0.045,
    yend = base_pop - 0.045,
    colour = "grey45",
    linewidth = 0.6,
    arrow = seta_dupla
  ) +
  # Dispersão dentro dos recifes: setas de mesmo comprimento
  geom_segment(
    data = grupos,
    aes(x = xmin, xend = xmax,
        y = seta_y, yend = seta_y,
        colour = recife),
    linewidth = 0.6,
    arrow = seta_dupla
  ) +
  
  # Identificação das médias de cada recife
  geom_text(
    data = grupos,
    aes(x = media, y = -0.225,
        label = rotulo, colour = recife),
    parse = TRUE,
    size = 6
  ) +
  
  # Identificação dos níveis
  annotate(
    "text",
    x = 28, y = base_pop + 0.05,
    label = "Nível populacional",
    hjust = 1, size = 5
  ) +
  annotate(
    "text",
    x = 28, y = 0.05,
    label = "Nível de grupo",
    hjust = 1, size = 5
  ) +
  
  scale_colour_manual(values = cores) +
  scale_fill_manual(values = cores) +
  coord_cartesian(
    xlim = c(2, 28),
    ylim = c(-0.27, 0.86),
    expand = FALSE,
    clip = "off"
  ) +
  theme_void(base_size = 16) +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = "transparent", colour = NA
    ),
    panel.background = element_rect(
      fill = "transparent", colour = NA
    ),
    plot.margin = margin(30, 20, 15, 20)
  )

p


ggsave(filename = "Figuras/shrinkage_plot.png",
       plot = p,
       width = 9,
       height = 7,
       dpi = 400,
       bg = "transparent")


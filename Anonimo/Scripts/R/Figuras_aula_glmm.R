#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#       Figures for lectures
#
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(patchwork)
library(ggplot2)


setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-LVM/")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Shrinkage demonstration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Set up parameters
set.seed(42)
n_groups <- 5
pop_mean <- 100
pop_sd <- 15

# Custom palette
palette <- c("#846267", "#588157", "#e07a5f", "#457b9d", "#f6bd60")

# Group parameters
# Let's pick 5 distinct means and standard deviations
group_means <- c(85, 95, 100, 104, 120)
group_sds <- c(4, 8, 3, 5, 9)

# Grid contínuo equivalente ao np.linspace
x <- seq(pop_mean - 3.5 * pop_sd, pop_mean + 3.5 * pop_sd, length.out = 1000)

# Estrutura base de tema equivalente ao ax.axis('off')
theme_off <- theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

# Top plot: Population density
pop_y <- dnorm(x, mean = pop_mean, sd = pop_sd)
df_pop <- data.frame(x = x, y = pop_y)

p1 <- ggplot(df_pop, aes(x = x, y = y)) +
  geom_area(fill = "gray85", alpha = 0.5) +
  geom_line(color = "gray70", linewidth = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_off

# Bottom plot: Group densities
df_groups <- map_df(1:n_groups, function(i) {
  data.frame(
    x = x,
    y = dnorm(x, mean = group_means[i], sd = group_sds[i]),
    group = as.factor(i)
  )
})

# CORREÇÃO AQUI: adicionado position = "identity" no geom_area
p2 <- ggplot(df_groups, aes(x = x, y = y, fill = group, color = group)) +
  geom_area(position = "identity", alpha = 0.35) +
  geom_line(linewidth = 0.75) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_off

# Layout empilhado
final_plot <- (p1 / plot_spacer() / p2) +
  plot_layout(heights = c(1, 0.2, 1)) &
  scale_x_continuous(limits = range(x), expand = c(0, 0))

# Exibir no RStudio
final_plot

ggsave("Figuras/shrinkage_plot.png", 
       plot = final_plot, 
       width = 11,
       height = 7, 
       dpi = 400)
cat("Saved test_plot.png\n")
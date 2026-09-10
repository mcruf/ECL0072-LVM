################################################################
#                                                              #
#     Gráficos exibidos na aula de estatística espacial        #
#                                                              #
###############################################################


# Load libraries
#~~~~~~~~~~~~~~~~
library(ggplot2)
library(patchwork)
library(sf)
library(spdep)
library(dplyr)
library(terra)
library(rnaturalearth)
library(viridis)



# Customizing ggplot theme
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_theme <- theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', margin = margin(b = 15), size = 13),
        axis.text.x = element_text(colour = "grey20", margin = margin(t = 5)),
        axis.text.y = element_text(colour = "grey20", margin = margin(r = 5)),
        axis.title = element_text(face = "bold", colour = "black"),
        strip.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(hjust = 0.5, face = 'bold', size = 10),
        legend.text = element_text(size = 9),
        legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.major.y = element_line(linetype = 'dotted'),
        panel.grid.major.x = element_line(linetype = 'dotted'),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) # Espaçamento lateral por painel





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Spatial correlation (postive, negative, none)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Update the function with transparent background settings
plot_grid <- function(mat, title, is_gradient = FALSE) {
  df <- expand.grid(x = 1:6, y = 1:6)
  df$val <- as.vector(mat)
  
  p <- ggplot(df, aes(x = x, y = y, fill = val)) +
    geom_tile(color = "gray40", linewidth = 0.6) +
    scale_y_reverse() +
    coord_fixed() +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, margin = margin(b = 10)),
      legend.position = "none",
      # Set ggplot canvas backgrounds to transparent
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
  
  if (is_gradient) {
    p + scale_fill_gradient(low = "white", high = "#e07a5f")
  } else {
    p + scale_fill_gradient(low = "white", high = "#e07a5f")
  }
}


# 2. Build the matrices and plots
mat_neg <- outer(1:6, 1:6, function(i, j) (i + j) %% 2)
mat_zero <- matrix(c(
  0, 0, 1, 0, 0, 0,
  0, 1, 0, 0, 0, 1,
  0, 0, 1, 1, 0, 1,
  0, 1, 1, 0, 1, 0,
  1, 0, 1, 0, 0, 0,
  1, 0, 0, 1, 1, 0
), nrow = 6, byrow = TRUE)

center_dist <- outer(1:6, 1:6, function(i, j) sqrt((i - 3.5)^2 + (j - 3.5)^2))
mat_pos <- 1 - (center_dist / max(center_dist))

p1 <- plot_grid(mat_neg, "Correlação espacial \nnegativa")
p2 <- plot_grid(mat_zero, "Ausência de correlação \nespacial")
p3 <- plot_grid(mat_pos, "Correlação espacial \npositiva", is_gradient = TRUE)

# 3. Combine with patchwork and apply transparent theme to the composite plot
final_plot <- (p3 + p2 + p1) & 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# 4. Save with bg = "transparent" (PNG format supports alpha transparency)
ggsave(
  filename = "Figuras/spatial_autocorrelation_types.png", 
  plot = final_plot, 
  width = 9, 
  height = 3.5, 
  dpi = 300, 
  bg = "transparent"
)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Moran scatterplot & LISA map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(42)

# Grid setup
grid <- st_make_grid(st_bbox(c(xmin = 0, xmax = 25, ymin = 0, ymax = 25), crs = 31984),
                     n = c(25, 25), what = "polygons") %>% 
  st_sf(id = 1:length(.), geometry = .)

centers <- st_coordinates(st_centroid(grid))

raw_richness <- 18 + 18 * exp(-((centers[,1] - 18)^2 + (centers[,2] - 20)^2) / 45) - 10 * exp(-((centers[,1] - 6)^2 + (centers[,2] - 7)^2) / 35) + rnorm(nrow(grid), mean = 0, sd = 5)
grid$richness <- round(pmax(1, raw_richness))

sample_high_low <- sample(which(grid$richness < 12), 5)
grid$richness[sample_high_low] <- grid$richness[sample_high_low] + 20

sample_low_high <- sample(which(grid$richness > 22), 5)
grid$richness[sample_low_high] <- pmax(1, grid$richness[sample_low_high] - 18)

# 1. Richness Plot
p_richness <- ggplot(grid) +
  geom_sf(aes(fill = richness), color = "white") +
  scale_fill_viridis_c(
    option = 'A', 
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,
      barwidth = unit(4, "cm"), 
      barheight = unit(0.4, "cm")
    )
  ) +
  coord_sf(expand = c(0,0)) +
  labs(title = "Species richness", fill = "Richness") +
  my_theme

# LISA stats
nb <- poly2nb(grid)
lw <- nb2listw(nb, style = "W")

grid$x <- as.numeric(scale(grid$richness))
grid$lag <- as.numeric(scale(lag.listw(lw, grid$richness)))

lisa <- localmoran(grid$richness, lw) 
grid$p_value <- lisa[, 5] 

grid <- grid %>%
  mutate(
    raw_quadrant = case_when(
      x >= 0 & lag >= 0 ~ "High-High",
      x >= 0 & lag < 0  ~ "High-Low",
      x < 0  & lag >= 0 ~ "Low-High",
      x < 0  & lag < 0  ~ "Low-Low"
    ),
    lisa_cluster = if_else(p_value < 0.05, raw_quadrant, "Not Significant"),
    lisa_cluster = factor(lisa_cluster, levels = c("High-High", "High-Low", "Low-High", "Low-Low", "Not Significant"))
  )

lisa_colors <- c(
  "High-High"       = "#e07a5f", 
  "High-Low"        = "#f6bd60", 
  "Low-High"        = "#588157", 
  "Low-Low"         = "#457b9d", 
  "Not Significant" = "grey85"
)

# 2. Moran Scatterplot (Usando shape = 21 e fill para ter o mesmo símbolo/tamanho do mapa)
p_scatter <- ggplot(grid, aes(x = x, y = lag)) +
              geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
              geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
              geom_point(aes(fill = lisa_cluster, alpha = lisa_cluster), shape = 21, size = 3.5, color = "white", alpha = 0.7, stroke = 0.2) +
              geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
              scale_fill_manual(values = lisa_colors, drop = FALSE, guide = guide_legend(title.position = "top", 
                                                                                         title.hjust = 0.5, 
                                                                                         nrow = 2,
                                                                                         byrow = T,
                                                                                         override.aes = list(size = 3.5, alpha = 1))) +
              scale_alpha_manual(values = c("High-High" = 0.9, "High-Low" = 0.9, "Low-High" = 0.9, "Low-Low" = 0.9, "Not Significant" = 0.4), guide = "none") +
              labs(title = "Moran scatterplot", x = "Source values", y = "Lagged values", fill = "Quadrant") +
              my_theme


# 3. Spatial Map
p_map <- ggplot(grid) +
  geom_sf(aes(fill = lisa_cluster), color = "white") +
  scale_fill_manual(values = lisa_colors, drop = FALSE, guide = guide_legend(title.position = "top", 
                                                                             title.hjust = 0.5,
                                                                             nrow = 2,              
                                                                             byrow = TRUE)) +
  coord_sf(expand = c(0,0)) +
  labs(title = "LISA cluster map", fill = "Quadrant") +
  my_theme
            

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combined Layout Fix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_combined <- p_richness + p_scatter + p_map + 
  plot_layout(ncol = 3) & 
  theme(legend.position = "bottom",
        legend.location = "panel", # Garante alinhamento exato abaixo do painel de cada plot
        legend.justification = "center",
        panel.spacing = unit(2, "lines"))


ggsave(filename = "Figuras/moran_scatterplot.png", 
       plot = p_combined, 
       width = 14, 
       height = 5.5, 
       dpi = 300, 
       bg = "transparent")


#~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Variogram models
#~~~~~~~~~~~~~~~~~~~~~~~~


set.seed(123)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Parâmetros globais para o ajuste
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
psill <- 10      # Patamar parcial (C)
nugget <- 1.5    # Efeito pepita (C0)
range_val <- 8   # Alcance (a)
max_dist <- 15

# Distâncias contínuas para desenhar as linhas suaves
h_seq <- seq(0, max_dist, length.out = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Gerar UM ÚNICO conjunto com MAIS PONTOS empíricos (30 pontos)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
h_points <- seq(0.4, max_dist, length.out = 30)

# Semivariância empírica baseada no modelo esférico com ruído
v_base <- vgm(psill = psill, model = "Sph", range = range_val, nugget = nugget)
gamma_base <- variogramLine(v_base, dist_vector = h_points)$gamma

df_empirico <- data.frame(
  dist = h_points,
  gamma = pmax(0, gamma_base + rnorm(length(h_points), mean = 0, sd = 0.75))
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Gerar as curvas teóricas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_model_line <- function(model_name, kappa = 0.5) {
  if (model_name == "Lin") {
    gamma <- ifelse(h_seq <= range_val, nugget + psill * (h_seq / range_val), nugget + psill)
    return(data.frame(dist = h_seq, gamma = gamma, model = "Linear"))
  }
  
  v_mod <- vgm(psill = psill, model = model_name, range = range_val, nugget = nugget, kappa = kappa)
  df <- variogramLine(v_mod, dist_vector = h_seq)
  return(df)
}

df_sph <- get_model_line("Sph") %>% mutate(model = "Esférico")
df_cir <- get_model_line("Cir") %>% mutate(model = "Circular")
df_exp <- get_model_line("Exp") %>% mutate(model = "Exponencial")
df_gau <- get_model_line("Gau") %>% mutate(model = "Gaussiano")
df_lin <- get_model_line("Lin") %>% mutate(model = "Linear")
df_mat <- get_model_line("Mat", kappa = 0.2) %>% mutate(model = "Matérn")

df_lines <- bind_rows(df_sph, df_cir, df_exp, df_lin, df_gau, df_mat)

# Configuração de cores e ordem da legenda
model_levels <- c("Esférico", "Circular", "Exponencial", "Linear", "Gaussiano", "Matérn")
model_colors <- c("Esférico"       = "#bc6c25",
                  "Circular"        = "#588157",
                  "Exponencial"     = "#e07a5f",
                  "Linear"          = "#846267",
                  "Gaussiano"        = "#f6bd60",
                  "Matérn"          = "#457b9d")

df_lines$model <- factor(df_lines$model, levels = model_levels)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Construção do gráfico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_variograms <- ggplot() +
  # Eixos conceituais na origem
  geom_vline(xintercept = 0, linewidth = 1, color = "gray20") +
  geom_hline(yintercept = 0, linewidth = 1, color = "gray20") +
  
  # As curvas teóricas
  geom_line(
    data = df_lines, 
    aes(x = dist, y = gamma, color = model), 
    linewidth = 1
  ) +
  
  # O conjunto densificado de pontos empíricos
  geom_point(data = df_empirico, 
             aes(x = dist, y = gamma), 
             size = 3.5, 
             color = "gray20", 
             shape = 19,
             stroke = 0.6,
             alpha = 0.70) +
  
  scale_color_manual(values = model_colors) +
  labs(x = "Distância", y = expression(bold(gamma(h))), color = 'Modelo') +
  coord_cartesian(xlim = c(0, max_dist + 0.5), ylim = c(-0.5, psill + nugget + 2.5)) +
  my_theme +
  theme(panel.border = element_blank(),
        legend.position = 'right')

p_variograms


ggsave(filename = "Figuras/variogram_models.png", 
       plot = p_variograms, 
       width = 8, 
       height = 4, 
       dpi = 500, 
       bg = "transparent")



############################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Gaussian Random Field simulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(MASS)
library(ggplot2)
library(viridis)
library(dplyr)


set.seed(123)



# 1. Spatial domain
#----------------------------------------------------

x <- seq(0, 10, length.out = 70)
y <- seq(0, 10, length.out = 50)

grid <- expand.grid(
  x = x,
  y = y
)



# 2. Spatial covariance function
#----------------------------------------------------

sigma2 <- 1.5
range <- 1.5


exponential_covariance <- function(dist, sigma2, range){
  
  sigma2 * exp(-dist/range)
  
}



# 3. Distance matrix
#----------------------------------------------------

dist_matrix <- as.matrix(dist(grid[,c("x","y")]))



# 4. Covariance matrix
#----------------------------------------------------

Sigma <- exponential_covariance(
  dist_matrix,
  sigma2,
  range
)



# 5. Gaussian random field
#----------------------------------------------------

grf <- mvrnorm(
  n = 1,
  mu = rep(0,nrow(grid)),
  Sigma = Sigma
)



# 6. Ecological trend (HOTSPOT)
#----------------------------------------------------

hotspot <- 20 *
  exp(
    -(
      ((grid$x-7)^2)/(2*1.5^2) +
        ((grid$y-7)^2)/(2*1.5^2)
    )
  )


# Secondary gradient
gradient <- 5*(grid$x/10)


# Combine process

grid$abundance <- hotspot +
  gradient +
  grf*3



# standardize

grid$abundance <- scales::rescale(
  grid$abundance,
  to=c(0,100)
)




# 7. Plot GRF
#----------------------------------------------------

p_grf <- ggplot(grid, aes(x, y,fill=abundance))+
          geom_raster(interpolate = TRUE, alpha = 0.8) +
          scale_fill_viridis_c(option="A", name="Abundância",
                               guide = guide_colorbar(
                                 title.position = "top", 
                                 title.hjust = 0.5,
                                 barwidth = unit(4, "cm"), 
                                 barheight = unit(0.4, "cm")
                               )) +
          coord_cartesian(expand = c(0, 0)) + 
          my_theme +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank())
        

ggsave(filename = "Figuras/GRF.png", 
       plot = p_grf, 
       width = 8, 
       height = 6, 
       dpi = 400, 
       bg = "transparent")



##############################################################


#~~~~~~~~~~~~~~~~~~~~~
# 5) Bathymetry 
#~~~~~~~~~~~~~~~~~~~~~~
# Desativar S2 para operações espaciais do sf
sf_use_s2(FALSE)

# 1. Carregar dados
batimetria <- rast("Dados/GIS/Environmental/bathymetry.nc")
bat <- -batimetria

world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")

# 2. Definição da extensão de Natal e recortes
ext_natal <- ext(-35.45, -34.7, -5.95, -5.45)

bat_natal <- crop(bat, ext_natal)
bbox_natal <- st_bbox(c(xmin = -35.45, xmax = -34.90, ymin = -5.95, ymax = -5.45), 
                      crs = st_crs(world))
natal <- st_crop(world, bbox_natal)

# 3. Converter rasters para DataFrames para uso no ggplot2
df_bat <- as.data.frame(bat, xy = TRUE)
colnames(df_bat) <- c("x", "y", "z")

df_natal <- as.data.frame(bat_natal, xy = TRUE)
colnames(df_natal) <- c("x", "y", "z")

# Dataframe com os limites da caixa de Natal para destacar no primeiro mapa
df_rect <- data.frame(
  xmin = xmin(ext_natal), xmax = xmax(ext_natal),
  ymin = ymin(ext_natal), ymax = ymax(ext_natal)
)

# ----------------------------------------------------
# Gráfico 1: Batimetria Regional
# ----------------------------------------------------
p1 <- ggplot() +
  # 1. Raster bathymetry layer
  geom_raster(data = df_bat, aes(x = x, y = y, fill = z)) +
  
  # 2. Add world map on top with white fill
  geom_sf(data = world, fill = "white", color = "gray80", linewidth = 0.2, inherit.aes = FALSE) +
  
  # 3. Red bounding box for study area
  geom_rect(data = df_rect, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = "red", fill = NA, linewidth = 1) +
  
  # 4. Color scale
  scale_fill_viridis_c(name = "Profundidade (m)", 
                       guide = guide_colorbar(title.position = "top", 
                                              title.hjust = 0.5,
                                              barwidth = unit(4, "cm"), 
                                              barheight = unit(0.4, "cm"))) +
  
  # 5. Crop the map extent to your raster data boundaries
  coord_sf(xlim = range(df_bat$x), ylim = range(df_bat$y), expand = FALSE) +
  
  labs(title = "Batimetira - Atlântico Sudoeste", x = "Longitude", y = "Latitude") +
  my_theme


p1


ggsave(filename = "Figuras/Batimetria_SAO.png", 
       plot = p1, 
       width = 6, 
       height = 8, 
       dpi = 400, 
       bg = "transparent")



# Gráfico 2: Batimetria Natal (Pixels com Borda Branca e Texto)
# ----------------------------------------------------
p2 <- ggplot() +
  # Células do raster com borda branca
  geom_tile(data = df_natal, aes(x = x, y = y, fill = z), 
            color = "white", linewidth = 0.5) +
  # Linhas do continente por cima dos pixels
  geom_sf(data = natal, fill = "white", color = "gray90", size = 1,inherit.aes = FALSE) +
  # Texto com os valores centralizados no pixel
  geom_text(data = df_natal, aes(x = x, y = y, label = round(z, 1)), 
            color = "white", size = 5, fontface = "bold") +
  # Limitar a visualização estritamente à extensão de Natal
  coord_sf(xlim = c(xmin(ext_natal), xmax(ext_natal)), 
           ylim = c(ymin(ext_natal), ymax(ext_natal)), 
           expand = FALSE) +
  scale_fill_viridis_c(name = "Profundidade (m)", guide = guide_colorbar(title.position = "top", 
                                                                         title.hjust = 0.5,
                                                                         barwidth = unit(4, "cm"), 
                                                                         barheight = unit(0.4, "cm"))) +
  labs(title = "Batimetria - Natal", x = "Longitude", y = "Latitude") +
  my_theme 
p2



ggsave(filename = "Figuras/Batimetria_Natal.png", 
       plot = p2, 
       width = 6, 
       height = 8, 
       dpi = 400, 
       bg = "transparent")




##############################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Mapa mundi geoide
#~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. Dados dos continentes
#--------------------------------------------

world <- ne_countries(
  scale = 50,
  returnclass = "sf"
)



# 2. Projeção ortográfica (efeito globo)
#--------------------------------------------

# Centro do globo próximo ao Nordeste do Brasil
crs_globe <- paste0(
  "+proj=ortho ",
  "+lat_0=-5.8 ",
  "+lon_0=-35.2 ",
  "+datum=WGS84"
)


world_ortho <- st_transform(world, crs_globe)



# 3. Criar grade de latitude/longitude
#--------------------------------------------

graticule <- st_graticule(
  lat = seq(-80,80,20),
  lon = seq(-180,180,20),
  crs = 4326
)

graticule_ortho <- st_transform(
  graticule,
  crs_globe
)



# 4. Ponto Natal
#--------------------------------------------

natal <- st_as_sf(
  data.frame(
    lon = -35.211,
    lat = -5.794
  ),
  coords = c("lon","lat"),
  crs = 4326
)

natal_ortho <- st_transform(
  natal,
  crs_globe
)



# 5. Plot
#--------------------------------------------

p_map <- ggplot() +
  
  # grade
  geom_sf(
    data = graticule_ortho,
    color = "gray85",
    linewidth = 0.3
  ) +
  
  # continentes
  geom_sf(data = world_ortho,
          fill = "#e59c82",
          color = "gray95",
          linewidth = 0.3,
  ) +
  
  # marcador Natal
  geom_sf(data = natal_ortho,
          shape = 10,
          size = 7,
          color = "black",
          stroke = 1.2) +
  
  # remove fundo
  theme_void()

p_map


ggsave(filename = "Figuras/globo_terrestre.png", 
       plot = p_map, 
       width = 6, 
       height = 8, 
       dpi = 400, 
       bg = "transparent")



########################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6) Meridianos e paralelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Latitude x Longitude
########################

library(sf)
library(rnaturalearth)
library(ggplot2)
library(patchwork)

sf_use_s2(FALSE)


# Dados
#-------

world <- ne_countries(
  scale = 110,
  returnclass = "sf"
)

# Projeção ortográfica
crs_ortho <- "+proj=ortho +lat_0=0 +lon_0=-15 +datum=WGS84"

world_ortho <- st_transform(world, crs_ortho)


# Borda do globo
#----------------

R <- 6378137

theta <- seq(0, 2*pi, length.out = 500)

xy <- cbind(
  R*cos(theta),
  R*sin(theta)
)

# Fecha o polígono
xy <- rbind(xy, xy[1, ])

circle <- st_polygon(list(xy))

ocean_circle <- st_sfc(circle, crs = crs_ortho)


# Funções auxiliares (restringindo ao hemisfério visível)
#-----------------------------------------------------------

make_parallel <- function(lat){
  # Gera apenas no arco visível em torno do centro lon_0 = -15
  st_linestring(
    cbind(
      seq(-104.9, 74.9, length.out = 500),
      rep(lat, 500)
    )
  )
}

make_meridian <- function(lon){
  st_linestring(
    cbind(
      rep(lon, 500),
      seq(-89.9, 89.9, length.out = 500)
    )
  )
}


# Paralelos
#------------

latitudes <- seq(-80, 80, 20)

parallels <- st_sfc(
  lapply(latitudes, make_parallel),
  crs = 4326
)

parallels <- st_transform(
  parallels,
  crs_ortho
)

equator <- st_sfc(
  make_parallel(0),
  crs = 4326
)

equator <- st_transform(
  equator,
  crs_ortho
)


# Meridianos
#-------------

# Meridianos visíveis no horizonte da projeção
longitudes <- seq(-100, 70, 20)

meridians <- st_sfc(
  lapply(longitudes, make_meridian),
  crs = 4326
)

meridians <- st_transform(
  meridians,
  crs_ortho
)

greenwich <- st_sfc(
  make_meridian(0),
  crs = 4326
)

greenwich <- st_transform(
  greenwich,
  crs_ortho
)


# Limites do globo
#------------------

bb <- st_bbox(ocean_circle)


# Painel Latitude (com indicação do Equador)
#---------------------------------------------

p_lat <-
  ggplot() +
  
  # Fundo do globo
  geom_sf(
    data = ocean_circle,
    fill = "white",
    colour = "gray95",
    linewidth = .7
  ) +
  
  # Paralelos
  geom_sf(
    data = parallels,
    colour = "grey70",
    linewidth = .45
  ) +
  
  # Equador em destaque
  geom_sf(
    data = equator,
    colour = "gray20",
    linewidth = 1
  ) +
  
  # Continentes
  geom_sf(
    data = world_ortho,
    fill = "#e59c82",
    colour = "gray95",
    linewidth = .3
  ) +
  
  # Rótulos Norte e Sul
  annotate(
    "text",
    x = 0,
    y = bb$ymax * 1.08,
    label = "Norte (+)",
    size = 4.5
  ) +
  
  annotate(
    "text",
    x = 0,
    y = bb$ymin * 1.08,
    label = "Sul (-)",
    size = 4.5
  ) +
  
  # Indicação do Equador na borda esquerda do globo
  annotate(
    "text",
    x = bb$xmin * 1.08,
    y = 0,
    label = "Equador (0°)",
    size = 4,
    hjust = 1,        # Alinha à direita do ponto
    fontface = "bold"
  ) +
  
  coord_sf(
    xlim = c(bb$xmin, bb$xmax),
    ylim = c(bb$ymin, bb$ymax),
    expand = FALSE,
    clip = "off"
  ) +
  
  theme_void() +
  theme(
    plot.margin = margin(t = 25, r = 35, b = 25, l = 75, unit = "pt")
  )


# Painel Longitude (com indicação de Greenwich, Oeste e Leste)
#--------------------------------------------------------------

p_lon <-
  ggplot() +
  
  geom_sf(
    data = ocean_circle,
    fill = "white",
    colour = "gray95",
    linewidth = .7
  ) +
  
  geom_sf(
    data = meridians,
    colour = "grey70",
    linewidth = .45
  ) +
  
  geom_sf(
    data = greenwich,
    colour = "black",
    linewidth = 1
  ) +
  
  geom_sf(
    data = world_ortho,
    fill = "#e59c82",
    colour = "gray95",
    linewidth = .2
  ) +
  
  # Título topo
  annotate(
    "text",
    x = 0,
    y = bb$ymax * 1.08,
    label = "Meridiano de Greenwich (0°)",
    size = 4,
    fontface = "bold"
  ) +
  
  # Rótulo Oeste (-) à esquerda
  annotate(
    "text",
    x = bb$xmin * 1.08,
    y = 0,
    label = "Oeste (-)",
    size = 4,
    hjust = 1
  ) +
  
  # Rótulo Leste (+) à direita
  annotate(
    "text",
    x = bb$xmax * 1.08,
    y = 0,
    label = "Leste (+)",
    size = 4,
    hjust = 0
  ) +
  
  coord_sf(
    xlim = c(bb$xmin, bb$xmax),
    ylim = c(bb$ymin, bb$ymax),
    expand = FALSE,
    clip = "off"
  ) +
  
  theme_void() +
  theme(
    # Margem esquerda e direita ampliadas para acomodar "Oeste (-)" e "Leste (+)"
    plot.margin = margin(t = 25, r = 75, b = 25, l = 50, unit = "pt")
  )



# Figura final
#--------------

p_lat + p_lon


############################################

#~~~~~~~~~~~~~~~~~~~~~~
# 7) CRS referenciado
#~~~~~~~~~~~~~~~~~~~~~~


###############################################################
# Transformação de Projeção (Natal, RN, Brasil)
###############################################################

###############################################################
# Transformação de Projeção (Natal, RN, Brasil)
###############################################################

library(sf)
library(rnaturalearth)
library(ggplot2)
library(patchwork)

sf_use_s2(FALSE)

#--------------------------------------------------------------
# 1. Coordenadas de Natal, RN, Brasil
#--------------------------------------------------------------

lat_natal <- -5.795
lon_natal <- -35.209

natal_sf <- st_sf(
  geometry = st_sfc(st_point(c(lon_natal, lat_natal)), crs = 4326)
)

#--------------------------------------------------------------
# 2. Dados Globais dos Continentes
#--------------------------------------------------------------

world <- ne_countries(scale = 110, returnclass = "sf")

# Projeção Ortográfica centrada em Natal, RN
crs_ortho <- sprintf("+proj=ortho +lat_0=%.3f +lon_0=%.3f +datum=WGS84", lat_natal, lon_natal)

world_ortho <- st_transform(world, crs_ortho)
natal_ortho <- st_transform(natal_sf, crs_ortho)

# Borda do Globo (Oceano)
R <- 6378137
theta <- seq(0, 2*pi, length.out = 500)
circle <- st_polygon(list(rbind(cbind(R*cos(theta), R*sin(theta)), c(R*cos(0), R*sin(0)))))
ocean_circle <- st_sfc(circle, crs = crs_ortho)

#--------------------------------------------------------------
# 3. Grade de Coordenadas (Graticule Vermelha)
#--------------------------------------------------------------

# Limites do hemisfério visível em torno de Natal
make_parallel <- function(lat) {
  st_linestring(cbind(seq(lon_natal - 89.9, lon_natal + 89.9, length.out = 300), rep(lat, 300)))
}

make_meridian <- function(lon) {
  st_linestring(cbind(rep(lon, 300), seq(-89.9, 89.9, length.out = 300)))
}

lats_seq <- seq(-80, 80, by = 10)
lons_seq <- seq(-180, 180, by = 10)

# Grade para o globo
grid_lat_ortho <- st_sfc(lapply(lats_seq, make_parallel), crs = 4326) |> st_transform(crs_ortho)
grid_lon_ortho <- st_sfc(lapply(lons_seq, make_meridian), crs = 4326) |> st_transform(crs_ortho)

# Grade plana retangular (WGS84 pura)
grid_flat_lat <- st_sfc(lapply(lats_seq, function(lat) st_linestring(cbind(seq(-180, 180, length.out = 100), rep(lat, 100)))), crs = 4326)
grid_flat_lon <- st_sfc(lapply(lons_seq, function(lon) st_linestring(cbind(rep(lon, 100), seq(-90, 90, length.out = 100)))), crs = 4326)

# Bounding box para corte
bb <- st_bbox(ocean_circle)




# 4. PAINEL 1: Globo Tridimensional (Ortográfico)
#--------------------------------------------------------------
p_ortho <- ggplot() +
  # Borda e fundo
  geom_sf(data = ocean_circle, fill = NA, colour = NA, linewidth = 0.6) +
  # Grade vermelha
  geom_sf(data = grid_lat_ortho, colour = "gray70", linewidth = 0.4) +
  geom_sf(data = grid_lon_ortho, colour = "gray70", linewidth = 0.4) +
  # Continentes
  geom_sf(data = world_ortho, fill = "#e59c82", colour = "gray95", linewidth = 0.35) +
  # Pino sobre Natal, RN (Marcador)
  geom_sf(data = natal_ortho, color = "black", size = 4.5, shape = 21, fill = "white", stroke = 1.8) +
  geom_sf(data = natal_ortho, color = "black", size = 1.8, shape = 16) +
  coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = FALSE, clip = "off") +
  theme_void() 


ggsave(filename = "Figuras/CRS_projetado.png", 
       plot = p_ortho, 
       width = 6, 
       height = 4, 
       dpi = 400, 
       bg = "transparent")



# 5. PAINEL 3: Mapa Plano Retangular (Equirretangular)
#--------------------------------------------------------------

p_flat <- ggplot() +
  # Fundo
  geom_sf(data = st_as_sfc(st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs = 4326)),
          fill = NA, colour = NA) +
  # Grade vermelha densa
  geom_sf(data = grid_flat_lat, colour = "gray70", linewidth = 0.4) +
  geom_sf(data = grid_flat_lon, colour = "gray70", linewidth = 0.4) +
  # Continentes
  geom_sf(data = world, fill = "#e59c82", colour = "gray95", linewidth = 0.35) +
  # Pino sobre Natal, RN
  geom_sf(data = natal_sf, color = "black", size = 4.5, shape = 21, fill = "white", stroke = 1.8) +
  geom_sf(data = natal_sf, color = "black", size = 1.8, shape = 16) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  theme_void() 

p_flat



ggsave(filename = "Figuras/CRS_projetado_cartesiano.png", 
       plot = p_flat, 
       width = 6, 
       height = 4, 
       dpi = 400, 
       bg = "transparent")


#######################################


###############################################################
# 8) Três Projeções Distintas (Mercator, Conic, Azimuthal, UTM)
###############################################################

library(sf)
library(rnaturalearth)
library(ggplot2)

sf_use_s2(FALSE)


# 1. Coordenadas e Ponto de Natal, RN, Brasil
#--------------------------------------------------------------

# Continentes base (WGS84)
world <- ne_countries(scale = 110, returnclass = "sf")



# 2. Grade de Coordenadas Globais (Graticule em WGS84)
#--------------------------------------------------------------

lats_seq <- seq(-80, 80, by = 20)
lons_seq <- seq(-180, 180, by = 30)

grid_lat <- st_sfc(
  lapply(lats_seq, function(lat) {
    st_linestring(cbind(seq(-180, 180, length.out = 300), rep(lat, 300)))
  }),
  crs = 4326
)

grid_lon <- st_sfc(
  lapply(lons_seq, function(lon) {
    st_linestring(cbind(rep(lon, 300), seq(-89.9, 89.9, length.out = 300)))
  }),
  crs = 4326
)



# 3. FIGURA 1: Projeção Cilíndrica (Mercator - EPSG:3857)
#--------------------------------------------------------------

crs_mercator <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

world_merc <- st_transform(world, crs_mercator)
grid_lat_merc <- st_transform(grid_lat, crs_mercator)
grid_lon_merc <- st_transform(grid_lon, crs_mercator)

p_mercator <- ggplot() +
  geom_sf(data = grid_lat_merc, colour = "gray70", linewidth = 0.4) +
  geom_sf(data = grid_lon_merc, colour = "gray70", linewidth = 0.4) +
  geom_sf(data = world_merc, fill = "#e59c82", colour = "gray95", linewidth = 0.35) +
  coord_sf(ylim = c(-12000000, 15000000), expand = FALSE) +
  theme_void()

p_mercator

ggsave("Figuras/Projecao_Mercator.png", plot = p_mercator, width = 8, height = 4, dpi = 400, bg = "transparent")



# 4. FIGURA 2: Projeção Cônica (World Equidistant Conic)
#--------------------------------------------------------------

bbox_crop <- st_bbox(c(xmin = -180, ymin = -60, xmax = 180, ymax = 85), crs = st_crs(4326))
world_cropped <- st_crop(world, bbox_crop)

# 3. Criar a grade de coordenadas (graticule) limitada
graticule <- st_graticule(lat = seq(-60, 80, by = 20), lon = seq(-180, 180, by = 30))

# Projeção Cônica Equidistante
crs_conic <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=60 +lat_2=60 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


p_conic <- ggplot() +
  # Grade de meridianos e paralelos
  geom_sf(data = st_transform(graticule, crs_conic), color = "gray70", size = 0.4) +
  # Países recortados sem a borda distorcida do sul
  geom_sf(data = st_transform(world_cropped, crs_conic), fill = "#e59c82", color = "gray95", size = 0.35) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(hjust = 0.5, face = "italic", size = 12, margin = margin(t = 10, b = 10)),
    plot.margin = margin(10, 10, 10, 10)
  )
p_conic

ggsave("Figuras/Projecao_Conica.png", plot = p_conic, width = 6, height = 6, dpi = 400, bg = "transparent")



# 5. FIGURA 3: Projeção Planar / Azimutal (Lambert Azimuthal Equal-Area)
#--------------------------------------------------------------

# Centrado em 0° N, 0° E para corresponder exatamente à ilustração b
crs_laea <- "+proj=laea +lat_0=50 +lon_0=10 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

p_planar <- ggplot() +
  geom_sf(data = st_transform(graticule, crs_laea), color = "gray70", size = 0.4) +
  geom_sf(data = st_transform(world, crs_laea), fill = "#e59c82", color = "gray95", size = 0.35) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(hjust = 0.5, face = "italic", size = 12, margin = margin(t = 10, b = 10)),
    plot.margin = margin(10, 10, 10, 10)
  )


p_planar

ggsave("Figuras/Projecao_Planar.png", plot = p_planar, width = 6, height = 6, dpi = 400, bg = "transparent")



############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9) Dados geoestatísticos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Input padrão
#~~~~~~~~~~~~~~~~~
CRS_wgs <- 4326 # Coordenada padrão (WGS84 - long/lat)
CRS_lea <- "+proj=laea +lat_0=-25 +lon_0=-20 +datum=WGS84 +units=m +no_defs" # Projeção LEA centrada no AS


# 1.1) Dados pesqueiros
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/fisheries.rds")


## Baixando mapa mundi
world <- ne_countries(scale = 50, returnclass = "sf") # Scale options: 110 (low), 50 (medium), 10 (high)


## Recortando mapa mundi para área de estudo (South Atlantic Ocean - SAO)
extent_bbox <- st_bbox(c(xmin = -70, xmax = -12,  ymin = -50, ymax = 5), 
                       crs = CRS_wgs) # Definindo área de recorte

sao <- st_crop(world, extent_bbox) # Recortando para SAO 


# 1.2) Transformando os dados para objeto sf (espacial)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CUIDADO: a função st_as_sf NÃO reprojeta dados; apenas informa a projeção original na qual as coordenadas dos dados foram coletadas!
dfsp <- st_as_sf(df, 
                 coords = c('Long','Lat'), 
                 crs = CRS_wgs) # As coordenadas estão originalmente em LAT/LONG; portanto, usar primeiro o CRS padrão (WGS84)



## Removendo registros em terra
dfsp2 <- dfsp[lengths(st_intersects(dfsp, sao)) == 0,] 



## Bubbleplot
p_geostatistics <- ggplot() +
                    geom_sf(data = dfsp2, aes(size = Prionace_glauca, color = Prionace_glauca), alpha = 0.5) + #Thunnus_albacares, Istiophorus_albicans
                    geom_sf(data = sao, fill = "gray99", color = "gray80", size = 0.5) +
                    scale_color_viridis_c(option = "A", guide = guide_colorbar(
                      title.position = "top", 
                      title.hjust = 0.5,
                      barwidth = unit(4, "cm"), 
                      barheight = unit(0.4, "cm")
                    )) + # option = A-I
                    guides(size = "none") +
                    labs(color = "Prionace glauca") +
                    labs(title = "Abundância do tubarão azul \nno Atlântico Sul") + 
                    coord_sf(expand = FALSE, ylim=c(-50, 2)) +
                    my_theme +
                    scale_size(range = c(0.8, 7)) +
                    theme(panel.background = element_rect(fill = '#dbe9ee', color = NA),
                          plot.title = element_text(hjust = 0.5, size = 13, margin = margin(t = 10, b = 10)),
                          plot.margin = margin(10, 10, 10, 10),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())

p_geostatistics

ggsave("Figuras/Dado_geoestatistico.png", plot = p_geostatistics, width = 6, height = 6, dpi = 400, bg = "transparent")




############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 10) Dados de área
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(covid19br)

dat <- add_geo(election2018Cities)

p_areal <- ggplot(dat) +
           geom_sf(aes(fill = EDHI), col = NA) + 
           scale_fill_viridis_c(option = "A", guide = guide_colorbar(
              title.position = "top", 
              title.hjust = 0.5,
              barwidth = unit(4, "cm"), 
              barheight = unit(0.4, "cm"))) + # option = A-I
           guides(size = "none") +
           coord_sf(expand = FALSE, datum = NA) +
           labs(title = "IDH da Educação") + 
           my_theme +
           theme(panel.background = element_rect(fill = NA, color = NA),
                  plot.title = element_text(hjust = 0.5, size = 13, margin = margin(t = 10, b = 10)),
                  plot.margin = margin(10, 10, 10, 10),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())


p_areal


ggsave("Figuras/Dado_areal.png", plot = p_areal, width = 6, height = 6, dpi = 400, bg = "transparent")



############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 11) Dados de padrões espaciais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(spatstat)

# Load the longleaf dataset
data(longleaf)

# Convert the SpatialPointsDataFrame to a regular data frame
longleaf_df <- as.data.frame(longleaf)

# Create the ggplot2 plot
p_point <- ggplot(longleaf_df, aes(x = x, y = y, size = marks, col = marks)) +
            geom_point( alpha = 0.7) +
            scale_color_viridis_c(option = 'A', guide = guide_colorbar(
              title.position = "top", 
              title.hjust = 0.5,
              barwidth = unit(4, "cm"), 
              barheight = unit(0.4, "cm"))) +
        guides(size = "none") +
        scale_size(range = c(0.8, 5)) +
  
            labs(title = "Posição & diâmetro de árvores",
                 x = "Easting",
                 y = "Northing") +
           my_theme +
        theme(panel.background = element_rect(fill = NA, color = NA),
              plot.title = element_text(hjust = 0.5, size = 13, margin = margin(t = 10, b = 10)),
              plot.margin = margin(10, 10, 10, 10),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

p_point      

ggsave("Figuras/Dado_pointpattern.png", plot = p_point, width = 6, height = 6, dpi = 400, bg = "transparent")




###########################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 11) Mapa mundi projeção equal earth
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")

p_equal <- ggplot() +
              geom_sf(data = world, fill = "#e59c82", color = "white", size = 0.2) +
              coord_sf(crs = 8857) +
              theme_minimal() +
              theme(axis.text = element_blank())

ggsave("Figuras/projecao_equal_earth.png", plot = p_equal, width = 6, height = 4, dpi = 400, bg = "transparent")





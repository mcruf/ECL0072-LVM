###################################################################################
#                                                                                 #
#                                 Prática da aula 06                              #
#                       - Explorando dados espaço-temporais -                      #
#                                                                                 #
####################################################################################





#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) # Processamento doas dados
library(sf) # Processamento de dados (espaciais) vetorizados - ie., shapefiles
library(rnaturalearth) # Baixar mapas do Natural Earth (https://www.naturalearthdata.com/)
library(ggplot2) # Visualização dos dados
library(viridis) # Paleta de cores
library(patchwork) # Confecção de figuras compostas
library(mapview) # Visualização de dados espacializados
library(gganimate) # Para gerar gráficos animados (gifs)
library(ncf) # Computar correlograma
library(gstat) # Computar variograma
library(spdep) # Computar Morans'I



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo tema geral do ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_theme <- theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                  axis.text.x = element_text(colour = "grey20", margin = margin(t = 10)),
                  axis.text.y = element_text(colour = "grey20", margin = margin(r = 10)),
                  axis.title = element_text(face = "bold", colour = "black", size = 16),
                  strip.background = element_rect(fill = 'white'),
                  legend.background = element_rect(fill = "transparent", colour = NA),
                  legend.box.background = element_rect(fill = "transparent", colour = NA),
                  panel.grid.major.y = element_line(linetype = 'dotted'),
                  panel.grid.major.x = element_line(linetype = 'dotted'),
                  axis.line = element_line(color = "black"),
                  plot.margin = margin(5, 10, 5, 5))



#~~~~~~~~~~~~~~~~~
# Input padrão
#~~~~~~~~~~~~~~~~~
CRS_wgs <- 4326 # Coordenada padrão (WGS84 - long/lat)
CRS_lea <- "+proj=laea +lat_0=-25 +lon_0=-20 +datum=WGS84 +units=m +no_defs" # Projeção LEA centrada no AS




#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando os dados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1) Dados pesqueiros
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/fisheries.rds")

head(as.data.frame(df))
str(df)


# 1.2) Shapefile da área de estudo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://www.naturalearthdata.com/ [Dados usados no presente tutorial]
# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html
# https://data.humdata.org/dataset/?q=brazil&ext_page_size=10&page=5


## Baixando mapa mundi
world <- ne_countries(scale = 50, returnclass = "sf") # Scale options: 110 (low), 50 (medium), 10 (high)
head(world)

### Visualizando...
plot(st_geometry(world), col = "gray90", graticule=T)

ggplot(data = world) +
  geom_sf(fill = "gray40", color = "white") +
  theme_minimal()


## Recortando mapa mundi para área de estudo (South Atlantic Ocean - SAO)
extent_bbox <- st_bbox(c(xmin = -70, xmax = -12,  ymin = -50, ymax = 5), 
                       crs = CRS_wgs) # Definindo área de recorte

sao <- st_crop(world, extent_bbox) # Recortando para SAO 


## Visualizando...
ggplot(data = sao) +
  geom_sf(fill = "gray40", color = "white") +
  theme_minimal()


#===========================================================================


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Tratando os dados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~


# 2.1) Corrigindo a natureza das variáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df$Month <- as.factor(df$Month)


# 2.2) Calculando riqueza por pescaria
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pa_df <- apply(df[, 9:23], MARGIN = 2, FUN = function(x) ifelse( x > 0, 1, 0))
df$Richness <- rowSums(pa_df)


# 2.3) Criando coluna mes-ano
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df$MonthYear <- paste(df$Month, df$Year, sep = '_')
df$MonthYear <- as.factor(df$MonthYear)
levels(df$MonthYear) # Oops, precisamos ordenar os níveis


# 2.4) Ordenando 'mes-ano' por ordem cronológica
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tlvls <- df %>%
  arrange(Year, Month) %>%
  pull(MonthYear) %>%
  unique()

df$MonthYear <- factor(df$MonthYear,
                       levels = Tlvls,
                       ordered = TRUE)

levels(df$MonthYear) # Sanity check



# 2.5) Transformando os dados para objeto sf (espacial)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CUIDADO: a função st_as_sf NÃO reprojeta dados; apenas informa a projeção original na qual as coordenadas dos dados foram coletadas!
dfsp <- st_as_sf(df, 
                 coords = c('Long','Lat'), 
                 crs = CRS_wgs) # As coordenadas estão originalmente em LAT/LONG; portanto, usar primeiro o CRS padrão (WGS84)

class(dfsp)
head(as.data.frame(dfsp))


## Visualizando...
ggplot(dfsp) +
  geom_sf(data = sao,fill = "gray40", color = "white") +
  geom_sf(col = '#e07a5f') +
  theme_minimal() # Oops! Há dados em ambiente terreste - precisam ser removidos!




# 2.6) Removendo registros em terra
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#dfsp2 <- st_filter(dfsp , sao, .predicate = st_disjoint)
dfsp2 <- dfsp[lengths(st_intersects(dfsp, sao)) == 0,] 
#rmvd <- dfsp[lengths(st_intersects(dfsp, sao)) == 1,] # Pontos em terra


## Visualizando
ggplot(dfsp2) +
  geom_sf(data = sao,fill = "gray40", color = "white") +
  geom_sf(col = '#e07a5f') +
  theme_minimal() # Oops! Há registros em ambiente terreste - precisam ser removidos!

print(paste("No. pontos removidos:", nrow(dfsp) - nrow(dfsp2)))


#===========================================================================


#~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Variável-resposta 
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Abundância do tubarão azul (Prionace glauca)


## Histograma
p1 <- ggplot(dfsp2, aes(x = Prionace_glauca)) +
  geom_histogram(col = 'white', fill = "#e07a5f") +
  labs(x="Abundância (N)",y = "Frequência")  +
  theme_minimal(base_size = 14) +
  coord_cartesian(expand = c(0,0)) +
  my_theme


## Boxplot  
p2 <- ggplot(dfsp2, aes(y = Prionace_glauca)) +
  geom_boxplot(fill = "#e07a5f") +
  labs(x = "",y = "Abundância (N)")  +
  theme_minimal(base_size = 14) +
  my_theme


## Visualizando
p1 + p2


summary(dfsp2$Prionace_glauca)
mean(dfsp2$Prionace_glauca == 0) #P roporção de zeros; ok

#sort(apply(df[,9:23], MARGIN = 2, FUN = function(x) mean(x == 0))) # Proporção de zeros para as demais espécies. Maioria é zero-inflacionada - típico de dados comunitários!



# 3.2) Dinâmica espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~

## Bubbleplot
ggplot() +
  geom_sf(data = dfsp2, aes(size = Prionace_glauca, color = Prionace_glauca), alpha = 0.5) + #Thunnus_albacares, Istiophorus_albicans
  geom_sf(data = sao, fill = "gray40", color = "white") +
  scale_color_viridis_c(option = "D") + # option = A-I
  guides(size = "none") +
  theme_light() 


### Bubbleplot interativo
mapview(dfsp2,
        zcol = "Prionace_glauca",
        cex = "Prionace_glauca",
        layer.name = "Abundância do tubarão azul")



## Gridplot
# O último gráfico, embora útil para explorar espacialmente os dados, é pouco prático
# em contextos cujos dados possuem muitas amostragens! 
# Nesses casos, pode-se agregar as amostras em um grid espacial:

### Reprojetando os dados 
dfsp2_r <- st_transform(dfsp2, crs = CRS_lea) 

### Definindo os limites do grid com base nos valores min/max das coordenadas
coords <- st_coordinates(dfsp2_r) # Extraindo as coordenadas do objeto sf
head(coords) # X = longitude, Y = latitude

xmin <- floor(min(coords[, 1]))
xmax <- ceiling(max(coords[, 1]))
ymin <- floor(min(coords[, 2]))
ymax <- ceiling(max(coords[, 2]))

### Definindo resolução do grid (medido na mesma unidade do CRS - ie., metros)
RES <- 200*10^3 # Grid de 200 x 200 km


grid <- st_make_grid(st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = CRS_lea),
                     cellsize = c(RES, RES), # Resolução espacial do grid
                     what = "polygons",
                     square = T) %>% # square=F -> grid hexagonal
  st_sf() %>% 
  mutate(grid_id = 1:nrow(.)) # Conferindo ID para cada célula do grid

class(grid)
head(grid)

### Curiando...
ggplot() +
  geom_sf(data = grid, fill = NA) + 
  geom_sf(data = st_transform(sao, crs = CRS_lea), fill = "gray40", color = "white") +
  guides(size = "none") +
  theme_light() 


### Juntando o grid ao banco de dados
dfgr <- st_join(dfsp2_r, grid)
dim(dfgr)[1] == dim(dfsp2_r)[1] #Sanity check
head(as.data.frame(dfgr))
plot(table(dfgr$grid_id))

dp <- length(unique(dfgr$grid_id))
gp <- length(unique(grid$grid_id))

print(paste("Os pontos amostrais interceptam", dp, "células de um total de", gp, "células"))


### Agregando os dados a nível de grid
dfgr$grid_id <- as.factor(dfgr$grid_id) # Obrigatório!
grid$grid_id <- as.factor(grid$grid_id) # Obrigatório!

aggregated <- dfgr %>% 
  st_drop_geometry() %>% # Remove temporariamente a geometria dos dados - processamento mais rápido!
  group_by(grid_id) %>% 
  summarize(Ntot = sum(Prionace_glauca),
            Navg = median(Prionace_glauca)) %>% 
  full_join(., grid, by = "grid_id") %>%  # Recuperando a geometria dos dados
  st_as_sf()


head(aggregated)


### Visualizando 
ggplot() +
  geom_sf(data = aggregated, aes(fill = Navg), color = "gray90") +
  #geom_sf(data = aggregated, aes(fill = log1p(Navg)), color = "gray90") + #Plotando na escala log
  geom_sf(data = st_transform(sao, crs = CRS_lea), fill = "gray40", color = "white") +
  scale_fill_viridis_c(option = "D", na.value="white") +
  theme_minimal()


### Visualização dinâmica
mapview(st_transform(aggregated, crs = CRS_wgs),
        zcol = "Navg")



#============= Avlaiando correlação espacial =============#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1 - Índice de Moran (Moran's I) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1 - Índice global
#~~~~~~~~~~~~~~~~~~~~~~
knn <- knearneigh(coords, k = 8) # Define o no. de pontos (vizinhos) mais próximos
nb <- knn2nb(knn) # Converte objeto knn para uma lista da classe nb
listw <- nb2listw(nb, style = "W") # Cria a matriz de pesos espaciais (W)
moran.test(dfsp2_r$Prionace_glauca, listw) # Teste de Moran's I
moran.plot(dfsp2_r$Prionace_glauca, listw)

# Interpretação dos 4 quadrantes:
# - 1) Superior direito (High-high): capturas de P. glauca altas cercadas por vizinhos também com altas capturas (hotspots)
# - 2) Inferior esquerdo (Low-low): capturas baixas cercadas por vizinhos com baixas capturas (coldspots)
# - 3) Superior esquerdo (Low-High): capturas baixas cercadas por vizinhos com altas capturas (outlier espacial)
# - 4) Inferior direito (High-Low): capturas altas cercadas por vizinhos com baixas capturas (outlier espacial)
# OBS.: a linha de regressão representa o índice de Moran global; como é positiva, indica presença de autocorrelação espacial positiva global nos dados avaliados


# 1.1 - Índice local
#~~~~~~~~~~~~~~~~~~~~~~
# LISA: local indicatiors of spatial association
# Decompõe o índice global em índices locais (i.e., para cada ponto amostral).
# Permite identificar onde ocorrem clusters significativos de alta captura (hotspots), de baixa captura (coldspots) ou discrepâncias espaciais (outliers espaciais)

local_moran <- localmoran(dfsp2_r$Prionace_glauca, listw) # Caclulando os índices locais
dim(local_moran)
head(local_moran)


## Associando os índices ao banco de dados (para posterior visualização)
lisa_df <- dfsp2_r %>%
  mutate(Ii = local_moran[, "Ii"], # Estatística local
         p_value = local_moran[, "Pr(z != E(Ii))"], # p-valor
         z_var = scale(Prionace_glauca)[, 1], # Escores-z padronizados para classificar os quadrantes
         z_lag = scale(lag.listw(listw, Prionace_glauca))[, 1]) %>%
  mutate(Mquadrants = case_when(p_value >= 0.05 ~ "Not significant", # Classificando os quadrantes
                                z_var > 0 & z_lag > 0 ~ "High-High (Hotspot)",
                                z_var < 0 & z_lag < 0 ~ "Low-Low (Coldspot)",
                                z_var > 0 & z_lag < 0 ~ "High-Low (Outlier)",
                                z_var < 0 & z_lag > 0 ~ "Low-High (Outlier)"),
         Mquadrants = factor(Mquadrants, levels = c("High-High (Hotspot)", "Low-Low (Coldspot)", # Reordenando os níveis
                                                    "High-Low (Outlier)", "Low-High (Outlier)", "Not significant")))

## Visualizando
ggplot(lisa_df) +
  geom_sf(aes(color = Mquadrants), size = 2, alpha = 0.8) +
  geom_sf(data = st_transform(sao, crs = CRS_lea), fill = "gray40", color = "white") +
  theme_minimal() 


#========================================================#


# 3.3) Dinâmica temporal
#~~~~~~~~~~~~~~~~~~~~~~~~~

### Distribuição temporal das amostras (amostra = pescaria)
# Importante para correlação temporal
table(df$Year)
table(df$Month)
table(df$Year, df$Month)


## Visualizando...
df %>% 
  group_by(Year, Month) %>% 
  dplyr::summarize(Frequency = n()) %>% 
  ggplot( aes(x = Month, y = Year, fill = Frequency, label = Frequency)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(col = 'gray30') +
  scale_fill_gradientn(colours = c("gray95", "#f7b267", "#f27059")) +
  theme_light(base_size = 14) +
  coord_cartesian(expand = c(0,0)) +
  labs(x="Mês",y = "Ano",fill='No. pescarias')  +
  my_theme





### Dinâmica temporal do tubarão-azul

#### Dinâmica anual
p3 <- ggplot(df, aes(x = Year, y = Prionace_glauca)) +
  geom_boxplot(fill = "#e07a5f", alpha =.7) +
  theme_bw(base_size = 14) +
  labs(x = "Ano",y = "Abundância total (N)")  


#### Dinâmica mensal (há sazonalidade?)
p4 <- ggplot(df, aes(x = Month, y = Prionace_glauca)) +
  geom_boxplot(fill = "#e07a5f", alpha =.7) +
  theme_bw(base_size = 14) +
  labs(x = "Mês",y = "Abundância total (N)")  


### Visualizando...
p3 + p4



### Dinâmica temporal da série histórica
p5 <- df %>% 
  group_by(MonthYear) %>% 
  dplyr::summarize(Ntot = sum(Prionace_glauca)) %>% 
  ggplot( aes(x = MonthYear, y = Ntot)) +
  geom_col(fill = "#e07a5f", alpha =.7) +
  coord_cartesian(expand = c(0,0)) +
  labs(x="Mês-Ano",y = "Abundância total (N)")  +
  theme_bw() +
  my_theme


p6  <- df %>% 
  group_by(MonthYear) %>% 
  dplyr::summarize(N_mean = mean(Prionace_glauca),
                   N_sd = sd(Prionace_glauca),
                   Nsamples = n()) %>% 
  mutate(N_se = N_sd / sqrt(Nsamples - 1), #erro padrão da média
         CI_lower = N_mean + qt((1-0.95)/2, Nsamples - 1) * N_se, #IC da média
         CI_upper = N_mean - qt((1-0.95)/2, Nsamples - 1) * N_se) %>% 
  ggplot( aes(x = MonthYear, y = N_mean, group = 1)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "#e07a5f", color = NA) +
  geom_line(col = "#e07a5f") +
  geom_point(col = "#e07a5f") +
  coord_cartesian(expand = c(0,0)) +
  labs(x="Mês-Ano",y = "Abundância média") +
  theme_bw() +
  my_theme


### Visualizando....
p5 / p6




# 3.4) Dinâmica espaço-temporal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Bubbleplot
ggplot() +
  geom_sf(data = dfsp2, aes(size = Prionace_glauca, color = Prionace_glauca), alpha = 0.5) + #Thunnus_albacares, Istiophorus_albicans
  geom_sf(data = sao, fill = "gray40", color = "white") +
  #facet_wrap(Year ~ .) +
  #facet_wrap(Month ~ .) +
  facet_wrap(Year ~ Month) +
  scale_color_viridis_c(option = "D") +
  guides(size = "none") +
  theme_bw() +
  my_theme


gganimate::transition_time(Date) +
  
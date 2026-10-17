#########################################################################################
#                                                                                       #
#                                     Prática da aula 08                                #
#                          - Modelos espaço-temporais via R-INLA -                      #
#                                                                                       #
#########################################################################################


# CONTEXTO
# Esta prática estenderá os modelos espaciais desenvolvidos na Prática 07b para incorporar explicitamente diferentes formas 
# de dependência temporal ao campo espacial. Utilizaremos diferentes estruturas espaço-temporais para investigar como
# a distribuição da abundância do tubarão-azul (Prionace glauca) varia mensalmente no Oceano Atlântico Sul.
# As estruturas espaço-temporais aqui exploradas são baseadas nas tipologias apresentadas por Paradinas et al. (2017):
# - https://www.sciencedirect.com/science/article/abs/pii/S2211675316300872

# DADOS
# Usaremos o mesmo banco de dados que foi utilizado ao longo das práticas das Aulas 6 e 7.

# OBJETIVOS:
# - Compreender e avaliar como diferentes estruturas de dependência espaço-temporal podem descrever a evolução da distribuição
#   do processo ecológico sob análise
# - Comparar modelos com diferentes estruturas espaço-temporais e avaliar o ajuste do melhor modelo
# - Gerar e interpretar mapas preditivos da abundância ao longo do espaço e do tempo

# ABORDAGEM ANALÍTICA
# Modelos bayesianos ajustados via R-INLA, com 5 estruturas espaço-temporais U(s,t):
#     - Modelo 0: Distribuição espacial
#         - Estrutura espaço-temporal: U(s,t) = W(s)
#         - Um único campo espacial representa todos os meses. Não há componente
#           temporal explícito. Serve como referência para avaliar a inclusão da dimensão temporal.
#     - Modelo 1: Distribuição persistente com mudanças aleatórias
#         - Estrutura espaço-temporal: U(s,t) = W(s) + V(t)
#         - O campo espacial permanece comum aos meses, enquanto um efeito temporal independente altera o nível do preditor em cada mês.
#           Essa alteração é compartilhada por todas as localidades.
#     - Modelo 2: Distribuição persistente com tendência temporal
#         - Estrutura espaço-temporal: U(s,t) = W(s) + g(t)
#         - O campo espacial permanece comum aos meses, mas o efeito temporal evolui de maneira suave
#           Nesta prática, g(t) será representado por um passeio aleatório de segunda ordem (RW2), que penaliza mudanças
#           na inclinação da trajetória temporal.
#     - Modelo 3: Distribuição oportunista
#         - Estrutura espaço-temporal: U(s,t) = W_t(s)
#         - Cada mês possui seu próprio campo espacial. Os campos compartilham os hiperparâmetros espaciais, mas são independentes entre meses,
#           condicionalmente a esses hiperparâmetros.
#     - Modelo 4: Distribuição progressiva:
#         - Estrutura espaço-temporal: U(s,t) segue uma estrutura espacial Matérn e temporal AR1.
#         - Cada mês possui seu próprio campo espacial, correlacionado com os demais meses. 
# - OBS.: Nos modelos 1 e 2, o padrão espacial é fixo e só a intensidade muda no tempo; nos Modelos 3 e 4, o próprio padrão espacial




#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(INLA) # Ajustar modelos espaciais (bayesianos)
library(fmesher) # Criação do mesh (novo!)
library(ggplot2) # Visualização dos dados
library(sf) # Processamento de dados vetorizados 
library(terra) # Processamento de rasters
library(stringr) # Manipulação de strings
library(mapview) # Visualização de dados espacializados
library(viridis) # Paleta de cores
library(tidyterra) # Visualização de rasters
library(dplyr) # Manipulação de dados
library(tidyr) # Manipulação de dados
library(akima) # Interpolação
library(fishualize) # Paleta de cores
library(gganimate) # Gerar gif


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando funções auxiliares
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("Praticas/auxiliary_functions.R")



#~~~~~~~~~~~~~~~~~
# Input padrão
#~~~~~~~~~~~~~~~~~
CRS_wgs <- 4326 # Coordenada padrão (WGS84 - long/lat)
CRS_lea <- "+proj=laea +lat_0=-25 +lon_0=-20 +datum=WGS84 +units=km +no_defs" # Projeção LEA centrada no AS & expressa em Km



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo tema geral do ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_theme <- theme_bw(base_size = 14) + 
            theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
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



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando os dados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1) Dados pesqueiros
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/fisheries_covars.rds")
str(df)



## Convertendo variáveis para fator
df[, c("Year", "Month", "Id", "Vessel", "Flag")] <- lapply(df[, c("Year", "Month", "Id", "Vessel", "Flag")], factor)




# 1.2) Dados ambientais
#~~~~~~~~~~~~~~~~~~~~~~~~

# 1.2.1) Global Ocean Physics Reanalysis (raster)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - thetao_depth: SST (sea surface temperature)
# - zos: SSH (sea surface height)
# - mlotst: MLT (mixed layer thickness)
# - deptho: BATHY (bathymetry)

## Variáveis físicas
vfisicas <- rast("Dados/GIS/Environmental/Global_Ocean_Physics_Reanalysis_2004_2005_monthly_surface_layers.nc") 

## Batimetria
batimetria <- rast("Dados/GIS/Environmental/bathymetry.nc")


# 1.2.2) Global Ocean Biogeochemistry Hindcast (raster)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vbio <- rast("Dados/GIS/Environmental/Global_Ocean_Biogeochemistry_Hindcast_2004_2005_monthly.nc") 



# 1.3) Dados vetoriais (shapefiles)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Representa a extensão da área de estudo (Atlântico Sudoeste)
sw_ocean <- read_sf("Dados/GIS/ICCAT/iccat_atsw.shp")

# Shapefile da América do Sul
world <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf") # Scale options: 110 (low), 50 (medium), 10 (high)
extent_bbox <- st_bbox(c(xmin = -70, xmax = -12,  ymin = -50, ymax = 5), crs = CRS_wgs) # Definindo área de recorte
sao <- st_crop(world, extent_bbox) # Recortando para SAO 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Padronizando os dados ambientais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.1) Padronizando a extensão espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Variáveis físicas
vfisicas2 <- crop(vfisicas, sw_ocean)

## Variáveis biológicas
vbio2 <- crop(vbio, sw_ocean)

## Batimetira
batimetria2 <- crop(batimetria, sw_ocean) #mantém a extensão

## EXTRA: Em alguns casos, é preciso recortar o raster de forma a manter os mesmos pixels dentro do polígono.
## Nestes casos, usa-se a função 'mask()' no objeto criado pela função 'crop()':

#batimetria3 <- mask(batimetria2, sw_ocean) 


# 2.2) Padronizando o sistema de referência de coordenadas (CRS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Sanity check - ok!
crs(batimetria2) == crs(vfisicas2)
crs(batimetria2) == crs(vbio2)
crs(vbio2) == crs(vfisicas2)

## Reprojetando uma variável (exemplo)
# crs_ref <-  crs(batimetria2, proj = TRUE)
# vbio2 <- terra::project(vbio, crs_ref)


# 2.3) Padronizando a resolução espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Padronizando batimetria
batimetria2r <- resample(batimetria2, vbio2, method = "bilinear")


## Padronizando variáveis físicas
vfisicas2r <- resample(vfisicas2, vbio2, method = "bilinear")


### Sanity check - ok!
res(vbio2) == res(batimetria2r)
res(vbio2) == res(vfisicas2r)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Extraindo os dados ambientais para os pontos amostrais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Filtrando os dados (pesqueiros) para a extensão da área de estudo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 3.1.1) Tornando os dados como objeto espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dfs <- st_as_sf(df, coords = c('Long','Lat'), crs = crs(sw_ocean))


# 3.1.2) Filtrando pescarias (pontos amostrais) fora da área de estudo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dfs_f <- dfs %>%  st_filter(sw_ocean, .predicate = st_intersects)


# 3.2) Extraindo os dados ambientais para cada pescaria
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
year_mapping <- c("1" = 2004, "2" = 2005) 

## Acessando o nome das variáveis
unique(word(names(vfisicas2r), 1, sep = "_"))
unique(word(names(vbio2), 1, sep = "_"))
unique(word(names(batimetria2r), 1, sep = "_"))



### Extraindo dados para thetao (sst - temperatura superficial do mar); variável dinâmica
dfs_f <- extract_env_dynamic(points_sf = dfs_f, # dados pesqueiros espacializados
                             env_raster = vfisicas2r, # camada ambiental (raster)
                             var_name = "thetao",# nome da variável dentro da camada ambiental
                             year_map = year_mapping)


### Extraindo dados para zos (ssh - altura da superfície do mar); variável dinâmica
dfs_f <- extract_env_dynamic(points_sf = dfs_f, # dados pesqueiros espacializados
                             env_raster = vfisicas2r, # camada ambiental (raster)
                             var_name = "zos",# nome da variável dentro da camada ambiental
                             year_map = year_mapping)


### Extraindo dados para mlotst (mlt - espessura da camada de mistura); variável dinâmica
dfs_f <- extract_env_dynamic(points_sf = dfs_f, # dados pesqueiros espacializados
                             env_raster = vfisicas2r, # camada ambiental (raster)
                             var_name = "mlotst",# nome da variável dentro da camada ambiental
                             year_map = year_mapping)



### Extraindo dados para nppv (produtividade primária); variável dinâmica
dfs_f <- extract_env_dynamic(points_sf = dfs_f, # dados pesqueiros espacializados
                             env_raster = vbio2, # camada ambiental (raster)
                             var_name = "nppv",# nome da variável dentro da camada ambiental
                             year_map = year_mapping)



### Extraindo dados para deptho (bathy - batimetria); variável estática
dfs_f$bathy  <- terra::extract(batimetria2r, dfs_f)[, 2]


### To see the progress...
head(as.data.frame(dfs_f))
colnames(dfs_f)[22:26] <- c("temperatura", "altura_mar", "camada_mistura", "prod_primaria", "batimetria") # Renomeando as variáveis



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Análise exploratória dos dados (breve)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 4.1) Dinâmica temporal 
#~~~~~~~~~~~~~~~~~~~~~~~~~
dfs_f %>% 
  group_by(Month) %>% 
  dplyr::summarize(N_mean = mean(Prionace_glauca),
                   N_sd = sd(Prionace_glauca),
                   Nsamples = n()) %>% 
  mutate(N_se = N_sd / sqrt(Nsamples), #erro padrão da média
         CI_lower = N_mean + qt((1-0.95)/2, Nsamples - 1) * N_se, #IC da média
         CI_upper = N_mean - qt((1-0.95)/2, Nsamples - 1) * N_se) %>% 
  ggplot( aes(x = Month, y = N_mean, group = 1)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "#e07a5f", color = NA) +
  geom_line(col = "#e07a5f") +
  geom_point(col = "#e07a5f") +
  coord_cartesian(expand = F) +
  labs(x="Mês-Ano",y = "Abundância média") +
  theme_bw() +
  my_theme



# 4.2) Dinâmica espaço-temporal 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Visualizando...
dfs_f %>% 
  filter(Year == '2') %>% 
  ggplot() +
    geom_sf(aes(size = Prionace_glauca, color = Prionace_glauca), alpha = 0.5) + #Thunnus_albacares, Istiophorus_albicans
    #geom_sf(aes(size = Thunnus_alalunga, color = Thunnus_alalunga), alpha = 0.5) + #Thunnus_albacares, Istiophorus_albicans
    geom_sf(data = sao, fill = "gray40", color = "white") +
    scale_color_viridis_c(option = "B") + # option = A-I
    guides(size = "none") +
    facet_wrap(Month ~ .) +
    my_theme



## Filtrando os dados para apenas um ano (acelerar computação posteriormente)
### Teste rodar os modelos com todo o banco de dados!
dfs_f <- filter(dfs_f, Year == '2')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Construindo o mesh (malha SPDE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 5.1) Transformando os dados para projeção LEA (expresso em km)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dfs_lea <- st_transform(dfs_f, CRS_lea)


# 5.2) Criando matriz de coordenadas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coords <- as.data.frame(st_coordinates(dfs_lea))


# 5.3) Definindo o mesh
#~~~~~~~~~~~~~~~~~~~~~~
max.edge <- diff(range(coords[,1]))/(3*5)
bound.outer <- diff(range(coords[,1]))/5

sw_oceant <- st_transform(sw_ocean, crs = CRS_lea) # Reprojetando o shp da área de estudo

mesh <- fm_mesh_2d_inla(boundary = sw_oceant,
                        loc = coords, 
                        max.edge = c(1,2)*max.edge, 
                        cutoff = max.edge/4, 
                        offset = c(max.edge, bound.outer*1.5))


plot(mesh)
points(coords, col = 'red', pch = 19, cex = 0.5 )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6) Definindo as matrizes de projeção, SPDE & stacks de estimação e predição
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# No contexto temporal, precisamos definir a matriz de projeção de modo a alinhar com a estrutura do efeito temporal.


# 6.1) Definindo o índice temporal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Os dados pesqueiros têm informação referente a um ano (ano = 2)
# com registros de pescarias ao longo dos 12 meses em cada ano (1-12). 

# Como definimos 'mês' como resolução temporal, precisamos criar um índice temporal
# inteiro e consecutivo que percorre toda a janela temporal (i.e., mês-ano: 1, 2, 3, ...., 12)

# ATENÇÃO! o índice temporal deve representar uma sequência regular de tempo (aqui, meses),
# construída a partir de um calendário completo para o período de estudo, incluindo os meses em que não houve amostragem.
# Por exemplo, se houver dados em janeiro e março, mas não em fevereiro, os índices temporais das observações devem ser 1 e 3, preservando o índice 2 para fevereiro (mesmo que não presente nos dados).
# Numerar apenas os meses observados como 1 e 2 faria o modelo tratar janeiro e março como meses consecutivos, distorcendo a distância temporal entre eles.
# Desta forma, índices temporais (meses) sem observações devem permanecer representados na estrutura temporal latente.
# Com o calendário completo, meses sem dados continuam existindo no campo latente e são preenchidos (interpolados) pela estrutura temporal do modelo.


years <- as.numeric(unique(dfs_lea$Year)) # Anos (índices numéricos)
dft <- expand.grid(Month = 1:12, Year = years) # expand.grid() cria todas as combinações entre os fatores (mês, ano)
dft$time <- seq_len(nrow(dft)) # Índices da janela temporal (1-24)
nt <- nrow(dft) # nº total de passos de tempo (inclui meses sem dados)

dfs_lea$time <- dft$time[match(paste(dfs_lea$Year, as.numeric(as.character(dfs_lea$Month))),
                               paste(dft$Year, dft$Month))]

## Sanity check - alguma pescaria fora do calendário?
stopifnot(!anyNA(dfs_lea$time)) 



# 6.2) Matrizes de projeção 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Modelos 0, 1 e 2: um campo espacial para todos as unidades temporais 
# - Modelos 3 e 4: um campo espacial para cada unidade temporal 


# 6.2.1) Matriz de estimação 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Campo espacial comum
Aest_com <- inla.spde.make.A(mesh = mesh, 
                             loc = as.matrix(coords))


## Campo espacial replicado por unidade de tempo (réplicas independentes)
Aest_rep <- inla.spde.make.A(mesh = mesh,
                             loc = as.matrix(coords),
                             repl = dfs_lea$time,
                             n.repl = nt)


## Campo espacial replicado por unidade de tempo (réplicas correlacionadas)
Aest_corr <- inla.spde.make.A(mesh = mesh,
                              loc = as.matrix(coords),
                              group = dfs_lea$time,
                              n.group = nt)

dim(Aest_com) # Nobs vs. nmesh 
dim(Aest_rep) # Nobs vs. nmesh * nt 
dim(Aest_corr) # Nobs vs. nmesh * nt 



# 6.2.2) Matriz de predição
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Predizemos nos vértices do mesh, replicados para cada unidade de tempo

# Extraindo as coordenadas da malha
mcoords <- data.frame(X = mesh$loc[,1], Y = mesh$loc[,2]) 
mesh.points <- st_as_sf(mcoords, coords = c("X", "Y"), crs = CRS_lea) # Transformando as coordenadas para objeto espacial (sf)

# Sanity check
plot(mesh)
plot(st_geometry(mesh.points),col='red', pch=19, cex = 0.3, add = T)


## Replica do mesh / tempo
nmesh <- mesh$n
loc_pred  <- as.matrix(mcoords)[rep(1:nmesh, times = nt), ]
time_pred <- rep(1:nt, each = nmesh)



## Campo espacial comum
Apred_com <- inla.spde.make.A(mesh, 
                              loc = loc_pred) 


## Campo espacial replicado por unidade de tempo (réplicas independentes)
Apred_rep  <- inla.spde.make.A(mesh, 
                               loc = loc_pred, 
                               repl = time_pred, 
                               n.repl = nt)


## Campo espacial replicado por unidade de tempo (réplicas correlacionadas)
Apred_corr  <- inla.spde.make.A(mesh, 
                                loc = loc_pred, 
                                group = time_pred, 
                                n.group = nt)

dim(Apred_com) # nmesh * nt vs nmesh
dim(Apred_rep) # nmesh * nt vs nmesh * nt
dim(Apred_corr) # nmesh * nt vs nmesh * nt




# 6.3) Definindo o SPDE
#~~~~~~~~~~~~~~~~~~~~~~~

## SPDE 'clássico'
spde <- inla.spde2.matern(mesh = mesh, alpha = 2) # alpha = parâmetro de suavidade da função de covariância Matérn (usando o valor default)


## SPDE com pc.prior (evita sobreajuste - overfitting)
spde_pc <- inla.spde2.pcmatern(mesh = mesh,
                               alpha = 2,
                               prior.range = c(200, 0.05), # P(range < 200 km) = 5% (valores achados considerando um modelo com covariáveis)
                               prior.sigma = c(1, 0.05))   # P(sigma > 1) = 0.05




# 6.4) Dados para estimação e predição
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Para simplificar, todas as covariáveis ambientais usadas na PREDIÇÃO serão consideradas como estáticas;
# i.e., usaremos a média de todos os meses em cada nó do mesh, como na prática da aula 07.
# Contudo, para o processo de ESTIMAÇÃO, cada pescaria (observação) continua com a covariável do seu respectivo mês-ano

# ATENÇÃO: mesmo com covariáveis "estáticas", a predição é feita para cada passo de tempo (nmesh * nt linhas),
# pois o termo espaço-temporal U(s,t) varia com t. A variação temporal dos mapas, portanto, vem apenas de U(s,t) e não do ambiente.


## 6.4.1) Definindo a média e DP das covariáveis nos pontos amostrais 
# Informação usada para padronizar as unidades entre a estimação e predição
m_temp <- mean(dfs_lea$temperatura, na.rm = T)    
sd_temp <- sd(dfs_lea$temperatura, na.rm = T)

m_sheight <- mean(dfs_lea$altura_mar, na.rm = T)
sd_sheight <- sd(dfs_lea$altura_mar, na.rm = T)

m_mlt <- mean(dfs_lea$camada_mistura, na.rm = T)
sd_mlt <- sd(dfs_lea$camada_mistura, na.rm = T)

m_npp <- mean(dfs_lea$prod_primaria, na.rm = T)
sd_npp <- sd(dfs_lea$prod_primaria, na.rm = T)

m_depth <- mean(dfs_lea$batimetria, na.rm = T)
sd_depth <- sd(dfs_lea$batimetria, na.rm = T)



## 6.4.2) Extraindo dados ambientais para cada ponto do mesh (grid)

### Transformando os pontos do mesh para WGS84
mesh.points_wgs <- st_transform(mesh.points, crs = CRS_wgs)


### Funçãozinha para facilitar a vida...
get_mean_cov <- function(raster, variable) {
  idx <- grep(paste0("^", variable), names(raster))
  r_mean <- mean(raster[[idx]], na.rm = TRUE)
  terra::extract(r_mean, mesh.points_wgs)[, 2]
}


### Extraindo os dados
# Cada covariável (nmesh) é replicada nt vezes, na mesma ordem de 'time_pred'

temp_p <- rep(get_mean_cov(vfisicas2r, "thetao"), times = nt)
sheight_p <- rep(get_mean_cov(vfisicas2r, "zos"), times = nt)
mlt_p <- rep(get_mean_cov(vfisicas2r, "mlotst"), times = nt)
npp_p <- rep(get_mean_cov(vbio2, "nppv"), times = nt)
depth_p <- rep(get_mean_cov(batimetria2r, "deptho"), times = nt)



## 6.4.3) Data-frames de estimação e predição

### Estimação
df_est <- data.frame(b0 = 1,
                     time = dfs_lea$time,
                     vessel = dfs_lea$Vessel,
                     nhooks = dfs_lea$Nhooks,
                     flag = dfs_lea$Flag,
                     temp = (dfs_lea$temperatura - m_temp) / sd_temp,
                     sheight = (dfs_lea$altura_mar - m_sheight) / sd_sheight,
                     mlt = (dfs_lea$camada_mistura - m_mlt) / sd_mlt,
                     npp = (dfs_lea$prod_primaria  - m_npp) / sd_npp,
                     depth = (dfs_lea$batimetria - m_depth) / sd_depth)


### Predição
df_pred <- data.frame(b0 = 1,
                      time = time_pred,
                      vessel = NA, # efeito iid -> NA na predição
                      nhooks = 1, # predição em Abundância por anzol (log(1) = 0)
                      flag = NA, # efeito iid -> NA na predição
                      temp = (temp_p - m_temp) / sd_temp,
                      sheight = (sheight_p - m_sheight) / sd_sheight,
                      mlt = (mlt_p - m_mlt) / sd_mlt,
                      npp = (npp_p - m_npp) / sd_npp,
                      depth = (depth_p - m_depth) / sd_depth)

dim(df_pred)



# 6.5) Stacks de estimação e predição
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Funçãozinha para facilitar a vida...
### Cria o stack de acordo com a estrutura espaço-temporal

build_stack <- function(type = c("common", "repl", "corr")) {
  
  type <- match.arg(type)
  
  ## Construindo os índices
  indexs <- switch(type,
                   common = inla.spde.make.index("spatial", spde$n.spde),
                   repl   = inla.spde.make.index("spatial", spde$n.spde, n.repl  = nt),
                   corr  = inla.spde.make.index("spatial", spde$n.spde, n.group = nt))
  
  ## Matrizes de estimação e predição
  Aest <- switch(type, 
                 common = Aest_com,  
                 repl = Aest_rep,  
                 corr = Aest_corr)
  
  Apred <- switch(type, 
                  common = Apred_com, 
                  repl = Apred_rep, 
                  corr = Apred_corr)
  
  
  ## Stacks de estimação e predição
  stk.est <- inla.stack(data = list(y = dfs_lea$Prionace_glauca),
                        A = list(Aest, 1),
                        effects = list(spatial = indexs, df_est),
                        tag = "est")
  
  stk.pred <- inla.stack(data = list(y = NA),
                         A = list(Apred, 1),
                         effects = list(spatial = indexs, df_pred),
                         tag = "pred")
  
  inla.stack(stk.est, stk.pred)
}


## Montando os stacks
stk_s  <- build_stack("common") # Campo espacial comum (modelos 0, 1, 2)
stk_sr <- build_stack("repl")   # Campo espacial replicado (modelo 3)
stk_sc <- build_stack("corr")  # Campo espacial correlacionado (modelo 4)




#~~~~~~~~~~~~~~~~~~~~~~~~
# 7) Rodando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~
inla.setOption(num.threads = 8) # Para acelerar os cálculos, em caso de muitos dados

# Rode os modelos abaixo sem o efeito das covariáveis e repare no impacto das covariáveis 
# na predição (especialmente quando o foco for a média)



# 7.1) Modelo espacial
#~~~~~~~~~~~~~~~~~~~~~~
# U(s,t) = W(s)
# Ignora o tempo: um único campo espacial, estimado com todos os dados juntos

form_0 <- y ~ 0 + b0 + depth + mlt + sheight + temp + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc)
#form_0 <- y ~ 0 + b0 + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc)


mod0 <- inla(form_0,
             family = "nbinomial",
             data = inla.stack.data(stk_s),
             control.predictor = list(compute = T,
                                      link = 1,
                                      A = inla.stack.A(stk_s)),
             control.inla = list(strategy = "simplified.laplace"),
             control.compute = list(return.marginals.predictor = F,
                                    dic = T, waic = T, cpo = T))




# 7.2) Modelo espaço-temporal: distribuição persistente com mudanças aleatórias em cada tempo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# U(s,t) = W(s) + V_t ,   V_t ~ N(0, sigma_V^2) 
# O padrão espacial é o mesmo em todos as unidades de tempos e só o nível médio de abundância varia, de forma aleatória, de um mês para o outro

form_1 <- y ~ 0 + b0 + depth + mlt + sheight + temp + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc) + f(time, model = "iid")
#form_1 <- y ~ 0 + b0 + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc) + f(time, model = "iid")


mod1 <- inla(form_1,
             family = "nbinomial",
             data = inla.stack.data(stk_s),
             control.predictor = list(compute = T,
                                      link = 1,
                                      A = inla.stack.A(stk_s)),
             control.inla = list(strategy = "simplified.laplace"),
             control.compute = list(return.marginals.predictor = F,
                                    dic = T, waic = T, cpo = T))



# 7.3) Modelo espaço-temporal: distribuição persistente com tendência temporal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# U_st = W(s) + g(t) 
# Ainda um único campo espacial, com variação de intensidade descrita por uma função linear/não-linear

form_2 <- y ~ 0 + b0 + depth + mlt + sheight + temp + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc) + f(time, model = "rw2", scale.model = T)
#form_2 <- y ~ 0 + b0 + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc) + f(time, model = "rw2", scale.model = T)


mod2 <- inla(form_2,
             family = "nbinomial",
             data = inla.stack.data(stk_s),
             control.predictor = list(compute = T,
                                      link = 1,
                                      A = inla.stack.A(stk_s)),
             control.inla = list(strategy = "simplified.laplace"),
             control.compute = list(return.marginals.predictor = F,
                                    dic = T, waic = T, cpo = T))



# 7.4) Modelo espaço-temporal: distribuição oportunística
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# U(s,t) = W_t(s) ,   W_t ~ N(0, Q^-1(\kappa, \tau)),  t iid
# Um campo espacial diferente para cada mês,todos com os mesmos hiperparâmetros espaciais (alcance e DP) e sem relação entre meses vizinhos.

form_3 <- y ~ 0 + b0 + depth + mlt + sheight + temp + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc, replicate = spatial.repl)
#form_3 <- y ~ 0 + b0 + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc, replicate = spatial.repl)

mod3 <- inla(form_3,
             family = "nbinomial",
             data = inla.stack.data(stk_sr),
             control.predictor = list(compute = T,
                                      link = 1,
                                      A = inla.stack.A(stk_sr)),
             control.inla = list(strategy = "simplified.laplace"),
             control.compute = list(return.marginals.predictor = F,
                                    dic = T, waic = T, cpo = T))



# 7.5) Modelo espaço-temporal: distribuição progressiva
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Também modela um campo espacial por mês, mas que estão correlacionados no tempo (AR1).
# Assim, o mapa de hoje se parece com o de ontem. 
# É o caso mais geral: rho -> 0 recupera a oportunista; rho -> 1, um campo persistente.


form_4 <- y ~ 0 + b0 + depth + mlt + sheight + temp + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc, group = spatial.group, control.group = list(model = "ar1"))
#form_4 <- y ~ 0 + b0 + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc, group = spatial.group, control.group = list(model = "ar1"))


## +/- 10 min para rodar (~1h30 com o banco de dados completo)
mod4 <- inla(form_4, 
             family = "nbinomial",
             data = inla.stack.data(stk_sc),
             control.predictor = list(compute = T,
                                      link = 1,
                                      A = inla.stack.A(stk_sc)),
             control.inla = list(strategy = "simplified.laplace"),
             control.compute = list(return.marginals.predictor = F,
                                    dic = T, waic = T, cpo = T))




### pc.prior no efeito temporal
tprior <- list(theta = list(prior = "pccor1", param = c(0, 0.9))) # P(\rho > 0) = 90%

form_4b <- y ~ 0 + b0 + depth + mlt + sheight + temp + offset(log(nhooks)) + f(vessel, model = "iid") + f(spatial, model = spde_pc, group = spatial.group, control.group = list(model = "ar1", hyper = tprior))
mod4b <- inla(form_4b, 
              family = "nbinomial",
              data = inla.stack.data(stk_sc),
              control.predictor = list(compute = T,
                                       link = 1,
                                       A = inla.stack.A(stk_sc)),
              control.inla = list(strategy = "simplified.laplace"),
              control.compute = list(return.marginals.predictor = F,
                                     dic = T, waic = T, cpo = T))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 8) Extraindo & avaliando os resultados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 8.1) Concatenando modelos e stacks em uma lista
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Modelos
#mod_list <- readRDS("~/Downloads/sp-temp_models.rds")

mod_list <- list(mod0, mod1, mod2, mod3, mod4, mod4b)
names(mod_list) <- c("mod0_Espacial", 
                     "mod1_Persist_iid", 
                     "mod2_Persist_tendencia",
                     "mod3_Oportunista",
                     "mod4_Progressiva_AR1",
                     "mod4_Progressiva_AR1-pc") # Nomeando a lista

#saveRDS(mod_list, "~/Downloads/sp-temp_models.rds")



## Stacks
stk_list <- list(stk_s, # mod0
                 stk_s, # mod1
                 stk_s, # mod2
                 stk_sr, # mod3
                 stk_sc, # mod4
                 stk_sc) # mod4b 

names(stk_list) <- c("mod0_Espacial", 
                     "mod1_Persist_iid", 
                     "mod2_Persist_tendencia",
                     "mod3_Oportunista",
                     "mod4_Progressiva_AR1",
                     "mod4_Progressiva_AR1-pc") # Nomeando a lista



# 8.2) Comparando os modelos quantitativamente
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DIC, WAIC e LCPO (quanto menor, melhor!).


mresults <- do.call("rbind", lapply(mod_list, mdiagnostic))
rownames(mresults) <- NULL
mresults$model <- factor(names(mod_list), levels = names(mod_list)) # Id do modelo


## Comparando os modelos
mresults %>% 
  arrange(WAIC) # DIC, LCPO


## Visualizando....
mresults %>%
  pivot_longer(cols = WAIC,
               names_to = "Metrica",
               values_to = "Valor") %>%
  ggplot(aes(x = model, y = Valor)) +
  geom_point(size = 5, col = "#e07a5f") +
  facet_wrap(Metrica ~ ., scales = "free_x") +
  coord_flip() +
  labs(y = NULL, x = NULL) +
  my_theme



# 8.3) Comparando o alcance espacial estimado
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Usa a função spatial_hyper() para extrair os hiperparâmetros (vide auxiliary_functions.R)
hyp_spl <- lapply(names(mod_list), function(n) cbind(model = n, spatial_hyper(mod_list[[n]])))
hyp_sp <- do.call("rbind", hyp_spl)
hyp_sp



# 8.4) Diagnóstico do melhor modelo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Selecionando o melhor modelo
best_name <- as.character(mresults$model[which.min(mresults$WAIC)]) # Seleção baseada no critério do menor WAIC
#best_name <- "mod3_Oportunista" # ...ou escolha manualmente

best_model <- mod_list[[best_name]]
best_stk <- stk_list[[best_name]]


## Extraindo os índices espaciais da matriz de estimação
idx_est <- inla.stack.index(best_stk, tag = "est")$data

dat <- data.frame(observed  = dfs_lea$Prionace_glauca,
                  predicted = best_model$summary.fitted.values[idx_est, "mean"],
                  pit = best_model$cpo$pit[idx_est],
                  time = dfs_lea$time)

dat$res_q <- qnorm(dat$pit) # Resíduos quantílicos                           


## QQ-plot dos resíduos quantílicos
dat %>%
  ggplot(aes(sample = res_q)) +
  stat_qq_line(col = "blue", linetype = "dashed", linewidth = 1) +
  stat_qq(size = 2, alpha = 0.6) +
  my_theme


## Histograma do PIT (esperado: uniforme)
dat %>%
  ggplot(aes(x = pit)) +
  geom_histogram(col = 'white', fill = "#e07a5f") + 
  my_theme


## Valores preditos vs. observados
ggplot(dat, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  my_theme


## Independência temporal dos resíduos
res_t <- tapply(dat$res_q, factor(dat$time, levels = 1:nt), mean, na.rm = TRUE)
acf(res_t, na.action = na.pass)


dat %>%
  #filter(!is.na(res_q)) %>%
  ggplot(aes(x = as.factor(time), y = res_q)) +
  geom_boxplot(fill = "#e07a5f", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Tempo (mês-ano)", y = "Resíduo quantílico") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Independência espacial dos resíduos
dfs_lea$res_q <- dat$res_q

ggplot() +
  geom_sf(data = sw_oceant, fill = "grey95", color = "grey60") +
  geom_sf(data = dfs_lea, aes(col = res_q, size = res_q), alpha = 0.7) +
  scale_color_viridis_c(option = "A") +
  guides(size = "none") +
  #facet_wrap(time ~ .) +
  theme_void()



#~~~~~~~~~~~~~~~~~~~~~~~
# 9) Mapas preditivos 
#~~~~~~~~~~~~~~~~~~~~~~~~


# 9.1) Extraindo as predições do modelo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Índices de predição
idx <- inla.stack.index(best_stk, tag = "pred")$data
length(idx) # deve ser igual a nmesh * nt


## Data.frame com o resultado as predições 
fator <- 1000 # Multiplicamos as médias/medianas preditas por 'fator' para expressar os resultados em nº de indivíduos por 1000 anzóis (mais legível)
preds <- data.frame(index = idx,
                    pred_mean = best_model$summary.fitted.values[idx, "mean"] * fator,
                    pred_median = best_model$summary.fitted.values[idx, "0.5quant"] * fator,
                    pred_ll = best_model$summary.fitted.values[idx, "0.025quant"] * fator,
                    pred_ul = best_model$summary.fitted.values[idx, "0.975quant"] * fator,
                    time = df_pred$time,
                    X = loc_pred[, 1],
                    Y = loc_pred[, 2])

head(preds)
dim(preds)
table(preds$time) # cada tempo deve ter nmesh linhas


## Transformando o dado para o formato longo
preds_l <- preds %>%
           dplyr::select(X, Y, time, pred_mean, pred_median, pred_ll, pred_ul) %>%
           tidyr::pivot_longer(cols = c(pred_mean, pred_median, pred_ll, pred_ul),
                                names_to = "variable",
                                values_to = "value")


## Convertendo para objeto espacial (sf)
pred_sf <- st_as_sf(preds_l, coords = c("X", "Y"), crs = CRS_lea)


## Reprojetando para WGS84 (long/lat)
pred_sf <- st_transform(pred_sf, CRS_wgs)



# 9.2) Interpolando as predições
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# As predições foram feitas nos vértices (irregulares) do mesh. 
# Para gerar os mapas, precisamos interpolar os valores das predições em uma grade regular


## Grade regular de saída (200 x 200 células), cobrindo a extensão dos nós do mesh
xy_all <- st_coordinates(pred_sf)
xo <- seq(min(xy_all[, 1]), max(xy_all[, 1]), length.out = 200) # longitudes da grade
yo <- seq(min(xy_all[, 2]), max(xy_all[, 2]), length.out = 200) # latitudes da grade



### 9.2.1) Interpolando para um único mês
tmp <- filter(pred_sf, variable == "pred_median", time == 1) # variable == "pred_mean"; time == 1,2,3,...,12
xy  <- st_coordinates(tmp) 

ir <- interp(x = xy[, 1],
             y = xy[, 2],
             z = tmp$value,
             xo = xo,
             yo = yo)


r1 <- rast(ir) # convertendo para raster
crs(r1) <- "EPSG:4326"
r1 <- crop(r1, sw_ocean)
r1 <- mask(r1, sw_ocean)

plot(r1)




### 9.2.2) Interpolando para toda a série temporal

vars  <- c("pred_mean", "pred_median", "pred_ll", "pred_ul") # variáveis a serem interpoladas
tlab <- paste0(year_mapping[as.character(dft$Year)], "-", sprintf("%02d", dft$Month)) # labels de cada unidade de tempo


rlist <- list() # Lista para armazenar os rasters de cada variável 

for (i in vars) {
  
  rtime <- list() # Lista para aramezar os rasters de cada unidade de tempo
  
  for (j in 1:nt) {
    
    ## Selecionando a variável e o tempo
    tmp <- subset(pred_sf, variable == i & time == j)
    xy  <- st_coordinates(tmp)
    
    ## Interpolando
    ir <- akima::interp(x = xy[, 1], y = xy[, 2], z = tmp$value, xo = xo, yo = yo)
    
    ## Convertendo para raster
    r <- rast(ir)
    crs(r) <- "EPSG:4326"
    
    ## Recortando e mascarando
    r <- crop(r, sw_ocean)
    r <- mask(r, sw_ocean)
    
    ## Nomeando a camada com o mês-ano
    names(r) <- tlab[j]
    
    rtime[[j]] <- r
  }
  
  rlist[[i]] <- rast(rtime) # empilha os meses dentro de cada variável
  
  print(paste("finished interpolation of:", i))
}


## Separando os rasters (cada um com nt camadas)
r_mean <- rlist[["pred_mean"]]
r_median <- rlist[["pred_median"]]
r_lo   <- rlist[["pred_ll"]]
r_hi   <- rlist[["pred_ul"]]

names(r_median)



### 9.2.3) Visualizando as abundâncias preditas ao longo do espaço-tempo
ext_raster <- ext(r_median)

## Mediana da abundância predita
ggplot() +
  geom_spatraster(data = r_median) + # r_mean
  facet_wrap(~lyr, ncol = 4) +
  geom_sf(data = sao, fill = 'gray20', color = "white", linewidth = 0.3) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  my_theme +
  coord_sf(xlim = c(ext_raster[1], ext_raster[2]), 
           ylim = c(ext_raster[3], ext_raster[4]), 
           expand = FALSE)


## Limite inferior do IC 95%
ggplot() +
  geom_spatraster(data = r_lo) +
  facet_wrap(~lyr, ncol = 4) +
  geom_sf(data = sao, fill = 'gray20', color = "white", linewidth = 0.3) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  my_theme +
  coord_sf(xlim = c(ext_raster[1], ext_raster[2]), 
           ylim = c(ext_raster[3], ext_raster[4]), 
           expand = FALSE)


## Limite superior do IC 95%
ggplot() +
  geom_spatraster(data = r_hi) +
  facet_wrap(~lyr, ncol = 4) +
  geom_sf(data = sao, fill = 'gray20', color = "white", linewidth = 0.3) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  my_theme +
  coord_sf(xlim = c(ext_raster[1], ext_raster[2]), 
           ylim = c(ext_raster[3], ext_raster[4]), 
           expand = FALSE)



### 9.2.4) Mapa da abundância média (média/mediana de todos os meses)
r_median_anual <- mean(r_median)
#r_mean_anual <- mean(r_mean)


ggplot() +
  geom_spatraster(data = r_median_anual) +
  geom_sf(data = sao, fill = 'gray20', color = "white", linewidth = 0.3) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  my_theme +
  coord_sf(xlim = c(ext_raster[1], ext_raster[2]), 
           ylim = c(ext_raster[3], ext_raster[4]), 
           expand = FALSE)


# ggplot() +
#   geom_spatraster(data = r_median - r_median_anual) +
#   geom_sf(data = sao, fill = 'gray20', color = "white", linewidth = 0.3) +
#   fishualize::scale_fill_fish(option = "Hypsypops_rubicundus") +
#   my_theme +
#   coord_sf(expand = FALSE)




### 9.2.5) Visualização dinâmica (gif)
# O gganimate precisa de um data.frame no formato longo (uma linha por pixel x mês),
# e não de um raster multicamadas.

## Convertendo raster para data frame
r_df <- as.data.frame(r_median, xy = TRUE, na.rm = TRUE) 
head(r_df)


## Transformando dados largo -> longo
r_long <- tidyr::pivot_longer(r_df,
                              cols = -c(x, y),
                              names_to = "mes",
                              values_to = "abundancia")

r_long$mes <- factor(r_long$mes, levels = tlab) # fixa a ordem cronológica das unidades temporais
head(r_long)


### Gerando o mapa animado (gif)

anim <- ggplot() +
        geom_tile(data = r_long, aes(x = x, y = y, fill = abundancia)) +
        geom_sf(data = sao, fill = "gray20", color = "white", linewidth = 0.3) +
        fishualize::scale_fill_fish(option = "Hypsypops_rubicundus") +
        labs(title = "Mês-ano: {current_frame}", fill = "Abundância") +
        my_theme +
        coord_sf(xlim = c(ext_raster[1], ext_raster[2]), 
                 ylim = c(ext_raster[3], ext_raster[4]), 
                 expand = FALSE) +
        gganimate::transition_manual(mes) # somamos com '+' (e não com %>%)

mygif <- gganimate::animate(anim,
                            nframes = nt,   # 1 quadro por mês
                            fps = 2,
                            width = 600, height = 800, res = 100)

mygif


gganimate::anim_save(filename = "~/Downloads/sp-distribution.gif",
                     animation = mygif,
                     path = "Resultados")



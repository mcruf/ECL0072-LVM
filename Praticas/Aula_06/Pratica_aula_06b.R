#########################################################################################
#                                                                                       #
#                                   Prática da aula 06                                  #
#        - Processamento de dados ambientais para estimação e predição espacial -       #
#                                                                                       #
#########################################################################################


# O presente script é uma continuação do script 'Pratica_aula_06a.R' e tem como objetivo preparar e extrair covariáveis ambientais
# para cada pescaria. Essas informações serão abordadas posteriormente no script da próxima aula ('Pratica_aula_07.R').

# Os dados ambientais (i.e., produtos de sensoriamento remoto) foram obtidos por meio do Copernicus Marine Service,
# via API utilizando python. Para detalhes sobre o download e a organização dos arquivos, consulte o script:
# Praticas/Aula_06/Copernicus_data_download_practical.ipynb


# Ao todo, foram baixadas cinco variáveis ambientais, provenientes de dois produtos distintos do Copernicus Marine Service:
# 1) Variáveis físicas (Global Ocean Physics Reanalysis)
#    - Sea surface temperature (SST)
#    - Sea surface height (SSH)
#    - Mixed layer thickness (MLT)
#    - Bathymetry (BATHY)
#
# 2) Variáveis biogeoquímicas (Global Ocean Biogeochemistry Hindcast)
#     - Net primary productivity (NPPV)

# Há um total de 4 variáveis dinâmicas (SST, SSH, MLT e NPPV), i.e., que possuem uma série temporal e foram baixadas na mesma 
# resolução temporal dos dados pesqueiros (mensal - 24 meses no total). Já a batimetria é uma variável estática, permanecendo constante ao longo do tempo.

# Ao longo desta prática, as diferentes camadas ambientais serão padronizadas quanto à extensão espacial, sistema de referência de coordenadas (CRS) e
# resolução espacial. Em seguida, os valores de cada variável ambiental serão extraídos para a localização e o período de cada pescaria.



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(terra) # Processamento de rasters
library(sf) # Processamento de dados vetorizados 
library(stringr) # Manipulação de strings


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando funções auxiliares
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("Praticas/auxiliary_functions.R")



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



##############################################################################
###      PARTE 1: Extraindo dados ambientais para os pontos amostrais      ###
##############################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando os dados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1) Global Ocean Physics Reanalysis (raster)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - thetao_depth: SST (sea surface temperature)
# - zos: SSH (sea surface height)
# - mlotst: MLT (mixed layer thickness)
# - deptho: BATHY (bathymetry)


## Variáveis físicas
vfisicas <- rast("Dados/GIS/Environmental/Global_Ocean_Physics_Reanalysis_2004_2005_monthly_surface_layers.nc") 
#sst <- rast("Dados/GIS/Environmental/Global_Ocean_Physics_Reanalysis_2004_2005_monthly_surface_layers.nc", subds = "thetao") #Carregando cada variável separadamente


## Batimetria
batimetria <- rast("Dados/GIS/Environmental/bathymetry.nc")


## Explorando a estrutura dos dados
dim(vfisicas) 
nlyr(vfisicas)
names(vfisicas) 
head(vfisicas)
vfisicas

names(batimetria)
head(batimetria)
batimetria


## Visualizando os dados

### Batimetria
hist(batimetria)
boxplot(batimetria)
plot(batimetria)
plot(-batimetria)


### Camada de mistura (os primeiros 12 meses)
plot(vfisicas[[1:12]], nc = 6, nr = 2) # painel com 6 colunas, 2 linhas
plot(vfisicas[[25:50]], nc = 6, nr = 2) # temperatura
plot(vfisicas[[49:60]], nc = 6, nr = 2) # altura do mar





# 1.2) Global Ocean Biogeochemistry Hindcast (raster)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vbio <- rast("Dados/GIS/Environmental/Global_Ocean_Biogeochemistry_Hindcast_2004_2005_monthly.nc") 
head(vbio)
plot(vbio[[1:12]], nc = 6, nr = 2) # altura do mar



# 1.3) Extensão da área de estudo (shapefile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Camada necessária para padronizar o recorte espacial entre as diferentes camadas ambientais.
# Usaremos a porção sudoeste do oceano atlântico como referência (mesma extensão dos dados pesqueiros) 

sw_ocean <- read_sf("Dados/GIS/ICCAT/iccat_atsw.shp")

plot(st_geometry(sw_ocean), col = 'lightblue') # Visualizando o dado
sw_ocean # Estrutura do dado vetorial



# 1.4) Dados pesqueiros (dataframe)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/fisheries.rds")

## Transformando para objeto sf (espacial)
CRS_wgs <- 4326 # Coordenada padrão (WGS84 - long/lat)

dfsp <- st_as_sf(df, 
                 coords = c('Long','Lat'), 
                 crs = CRS_wgs) # As coordenadas estão originalmente em LAT/LONG; portanto, usar primeiro o CRS padrão (WGS84)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Padronizando os dados ambientais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Antes de poder utilizar as variáveis ambientais em uma análise estatística (e.g., GLM, GAM, etc.),
# é necessário assegurar que todas as camadas compartilhem a mesma grade espacial (i.e., extensão, CRS e resolução).


# 2.1) Padronizando a extensão espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Em tese, não seria necessário esta seção no presente script, uma vez que a extensão das camadas ambientais já foi previamente padronizada quando os dados 
# foram extraídos do Copernicus via API do python.


## Variáveis físicas
vfisicas2 <- crop(vfisicas, sw_ocean)

## Variáveis biológicas
vbio2 <- crop(vbio, sw_ocean)

## Batimetira
batimetria2 <- crop(batimetria, sw_ocean) 


## EXTRA: Em alguns casos, é preciso recortar o raster de forma a manter os mesmos pixels dentro do polígono.
## Nestes casos, usa-se a função 'mask()' no objeto criado pela função 'crop()':

#batimetria3 <- mask(batimetria2, sw_ocean) 



# 2.2) Padronizando o sistema de referência de coordenadas (CRS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# É importante assegurar que todas as camadas ambientais (i.e., rasters)
# estejam projetados para o mesmo CRS. 
# Para consultar as referências disponsíveis:
# - https://spatialreference.org/
# - https://epsg.io/


crs(batimetria2, proj = TRUE)
crs(batimetria2, describe = TRUE)


## Sanity check - ok!
crs(batimetria2) == crs(vfisicas2)
crs(batimetria2) == crs(vbio2)
crs(vbio2) == crs(vfisicas2)


## Reprojetando uma variável (exemplo)
# crs_ref <-  crs(batimetria2, proj = TRUE)
# vbio2 <- terra::project(vbio, crs_ref)



# 2.3) Padronizando a resolução espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ao utilizar rasters (dados) ambientais de diferentes fontes, é extremamente (!)
# importante assegurar que todas as camadas tenham a mesma resolução espacial.

# ATENÇÃO: 
# Nunca devemos aumentar artificialmente a resolução de uma variável.
# Reamostrar um raster de 0.25° para 0.083° cria novos pixels por interpolação,
# mas não cria nova informação. Por isso, a recomendação é sempre padronizar todas as camadas para a
# resolução da variável de menor resolução espacial (isto é, a mais grosseira).

# ATENÇÃO-2: 
# Antes de reamostrar os raster, é igualmente importante se certificar de que todos os rasters estejam
# na mesma extensão espacial.


## Verificando a resolução entre as diferentes camadas
res(vfisicas2)
res(batimetria2)
res(vbio2) #vbio possui resolução menor (0.25) do que as demais camadas.


## Padronizando batimetria
batimetria2r <- resample(batimetria2, vbio2, method = "bilinear")
res(batimetria2r) #ok!

par(mfrow=c(1,2))
plot(batimetria2, main = "Original")
plot(batimetria2r, main = "Reamostrado")
par(mfrow=c(1,1))


## Padronizando variáveis físicas
vfisicas2r <- resample(vfisicas2, vbio2, method = "bilinear")
res(vfisicas2r) #ok!



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Extraindo os dados ambientais para os pontos amostrais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Filtrando os dados (pesqueiros) para a extensão da área de estudo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Visualizando no mapa
plot(st_geometry(sw_ocean), col = '#ccdbdc')
plot(st_geometry(dfsp), pch = 19, cex = 0.7, col = "#e29578", add = T) #Eita! Há pontos em ambiente teresstre e fora da área de estudo.


## Filtrando pescarias (pontos amostrais) fora da área de estudo
dfsp_f <- dfsp %>% 
          st_filter(sw_ocean, .predicate = st_intersects)

dim(dfsp)[1] - dim(dfsp_f)[1] #No. de pontos removidos

plot(st_geometry(dfsp_f), pch = 19, cex = 0.7, col = 1, add = T) #Sucesso!



# 3.2) Extraindo os dados ambientais para cada pescaria
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Os dados pesqueiros possuem uma resolução mensal, tal como as variáveis ambientais.
# Cada pescaria está associada a um mês de um determinado ano (mes_ano). 
# Portanto, precisamos assegurar que a covariável a ser extraída corresponda ao mesmo mês_ano.
# ATENÇÃO: Isso se aplica apenas para as variáveis dinâmicas (nppv, sst, ssh, mlt)
# Usaremos a função 'extract_env_dynamic()' para extrair os valores das variáveis dinâmicas
# para cada pescaria. Para maiores detalhamento da função, vide script: /Praticas/auxiliary_functions.R


## Mapeando os anos dos dados pesqueiros aos dados ambientais (workaround solution - 'gambiarra')
### Os anos nos dados pesqueiros estão identificados apenas como '1' e '2'. Os dados ambientais
### foram baixados arbitrariamente para os anos 2004-2005

year_mapping <- c("1" = 2004, "2" = 2005) 

## Acessando o nome das variáveis
unique(word(names(vfisicas2r), 1, sep = "_"))
unique(word(names(vbio2), 1, sep = "_"))
unique(word(names(batimetria2r), 1, sep = "_"))


### Extraindo dados para thetao (sst - temperatura superficial do mar); variável dinâmica
dfsp_f <- extract_env_dynamic(points_sf = dfsp_f, # dados pesqueiros espacializados
                              env_raster = vfisicas2r, # camada ambiental (raster)
                              var_name = "thetao",# nome da variável dentro da camada ambiental
                              year_map = year_mapping)


### Extraindo dados para zos (ssh - altura da superfície do mar); variável dinâmica
dfsp_f <- extract_env_dynamic(points_sf = dfsp_f, 
                              env_raster = vfisicas2r, 
                              var_name = "zos",
                              year_map = year_mapping)


### Extraindo dados para mlotst (mlt - espessura da camada de mistura); variável dinâmica
dfsp_f <- extract_env_dynamic(points_sf = dfsp_f, 
                              env_raster = vfisicas2r, 
                              var_name = "mlotst",
                              year_map = year_mapping)


### Extraindo dados para nppv (produtividade primária); variável dinâmica
dfsp_f <- extract_env_dynamic(points_sf = dfsp_f, 
                              env_raster = vbio2, 
                              var_name = "nppv",
                              year_map = year_mapping)



### Extraindo dados para deptho (bathy - batimetria); variável estática
dfsp_f$bathy  <- extract(batimetria2r, dfsp_f)[, 2]



head(as.data.frame(dfsp_f)) # Cenas para o próximo capítulo...



#### Salvando os dados
coords <- st_coordinates(dfsp_f) # Recuperando as coordenadas (long/lat)
dfsp_f <- st_drop_geometry(dfsp_f) # Remove geometria dos dados (i.e., spatial_data_frame -> data_frame)
dfsp_f$Long <- coords[, 1] # Adicionando Long aos dados
dfsp_f$Lat <- coords[, 2] # Adicionando Lat aos dados
dfsp_f <- dfsp_f[, c(1:6,27:28, 7:26)] # Reordenando as colunas

saveRDS(dfsp_f, "Dados/fisheries_covars.rds")

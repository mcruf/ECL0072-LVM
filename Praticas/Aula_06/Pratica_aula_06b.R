#################################################
#                                               #
#             Prática da aula 06                #
#      - Processando dados ambientais -         #
#                                               #
#################################################


# Dados ambientais correspondem a uma mistura de variáveis dinâmicas e estáticas.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIXME: Definindo o diretório de trabalho 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-LVM/")


#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(terra) #Processamento de rasters
library(sf) #Processamento de dados vetorizados 



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


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
vfisicas <- rast("Dados/GIS/Environmental/Global_Ocean_Physics_Reanalysis_2000_2001_monthly_surface_layers.nc") 
#sst <- rast("Dados/GIS/Environmental/Global_Ocean_Physics_Reanalysis_2000_2001_monthly_surface_layers.nc", subds = "thetao") #Carregando cada variável separadamente


## Batimetria
batimetria <- rast("Dados/GIS/Environmental/bathymetry.nc")


## Explorando a estrutura dos dados
dim(vifisicas)
nlyr(vifisicas)
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

### Camada de mistura (os 12 últimos meses)
par(mfrow=c(2,6))

meses <- 1:12

for(i in 61:72){
  plot(vfisicas[[i]], range = c(0, 550), main = meses[i - 60])
}

par(mfrow=c(1,1))


# 1.2) Global Ocean Biogeochemistry Hindcast (raster)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vbio <- rast("Dados/GIS/Environmental/Global_Ocean_Biogeochemistry_Hindcast_2000_2001_monthly.nc") 
head(vbio)
plot(vbio[[1]])



# 1.3) Extensão da área de estudo (shapefile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Camada necessária para padronizar o recorte espacial entre as diferentes camadas ambientais.
## Usaremos a porção sudoeste do oceano atlântico como referência (mesma extensão dos dados pesqueiros) 
sw_ocean <- read_sf("Dados/GIS/ICCAT/iccat_atsw.shp")

plot(st_geometry(sw_ocean), col = 'lightblue') #Visualizando o dado
sw_ocean # Estrutura do dado vetorial



# 1.4) Dados pesqueiros (dataframe)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/fisheries.rds")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Padronizando os dados ambientais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Antes de poder utilizar as variáveis ambientais em uma análise estatística (e.g., GLM, GAM, etc.),
# é necessário assegurar que todas as camadas compartilhem exatamente a mesma grade espacial
# (i.e., extensão, CRS e resolução).


# 2.1) Padronizando a extensão espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Variáveis físicas
vfisicas2 <- crop(vfisicas, sw_ocean)

## Variáveis biológicas
vbio2 <- crop(vbio, sw_ocean)

## Batimetira
batimetria2 <- crop(batimetria, sw_ocean) #mantém a extensão


## EXTRA: Em alguns casos, é preciso recortar o raster de forma a matner os mesmos pixels dentro do polígono.
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
plot(batimetria2r, main = "Original")
plot(batimetria2r, main = "Reamostrado")
par(mfrow=c(1,1))


## Padronizando variáveis físticas
vfisicas2r <- resample(vfisicas2, vbio2, method = "bilinear")
res(vfisicas2r) #ok!


  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Extraindo os dados ambientais para os pontos amostrais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Filtrando os dados (pesqueiros) para a extensão da área de estudo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Tornando os dados como objeto espacial
dfs <- st_as_sf(df, coords = c('Long','Lat'), crs = crs(sw_ocean))
class(dfs)
head(dfs)


## Visualizando no mapa
plot(st_geometry(sw_ocean), col = '#ccdbdc')
plot(st_geometry(dfs), pch = 19, cex = 0.7, col = "#e29578", add = T) #Eita! Há pontos em ambiente teresstre e fora da área de estudo.


## Filtrando pescarias (pontos amostrais) fora da área de estudo
dfs_f <- dfs %>% 
         st_filter(sw_ocean, .predicate = st_intersects)

dim(dfs)[1] - dim(dfs_f)[1] #No. de pontos removidos

plot(st_geometry(dfs_f), pch = 19, cex = 0.7, col = 1, add = T) #Sucesso!



# 3.2) Extraindo os dados ambientais para cada pescaria
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Os dados pesqueiros possuem uma resolução mensal, tal como as variáveis ambientais.
# Cada pescaria está associada a um mês de um determinado ano (mes_ano). 
# Portanto, precisamos assegurar que a covariável a ser extraída corresponda ao mesmo mês_ano.
# ATENÇÃO: Isso se aplica apenas para as variáveis dinâmicas (nppv, sst, ssh, mlt)









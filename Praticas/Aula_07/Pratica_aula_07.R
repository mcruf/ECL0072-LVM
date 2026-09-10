#####################################################################
#                                                                   #
#                       Prática da aula 07                          #                                #
#               - Modelos espaciais via R-INLA -                    #
#                                                                   #
#####################################################################


# FIXME: Descrição do script


# Dados da pesca atuneira no SAO

# Foram baixadas um total de 5 variáveis ambientais, provenientes de 2 produtos distintos do Copernicus:
# i) Global Ocean Physics Reanalysis: sea water tempertaure (sst), sea surface height (ssh), mixed layer thickness (mlt), bathymetry (bathy)
# ii) Global Ocean Biogeochemistry Hindcast: net primary productivity (nppv)
# Há 4 variáveis dinâmicas (i.e., variáveis baixadas para a mesma resolução temporal dos dados pesqueiros) e 1 variável estática (batimetira).



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(INLA) # Ajustar modelos espaciais (bayesianos)
library(ggplot2) # Visualização dos dados
library(sf)  # Processamento de dados vetorizados 
library(raster) # Processamento de rasters
library(terra) # Processamento de rasters
library(stringr) # Manipulação de strings


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando funções auxiliares
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("Praticas/auxiliary_functions.R")


#~~~~~~~~~~~~~~~~~
# Input padrão
#~~~~~~~~~~~~~~~~~
CRS_wgs <- 4326 # Coordenada padrão (WGS84 - long/lat)
CRS_lea <- "+proj=laea +lat_0=-25 +lon_0=-20 +datum=WGS84 +units=km +no_defs" # Projeção LEA centrada no AS & expressa em Km



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando os dados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1.1) Dados pesqueiros
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/fisheries.rds")
str(df)



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
dfs_f <- extract_env_dynamic(points_sf = dfs_f, #dados pesqueiros espacializados
                             env_raster = vfisicas2r, #camada ambiental (raster)
                             var_name = "thetao",#nome da variável dentro da camada ambiental
                             year_map = year_mapping)


### Extraindo dados para zos (ssh - altura da superfície do mar); variável dinâmica
dfs_f <- extract_env_dynamic(points_sf = dfs_f, #dados pesqueiros espacializados
                             env_raster = vfisicas2r, #camada ambiental (raster)
                             var_name = "zos",#nome da variável dentro da camada ambiental
                             year_map = year_mapping)


### Extraindo dados para mlotst (mlt - espessura da camada de mistura); variável dinâmica
dfs_f <- extract_env_dynamic(points_sf = dfs_f, #dados pesqueiros espacializados
                             env_raster = vfisicas2r, #camada ambiental (raster)
                             var_name = "mlotst",#nome da variável dentro da camada ambiental
                             year_map = year_mapping)



### Extraindo dados para nppv (produtividade primária); variável dinâmica
dfs_f <- extract_env_dynamic(points_sf = dfs_f, #dados pesqueiros espacializados
                             env_raster = vbio2, #camada ambiental (raster)
                             var_name = "nppv",#nome da variável dentro da camada ambiental
                             year_map = year_mapping)



### Extraindo dados para deptho (bathy - batimetria); variável estática
dfs_f$bathy  <- extract(batimetria2r, dfs_f)[, 2]


### To see the progress...
head(as.data.frame(dfs_f))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Construindo o mesh (malha SPDE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Em modelos espaciais, é prática comum converter a projeção para UTM. 
# Contudo, como a área de estudo é ampla e abrange múltiplas zonas UTM, 
# será usada uma projeção LEA centrada nas coordenadas lat = -25 e long = -20 e com unidades expressas em Km.


# No R-INLA, podemos construir a malha espacial a partir das coordenadas dos dados, 
# de um polígono espacial de contorno (domain), ou combinando ambas as abordagens.

# Existem três parâmetros fundamentais para a definição da malha:
# - (1) max.edge: Comprimento máximo permitido para os lados dos triângulos. 
#    Quanto menor o valor, maior a resolução espacial da malha.
# - (2) offset: Define a extensão limite do domínio para as bordas interna e externa 
#    (útil para evitar o "efeito de borda" / boundary effect na amostragem).
# - (3) cutoff: Distância mínima permitida entre pontos. Pontos mais próximos que 
#    este limite são fundidos em um único vértice. Isso evita triângulos 
#    excessivamente pequenos, reduzindo a carga computacional; parâmetro crítico 
#    quando há pontos muito adensados.

# Para referências e detalhes teóricos adicionais, consulte:
# - https://becarioprecario.bitbucket.io/spde-gitbook/
# - https://haakonbakkagit.github.io/btopic104.html
# - https://rpubs.com/jafet089/886687



# 4.1) Transformando os dados para projeção LEA (expresso em metros)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dfs_lea <- st_transform(dfs_f, CRS_lea)



# 4.2) Criando matriz de coordenadas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coords <- as.data.frame(st_coordinates(dfs_lea))


# 4.3) Definindo o mesh
#~~~~~~~~~~~~~~~~~~~~~~
# Diversos estudos, tutoriais e discussões metodológicas sugerem que o valor 
# de 'max.edge' deva ser entre 1/3 e 1/10 do alcance espacial (spatial range) 
# do processo (Bakka et al., https://haakonbakkagit.github.io/btopic104.html).

# No entanto, como o alcance é um parâmetro estimado pelo próprio modelo e 
# permanece desconhecido a priori, podemos assumir uma postura conservadora:
# supõe-se que o alcance corresponda a cerca de 1/3 da extensão da área de estudo 
# e, a partir disso, define-se o 'max.edge' como 1/5 desse alcance estimado.

# Vale destacar que no R-INLA, o parâmetro de alcance (range) do GMRF representa 
# a distância na qual a autocorrelação espacial cai para aproximadamente 0,13 (ou seja, exp(-2)).

# Após o ajuste do primeiro modelo, recomenda-se verificar o alcance espacial
# estimado e, se necessário, reconstruir o mesh utilizando valores mais
# adequados para max.edge.

sw_oceant <- st_transform(sw_ocean, crs = CRS_lea) 

max.edge = diff(range(coords[,1]))/(3*5)
bound.outer = diff(range(coords[,1]))/5


mesh1 <- inla.mesh.2d(loc = coords)
mesh2 <- inla.mesh.2d(loc = coords, max.edge = c(2,3)*max.edge)


mesh_final <- inla.mesh.2d(#boundary = sw_oceant,
                     loc = coords, 
                     max.edge = c(1,2)*max.edge, #inner and outer triangle distance
                     #max.edge = c(5, 10),
                     #cutoff = 0.01,
                     cutoff = max.edge/4, #max. distance between two points
                     offset=c(max.edge, bound.outer)) #domain extension (buffer zone)



mesh <- inla.mesh.2d(boundary = sw_oceant,
                     loc = coords, 
                     max.edge = c(1,2)*max.edge, #inner and outer triangle distance
                     #max.edge = c(5, 10),
                     #cutoff = 0.01,
                     cutoff = max.edge/4, #max. distance between two points
                     offset=c(max.edge, bound.outer)) #domain extension (buffer zone)


plot(mesh)

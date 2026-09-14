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
library(INLA)    # Ajustar modelos espaciais (bayesianos)
library(fmesher) # Criação do mesh (novo!)
library(ggplot2) # Visualização dos dados
library(sf)      # Processamento de dados vetorizados 
library(raster)  # Processamento de rasters
library(terra)   # Processamento de rasters
library(stringr) # Manipulação de strings
library(corrplot) # Visualização de dados
library(mapview) # Visualização de dados espacializados
library(virids) # Paleta de cores


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
df <- readRDS("Dados/fisheries.rds")
str(df)

df$Month <- as.factor(df$Month) # Convertendo mês para fator


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
dfs_f$bathy  <- extract(batimetria2r, dfs_f)[, 2]


### To see the progress...
head(as.data.frame(dfs_f))
colnames(dfs_f)[23:27] <- c("temperatura", "altura_mar", "camada_mistura", "prod_primaria", "batimetria") # Renomeando as variáveis




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Análise exploratória dos dados ambientais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 4.1) Relação de P.glauca com as variáveis ambientais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dfs_f %>%
  tidyr::pivot_longer(cols = temperatura:batimetria,
                      names_to = "Variavel",
                      values_to = "Valor") %>% 
  #filter(Variavel == 'prod_primaria', Valor < 120) %>% 
  #filter(Variavel == 'altura_mar', Valor > -0.1) %>% 
  ggplot(aes(x = Valor, y = Prionace_glauca)) +
  geom_point(alpha = 0.5, color = "#e07a5f", size = 3) +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 10, bs = "cs"), col = 'black') +
  #geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) +
  facet_wrap(Variavel ~ . , scales = 'free') +
  my_theme


## Curiando...
mapview(dfs_f, 
        zcol = "prod_primaria",
        cex = "prod_primaria")



# 4.2) Avaliando multicolinearidade entre as covariáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colnames(dfs_f)
covars <- dfs_f[ , 23:27] # Selecionando apenas as colunas das covariáveis
covars <- st_drop_geometry(covars)
covars_cor <- cor(covars, method = 'spearman') 
corrplot(covars_cor, method = "color", type = "lower", addCoef.col = "black")

## Outra forma de visualizar...
GGally::ggpairs(covars) + my_theme


# Covariávies não são substancialmente correlacionadas entre si - todas podem ser inclusas no modelo.
# Embora haja algumas covariáveis com outliers marcantes (prod. primária & altura do mar), estes serão mantidos.
# Posteriormente, pode-se testar os modelos da seção 7 com os dados SEM esses outliers e ver qual o impacto destes sobre as estimativas dos parâmetros.
# Perceba, contudo, que outliers são mais preocupantes a nível da variável resposta!



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Construindo o mesh (malha SPDE)
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



# 5.1) Transformando os dados para projeção LEA (expresso em metros)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dfs_lea <- st_transform(dfs_f, CRS_lea)



# 5.2) Criando matriz de coordenadas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coords <- as.data.frame(st_coordinates(dfs_lea))



# 5.3) Definindo o mesh
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


max.edge <- diff(range(coords[,1]))/(3*5)
bound.outer <- diff(range(coords[,1]))/5


sw_oceant <- st_transform(sw_ocean, crs = CRS_lea) # Reprojetando o shp da área de estudo



# Mesh 1: Malha padrão usando apenas os pontos amosrrais 
mesh1 <- fm_mesh_2d_inla(loc = coords); plot(mesh1) # Triângulos muito grandes e irregulares


# Mesh 2: Malha com controle do tamanho máximo das arestas (max.edge = 200)
mesh2 <- fm_mesh_2d_inla(loc = coords, 
                         max.edge = c(200)) ; plot(mesh2) # Homogeniza o tamanho dos triângulos na área interna (quanto menor o valor, menor os trângulos)


# Mesh 3: Mesh com distância mínima entre vértices (cutoff = 30)
mesh3 <- fm_mesh_2d_inla(loc = coords, 
                         max.edge = c(200),
                         cutoff = 30) ; plot(mesh3) # Evita triângulos minúsculos onde os pontos são muito adensados


# Mesh 4: Mesh com uma extensão externa (offset) 
mesh4 <- fm_mesh_2d_inla(loc = coords, 
                         max.edge = c(200),
                         cutoff = 30,
                         offset = c(100, 300)) ; plot(mesh4) #  Cria uma zona de transição para evitar efeitos de borda (boundary effects)


# Mesh 5: Mesh tomando por base o polígono do SAO 
mesh5 <- fm_mesh_2d_inla(boundary = sw_oceant,
                         loc = coords, 
                         max.edge = c(200),
                         cutoff = 30,
                         offset = c(100, 300)) ; plot(mesh5) # Moldando melhor a malha à área de estudo


# Mesh 6: Mesh reparametrizado  
mesh6 <- fm_mesh_2d_inla(boundary = sw_oceant,
                         loc = coords, 
                         max.edge = c(1,2)*max.edge, 
                         cutoff = max.edge/4, 
                         offset = c(max.edge, bound.outer*1.5)) ; plot(mesh6)

par(mfrow = c(2, 3))
plot(mesh1); title("Mesh 1")
plot(mesh2); title("Mesh 2")
plot(mesh3); title("Mesh 3")
plot(mesh4); title("Mesh 4")
plot(mesh5); title("Mesh 5")
plot(mesh6); title("Mesh 6")
par(mfrow = c(1,1))


# O mesh6 será escolhido como malha
mesh <- mesh6


## Explorando o objeto 'mesh'
names(mesh)

mesh$n # Número total de vértices (nós)
nrow(mesh$graph$tv) # Número total de triângulos
head(mesh$loc) # Coordenadas de cada vértice

## Just for fun
plot(mesh)
points(mesh$loc, col=2, pch = 19, cex = 0.3)
points(coords, col = 'blue', pch = 19, cex =0.5 )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6) Definindo as matrizes de projeção, SPDE & stacks de estimação e predição
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# O SPDE e todos os outros componentes do modelo são definidos para cada vértice do mesh,
# ao passo que o conjunto de dados (i.e., a variável resposta) está definido apenas em um subconjunto desses vértices.
# É necessário, portanto, garantir que tanto os dados quanto o SPDE estejam relacionados ao mesmo índice dos vértices do mesh.
# Isso é feito por meio da matriz de projeção "A". 
# A matriz A possui o número de linhas igual ao número de observações  e o número de colunas igual ao número de vértices da triangulação.

# Como a estimação do modelo e a predição espacial ocorrem em locais diferentes, é necessário construir duas matrizes "A" distintas:
# 1) Matriz A de Estimação (Aest): conecta o mesh aos locais onde os dados foram efetivamente observados (coordenadas de amostragem).
# 2) Matriz A de Predição (Apred): conecta o mesh aos locais não amostrados onde queremos prever o fenômeno (ex: uma grade/grid regular).



# 6.1) Matriz de estimação 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Aest <- inla.spde.make.A(mesh = mesh, loc = as.matrix(coords))
dim(Aest)



# 6.2) Matriz de predição
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Para construir a matriz de projeção de predição (Apred), é necessário definir a grade (grid) sobre a qual as predições espaciais serão realizadas.
# Essa grade pode ser gerada de duas formas:
#   1) Manualmente (e.g., uma grade regular cobrindo a área de estudo):
#      - Vantagem: Resolução espacial fixa e conhecida (ex: 10 km x 10 km)
#      - Desvantagem: Maior custo computacional (se usar uma grade muito fina)
#   2) Utilizando os próprios vértices (nós) do mesh:
#      - Vantagem: Computacionalmente muito mais rápido & mais preciso nos locais amostrado
#      - Desvantagem: A resolução espacial torna-se heterogênea e variável no espaço (mais fina onde há alta densidade 
#        de pontos e mais grosseira nas bordas)



### Usando os vértices do mesh ###

# Extraindo as coordenadas da malha
mcoords <- data.frame(X = mesh$loc[,1], Y = mesh$loc[,2]) 
mesh.points <- st_as_sf(mcoords, coords = c("X", "Y"), crs = CRS_lea) # Transformando as coordenadas para objeto espacial (sf)

# Sanity check
plot(mesh)
points(mesh.points, col='red', pch=19, cex = 0.3)


# Definindo a matriz de projeção Apred
Apred <- inla.spde.make.A(mesh, loc = mesh.points) 
dim(Apred)


### Usando um grid com resolução definida ###

# # Definindo extensão do grid 
# xmin <- floor(min(coords$X))
# xmax <- ceiling(max(coords$X))
# ymin <- floor(min(coords$Y))
# ymax <- ceiling(max(coords$Y))
# 
# ext <- st_bbox(sw_oceant)
# 
# # Construindo o grid
# RES <- 50 # Grid de 50 x 50 km
# 
# grid <- st_make_grid(st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = CRS_lea),
#                      cellsize = c(RES, RES), 
#                      what = "polygons",
#                      square = T)
# 
# plot(grid)
# plot(st_geometry(sw_oceant), add=T, col = 'gray70')
# dim(grid)
# 
# 
# # Extraindo as coordenadas do grid
# gcoords <- data.frame(Long = st_coordinates(grid)[,1], Lat = st_coordinates(grid)[,2]) 
# grid.points <- st_as_sf(gcoords, coords = c("Long", "Lat"), crs = CRS_lea) # Transformando as coordenadas para objeto espacial (sf)
# 
# 
# # Definindo a matriz de projeção Apred
# Apred <- inla.spde.make.A(mesh, loc = grid.points)
# dim(Apred)



# 6.3) Definindo o SPDE
#~~~~~~~~~~~~~~~~~~~~~~~

## SPDE 'clássico'
spde <- inla.spde2.matern(mesh = mesh, alpha = 2) # alpha = parâmetro de suavidade da função de covariância Matérn (usando o valor default)


## SPDE com pc.prior (evita sobreajuste - overfitting)
# spde_pc <- inla.spde2.pcmatern(mesh = mesh,
#                                alpha = 2,
#                                prior.range = c(500, 0.01), # P(range < 500 km) = 0.01 (baseado +/- no variograma empírico); reavaliar após a primeira rodada dos modelos
#                                prior.sigma = c(1, 0.01) )   # P(sigma > 1) = 0.01
                                
                    




# 6.4) Construção dos stacks de estimação e predição
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A função inla.stack() é utilizada para organizar todas as informações necessárias para o ajuste do modelo. 
# Em uma stack são reunidos os dados (variável resposta), as matrizes de projeção (Aest/Apred), os efeitos fixos e os efeitos aleatórios.
# Essa organização permite que o INLA associe corretamente cada observação aos componentes do modelo.

# Argumentos principais da função:

# @ tag: identificador do stack (ex: 'est' para estimação, 'pred' para predição)
# @ data: lista contendo a variável resposta 
# @ A: lista com as matrizes de projeção
# @ effects: lista contendo os efeitos fixos e aleatórios.


# Assim como fizemos com a matriz de projeção "A", é necessário construir um stack para a ESTIMAÇÃO e outro para a PREDIÇÃO:
# - Stack de Estimação: Construído a partir do conjunto de dados observado e utilizada para ajustar o modelo
# - Stack de Predição: Construído a partir da malha espacial (mesh/grid). 

# ATENÇÃO: Para realizar a predição, o valor de cada covariável do modelo deve ser previamente extraído para todos os pontos da grade espacial.


# 6.4.1) Stack de estimação
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ATENÇÃO: Os nomes atribuídos aos efeitos dentro da função inla.stack() devem ser exatamente os mesmos utilizados 
# posteriormente na especificação da fórmula do modelo (model formula).

# ATENÇÃO-2: Todas as covariáveis serão padronizadas! 


indexs <- inla.spde.make.index("spatial", spde$n.spde) # Cria os índices espaciais (necessário para ajuste do modelo)

stk.est <- inla.stack(data = list(y = dfs_lea$Prionace_glauca),
                      A = list(Aest, 1),
                      effects = list(spatial = indexs, #mesma coisa que 1:spde$n.spde
                                     list(data.frame(b0 = 1, # Intercepto - obrigatório
                                                     year = dfs_lea$Year,
                                                     month = dfs_lea$Month,
                                                     fid = dfs_lea$Id,
                                                     vessel = dfs_lea$Vessel,
                                                     flag = dfs_lea$Flag,
                                                     nhooks = dfs_lea$Nhooks,
                                                     temp = scale(dfs_lea$temperatura),
                                                     sheight = scale(dfs_lea$altura_mar),
                                                     mlt = scale(dfs_lea$camada_mistura),
                                                     npp = scale(dfs_lea$prod_primaria),
                                                     depth = scale(dfs_lea$batimetria),
                                                     X_coord = inla.group(coords[, 1], n = 50),
                                                     Y_coord = inla.group(coords[, 2], n = 50)))),
                      tag = "est")




# 6.4.2) Stack de predição
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# A predição implica em predizer valores da variável resposta em locais que NÃO foram amostrados, ie., predizer ao longo de todo SAO.
# No stack de predição a variável resposta é, portanto, configurada como NA, uma vez que são exatamente esses os valores que o modelo irá predizer.

# Como a estimação inclui um conjunto de covariávies, isso implica que para a etapa de predição, é necessário extrair os valores das covariáveis
# para todos os pontos do grid de predicão. 
# CUIDADO: dados faltantes (NAs) nas covariáveis para algumas regiões inviabilizam a predição naquele local.


## Extraindo dados ambientais para cada ponto do mesh (grid)
dfp <- mesh.points



### temperatura do mar (thetao)
idx <- grep(paste0("^", 'thetao'), names(vfisicas2r)) # Reavendo índice das colunas com a variável desejada
rast_sst <- vfisicas2[[idx]]
mean_sst <- mean(rast_sst, na.rm = TRUE)
plot(mean_sst)

dfp$TEMPERATURA  <- extract(mean_sst, mesh.points)[, 2]
ggplot() + geom_sf(data = dfp, aes(col = TEMPERATURA))
ggplot() + geom_sf(data = filter(dfp, !is.na(TEMPERATURA)), aes(col = TEMPERATURA))


### altura do mar (zos)
idx <- grep(paste0("^", 'zos'), names(vfisicas2r)) # Reavendo índice das colunas com a variável desejada
rast_ssh <- vfisicas2[[idx]]
mean_ssh <- mean(rast_ssh, na.rm = TRUE)
plot(mean_ssh)

dfp$ALTURA_MAR  <- extract(mean_ssh, mesh.points)[, 2]
# ggplot() + geom_sf(data = dfp, aes(col = ALTURA_MAR))
# ggplot() + geom_sf(data = filter(dfp, !is.na(ALTURA_MAR)), aes(col = ALTURA_MAR))


### camada de mistura (mlotst)
idx <- grep(paste0("^", 'mlotst'), names(vfisicas2r)) # Reavendo índice das colunas com a variável desejada
rast_mlt <- vfisicas2[[idx]]
mean_mlt <- mean(rast_mlt, na.rm = TRUE)
plot(mean_mlt)

dfp$CAMADA_MISTURA  <- extract(mean_mlt, mesh.points)[, 2]
# ggplot() + geom_sf(data = dfp, aes(col = CAMADA_MISTURA))
# ggplot() + geom_sf(data = filter(dfp, !is.na(CAMADA_MISTURA)), aes(col = CAMADA_MISTURA))


### produtividade primaria
idx <- grep(paste0("^", 'nppv'), names(vbio2)) # Reavendo índice das colunas com a variável desejada
rast_nppv <- vbio2[[idx]]
mean_nppv <- mean(rast_nppv, na.rm = TRUE)
plot(mean_nppv)

dfp$PROD_PRIMARIA  <- extract(mean_nppv, mesh.points)[, 2]
# ggplot() + geom_sf(data = dfp, aes(col = PROD_PRIMARIA))
# ggplot() + geom_sf(data = filter(dfp, !is.na(PROD_PRIMARIA)), aes(col = PROD_PRIMARIA))


### batimetria
dfp$BATIMETRIA  <- extract(batimetria2r, mesh.points)[, 2]
# ggplot() + geom_sf(data = dfp, aes(col = BATIMETRIA))
# ggplot() + geom_sf(data = filter(dfp, !is.na(BATIMETRIA)), aes(col = BATIMETRIA))


head(dfp)


stk.pred <- inla.stack(data = list(y = NA), #Definir a variável-resposta como NA - isso é o que queremos predizer!
                       A = list(Apred, 1),
                       effects = list(spatial = indexs, 
                                      list(data.frame(b0 = 1, # Intercepto - obrigatório
                                                      year = "2", # Usando o segundo ano como referência
                                                      month = "1", # Usando 'janeiro' como mês de referência
                                                      fid = NA, # Efeito aleatório não estruturado - sempre como NA, pois não contribui para o preditor linear
                                                      vessel = NA, # Efeito aleatório não estruturado - sempre como NA, pois não contribui para o preditor linear
                                                      flag = "BRA", # Brasil como país de referência
                                                      nhooks = round(mean(dfs_lea$Nhooks)), # Esforço médio - constante em toda a área; idealmente, fazer um raster do esforço
                                                      temp = scale(dfp$TEMPERATURA),
                                                      sheight = scale(dfp$ALTURA_MAR),
                                                      mlt = scale(dfp$CAMADA_MISTURA),
                                                      npp = scale(dfp$PROD_PRIMARIA),
                                                      depth = scale(dfp$BATIMETRIA),
                                                      X_coord = inla.group(mcoords[, 1], n = 50),
                                                      Y_coord = inla.group(mcoords[, 2], n = 50)))),
                       tag = "pred")



# 6.4.3) Juntando os stacks
#~~~~~~~~~~~~~~~~~~~~~~~~~~
stk.full <- inla.stack(stk.est, stk.pred)



#~~~~~~~~~~~~~~~~~~~~~~~~
# 7) Rodando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~



# 7.1) Definindo a estrutura dos modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
form1 <- y ~ 0 + b0 # Modelo nulo
form2 <- y ~ 0 + b0 + depth + npp + mlt + sheight + temp + offset(log(nhooks))
form3 <- y ~ 0 + b0 + depth + npp + mlt + sheight + temp + offset(log(nhooks)) + f(Y_coord, model = 'rw1') + f(X_coord, model = 'rw1') 
form4 <- y ~ 0 + b0 + depth + npp + mlt + sheight + temp + offset(log(nhooks)) + f(spatial, model = spde)
form5 <- y ~ 0 + b0 + depth + npp + mlt + sheight + temp + offset(log(nhooks)) + f(spatial, model = spde) + f(vessel, model = "iid")
form6 <- y ~ 0 + b0 + depth + npp + mlt + sheight + temp + offset(log(nhooks)) + f(spatial, model = spde) + f(vessel, model = "iid") + f(fid, model = 'iid')


formulas <- c(form1, form2, form3, form4, form5, form6) # Concatenando as fórmulas



# 7.2) Rodando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~
inla.setOption(num.threads = 8) # Para acelerar os cálculos, em caso de muitos dados



## Distribuição Poisson
pois_mod <- list()

for(model in seq_along(formulas)){
  
  pois_mod[[model]] <- inla(formulas[[model]],
                            family = "poisson",
                            data = inla.stack.data(stk.full), # Realiza o processo de estimação e predição simultâneamente
                            control.predictor = list(compute = TRUE, # Computa a distribuição posterior do preditor linear
                                                     link = 1, # Mapeia a distribuição posterior estimada de volta para a escala original dos dados
                                                     A = inla.stack.A(stk.full)),
                             control.inla = list(strategy = "simplified.laplace"), # Para rodar mais rápido; bom na fase exploratória!
                             control.compute = list(return.marginals.predictor = F,
                                                    dic = T, waic=T, cpo=T),
                             verbose = F)
  
  print(paste("finished model", model, "out of", length(formulas)))
  
}



## Distribuição Binomial Negativa
nb_mod <- list()

for(model in seq_along(formulas)){
  
  nb_mod[[model]] <- inla(formulas[[model]],
                          family = "nbinomial",
                          data = inla.stack.data(stk.full), # Realiza o processo de estimação e predição simultâneamente
                          control.predictor = list(compute = TRUE, # Computa a distribuição posterior do preditor linear
                                                   link = 1, # Mapeia a distribuição posterior estimada de volta para a escala original dos dados
                                                   A = inla.stack.A(stk.full)),
                          control.inla = list(strategy = "simplified.laplace"), # Para rodar mais rápido; bom na fase exploratória!
                          control.compute = list(return.marginals.predictor = F,
                                                 dic = T, waic=T, cpo=T),
                          verbose = F)
  
  print(paste("finished model", model, "out of", length(formulas)))
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 8) Extraindo & avaliando os resultados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 8.1) Extraindo os resultados para avaliar e selecionar os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Usaremos a função 'mdiagnostic'; para maiores detalhes, vide a função no script 'Pratucas/auxiliary_functions.R'
# A função extrai as seguintes métricas: DIC, WAIC e LCPO

rpois <- do.call("rbind", lapply(pois_mod, mdiagnostic)); rpois$model <- as.factor(1:length(formulas)); rpois$family <- "Pois"
rnb <- do.call("rbind", lapply(nb_mod, mdiagnostic)); rnb$model <- as.factor(1:length(formulas)) ; rnb$family <- "NB"

mresults <- rbind(rpois, rnb)

# Curiando...
head(mresults)


# 8.1.1) Comparando os modelos quantitativamente (via DIC, WAIC e LCPO)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mresults %>% 
  tidyr::pivot_longer(cols = DIC:LCPO,
                      names_to = "Metrica",
                      values_to = "Valor") %>% 
  filter(Metrica != "LCPO") %>% 
  ggplot(aes(x = model, y = Valor, group = family, col = family)) + 
  geom_point(size = 5) +
  facet_wrap(Metrica ~ ., scales = 'free', ncol = 3) +
  my_theme


### Vamos rever os resultados sem a distribuição Poisson
mresults %>% 
  tidyr::pivot_longer(cols = DIC:LCPO,
                      names_to = "Metrica",
                      values_to = "Valor") %>%
  filter(family != "Pois", Metrica != "LCPO") %>% 
  ggplot(aes(x = model, y = Valor, group = family, col = family)) + 
  geom_point(size = 5) +
  facet_wrap(Metrica ~ ., scales = 'free', ncol = 3) +
  my_theme


mresults %>% 
  #filter(family != "Pois") %>% 
  arrange(WAIC, DIC) # Modelo no. 6 é o melhor modelo



# 8.1.2) Visualização dos pressupostos & ajuste do modelo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Extraindo os índices relacionados às estimações no objeto do modelo
idx <- inla.stack.index(stack = stk.full, tag = "est")$data 

### Extraindo os resultados dos modelos
allm <- c(pois_mod, nb_mod)
names(allm) <- c(paste("Pois", 1:length(formulas), sep = "_"), paste("NB", 1:length(formulas), sep = "_"))

dat <- list()

for(i in 1:length(allm)){
  
  family_name <- sub("_.*", "", names(allm)[i])   
  model_number <- sub(".*_", "", names(allm)[i])  
  
  
  dat[[i]] <- data.frame(predicted = allm[[i]]$summary.fitted.values[idx,'mean'], 
                         lower = allm[[i]]$summary.fitted.values[idx, '0.025quant'], 
                         upper = allm[[i]]$summary.fitted.values[idx, '0.975quant'], 
                         observed = dfs_lea$Prionace_glauca, # Inserir aqui a coluna correspondente à variável resposta!
                         family = family_name,
                         model = model_number)
  
}
dat <- do.call("rbind", dat)

# Curiando...
head(dat)


## Calculando os resíduos
dat$residual <- dat$predicted - dat$observed
dat$standardResidual <- dat$residual/stats::sd(dat$residual)
dat$standardUpper <- (dat$upper - dat$observed)/stats::sd(dat$residual)
dat$standardLower <- (dat$lower - dat$observed)/stats::sd(dat$residual)



### PRESSUPOSTO DE HOMOCEDASTICIDADE  
dat %>% 
  filter(family == "NB") %>% 
  #filter(family == "Pois") %>% 
  mutate(model = factor(model, levels = 1:10)) %>% 
  ggplot(aes(x = predicted, y = standardResidual)) + 
        geom_point() + 
        labs(y = "Resíduos padronizados", 
             x = "Valores ajustados",
             title = "Homocedasticidade") + 
        facet_wrap(model ~ ., scales = "free", nrow = 2) +
        geom_smooth(se = T) + 
        geom_hline(yintercept = 0, linetype = 2, col = "red") + 
        geom_segment(ggplot2::aes_string(y = "standardLower", 
                                         yend = "standardUpper", xend = "predicted"), alpha = 0.4) + 
        my_theme
      



### PRESSUPOSTO DE INDEPENDÊNCIA RESIDUAL
tmp <- dat %>%
  subset(family == "NB" & model == "6")
  #subset(family == "NB" & model == "2")

#### Independência temporal
forecast::ggAcf(tmp$residual,lag.max=24) +
  labs(title='Independência residual') + 
  theme_bw()


#### Independenência espacial
tmp <- dat %>%
        subset(family == "NB" & model == "6") 
        #subset(family == "NB" & model == "4") 

dfs_lea$std_residuals <- tmp$standardResidual


ggplot(dfs_lea) +
  geom_sf(aes(col = std_residuals, size = std_residuals), alpha = 0.5) +
  scale_color_viridis(option = 'A') +
  guides(size = "none") +
  my_theme



#### RESÍDUOS QUANTÍLICOS (baseado nos valores PIT)

# A Transformação Integral de Probabilidade (PIT) avalia a calibração da distribuição preditiva posterior. 
# Em um modelo bem especificado, os valores de PIT devem seguir uma distribuição uniforme U(0,1). A aplicação da
# transformação normal inversa (qnorm) converte os valores de PIT em resíduos com escore normal, os quais podem ser avaliados usando um Q-Q plot padrão.
# Desvios da linha de referência indicam potenciais problemas na especificação do modelo, tais como verossimilhança inadequada, covariáveis ausentes ou
# fontes de variação não consideradas.


pit_model <- nb_mod[[6]]$cpo$pit # Extraindo valores PIT do melhor modelo
#pit_model <- nb_mod[[4]]$cpo$pit # Para comparar...

pit_clean <- pit_model[is.finite(pit_model) & pit_model > 0 & pit_model < 1] # Removendo valores de PIT inválidos
df_qq <- data.frame(rpit = qnorm(pit_clean)) # Transforma os PITs da escala uniforme para escala normal (padrão do qqplot)



# Plotando...
ggplot(df_qq, aes(sample = rpit)) +
  stat_qq_line(col = 'blue', linetype = 'dashed', linewidth = 1) +
  stat_qq(size = 2, alpha = 0.6) +
  my_theme

hist(pit_clean) # Simetria à direita; indicativo de subsestimação da variável resposta (mod6)



#### VALORES PREDITOS VS. OBSERVADOS
dat %>%
  filter(family == "NB", model %in% c('4', '5', '6')) %>% 
  #mutate(model = factor(model, levels = 1:10)) %>% 
  ggplot(aes(x = observed, y = predicted)) + 
    geom_point() + 
    labs(y = "Predicted",
         x = "Observed",
         title = "Obs. vs. pred.") + 
    geom_smooth(method = "lm") +
    facet_wrap(model ~ ., ncol = 3, scales = "free_y") +
    my_theme
  

# Há uma tendência sistemática de subestimação da abundância do tubarão azul (P. glauca)



# 8.2) Avaliando o resultado estatístico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
modelo <- nb_mod[[6]]
modelo4 <- nb_mod[[4]] # Para comparar

summary(modelo) # Sumário padrão
summary(modelo4) # Repare na influência dos efeitos aleatórios (modelo 6) sobre a estimativa das covariáveis!





# 8.3) Avaliando os mapas preditivos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Extraindo os índices relacionados às predições no objeto do modelo
idxs <- inla.stack.index(stack = stk.full, tag = "pred")$data


### Extraindo distribuição posterior do efeito espacial (média, dp, IC de 95%)
sp.mean <- modelo$summary.random$spatial$mean
sp.sd   <- modelo$summary.random$spatial$sd
sp.025  <- modelo$summary.random$spatial$`0.025quant`
sp.975  <- modelo$summary.random$spatial$`0.975quant`


### Extraindo distribuição posterior dos valores preditos (média, dp, IC de 95%)
pred.mean <- modelo$summary.fitted.values[idxs, "mean"]
pred.sd <- modelo$summary.fitted.values[idxs, "sd"]
pred.025 <- modelo$summary.fitted.values[idxs, "0.025quant"]
pred.975 <- modelo$summary.fitted.values[idxs, "0.975quant"]


### Armazenando infos em dataframe
#### Transformando as coordenadas (serão usadas para interpolação)
mcoords_sp <- st_as_sf(mcoords, coords = c("X", "Y"), crs = CRS_lea)
mcoords_wgs <- st_transform(mcoords_sp, CRS_wgs) # Re-projetando para WGS84 

pred_df <- data.frame(Long = st_coordinates(mcoords_wgs)[, 1],
                      Lat = st_coordinates(mcoords_wgs)[, 2],
                      pred.mean = pred.mean,
                      pred.sd = pred.sd,
                      pred.025 = pred.025,
                      pred.975 = pred.975,
                      sp.mean = sp.mean,
                      sp.sd = sp.sd,
                      sp.025 = sp.025,
                      sp.975 = sp.975)



#### Interpolando os valores

iVAR <- colnames(pred_df)[3:10] # Variáveis a serem interpoladas


raster_list <- lapply(iVAR, function(var_name) {
  
  # Interpolação triangulada
  interp_res <- interp(x = pred_df$Long, 
                       y = pred_df$Lat, 
                       z = pred_df[[var_name]],
                       xo = seq(min(df_interp$Long), max(df_interp$Long), length.out = 300),
                       yo = seq(min(df_interp$Lat), max(df_interp$Lat), length.out = 300))
  
  # Conversão para raster
  r <- rast(interp_res)
  crs(r) <- "EPSG:4326"
  
  # Recorando raster para área de estudo (mais bonitinho)
  r_masked <- mask(crop(r, sw_ocean), sw_ocean)
  names(r_masked) <- var_name # Renomeando os rasters
  
  return(r_masked)
  
})


### Empilhando todos os rasters 
rstack <- rast(raster_list)
names(rstack)


#### Visualizando as abundâncias preditas
par(mfrow = c(1, 2))

plot(rstack[[1]], col = viridis(100, option = "A"), main = 'Abundância média predita')
plot(st_geometry(sw_ocean), add = TRUE, border = "black")

plot(rstack[[2]], col = viridis(100, option = "A"), main = 'Desvio padrão')
plot(st_geometry(sw_ocean), add = TRUE, border = "black")



plot(rstack[[3]], col = viridis(100, option = "A"), main = 'limite inferior do IC-95%')
plot(st_geometry(sw_ocean), add = TRUE, border = "black")


plot(rstack[[4]], col = viridis(100, option = "A"), main = 'limite superior do IC-95%')
plot(st_geometry(sw_ocean), add = TRUE, border = "black")

par(mfrow = c(1, 1))



#### Visualizando o efeito espacial

par(mfrow = c(1, 2))

plot(rstack[[5]], col = viridis(100, option = "A"), main = 'Média do efeito espacial')
plot(st_geometry(sw_ocean), add = TRUE, border = "black")

plot(rstack[[6]], col = viridis(100, option = "A"), main = 'Desvio padrão do efeito espacial')
plot(st_geometry(sw_ocean), add = TRUE, border = "black")



plot(rstack[[7]], col = viridis(100, option = "A"), main = 'Limite inferior do IC-95% do efeito espacial')
plot(st_geometry(sw_ocean), add = TRUE, border = "black")


plot(rstack[[8]], col = viridis(100, option = "A"), main = 'Limite superior do IC-95% do efeito espacial')
plot(st_geometry(sw_ocean), add = TRUE, border = "black")

par(mfrow = c(1, 1))




# Convert to sf object
pred.sf <- st_as_sf(pred.df, coords = c("X", "Y"), crs = CRS_lea)


ggplot() +
  geom_sf(data = pred.sf, aes(color = pred.mean), size = 1) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() 


# Interpoalte results
library(akima)

## predictions

### expand grid
i.pred.mean <- with(pred.df, interp(Long, Lat, pred.mean, nx=200, ny=200))
i.pred.sd <- with(pred.df, interp(Long, Lat, pred.sd, nx=200, ny=200))
i.pred.025 <- with(pred.df, interp(Long, Lat, pred.025, nx=200, ny=200))
i.pred.975 <- with(pred.df, interp(Long, Lat, pred.975, nx=200, ny=200))



### Convert to dataframe for ggplot
i.pred.mean <- data.frame(expand.grid(Long = i.pred.mean$x, Lat = i.pred.mean$y), 
                          predictions = as.vector(i.pred.mean$z))

i.pred.sd <- data.frame(expand.grid(Long = i.pred.sd$x, Lat = i.pred.sd$y), 
                        predictions = as.vector(i.pred.sd$z))

i.pred.025 <- data.frame(expand.grid(Long = i.pred.025$x, Lat = i.pred.025$y), 
                         predictions = as.vector(i.pred.025$z))

i.pred.975 <- data.frame(expand.grid(Long = i.pred.975$x, Lat = i.pred.975$y), 
                         predictions = as.vector(i.pred.975$z))



## spatial effect

### expand grid
i.sp.mean <- with(pred.df, interp(Long, Lat, sp.mean, nx=200, ny=200))
i.sp.sd <- with(pred.df, interp(Long, Lat, sp.sd, nx=200, ny=200))
i.sp.025 <- with(pred.df, interp(Long, Lat, sp.025, nx=200, ny=200))
i.sp.975 <- with(pred.df, interp(Long, Lat, sp.975, nx=200, ny=200))


### Convert to dataframe for ggplot
i.sp.mean <- data.frame(expand.grid(Long = i.sp.mean$x, Lat = i.sp.mean$y), 
                        predictions = as.vector(i.sp.mean$z))

i.sp.sd <- data.frame(expand.grid(Long = i.sp.sd$x, Lat = i.sp.sd$y), 
                      predictions = as.vector(i.sp.sd$z))

i.sp.025 <- data.frame(expand.grid(Long = i.sp.025$x, Lat = i.sp.025$y), 
                       predictions = as.vector(i.sp.025$z))

i.sp.975 <- data.frame(expand.grid(Long = i.sp.975$x, Lat = i.sp.975$y), 
                       predictions = as.vector(i.sp.975$z))




# Plot spatial rpedictionx§ of bycatch
ggplot() +
  geom_raster(data = i.pred.975, aes(x = Long, y = Lat, fill = predictions)) + 
  geom_sf(data = land, fill = "grey70", color = "black") +  # Add land as grey
  #scale_fill_viridis_c(option = "viridis") +
  fishualize::scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_minimal() 


# Plot spatial effect
ggplot() +
  geom_raster(data = i.sp.mean, aes(x = Long, y = Lat, fill = predictions)) + 
  geom_sf(data = land, fill = "grey70", color = "black") +  # Add land as grey
  #scale_fill_viridis_c(option = "viridis") +
  fishualize::scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_minimal() 


ggplot() +
  geom_raster(data = i.sp.sd, aes(x = Long, y = Lat, fill = predictions)) + 
  geom_sf(data = land, fill = "grey70", color = "black") +  # Add land as grey
  #scale_fill_viridis_c(option = "viridis") +
  fishualize::scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_minimal() 


# pgrid <- inla.mesh.projector(mesh,dims=c(250,250))
# 
# prd.m <- inla.mesh.project(pgrid,res$summary.random$spat$mean) # média do GMRF
# prd.s <- inla.mesh.project(pgrid,res$summary.random$spat$sd) # desvio padrão do GMRF
# 
# 
# ms.m <- inla.mesh.project(pgrid,res$summary.fitted.values[index,1]) # média do preditor linear
# sd.s <- inla.mesh.project(pgrid,(res$summary.fitted.values[index,2])) # desv. padrão do preditor
# 
# 
# gridExtra::grid.arrange(levelplot(ms.m, scales=list(draw=F), xlab='', ylab='', main='mean',col.regions = heat.colors(16)),
#                         levelplot(sd.s, scal=list(draw=F), xla='', yla='', main='sd' ,col.regions = heat.colors(16)), nrow=1)
# 
# 
# 
# x <- as.matrix(mcoords)
# z <- as.matrix(exp(ms.m))
# 
# tt <- rasterize(x, bat, field=z, fun='last', background=0)
# plot(tt)



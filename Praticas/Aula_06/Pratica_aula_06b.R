#################################################
#                                               #
#             Prática da aula 06                #
#      - Processando dados ambientais -         #
#                                               #
#################################################



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo o diretório de trabalho
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-LVM/")


#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(terra)



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando os dados ambientais
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1) Global Ocean Physics Reanalysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# thetao_depth: SST (sea surface temperature)
# zos: SSH (sea surface height)
# mlotst: MLT (mixed layer thickness)
# deptho: BATHY (bathymetry)

## Variáveis físicas
vfisicas <- rast("Dados/GIS/Environmental/Global_Ocean_Physics_Reanalysis_2000_2001_monhtly_surface_layers.nc") 

## Batimetria
batimetria <- rast("Dados/GIS/Environmental/bathymetry.nc")


## Explorando a estrutura dos dados
names(vfisicas) 
head(vfisicas)

names(batimetria)
head(batimetria)


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



#########################################
#                                       #
#         Preparando os dados           #
#                                       #
#########################################

# Load packages
#~~~~~~~~~~~~~~~~~
library(dplyr)
library(sf)
library(spData)
library(MASS)



# Set general WD
#~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Orientacoes/ICCAT/")



# Load data
#~~~~~~~~~~~~~
dados <- read.csv("Dados/iccat.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filter & manipulate data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Filter only one year
#~~~~~~~~~~~~~~~~~~~~~~~
dados <- filter(dados, Year == "2003")



# Filter subset of species
#~~~~~~~~~~~~~~~~~~~~~~~~~~

## Species to filter out
#"Acanthocybium_solandri"
#"Teleost"

out <- c("Thunnus_alalunga", "Thunnus_obesus","Thunnini",
         "Tetrapturus_pfluegeri","Tetrapturus_sp","Istiophoridae",
         "Barracudas","Sphyrna_spp",
         "Alopias_supeciliosus", "Carcharhinus_longimanus",
         "Carcharhinus_signatus","Carcharinus_sp","Pseudocarcharias_kamoharai")



dados <- dados[, !(names(dados) %in% out)]




# Filter out subset of countries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# only top 4 countries
dados$Flag

dados <- filter(dados, Flag == c("BRA", "ESP", "URY", "HND"))



# Filter out subset of vessels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# only most common vessels

vessels <- dados %>% 
  count(Vessel) %>% 
  filter(n > 30) %>%
  dplyr::select(Vessel)


dados <- filter(dados, Vessel %in% vessels$Vessel)



# Filter out non-relevant columns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out2 <- c("Hpb", "Set_Date", "Set_Launch_Start",
          "Set_Launch_End", "Longline")

dados <- dados[, !(names(dados) %in% out2)]



# Change factor levels where appropriate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(dados$Flag)

dados$Flag <- as.factor(dados$Flag)
levels(dados$Flag) <- c("A", "B", "C")



# Change column names do portuguese
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colnames(dados)[c(1:5, 19)] <- c("Ano", "Mes", "N_anzol", "Barco", "Pais", "Teleosteos")



# Simulate abundances as function of long/lat
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All species should show some spatial dependency.
# For simplicty, targeted species will all located in one hotspot area
# whereas non-targeted species can exhibit a different hotspot.
# Also, non-targeted species will be more zero-inflated

set.seed(123)


#### Fishing effort hotspots ####
effort_centers <- list(c(-35, 0),   # hotspot 1 (e.g. tropical Atlantic)
                       c(-35, -20)) # hotspot 2 (southern)


# simulate fishing sets clustered around hotspots
n <- nrow(dados)
effort_assign <- sample(1:2, size = n, replace = TRUE, prob = c(0.7, 0.7)) # more in hotspot1

coords <- do.call(rbind, lapply(1:n, function(i){
  mvrnorm(1, mu = effort_centers[[effort_assign[i]]], Sigma = matrix(c(20,0,0,20),2,2))
}))

dados$Long <- coords[,1]
dados$Lat  <- coords[,2]


### Simulate species abundances ###
target_species <- c("Thunnus_albacares", "Thunnus_obesus", "Thunnus_alalunga",
                    "Thunnus_atlanticus", "Katsuwonus_pelamis", 
                    "Thunnini", "Prionace_glauca")

gaussian_intensity <- function(lon, lat, center, sd = 4){
  exp(-((lon - center[1])^2 + (lat - center[2])^2) / (2*sd^2))
}


species <- colnames(dados[,8:27])


for(col in species){
  
  zero_prob <- if(col %in% target_species) runif(1, 0.1, 0.4) else runif(1, 0.4, 0.9)
  zeros <- rbinom(n, 1, zero_prob)
  
  if(col %in% target_species){
    # abundance linked to fishing effort hotspot1
    intensity <- gaussian_intensity(dados$Long, dados$Lat, effort_centers[[1]])
    lambda <- 40 * intensity + 5
  } else {
    if(runif(1) < 0.5){
      # overlaps with target species
      intensity <- gaussian_intensity(dados$Long, dados$Lat, effort_centers[[1]])
      lambda <- 10 * intensity + 2
    } else {
      # separate distribution around hotspot2
      intensity <- gaussian_intensity(dados$Long, dados$Lat, effort_centers[[2]])
      lambda <- 8 * intensity + 1
    }
  }
  
  counts <- rpois(n, lambda = lambda)
  dados[[col]] <- ifelse(zeros == 1, 0, counts)
}


### Visualize on a map
library(sf)

## CRS
CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

dfs <- st_as_sf(x = dados,                         
                coords = c("Long", "Lat"),
                crs = CRS)

data("world")    

world %>% 
  filter(continent %in% c("Africa","South America")) %>%
  ggplot() +
  geom_sf() + 
  geom_sf(data = dfs, shape = 19, col = "red", size = 1) 


### Remove samples on lan
# 6. Check for samples taken on land
######################################

## Load world shapefile
land <- st_read("~/OneDrive/Arbeit/Projects/Sharks_ICCAT/GIS/ne_10m_land/ne_10m_land.shp")
land <- st_crop(land, 
                xmin = min(dados$Long)-20,
                xmax = max(dados$Long)+20, 
                ymin = min(dados$Lat), 
                ymax = max(dados$Lat))


## Convert df to spdf
spdf <- dados %>% st_as_sf(coords=c('Long','Lat'), crs=st_crs(land))


# Keep only points that do NOT intersect with land
spdf2 <- spdf[lengths(st_intersects(spdf, land)) == 0,] #spdf2 <- spdf[!st_intersects(spdf, land, sparse = FALSE), ]


# Print summary
print(paste("Original points:", nrow(spdf)))
print(paste("Filtered points:", nrow(spdf2)))
print(paste("Removed points:",nrow(spdf)-nrow(spdf2)))

# Quick'n dirty visualization
plot(st_geometry(land), col= 'gray70')
plot(spdf, col = "red", pch= 19, cex= 0.5, add=T)
plot(spdf2, col = "blue", pch= 19, cex= 0.5, add=T)


dados2 <- st_drop_geometry(spdf2)
coords <- st_coordinates(spdf2)


dados2[, c("lon_sim", "lat_sim", "geometry")] <- NULL

dados2 <- cbind(dados2, coords)



## More customization
colnames(dados2)[26:33] <- c("sst", "sal", "chla", "depth", "dlm", "il", "Long", "Lat")


dados2 <- dados2[ , c(1:5,32,33,6:31)] 



# Save dataset
saveRDS(dados2, "~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/Dados/fisheries2.rds")

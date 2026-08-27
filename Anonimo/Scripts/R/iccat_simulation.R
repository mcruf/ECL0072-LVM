#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                         #
#       Simulate ICCAT data example       #
#                                         #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(sf)
library(mapview)
library(dplyr)
set.seed(1234)

setwd("~/OneDrive/Arbeit/Supervision/ICCAT/")

## Load data
df <- readRDS("Dados/iccat_updated_aug26.rds")

## Load ICCAT files
subarea <- st_read("Dados/GIS/ICCAT/ICCAT_gis.gpkg", layer = "iccat_subareas")
atsw <- filter(subarea, CODE == "AT-SW")


## Save  atlantic south-west subarea
#write_sf(obj = atsw, 
#         dsn = "~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-LVM/Dados/GIS/ICCAT/iccat_atsw.shp")


dfs <- filter(df, Year %in% c(2001, 2002), Flag %in% c("BRA", "ESP", "HND", "TAI", "VCT"))
table(dfs$Year, dfs$Month)

dados_sp <- st_as_sf(dfs, coords = c('Long','Lat'), crs = 4326) #As coordenadas estão originalmente em LAT/LONG; portanto, usar primeiro o CRS padrão (WGS84)



## Keep points only within AT-SW
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dados_sp2 <- dados_sp %>% 
            st_filter(atsw, .predicate = st_intersects) %>% 
            group_by(Year, Month) %>%
            group_modify(~{n_keep <- min(nrow(.x), sample(12:38, 1))
            slice_sample(.x, n = n_keep)}) %>%
            droplevels() %>% 
            st_as_sf() 
           

# plot(st_geometry(atsw))
# plot(st_geometry(dados_sp2),pch=19, col =dados_sp2$Year, add=T)


## Clean-up dataset
#~~~~~~~~~~~~~~~~~~~

## Select columns
colSums(dfsub[,15:53])
colnames(dados_sp2)

COLSKEEP <- c("Year", "Month", "Id", "Vessel", "Flag", "Nhooks", 
              "Thunnus_albacares", "Thunnus_obesus","Thunnus_alalunga",
              "Xiphias_gladius", "Istiophorus_albicans","Makaira_nigricans","Istiophoridae",
              "Coryphaena_hippurus","Acanthocybium_solandri","Prionace_glauca","Sphyrna_lewini",
              "Alopias_superciliosus", "Carcharhinus_falciformis","Carcharhinus_sp","Other_fish_unclassified")

dados_sp2 <- dados_sp2[ , COLSKEEP]



## Change vessel names
levels(dados_sp2$Vessel) <- paste("V", 1:nlevels(dados_sp2$Vessel), sep = "")


## Change fishery ID
levels(dados_sp2$Id) <- as.factor(1:nrow(dados_sp2))


## Change Year ID
dados_sp2$Year <- as.factor(dados_sp2$Year)
levels(dados_sp2$Year) <- c("1", "2")



## Retrieve long/lat from fisheries
coords <- st_coordinates(dados_sp2)

dados_sp2$Long <- coords[,1]
dados_sp2$Lat <- coords[,2]

data <- dados_sp2 %>%
        st_drop_geometry()


## Add small perturbation to catches
species <- c("Thunnus_albacares", "Thunnus_obesus", "Thunnus_alalunga",
  "Xiphias_gladius", "Istiophorus_albicans",
  "Makaira_nigricans", "Istiophoridae",
  "Coryphaena_hippurus", "Acanthocybium_solandri",
  "Prionace_glauca", "Sphyrna_lewini",
  "Alopias_superciliosus", "Carcharhinus_falciformis",
  "Carcharhinus_sp", "Other_fish_unclassified")


# data_sim <- data %>%
#             mutate( across(all_of(species),
#                      ~ pmax(0, .x + sample(-30:80, length(.x), replace = TRUE))))


mult <- runif(nrow(data), 0.8, 1.2)

data_sim <- data %>%
            ungroup() %>%
            mutate(mult = runif(n(), 0.4, 2),
            across(all_of(species), ~ round(pmax(0, .x * mult)))) %>%
            select(-mult)
          
plot(data_sim$Thunnus_alalunga~data$Thunnus_alalunga)


## Add perturbation to location
data_sim <- data_sim %>%
              mutate(Long = Long + rnorm(n(), 0, 1.5),
                     Lat  = Lat  + rnorm(n(), 0, 1))


plot(data_sim$Thunnus_alalunga~data_sim$Lat)


## Add perturbation to fishing effort
data_sim <- data_sim %>%
            mutate(Nhooks = round(Nhooks * runif(n(), 0.9, 1.1)))


summary(data_sim$Nhooks)
boxplot(data_sim$Nhooks~data_sim$Month)



data_simsp <- st_as_sf(data_sim, coords = c('Long','Lat'), crs = 4326) #As coordenadas estão originalmente em LAT/LONG; portanto, usar primeiro o CRS padrão (WGS84)

plot(st_geometry(atsw))
plot(st_geometry(data_simsp),pch=19, col = 2, add=T)


## Save data
saveRDS(data_sim, "~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-LVM/Dados/fisheries.rds")



###################################################################################
#                                                                                 #
#                                 Prática da aula 06                              #
#                       - Explorando dados espaço-temporais -                      #
#                                                                                 #
####################################################################################





#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
#library(terra) #Processamento de rasters
library(dplyr) #Processamento doas dados
library(sf) #Processamento de dados (espaciais) vetorizados - ie., shapefiles
library(rnaturalearth) #Baixar mapas do Natural Earth (https://www.naturalearthdata.com/)
library(ggplot2) #Visualização dos dados
library(patchwork) #Visualização de dados


#~~~~~~~~~~~~~~~~~~~~~~
# Definindo tema ggplot
#~~~~~~~~~~~~~~~~~~~~~~
my_theme <- theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, face = 'bold'),
                  legend.title = element_blank(),
                  axis.text.x = element_text(colour = "grey20", margin = margin(t = 10)),
                  axis.text.y = element_text(colour = "grey20", margin = margin(r = 10)),
                  axis.title = element_text(face = "bold", colour = "black", size = 16),
                  strip.background = element_rect(fill = 'white'),
                  legend.background = element_rect(fill = "transparent", colour = NA),
                  legend.box.background = element_rect(fill = "transparent", colour = NA),
                  axis.line = element_line(color = "black"),
                  plot.margin = margin(5, 10, 5, 5))



#~~~~~~~~~~~~~~~~~
# Input padrão
#~~~~~~~~~~~~~~~~~
CRS <- 4326 #Coordenada padrão (WGS84 - long/lat)

  


#><><><><><><><><><><><><><><><><><


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
# https://www.naturalearthdata.com/

## Baixando mapa mundi
world <- ne_countries(scale = 50, returnclass = "sf") #Scale options: 110 (low), 50 (medium), 10 (high)
head(world)

### Visualizando...
plot(st_geometry(world), col = "gray90", graticule=T)

ggplot(data = world) +
  geom_sf(fill = "gray40", color = "white") +
  theme_minimal()


## Recortando mapa mundi para área de estudo (South Atlantic Ocean - SAO)
extent_bbox <- st_bbox(c(xmin = -70, xmax = -12,  ymin = -50, ymax = 5), 
                       crs = st_crs(4326)) #Definindo área de recorte

sao <- st_crop(world, extent_bbox) #Recortando para SAO 


## Visualizando...
ggplot(data = sao) +
  geom_sf(fill = "gray40", color = "white") +
  theme_minimal()




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
levels(df$MonthYear) #Oops, precisamos ordenar os níveis


# 2.4) Ordenando 'mes-ano' por ordem cronológica
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tlvls <- df %>%
         arrange(Year, Month) %>%
         pull(MonthYear) %>%
         unique()

df$MonthYear <- factor(df$MonthYear,
                       levels = Tlvls,
                       ordered = TRUE)

levels(df$MonthYear) #Sanity check



# 2.5) Transformando os dados para objeto sf (espacial)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CUIDADO: a função st_as_sf NÃO reprojeta dados; apenas informa a projeção original na qual as coordenadas dos dados foram coletadas.
dfsp <- st_as_sf(df, 
                 coords = c('Long','Lat'), 
                 crs = CRS) #As coordenadas estão originalmente em LAT/LONG; portanto, usar primeiro o CRS padrão (WGS84)

class(dfsp)
head(as.data.frame(dfsp))


## Visualizando...
ggplot(dfsp) +
  geom_sf(data = sao,fill = "gray40", color = "white") +
  geom_sf(col = '#e07a5f') +
  theme_minimal() #Oops! Há dados em ambiente terreste - precisam ser removidos!



# 2.6) Removendo registros em terra
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#dfsp2 <- st_filter(dfsp , sao, .predicate = st_disjoint)
dfsp2 <- dfsp2[lengths(st_intersects(dfsp2, sao)) == 0,] 
#rmvd <- dfsp2[lengths(st_intersects(dfsp2, sao)) == 1,] #Points em terra

## Visualizando
ggplot(dfsp2) +
  geom_sf(data = sao,fill = "gray40", color = "white") +
  geom_sf(col = '#e07a5f') +
  theme_minimal() #Oops! Há dados em ambiente terreste - precisam ser removidos!


print(paste("No. pontos removidos:", nrow(dfsp) - nrow(dfsp2)))



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
mean(dfsp2$Prionace_glauca == 0) #Proporção de zeros; ok

#sort(apply(df[,9:23], MARGIN = 2, FUN = function(x) mean(x == 0))) #Proporção de zeros para as demais espécies. Maioria é zero-inflacionada - típico de dados comunitários!



# 3.1) Dinâmica espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot() +
  geom_sf(data = dfsp2, aes(size = Prionace_glauca, color = Prionace_glauca), alpha = 0.5) +
  geom_sf(data = sao, fill = "gray40", color = "white") +
  scale_color_viridis_c(option = "D") +
  guides(size = "none") +
  theme_light() 







# 2.3) Distribuição temporal das amostras 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Importante para correlação temporal
# amostra = pescaria
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
 




# 2.4) Distribuição temporal das capturas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 2.4.1) Dinâmica anual
p3 <- ggplot(df, aes(x = Year, y = Prionace_glauca)) +
  geom_boxplot(fill = "#e07a5f", alpha =.7) +
  theme_bw(base_size = 14) +
  labs(x = "Ano",y = "Abundância total (N)")  


## 2.4.2) Dinâmica mensal (há sazonalidade?)
p4 <- ggplot(df, aes(x = Month, y = Prionace_glauca)) +
  geom_boxplot(fill = "#e07a5f", alpha =.7) +
  theme_bw(base_size = 14) +
  labs(x = "Mês",y = "Abundância total (N)")  


### Visualizando...
p3 + p4



## 2.4.3) Dinâmica mensal (há sazonalidade?)
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



# 2.5) Distribuição espacial das capturas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##### BD Peixe-leC#o ####

##### Instalando e abrindo pacotes #####

if(!require(readxl)){install.packages('readxl'); library(readxl)}
if(!require(plyr)){install.packages('plyr'); library(plyr)}
if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)}
if(!require(dplyr)){install.packages('dplyr'); library(dplyr)}


##### Abrindo Banco de dados #####

df <- readr::read_csv("C:/Users/Dell/Documents/analises_lionfish/BD_lionfish_biometria.csv", na = c("na","NA"))

##### Analises exploratorias do BD

# Estrutura (tipos de dados de cada coluna)
str(df)

# Criando nova coluna de estrato de profundidade

df$estrato <- ifelse(df$prof_m <= 30, "raso", "fundo")

##### Convertendo colunas para fator

df$uc   <- as.factor(df$uc)
df$data  <- as.factor(df$data)
df$site <- as.factor(df$site)
df$expo <- as.factor(df$expo)
df$estrato <- as.factor(df$estrato)

# Conferir se deu certo
str(df)

#### Transformando categorias de exposiC'C#o, uc, e estrato em formato binC!rio

df$cat_uc <- ifelse(df$uc == "PARNAFN",1, 0) %>% 
  as.factor()

df$cat_expo <- ifelse(df$expo == "dentro",1, 0) %>% 
  as.factor()

df$cat_estrato <- ifelse(df$estrato == "raso",1, 0) %>% 
  as.factor()

#### CorrelaC'C#o entre comprimento total e estrato

#nuvem de pontos
df %>%
  filter(!is.na(ct_cm) & !is.na(cat_estrato)) %>%  #tirando NA 
  ggplot(aes(x = ct_cm, y = cat_estrato)) +
  geom_point(size = 4, col = 'gray50', alpha = 0.25) +
  theme_minimal()

#boxplot
df %>%
  filter(!is.na(ct_cm) & !is.na(cat_estrato)) %>% 
  ggplot(aes(x = cat_estrato, y = ct_cm)) +
  geom_boxplot(fill = "cyan3", color = "cyan4", alpha = 0.6, outlier.color = "darkgoldenrod1") +
  theme_minimal() +
  labs(x = "Estrato de profundidade", y = "Comprimento total (cm)",
       title = "Comprimento x estrato")

#### CorrelaC'C#o entre comprimento total e expo

df %>%
  filter(!is.na(ct_cm) & !is.na(cat_expo)) %>%  #tirando NA 
  ggplot(aes(x = ct_cm, y = cat_expo)) +
  geom_point(size = 4, col = 'gray50', alpha = 0.25) +
  theme_minimal()

#boxplot

df %>%
  filter(!is.na(ct_cm) & !is.na(cat_expo)) %>% 
  ggplot(aes(x = cat_expo, y = ct_cm)) +
  geom_boxplot(fill = "cyan3", color = "cyan4", alpha = 0.6, outlier.color = "darkgoldenrod1") +
  theme_minimal() +
  labs(x = "Exposi????o", y = "Comprimento total (cm)",
       title = "Comprimento x ExposiC'C#o")


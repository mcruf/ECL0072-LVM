# GLM para dados binários ----

# Carregando os pacotes necessários
library(ggplot2) # para plotar
library(dplyr) #manipular os dados
library(DHARMa) #avaliação dos residuos
library(performance) #avaliação dos residuos

# Fonte de dados ----
dados <- read.csv("C:/Users/bvcg1/OneDrive/Área de Trabalho/Relatório disciplina Marie/binomial/dados_ocorrencia_cap2.csv")
str(dados)
dados <- subset(dados, select = c(-X,-X.1,-area_habitat,-n_frags,-cv_area_frags,-cv_dist_eucl,-shape_cv,-mean_cdi,-cv_cdi,-prop_cdi)) #removendo a coluna "X                
str(dados)             
write.csv(dados, "C:/Users/bvcg1/OneDrive/Área de Trabalho/Relatório disciplina Marie/binomial/relat_dados_ocorrencia_cap2.csv")
#Não devo usar o ID da paisagem e/ou tamanho do buffer
cols <- c("ID", "buffer")
dados[, cols] <- lapply(dados[, cols], factor)


# Análise exploratória ----
# Escolhendo o buffer de 3000 para começar a ver as correlações 
b3000 <- subset(dados, buffer== "3000")
b3000_occur <- subset(b3000, situacao== "presenca")
b3000_absc <- subset(b3000, situacao== "ausencia")
hist(b3000_occur$prop_habitat)
hist(b3000_absc$prop_habitat)
hist(b3000_occur$dens_frags)
hist(b3000_absc$dens_frags)
hist(b3000_occur$med_dist_eucl)
hist(b3000_absc$med_dist_eucl)
hist(b3000_occur$coesao)
hist(b3000_absc$coesao)

#### Correlation matrix
nrow(b3000)
names(b3000)
ncol(b3000)
M <- cor(b3000[4:18], use = "complete.obs")
round(M,2)
head(round(M,2))

library(corrplot)
corrplot(M, method = 'ellipse', diag = FALSE, type = 'lower')

str(dados)
dados$situacao <- ifelse(dados$situacao == "presenca", 1, 0)

b399 <- subset(dados, buffer== "399")
table(b399$situacao)
M <- cor(b399[4:18], use = "complete.obs")
round(M,2)
head(round(M,2))
corrplot(M, method = 'ellipse', diag = FALSE, type = 'lower')

b688 <- subset(dados, buffer== "688")
table(b688$situacao)

b977 <- subset(dados, buffer== "977")
table(b977$situacao)

b1266 <- subset(dados, buffer== "1266")
table(b1266$situacao)

b1555 <- subset(dados, buffer== "1555")
table(b1555$situacao)

b1844 <- subset(dados, buffer== "1844")
table(b1844$situacao)

b2133 <- subset(dados, buffer== "2133")
table(b2133$situacao)

b2422 <- subset(dados, buffer== "2422")
table(b2422$situacao)

b2711 <- subset(dados, buffer== "2711")
table(b2711$situacao)

b3000 <- subset(dados, buffer== "3000")
table(b3000$situacao)

# Modelos ----
# Vari?veis preditoras
predictors <- c("prop_florest", "dens_borda", "shape_mn", "clumpy")
response_variable <- "situacao"

# Criar todas as combina??es poss?veis de vari?veis preditoras
all_combinations <- lapply(1:length(predictors), function(r) combn(predictors, r, simplify = TRUE))
all_combinations

### Nulo ----
b399_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b399)
b688_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b688)
b977_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b977)
b1266_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b1266)
b1555_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b1555)
b1844_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b1844)
b2133_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b2133)
b2422_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b2422)
b2711_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b2711)
b3000_null_model <- glm(situacao ~ 1, family = binomial(link = logit), data = b3000)

### Univariados ----
# scale(prop_habitat)
b399_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b399)
b688_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b688)
b977_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b977)
b1266_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b1266)
b1555_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b1555)
b1844_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b1844)
b2133_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b2133)
b2422_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b2422)
b2711_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b2711)
b3000_FC <- glm(situacao ~ scale(prop_habitat), family = binomial(link = logit), data = b3000)

# scale(dens_borda)
b399_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b399)
b688_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b688)
b977_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b977)
b1266_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b1266)
b1555_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b1555)
b1844_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b1844)
b2133_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b2133)
b2422_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b2422)
b2711_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b2711)
b3000_EDG <- glm(situacao ~ scale(dens_borda), family = binomial(link = logit), data = b3000)

# scale(shape_mn)
b399_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b399)
b688_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b688)
b977_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b977)
b1266_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b1266)
b1555_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b1555)
b1844_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b1844)
b2133_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b2133)
b2422_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b2422)
b2711_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b2711)
b3000_SH <- glm(situacao ~ scale(shape_mn), family = binomial(link = logit), data = b3000)

# scale(clumpy)
b399_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b399)
b688_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b688)
b977_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b977)
b1266_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b1266)
b1555_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b1555)
b1844_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b1844)
b2133_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b2133)
b2422_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b2422)
b2711_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b2711)
b3000_CL <- glm(situacao ~ scale(clumpy), family = binomial(link = logit), data = b3000)


## Duas variáveis ----
# scale(prop_habitat) + scale(dens_borda)
b399_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b399)
b688_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b688)
b977_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b977)
b1266_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b1266)
b1555_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b1555)
b1844_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b1844)
b2133_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b2133)
b2422_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b2422)
b2711_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b2711)
b3000_FCeEDG <- glm(situacao ~ scale(prop_habitat) + scale(dens_borda), family = binomial(link = logit), data = b3000)

# scale(prop_habitat) + scale(shape_mn)
b399_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b399)
b688_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b688)
b977_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b977)
b1266_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b1266)
b1555_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b1555)
b1844_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b1844)
b2133_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b2133)
b2422_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b2422)
b2711_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b2711)
b3000_FCeSH <- glm(situacao ~ scale(prop_habitat) + scale(shape_mn), family = binomial(link = logit), data = b3000)

# scale(prop_habitat) + scale(clumpy)
b399_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b399)
b688_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b688)
b977_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b977)
b1266_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b1266)
b1555_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b1555)
b1844_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b1844)
b2133_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b2133)
b2422_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b2422)
b2711_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b2711)
b3000_FCeCL <- glm(situacao ~ scale(prop_habitat) + scale(clumpy), family = binomial(link = logit), data = b3000)

# scale(dens_borda) + scale(shape_mn)
b399_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b399)
b688_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b688)
b977_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b977)
b1266_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b1266)
b1555_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b1555)
b1844_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b1844)
b2133_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b2133)
b2422_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b2422)
b2711_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b2711)
b3000_EDGeSH <- glm(situacao ~ scale(dens_borda) + scale(shape_mn), family = binomial(link = logit), data = b3000)

# scale(dens_borda) + scale(clumpy)
b399_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b399)
b688_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b688)
b977_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b977)
b1266_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b1266)
b1555_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b1555)
b1844_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b1844)
b2133_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b2133)
b2422_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b2422)
b2711_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b2711)
b3000_EDGeCL <- glm(situacao ~ scale(dens_borda) + scale(clumpy), family = binomial(link = logit), data = b3000)

# scale(shape_mn) + scale(clumpy)
b399_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b399)
b688_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b688)
b977_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b977)
b1266_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b1266)
b1555_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b1555)
b1844_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b1844)
b2133_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b2133)
b2422_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b2422)
b2711_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b2711)
b3000_SHeCL <- glm(situacao ~ scale(shape_mn) + scale(clumpy), family = binomial(link = logit), data = b3000)

## Tres variaveis ----
# scale(prop_habitat) + scale(dens_borda) + scale(shape_mn)
b399_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b399)
b688_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b688)
b977_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b977)
b1266_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b1266)
b1555_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b1555)
b1844_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b1844)
b2133_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b2133)
b2422_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b2422)
b2711_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b2711)
b3000_FCeEDGeSH<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn), family = binomial(link = logit), data = b3000)

# scale(prop_habitat) + scale(dens_borda) + scale(clumpy)
b399_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b399)
b688_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b688)
b977_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b977)
b1266_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b1266)
b1555_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b1555)
b1844_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b1844)
b2133_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b2133)
b2422_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b2422)
b2711_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b2711)
b3000_FCeEDGeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(clumpy), family = binomial(link = logit), data = b3000)

# scale(prop_habitat) + scale(shape_mn) + scale(clumpy)
b399_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b399)
b688_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b688)
b977_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b977)
b1266_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b1266)
b1555_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b1555)
b1844_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b1844)
b2133_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b2133)
b2422_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b2422)
b2711_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b2711)
b3000_FCeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b3000)

# scale(dens_borda) + scale(shape_mn) + scale(clumpy)
b399_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b399)
b688_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b688)
b977_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b977)
b1266_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b1266)
b1555_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b1555)
b1844_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b1844)
b2133_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b2133)
b2422_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b2422)
b2711_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b2711)
b3000_EDGeSHeCL<- glm(situacao ~scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b3000)


## Quatro Variaveis ----

# scale(prop_habitat) + scale(dens_borda) + scale(shape_mn) + scale(clumpy)
b399_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b399)
b688_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b688)
b977_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b977)
b1266_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b1266)
b1555_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b1555)
b1844_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b1844)
b2133_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b2133)
b2422_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b2422)
b2711_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b2711)
b3000_FCeEDGeSHeCL<- glm(situacao ~ scale(prop_habitat)+scale(dens_borda)+scale(shape_mn)+scale(clumpy), family = binomial(link = logit), data = b3000)


## Tabela AIC ----
#### Completa ----
modelos <- list (b399_null_model,
                 b688_null_model,
                 b977_null_model,
                 b1266_null_model,
                 b1555_null_model,
                 b1844_null_model,
                 b2133_null_model,
                 b2422_null_model,
                 b2711_null_model,
                 b3000_null_model,
                 b399_FC,
                 b688_FC,
                 b977_FC,
                 b1266_FC,
                 b1555_FC,
                 b1844_FC,
                 b2133_FC,
                 b2422_FC,
                 b2711_FC,
                 b3000_FC,
                 b399_EDG,
                 b688_EDG,
                 b977_EDG,
                 b1266_EDG,
                 b1555_EDG,
                 b1844_EDG,
                 b2133_EDG,
                 b2422_EDG,
                 b2711_EDG,
                 b3000_EDG,
                 b399_SH,
                 b688_SH,
                 b977_SH,
                 b1266_SH,
                 b1555_SH,
                 b1844_SH,
                 b2133_SH,
                 b2422_SH,
                 b2711_SH,
                 b3000_SH,
                 b399_CL,
                 b688_CL,
                 b977_CL,
                 b1266_CL,
                 b1555_CL,
                 b1844_CL,
                 b2133_CL,
                 b2422_CL,
                 b2711_CL,
                 b3000_CL,
                 b399_FCeEDG,
                 b688_FCeEDG,
                 b977_FCeEDG,
                 b1266_FCeEDG,
                 b1555_FCeEDG,
                 b1844_FCeEDG,
                 b2133_FCeEDG,
                 b2422_FCeEDG,
                 b2711_FCeEDG,
                 b3000_FCeEDG,
                 b399_FCeSH,
                 b688_FCeSH,
                 b977_FCeSH,
                 b1266_FCeSH,
                 b1555_FCeSH,
                 b1844_FCeSH,
                 b2133_FCeSH,
                 b2422_FCeSH,
                 b2711_FCeSH,
                 b3000_FCeSH,
                 b399_FCeCL,
                 b688_FCeCL,
                 b977_FCeCL,
                 b1266_FCeCL,
                 b1555_FCeCL,
                 b1844_FCeCL,
                 b2133_FCeCL,
                 b2422_FCeCL,
                 b2711_FCeCL,
                 b3000_FCeCL,
                 b399_EDGeSH,
                 b688_EDGeSH,
                 b977_EDGeSH,
                 b1266_EDGeSH,
                 b1555_EDGeSH,
                 b1844_EDGeSH,
                 b2133_EDGeSH,
                 b2422_EDGeSH,
                 b2711_EDGeSH,
                 b3000_EDGeSH,
                 b399_EDGeCL,
                 b688_EDGeCL,
                 b977_EDGeCL,
                 b1266_EDGeCL,
                 b1555_EDGeCL,
                 b1844_EDGeCL,
                 b2133_EDGeCL,
                 b2422_EDGeCL,
                 b2711_EDGeCL,
                 b3000_EDGeCL,
                 b399_SHeCL,
                 b688_SHeCL,
                 b977_SHeCL,
                 b1266_SHeCL,
                 b1555_SHeCL,
                 b1844_SHeCL,
                 b2133_SHeCL,
                 b2422_SHeCL,
                 b2711_SHeCL,
                 b3000_SHeCL,
                 b399_FCeEDGeSH,
                 b688_FCeEDGeSH,
                 b977_FCeEDGeSH,
                 b1266_FCeEDGeSH,
                 b1555_FCeEDGeSH,
                 b1844_FCeEDGeSH,
                 b2133_FCeEDGeSH,
                 b2422_FCeEDGeSH,
                 b2711_FCeEDGeSH,
                 b3000_FCeEDGeSH,
                 b399_FCeEDGeCL,
                 b688_FCeEDGeCL,
                 b977_FCeEDGeCL,
                 b1266_FCeEDGeCL,
                 b1555_FCeEDGeCL,
                 b1844_FCeEDGeCL,
                 b2133_FCeEDGeCL,
                 b2422_FCeEDGeCL,
                 b2711_FCeEDGeCL,
                 b3000_FCeEDGeCL,
                 b399_FCeSHeCL,
                 b688_FCeSHeCL,
                 b977_FCeSHeCL,
                 b1266_FCeSHeCL,
                 b1555_FCeSHeCL,
                 b1844_FCeSHeCL,
                 b2133_FCeSHeCL,
                 b2422_FCeSHeCL,
                 b2711_FCeSHeCL,
                 b3000_FCeSHeCL,
                 b399_EDGeSHeCL,
                 b688_EDGeSHeCL,
                 b977_EDGeSHeCL,
                 b1266_EDGeSHeCL,
                 b1555_EDGeSHeCL,
                 b1844_EDGeSHeCL,
                 b2133_EDGeSHeCL,
                 b2422_EDGeSHeCL,
                 b2711_EDGeSHeCL,
                 b3000_EDGeSHeCL,
                 b399_FCeEDGeSHeCL,
                 b688_FCeEDGeSHeCL,
                 b977_FCeEDGeSHeCL,
                 b1266_FCeEDGeSHeCL,
                 b1555_FCeEDGeSHeCL,
                 b1844_FCeEDGeSHeCL,
                 b2133_FCeEDGeSHeCL,
                 b2422_FCeEDGeSHeCL,
                 b2711_FCeEDGeSHeCL,
                 b3000_FCeEDGeSHeCL)

mod_names <- c("b399_null_model",
               "b688_null_model",
               "b977_null_model",
               "b1266_null_model",
               "b1555_null_model",
               "b1844_null_model",
               "b2133_null_model",
               "b2422_null_model",
               "b2711_null_model",
               "b3000_null_model",
               "b399_FC",
               "b688_FC",
               "b977_FC",
               "b1266_FC",
               "b1555_FC",
               "b1844_FC",
               "b2133_FC",
               "b2422_FC",
               "b2711_FC",
               "b3000_FC",
               "b399_EDG",
               "b688_EDG",
               "b977_EDG",
               "b1266_EDG",
               "b1555_EDG",
               "b1844_EDG",
               "b2133_EDG",
               "b2422_EDG",
               "b2711_EDG",
               "b3000_EDG",
               "b399_SH",
               "b688_SH",
               "b977_SH",
               "b1266_SH",
               "b1555_SH",
               "b1844_SH",
               "b2133_SH",
               "b2422_SH",
               "b2711_SH",
               "b3000_SH",
               "b399_CL",
               "b688_CL",
               "b977_CL",
               "b1266_CL",
               "b1555_CL",
               "b1844_CL",
               "b2133_CL",
               "b2422_CL",
               "b2711_CL",
               "b3000_CL",
               "b399_FCeEDG",
               "b688_FCeEDG",
               "b977_FCeEDG",
               "b1266_FCeEDG",
               "b1555_FCeEDG",
               "b1844_FCeEDG",
               "b2133_FCeEDG",
               "b2422_FCeEDG",
               "b2711_FCeEDG",
               "b3000_FCeEDG",
               "b399_FCeSH",
               "b688_FCeSH",
               "b977_FCeSH",
               "b1266_FCeSH",
               "b1555_FCeSH",
               "b1844_FCeSH",
               "b2133_FCeSH",
               "b2422_FCeSH",
               "b2711_FCeSH",
               "b3000_FCeSH",
               "b399_FCeCL",
               "b688_FCeCL",
               "b977_FCeCL",
               "b1266_FCeCL",
               "b1555_FCeCL",
               "b1844_FCeCL",
               "b2133_FCeCL",
               "b2422_FCeCL",
               "b2711_FCeCL",
               "b3000_FCeCL",
               "b399_EDGeSH",
               "b688_EDGeSH",
               "b977_EDGeSH",
               "b1266_EDGeSH",
               "b1555_EDGeSH",
               "b1844_EDGeSH",
               "b2133_EDGeSH",
               "b2422_EDGeSH",
               "b2711_EDGeSH",
               "b3000_EDGeSH",
               "b399_EDGeCL",
               "b688_EDGeCL",
               "b977_EDGeCL",
               "b1266_EDGeCL",
               "b1555_EDGeCL",
               "b1844_EDGeCL",
               "b2133_EDGeCL",
               "b2422_EDGeCL",
               "b2711_EDGeCL",
               "b3000_EDGeCL",
               "b399_SHeCL",
               "b688_SHeCL",
               "b977_SHeCL",
               "b1266_SHeCL",
               "b1555_SHeCL",
               "b1844_SHeCL",
               "b2133_SHeCL",
               "b2422_SHeCL",
               "b2711_SHeCL",
               "b3000_SHeCL",
               "b399_FCeEDGeSH",
               "b688_FCeEDGeSH",
               "b977_FCeEDGeSH",
               "b1266_FCeEDGeSH",
               "b1555_FCeEDGeSH",
               "b1844_FCeEDGeSH",
               "b2133_FCeEDGeSH",
               "b2422_FCeEDGeSH",
               "b2711_FCeEDGeSH",
               "b3000_FCeEDGeSH",
               "b399_FCeEDGeCL",
               "b688_FCeEDGeCL",
               "b977_FCeEDGeCL",
               "b1266_FCeEDGeCL",
               "b1555_FCeEDGeCL",
               "b1844_FCeEDGeCL",
               "b2133_FCeEDGeCL",
               "b2422_FCeEDGeCL",
               "b2711_FCeEDGeCL",
               "b3000_FCeEDGeCL",
               "b399_FCeSHeCL",
               "b688_FCeSHeCL",
               "b977_FCeSHeCL",
               "b1266_FCeSHeCL",
               "b1555_FCeSHeCL",
               "b1844_FCeSHeCL",
               "b2133_FCeSHeCL",
               "b2422_FCeSHeCL",
               "b2711_FCeSHeCL",
               "b3000_FCeSHeCL",
               "b399_EDGeSHeCL",
               "b688_EDGeSHeCL",
               "b977_EDGeSHeCL",
               "b1266_EDGeSHeCL",
               "b1555_EDGeSHeCL",
               "b1844_EDGeSHeCL",
               "b2133_EDGeSHeCL",
               "b2422_EDGeSHeCL",
               "b2711_EDGeSHeCL",
               "b3000_EDGeSHeCL",
               "b399_FCeEDGeSHeCL",
               "b688_FCeEDGeSHeCL",
               "b977_FCeEDGeSHeCL",
               "b1266_FCeEDGeSHeCL",
               "b1555_FCeEDGeSHeCL",
               "b1844_FCeEDGeSHeCL",
               "b2133_FCeEDGeSHeCL",
               "b2422_FCeEDGeSHeCL",
               "b2711_FCeEDGeSHeCL",
               "b3000_FCeEDGeSHeCL")
library("AICcmodavg")
modelos_AIC <- aictab(cand.set = modelos, modnames = mod_names)
modelos_AIC <- data.frame(modelos_AIC)
modelos_AIC


summary(b399_FCeCL)
summary(b399$clumpy)
summary(b399$prop_habitat)
hist(b399$prop_habitat)

# Validação do modelo ----
library(DHARMa)

## Simulando os resíduos a partir do modelo escolhido ##
resid_sim <- simulateResiduals(b399_FCeCL, n=1000)

plot(resid_sim)


library(performance)
check_model(resid_sim)


## Teste de autocorrelação
check_autocorrelation(b399_FCeCL)
acf(residuals(b399_FCeCL))

# Analisando os resultados ----
summary(b399_FCeCL)

head(modelos_AIC)
summary(b399_EDGeCL)

citation("landscapemetrics")

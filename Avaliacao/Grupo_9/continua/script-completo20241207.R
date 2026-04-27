# CAPITULO 2 - An?lises densidade ####
# SEM CDI

library(ggplot2)
library(car)
library(corrplot)
library(raster)
library(rgdal)
library(stringr)
library(AICcmodavg)
library(landscapemetrics)

## AN?LISES ESTAT?STICAS----

## Selecao inicial das variaveis ----
dados_cap2 <- read.csv("G:/Bianca_mestrado/cap_2_versao_2/dados_cap2.csv")
head(dados_cap2)
dados_cap2$dens_groups <- dados_cap2$n_groups/dados_cap2$area_frag

b3000 <- subset(dados_cap2, buffer== "buffer3000")
b3000 <- subset(b3000, select = -X) #removendo a coluna "X

####Explorando os dados
par(mfrow = c(1, 1))
table((b3000$n_groups/b3000$area_frag))
hist((b3000$n_groups/b3000$area_frag))
hist(b3000$prop_florest)
hist(b3000$dens_frags)
plot(x = b3000$prop_florest, y=(b3000$n_groups/b3000$area_frag))
plot(x = b3000$dens_frags, y=(b3000$n_groups/b3000$area_frag))
plot(x = b3000$med_dist_eucl, y=(b3000$n_groups/b3000$area_frag)) #até aqui parece que quem estão "puxando" os resultados são o n_groups 8 e 11
plot(x = b3000$coesao, y=(b3000$n_groups/b3000$area_frag)) #aqui parece que mesmo excluindo o 8 e o 11 (n_groups) a relação ainda seria positiva

#### Correlation matrix
nrow(b3000)
names(b3000)
ncol(b3000)
M <- cor(b3000[5:19], use = "complete.obs")
round(M,2)
head(round(M,2))

#Visualizing the correlation matrix
library(corrplot)
corrplot(M, method= "color", tl.col = "black")
col<- colorRampPalette(c("#00441b", "#f7f7f7", "#40004b"))(20)
corrplot(M, method= "color", tl.col = "black", col=col)

#### Olhando para as relações entre as variáveis preditoras
library (car)
# Modelo cheio
modelo_cheio_b3000_1 <- glm((n_groups/area_frag) ~ area + prop_florest + n_frags + dens_frags + media_area_frags + cv_area_frags + med_dist_eucl + cv_dist_eucl + pcore + dens_borda + shape_mn + shape_cv + iagreg + clumpy + coesao, family = poisson(link = "log"), data = b3000)
summary(modelo_cheio_b3000_1)
vif_values <- vif(modelo_cheio_b3000_1) #todos valores de VIF muito altos
vif_values

# Olhando com cuidado para prop_florest porque é a melhor varável de habitat amount e excluindo outras variáveis que também indicam habitat amount e que estão relacionadas com ela
round(M,2)
# area - não vamos usar, pois quer dizer a mesma coisa, e por este motivo está altamente correlacionado com prop_forest
cor.test(b3000$prop_florest, b3000$area)
plot(b3000$prop_florest ~ b3000$area)

# Modelo sem area
modelo_b3000_2 <- glm((n_groups/area_frag) ~ prop_florest + n_frags + dens_frags + media_area_frags + cv_area_frags + med_dist_eucl + cv_dist_eucl + pcore + dens_borda + shape_mn + shape_cv + iagreg + clumpy + coesao, family = poisson(link = "log"), data = b3000)
vif_values <- vif(modelo_b3000_2) #todos valores de VIF muito altos
vif_values

names(b3000)
M <- cor(b3000[6:19], use = "complete.obs") #sem area
round(M,2)

# media_area_frags - não vamos usar, está altamente correlacionado com forest_cover
cor.test(b3000$prop_florest, b3000$media_area_frags)
plot(b3000$prop_florest ~ b3000$media_area_frags)
# Como tiramos e media da área dos fragmentos, então também vamos tirar o CV

# Modelo sem area, sem media_area_frags e sem cv_area_frags
modelo_b3000_3 <- glm((n_groups/area_frag) ~ prop_florest + n_frags + dens_frags + med_dist_eucl + cv_dist_eucl + pcore + dens_borda + shape_mn + shape_cv +  iagreg + clumpy + coesao, family = poisson(link = "log"), data = b3000)
vif_values <- vif(modelo_b3000_3) #todos valores de VIF muito altos
vif_values

names(b3000)
M <- cor(b3000[c(6:8, 11:19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags
round(M,2)

# pcore - não vamos usar, está altamente correlacionado com forest_cover
cor.test(b3000$prop_florest, b3000$pcore)
plot(b3000$prop_florest ~ b3000$pcore)

# Modelo sem area, sem media_area_frags e sem cv_area_frags, sem pcore
modelo_b3000_4 <- glm((n_groups/area_frag) ~ prop_florest + n_frags + dens_frags + med_dist_eucl + cv_dist_eucl + dens_borda + shape_mn + shape_cv + iagreg + clumpy + coesao, family = poisson(link = "log"), data = b3000)
vif_values <- vif(modelo_b3000_4) #todos valores de VIF muito altos
vif_values

names(b3000)
M <- cor(b3000[c(6:8, 11:12, 14:19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore
round(M,2)

# Olhando com cuidado pra geral, com foco nas medidas de fragmentação, ou naquelas que estão mais correlacionadas
# n_frags - não vamos usar pois tem alto VIF e está relacionada com dens_frags
cor.test(b3000$n_frags, b3000$dens_frags)
plot(b3000$prop_florest ~ b3000$pcore)
corrplot(M, method= "color", tl.col = "black")

# Modelo sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem n_frags
modelo_b3000_5 <- glm((n_groups/area_frag) ~ prop_florest + dens_frags + med_dist_eucl + cv_dist_eucl + dens_borda + shape_mn + shape_cv + iagreg + clumpy + coesao, family = poisson(link = "log"), data = b3000)
vif_values <- vif(modelo_b3000_5) #todos valores de VIF muito altos
vif_values

names(b3000)
M <- cor(b3000[c(6,8, 11:12, 14:19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem prop_cdi
round(M,2)
corrplot(M, method= "color", tl.col = "black")

# iagreg - não vamos usar pois tem o maior VIF e está relacionada com clumpy, coesao e prop_florest
cor.test(b3000$iagreg, b3000$clumpy)
cor.test(b3000$iagreg, b3000$coesao)
cor.test(b3000$iagreg, b3000$prop_florest)
plot(b3000$iagreg ~ b3000$clumpy)
plot(b3000$iagreg ~ b3000$coesao)
plot(b3000$iagreg ~ b3000$prop_florest)

# Modelo sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem n_frags, sem igreg
modelo_b3000_6 <- glm((n_groups/area_frag) ~ prop_florest + dens_frags + med_dist_eucl + cv_dist_eucl + dens_borda + shape_mn + shape_cv + clumpy + coesao, family = poisson(link = "log"), data = b3000)
vif_values <- vif(modelo_b3000_6) #todos valores de VIF muito altos
vif_values

names(b3000)
M <- cor(b3000[c(6,8, 11:12, 14:16, 18:19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg
round(M,2)
corrplot(M, method= "color", tl.col = "black")

# dens_borda - não vamos usar pois tem o maior VIF e está relacionada com med_dist_eucl e  prop_florest
cor.test(b3000$dens_borda, b3000$med_dist_eucl)
cor.test(b3000$dens_borda, b3000$prop_florest)
plot(b3000$dens_borda ~ b3000$med_dist_eucl)
plot(b3000$dens_borda ~ b3000$prop_florest)

# Modelo sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem n_frags, sem igreg, sem dens_borda
modelo_b3000_7 <- glm((n_groups/area_frag) ~ prop_florest + dens_frags + med_dist_eucl + cv_dist_eucl + shape_mn + shape_cv + clumpy + coesao, family = poisson(link = "log"), data = b3000)
vif_values <- vif(modelo_b3000_7) #todos valores de VIF muito altos
vif_values

names(b3000)
M <- cor(b3000[c(6,8, 11:12, 15:16, 18:19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M,2)
corrplot(M, method= "color", tl.col = "black")

# shape_cv - não vamos usar pois vamos usar só a média do shape
# Modelo sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem n_frags, sem igreg, sem dens_borda, sem shape_cv
modelo_b3000_8 <- glm((n_groups/area_frag) ~ prop_florest + dens_frags + med_dist_eucl + cv_dist_eucl + shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b3000)
vif_values <- vif(modelo_b3000_8) #todos valores de VIF muito altos
vif_values

names(b3000)
M <- cor(b3000[c(6,8, 11:12, 15, 18:19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M,2)
corrplot(M, method= "color", tl.col = "black")

# med_dist_eucl e cv_dist_eucl - não vamos usar pois tem o maior VIF e está relacionada com prop_florest
cor.test(b3000$med_dist_eucl, b3000$cv_dist_eucl)
cor.test(b3000$med_dist_eucl, b3000$prop_florest)
cor.test(b3000$cv_dist_eucl, b3000$prop_florest)
plot(b3000$med_dist_eucl ~ b3000$cv_dist_eucl)
plot(b3000$med_dist_eucl ~ b3000$prop_florest)
plot(b3000$cv_dist_eucl ~ b3000$prop_florest)

# Modelo sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem prop_cdi, sem n_frags, sem igreg, sem dens_borda, sem med_dist_eucl e cv_dist_eucl
modelo_b3000_9 <- glm((n_groups/area_frag) ~ prop_florest + dens_frags + shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b3000)
vif_values <- vif(modelo_b3000_9) #todos valores de VIF muito altos
vif_values

names(b3000)
M <- cor(b3000[c(6,8, 15, 18:19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M,2)
corrplot(M, method= "color", tl.col = "black")

# Continuando a seleção de variáveis, só que agora olhando para todas as escalas de paisagem
# Considerando apenas as variáveis pré-selecionadas na escala de 3000m (sentido conceitual e menos correlacionadas). São elas:
# prop_florest
# dens_frags
# clumpy
# coesao
# shape_mn

#### Subconjuntos de dados por buffer----
dados_cap2 <- read.csv("C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/dados_cap2.csv")
head(dados_cap2)
dados_cap2$dens_groups <- dados_cap2$n_groups/dados_cap2$area_frag

b399 <- subset(dados_cap2, buffer== "buffer399")
b399 <- subset(b399, select = -X) #removendo a coluna "X
b688 <- subset(dados_cap2, buffer== "buffer688")
b688 <- subset(b688, select = -X) #removendo a coluna "X
b977 <- subset(dados_cap2, buffer== "buffer977")
b977 <- subset(b977, select = -X) #removendo a coluna "X
b1266 <- subset(dados_cap2, buffer== "buffer1266")
b1266 <- subset(b1266, select = -X) #removendo a coluna "X
b1555 <- subset(dados_cap2, buffer== "buffer1555")
b1555 <- subset(b1555, select = -X) #removendo a coluna "X
b1844 <- subset(dados_cap2, buffer== "buffer1844")
b1844 <- subset(b1844, select = -X) #removendo a coluna "X
b2133 <- subset(dados_cap2, buffer== "buffer2133")
b2133 <- subset(b2133, select = -X) #removendo a coluna "X
b2422 <- subset(dados_cap2, buffer== "buffer2422")
b2422 <- subset(b2422, select = -X) #removendo a coluna "X
b2711 <- subset(dados_cap2, buffer== "buffer2711")
b2711 <- subset(b2711, select = -X) #removendo a coluna "X
b3000 <- subset(dados_cap2, buffer== "buffer3000")
b3000 <- subset(b3000, select = -X) #removendo a coluna "X

#### VIF 1----
modelo_b399 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b399)
vif_values_b399 <- vif(modelo_b399)
vif_values_b399
modelo_b688 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b688)
vif_values_b688 <- vif(modelo_b688)
vif_values_b688
modelo_b977 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b977)
vif_values_b977 <- vif(modelo_b977)
vif_values_b977
modelo_b1266 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b1266)
vif_values_b1266 <- vif(modelo_b1266)
vif_values_b1266
modelo_b1555 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b1555)
vif_values_b1555 <- vif(modelo_b1555)
vif_values_b1555
modelo_b1844 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b1844)
vif_values_b1844 <- vif(modelo_b1844)
vif_values_b1844
modelo_b2133 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b2133)
vif_values_b2133 <- vif(modelo_b2133)
vif_values_b2133
modelo_b2422 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b2422)
vif_values_b2422 <- vif(modelo_b2422)
vif_values_b2422
modelo_b2711 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b2711)
vif_values_b2711 <- vif(modelo_b2711)
vif_values_b2711
modelo_b3000 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy + coesao, family = poisson(link = "log"), data = b3000)
vif_values_b3000 <- vif(modelo_b3000)
vif_values_b3000

vif_buffers <- rbind(vif_values_b399, vif_values_b688, vif_values_b977, vif_values_b1266, vif_values_b1555, vif_values_b1844, vif_values_b2133, vif_values_b2422, vif_values_b2711, vif_values_b3000)
vif_buffers
write.csv(vif_buffers, "C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/vif1_completo_dens_20241207.csv")

# Fizemos os VIFs com as variáveis preditoras padronizadas scale() e os valores de VIF foram os mesmos. Só faz diferença no beta.

### Correlacoes----
names(b399)
M_399 <- cor(b399[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_399,2)
corrplot(M_399, method= "color", tl.col = "black")

a <-cor.test(b399$prop_florest, b399$dens_frags)
b399_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$prop_florest, b399$med_dist_eucl)
b399_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$prop_florest, b399$shape_mn)
b399_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$prop_florest, b399$clumpy)
b399_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$prop_florest, b399$coesao)
b399_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b399$dens_frags, b399$med_dist_eucl)
b399_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$dens_frags, b399$shape_mn)
b399_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$dens_frags, b399$clumpy)
b399_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$dens_frags, b399$coesao)
b399_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)


a <-cor.test(b399$med_dist_eucl, b399$shape_mn)
b399_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$med_dist_eucl, b399$clumpy)
b399_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$med_dist_eucl, b399$coesao)
b399_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b399$shape_mn, b399$clumpy)
b399_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b399$shape_mn, b399$coesao)
b399_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b399$clumpy, b399$coesao)
b399_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b399_cor <- rbind(b399_propflorest_densfrags,
                  b399_propflorest_disteucl,
                  b399_propflorest_shape,
                  b399_propflorest_clumpy,
                  b399_propflorest_coesao,
                  b399_densfrags_disteucl,
                  b399_densfrags_shape,
                  b399_densfrags_clumpy,
                  b399_densfrags_coesao,
                  b399_disteucl_shape,
                  b399_disteucl_clumpy,
                  b399_disteucl_coesao,
                  b399_shape_clumpy,
                  b399_shape_coesao,
                  b399_clumpy_coesao)
b399_cor <- data.frame(b399_cor)
names(b399_cor) <- c("cor", "p")

names(b688)
M_688 <- cor(b688[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_688,2)
corrplot(M_688, method= "color", tl.col = "black")

a <-cor.test(b688$prop_florest, b688$dens_frags)
b688_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$prop_florest, b688$med_dist_eucl)
b688_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$prop_florest, b688$shape_mn)
b688_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$prop_florest, b688$clumpy)
b688_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$prop_florest, b688$coesao)
b688_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b688$dens_frags, b688$med_dist_eucl)
b688_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$dens_frags, b688$shape_mn)
b688_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$dens_frags, b688$clumpy)
b688_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$dens_frags, b688$coesao)
b688_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b688$med_dist_eucl, b688$shape_mn)
b688_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$med_dist_eucl, b688$clumpy)
b688_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$med_dist_eucl, b688$coesao)
b688_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b688$shape_mn, b688$clumpy)
b688_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b688$shape_mn, b688$coesao)
b688_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b688$clumpy, b688$coesao)
b688_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b688_cor <- rbind(b688_propflorest_densfrags,
                  b688_propflorest_disteucl,
                  b688_propflorest_shape,
                  b688_propflorest_clumpy,
                  b688_propflorest_coesao,
                  b688_densfrags_disteucl,
                  b688_densfrags_shape,
                  b688_densfrags_clumpy,
                  b688_densfrags_coesao,
                  b688_disteucl_shape,
                  b688_disteucl_clumpy,
                  b688_disteucl_coesao,
                  b688_shape_clumpy,
                  b688_shape_coesao,
                  b688_clumpy_coesao)
b688_cor <- data.frame(b688_cor)
names(b688_cor) <- c("cor", "p")

names(b977)
M_977 <- cor(b977[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_977,2)
corrplot(M_977, method= "color", tl.col = "black")

a <-cor.test(b977$prop_florest, b977$dens_frags)
b977_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$prop_florest, b977$med_dist_eucl)
b977_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$prop_florest, b977$shape_mn)
b977_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$prop_florest, b977$clumpy)
b977_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$prop_florest, b977$coesao)
b977_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b977$dens_frags, b977$med_dist_eucl)
b977_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$dens_frags, b977$shape_mn)
b977_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$dens_frags, b977$clumpy)
b977_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$dens_frags, b977$coesao)
b977_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b977$med_dist_eucl, b977$shape_mn)
b977_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$med_dist_eucl, b977$clumpy)
b977_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$med_dist_eucl, b977$coesao)
b977_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b977$shape_mn, b977$clumpy)
b977_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b977$shape_mn, b977$coesao)
b977_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b977$clumpy, b977$coesao)
b977_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b977_cor <- rbind(b977_propflorest_densfrags,
                  b977_propflorest_disteucl,
                  b977_propflorest_shape,
                  b977_propflorest_clumpy,
                  b977_propflorest_coesao,
                  b977_densfrags_disteucl,
                  b977_densfrags_shape,
                  b977_densfrags_clumpy,
                  b977_densfrags_coesao,
                  b977_disteucl_shape,
                  b977_disteucl_clumpy,
                  b977_disteucl_coesao,
                  b977_shape_clumpy,
                  b977_shape_coesao,
                  b977_clumpy_coesao)
b977_cor <- data.frame(b977_cor)
names(b977_cor) <- c("cor", "p")

names(b1266)
M_1266 <- cor(b1266[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_1266,2)
corrplot(M_1266, method= "color", tl.col = "black")

a <-cor.test(b1266$prop_florest, b1266$dens_frags)
b1266_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$prop_florest, b1266$med_dist_eucl)
b1266_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$prop_florest, b1266$shape_mn)
b1266_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$prop_florest, b1266$clumpy)
b1266_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$prop_florest, b1266$coesao)
b1266_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1266$dens_frags, b1266$med_dist_eucl)
b1266_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$dens_frags, b1266$shape_mn)
b1266_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$dens_frags, b1266$clumpy)
b1266_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$dens_frags, b1266$coesao)
b1266_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1266$med_dist_eucl, b1266$shape_mn)
b1266_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$med_dist_eucl, b1266$clumpy)
b1266_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$med_dist_eucl, b1266$coesao)
b1266_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1266$shape_mn, b1266$clumpy)
b1266_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1266$shape_mn, b1266$coesao)
b1266_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1266$clumpy, b1266$coesao)
b1266_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b1266_cor <- rbind(b1266_propflorest_densfrags,
                   b1266_propflorest_disteucl,
                   b1266_propflorest_shape,
                   b1266_propflorest_clumpy,
                   b1266_propflorest_coesao,
                   b1266_densfrags_disteucl,
                   b1266_densfrags_shape,
                   b1266_densfrags_clumpy,
                   b1266_densfrags_coesao,
                   b1266_disteucl_shape,
                   b1266_disteucl_clumpy,
                   b1266_disteucl_coesao,
                   b1266_shape_clumpy,
                   b1266_shape_coesao,
                   b1266_clumpy_coesao)
b1266_cor <- data.frame(b1266_cor)
names(b1266_cor) <- c("cor", "p")

names(b1555)
M_1555 <- cor(b1555[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_1555,2)
corrplot(M_1555, method= "color", tl.col = "black")

a <-cor.test(b1555$prop_florest, b1555$dens_frags)
b1555_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$prop_florest, b1555$med_dist_eucl)
b1555_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$prop_florest, b1555$shape_mn)
b1555_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$prop_florest, b1555$clumpy)
b1555_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$prop_florest, b1555$coesao)
b1555_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1555$dens_frags, b1555$med_dist_eucl)
b1555_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$dens_frags, b1555$shape_mn)
b1555_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$dens_frags, b1555$clumpy)
b1555_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$dens_frags, b1555$coesao)
b1555_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1555$med_dist_eucl, b1555$shape_mn)
b1555_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$med_dist_eucl, b1555$clumpy)
b1555_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$med_dist_eucl, b1555$coesao)
b1555_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1555$shape_mn, b1555$clumpy)
b1555_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1555$shape_mn, b1555$coesao)
b1555_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1555$clumpy, b1555$coesao)
b1555_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b1555_cor <- rbind(b1555_propflorest_densfrags,
                   b1555_propflorest_disteucl,
                   b1555_propflorest_shape,
                   b1555_propflorest_clumpy,
                   b1555_propflorest_coesao,
                   b1555_densfrags_disteucl,
                   b1555_densfrags_shape,
                   b1555_densfrags_clumpy,
                   b1555_densfrags_coesao,
                   b1555_disteucl_shape,
                   b1555_disteucl_clumpy,
                   b1555_disteucl_coesao,
                   b1555_shape_clumpy,
                   b1555_shape_coesao,
                   b1555_clumpy_coesao)
b1555_cor <- data.frame(b1555_cor)
names(b1555_cor) <- c("cor", "p")

names(b1844)
M_1844 <- cor(b1844[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_1844,2)
corrplot(M_1844, method= "color", tl.col = "black")

a <-cor.test(b1844$prop_florest, b1844$dens_frags)
b1844_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$prop_florest, b1844$med_dist_eucl)
b1844_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$prop_florest, b1844$shape_mn)
b1844_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$prop_florest, b1844$clumpy)
b1844_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$prop_florest, b1844$coesao)
b1844_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1844$dens_frags, b1844$med_dist_eucl)
b1844_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$dens_frags, b1844$shape_mn)
b1844_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$dens_frags, b1844$clumpy)
b1844_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$dens_frags, b1844$coesao)
b1844_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1844$med_dist_eucl, b1844$shape_mn)
b1844_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$med_dist_eucl, b1844$clumpy)
b1844_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$med_dist_eucl, b1844$coesao)
b1844_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1844$shape_mn, b1844$clumpy)
b1844_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b1844$shape_mn, b1844$coesao)
b1844_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b1844$clumpy, b1844$coesao)
b1844_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b1844_cor <- rbind(b1844_propflorest_densfrags,
                   b1844_propflorest_disteucl,
                   b1844_propflorest_shape,
                   b1844_propflorest_clumpy,
                   b1844_propflorest_coesao,
                   b1844_densfrags_disteucl,
                   b1844_densfrags_shape,
                   b1844_densfrags_clumpy,
                   b1844_densfrags_coesao,
                   b1844_disteucl_shape,
                   b1844_disteucl_clumpy,
                   b1844_disteucl_coesao,
                   b1844_shape_clumpy,
                   b1844_shape_coesao,
                   b1844_clumpy_coesao)
b1844_cor <- data.frame(b1844_cor)
names(b1844_cor) <- c("cor", "p")

names(b2133)
M_2133 <- cor(b2133[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_2133,2)
corrplot(M_2133, method= "color", tl.col = "black")

a <-cor.test(b2133$prop_florest, b2133$dens_frags)
b2133_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$prop_florest, b2133$med_dist_eucl)
b2133_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$prop_florest, b2133$shape_mn)
b2133_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$prop_florest, b2133$clumpy)
b2133_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$prop_florest, b2133$coesao)
b2133_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2133$dens_frags, b2133$med_dist_eucl)
b2133_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$dens_frags, b2133$shape_mn)
b2133_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$dens_frags, b2133$clumpy)
b2133_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$dens_frags, b2133$coesao)
b2133_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2133$med_dist_eucl, b2133$shape_mn)
b2133_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$med_dist_eucl, b2133$clumpy)
b2133_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$med_dist_eucl, b2133$coesao)
b2133_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2133$shape_mn, b2133$clumpy)
b2133_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2133$shape_mn, b2133$coesao)
b2133_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2133$clumpy, b2133$coesao)
b2133_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b2133_cor <- rbind(b2133_propflorest_densfrags,
                   b2133_propflorest_disteucl,
                   b2133_propflorest_shape,
                   b2133_propflorest_clumpy,
                   b2133_propflorest_coesao,
                   b2133_densfrags_disteucl,
                   b2133_densfrags_shape,
                   b2133_densfrags_clumpy,
                   b2133_densfrags_coesao,
                   b2133_disteucl_shape,
                   b2133_disteucl_clumpy,
                   b2133_disteucl_coesao,
                   b2133_shape_clumpy,
                   b2133_shape_coesao,
                   b2133_clumpy_coesao)
b2133_cor <- data.frame(b2133_cor)
names(b2133_cor) <- c("cor", "p")

names(b2422)
M_2422 <- cor(b2422[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_2422,2)
corrplot(M_2422, method= "color", tl.col = "black")

a <-cor.test(b2422$prop_florest, b2422$dens_frags)
b2422_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$prop_florest, b2422$med_dist_eucl)
b2422_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$prop_florest, b2422$shape_mn)
b2422_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$prop_florest, b2422$clumpy)
b2422_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$prop_florest, b2422$coesao)
b2422_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2422$dens_frags, b2422$med_dist_eucl)
b2422_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$dens_frags, b2422$shape_mn)
b2422_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$dens_frags, b2422$clumpy)
b2422_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$dens_frags, b2422$coesao)
b2422_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2422$med_dist_eucl, b2422$shape_mn)
b2422_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$med_dist_eucl, b2422$clumpy)
b2422_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$med_dist_eucl, b2422$coesao)
b2422_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2422$shape_mn, b2422$clumpy)
b2422_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2422$shape_mn, b2422$coesao)
b2422_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2422$clumpy, b2422$coesao)
b2422_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b2422_cor <- rbind(b2422_propflorest_densfrags,
                   b2422_propflorest_disteucl,
                   b2422_propflorest_shape,
                   b2422_propflorest_clumpy,
                   b2422_propflorest_coesao,
                   b2422_densfrags_disteucl,
                   b2422_densfrags_shape,
                   b2422_densfrags_clumpy,
                   b2422_densfrags_coesao,
                   b2422_disteucl_shape,
                   b2422_disteucl_clumpy,
                   b2422_disteucl_coesao,
                   b2422_shape_clumpy,
                   b2422_shape_coesao,
                   b2422_clumpy_coesao)
b2422_cor <- data.frame(b2422_cor)
names(b2422_cor) <- c("cor", "p")

names(b2711)
M_2711 <- cor(b2711[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_2711,2)
corrplot(M_2711, method= "color", tl.col = "black")

a <-cor.test(b2711$prop_florest, b2711$dens_frags)
b2711_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$prop_florest, b2711$med_dist_eucl)
b2711_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$prop_florest, b2711$shape_mn)
b2711_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$prop_florest, b2711$clumpy)
b2711_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$prop_florest, b2711$coesao)
b2711_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2711$dens_frags, b2711$med_dist_eucl)
b2711_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$dens_frags, b2711$shape_mn)
b2711_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$dens_frags, b2711$clumpy)
b2711_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$dens_frags, b2711$coesao)
b2711_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2711$med_dist_eucl, b2711$shape_mn)
b2711_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$med_dist_eucl, b2711$clumpy)
b2711_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$med_dist_eucl, b2711$coesao)
b2711_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2711$shape_mn, b2711$clumpy)
b2711_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b2711$shape_mn, b2711$coesao)
b2711_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b2711$clumpy, b2711$coesao)
b2711_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b2711_cor <- rbind(b2711_propflorest_densfrags,
                   b2711_propflorest_disteucl,
                   b2711_propflorest_shape,
                   b2711_propflorest_clumpy,
                   b2711_propflorest_coesao,
                   b2711_densfrags_disteucl,
                   b2711_densfrags_shape,
                   b2711_densfrags_clumpy,
                   b2711_densfrags_coesao,
                   b2711_disteucl_shape,
                   b2711_disteucl_clumpy,
                   b2711_disteucl_coesao,
                   b2711_shape_clumpy,
                   b2711_shape_coesao,
                   b2711_clumpy_coesao)
b2711_cor <- data.frame(b2711_cor)
names(b2711_cor) <- c("cor", "p")

names(b3000)
M_3000 <- cor(b3000[c(6, 8, 11, 15, 18, 19)], use = "complete.obs") #sem area, sem media_area_frags e sem cv_area_frags, sem pcore, sem igreg, sem dens_bord
round(M_3000,2)
corrplot(M_3000, method= "color", tl.col = "black")

a <-cor.test(b3000$prop_florest, b3000$dens_frags)
b3000_propflorest_densfrags <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$prop_florest, b3000$med_dist_eucl)
b3000_propflorest_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$prop_florest, b3000$shape_mn)
b3000_propflorest_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$prop_florest, b3000$clumpy)
b3000_propflorest_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$prop_florest, b3000$coesao)
b3000_propflorest_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b3000$dens_frags, b3000$med_dist_eucl)
b3000_densfrags_disteucl <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$dens_frags, b3000$shape_mn)
b3000_densfrags_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$dens_frags, b3000$clumpy)
b3000_densfrags_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$dens_frags, b3000$coesao)
b3000_densfrags_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b3000$med_dist_eucl, b3000$shape_mn)
b3000_disteucl_shape <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$med_dist_eucl, b3000$clumpy)
b3000_disteucl_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$med_dist_eucl, b3000$coesao)
b3000_disteucl_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b3000$shape_mn, b3000$clumpy)
b3000_shape_clumpy <- round(c(a$estimate, a$p.value), 4)
a <-cor.test(b3000$shape_mn, b3000$coesao)
b3000_shape_coesao <- round(c(a$estimate, a$p.value), 4)

a <-cor.test(b3000$clumpy, b3000$coesao)
b3000_clumpy_coesao <- round(c(a$estimate, a$p.value), 4)

b3000_cor <- rbind(b3000_propflorest_densfrags,
                   b3000_propflorest_disteucl,
                   b3000_propflorest_shape,
                   b3000_propflorest_clumpy,
                   b3000_propflorest_coesao,
                   b3000_densfrags_disteucl,
                   b3000_densfrags_shape,
                   b3000_densfrags_clumpy,
                   b3000_densfrags_coesao,
                   b3000_disteucl_shape,
                   b3000_disteucl_clumpy,
                   b3000_disteucl_coesao,
                   b3000_shape_clumpy,
                   b3000_shape_coesao,
                   b3000_clumpy_coesao)
b3000_cor <- data.frame(b3000_cor)
names(b3000_cor) <- c("cor", "p")
b <- cbind(b399_cor, b688_cor, b977_cor, b1266_cor, b1555_cor, b1844_cor, b2133_cor, b2422_cor, b2711_cor, b3000_cor)

names(b) <- c("b399_cor", "b399_p", "b688_cor", "b688_p", "b977_cor", "b977_p", "b1266_cor", "b1266_p", "b1555_cor", "b1555_p", "b1844_cor", "b1844_p", "b2133_cor", "b2133_p", "b2422_cor", "b2422_p", "b2711_cor", "b2711_p", "b3000_cor", "b3000_p")
write.csv(b, "C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/correlacoes_dens20241207.csv")

## Decisão: TIRAR INICIALMENTE COESÃO. O VIF é alto, e está correlacionada com a proporção de floresta em todos os buffers. Queremos tentar manter a proporção de floresta por ser a variável de disponibilidade de habitat.

# Precisamos refazer o VIF sem coesão
#### VIF 2 - sem coesão----
modelo_b399 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b399)
vif_values_b399 <- vif(modelo_b399)
vif_values_b399
modelo_b688 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b688)
vif_values_b688 <- vif(modelo_b688)
vif_values_b688
modelo_b977 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b977)
vif_values_b977 <- vif(modelo_b977)
vif_values_b977
modelo_b1266 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b1266)
vif_values_b1266 <- vif(modelo_b1266)
vif_values_b1266
modelo_b1555 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b1555)
vif_values_b1555 <- vif(modelo_b1555)
vif_values_b1555
modelo_b1844 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b1844)
vif_values_b1844 <- vif(modelo_b1844)
vif_values_b1844
modelo_b2133 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b2133)
vif_values_b2133 <- vif(modelo_b2133)
vif_values_b2133
modelo_b2422 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b2422)
vif_values_b2422 <- vif(modelo_b2422)
vif_values_b2422
modelo_b2711 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b2711)
vif_values_b2711 <- vif(modelo_b2711)
vif_values_b2711
modelo_b3000 <- glm(dens_groups ~ prop_florest + dens_frags + med_dist_eucl+ shape_mn + clumpy, family = poisson(link = "log"), data = b3000)
vif_values_b3000 <- vif(modelo_b3000)
vif_values_b3000

vif_buffers <- rbind(vif_values_b399, vif_values_b688, vif_values_b977, vif_values_b1266, vif_values_b1555, vif_values_b1844, vif_values_b2133, vif_values_b2422, vif_values_b2711, vif_values_b3000)
vif_buffers
write.csv(vif_buffers, "C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/vif2_dens20240712.csv")

## Decisão: TIRAR TAMBÉM MÉDIA DA DISTÂNCIA EUCLIDEANA ENTRE FRAGMENTOS. O VIF é alto, e está correlacionada com a proporção de floresta na maioria os buffers. Queremos tentar manter a proporção de floresta por ser a variável de disponibilidade de habitat.

# Precisamos refazer o VIF sem coesão e sem dist euclideana
#### VIF 3 - sem coesão e sem dist eucl----
modelo_b399 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b399)
vif_values_b399 <- vif(modelo_b399)
vif_values_b399
modelo_b688 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b688)
vif_values_b688 <- vif(modelo_b688)
vif_values_b688
modelo_b977 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b977)
vif_values_b977 <- vif(modelo_b977)
vif_values_b977
modelo_b1266 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b1266)
vif_values_b1266 <- vif(modelo_b1266)
vif_values_b1266
modelo_b1555 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b1555)
vif_values_b1555 <- vif(modelo_b1555)
vif_values_b1555
modelo_b1844 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b1844)
vif_values_b1844 <- vif(modelo_b1844)
vif_values_b1844
modelo_b2133 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b2133)
vif_values_b2133 <- vif(modelo_b2133)
vif_values_b2133
modelo_b2422 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b2422)
vif_values_b2422 <- vif(modelo_b2422)
vif_values_b2422
modelo_b2711 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b2711)
vif_values_b2711 <- vif(modelo_b2711)
vif_values_b2711
modelo_b3000 <- glm(dens_groups ~ prop_florest + dens_frags + shape_mn + clumpy, family = poisson(link = "log"), data = b3000)
vif_values_b3000 <- vif(modelo_b3000)
vif_values_b3000

vif_buffers <- rbind(vif_values_b399, vif_values_b688, vif_values_b977, vif_values_b1266, vif_values_b1555, vif_values_b1844, vif_values_b2133, vif_values_b2422, vif_values_b2711, vif_values_b3000)
vif_buffers
write.csv(vif_buffers, "C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/vif3_dens20241207.csv")


# Agora os valores VIF estão bom e não há correlações importantes entres as variáveis que foram mantidas.
# São elas: prop_florest + dens_frags + shape_mn + clumpy

#### Histogramas----

hist(b399$prop_florest)
hist(b688$prop_florest)
hist(b977$prop_florest)
hist(b1266$prop_florest)
hist(b1555$prop_florest)
hist(b1844$prop_florest)
hist(b2133$prop_florest)
hist(b2422$prop_florest)
hist(b2711$prop_florest)
hist(b3000$prop_florest)

hist(b399$dens_frags)
hist(b688$dens_frags)
hist(b977$dens_frags)
hist(b1266$dens_frags)
hist(b1555$dens_frags)
hist(b1844$dens_frags)
hist(b2133$dens_frags)
hist(b2422$dens_frags)
hist(b2711$dens_frags)
hist(b3000$dens_frags)

hist(b399$shape_mn)
hist(b688$shape_mn)
hist(b977$shape_mn)
hist(b1266$shape_mn)
hist(b1555$shape_mn)
hist(b1844$shape_mn)
hist(b2133$shape_mn)
hist(b2422$shape_mn)
hist(b2711$shape_mn)
hist(b3000$shape_mn)

hist(b399$clumpy) ##### ATENÇÃO!! Conferir esses valores
#Conferi, de fato o landscapemetrics está calculando o clumpy, apenas nessa escala, como sendo maior que 1 em quatro paisagens: 
#ID66_buffer399 (substituir por 238 no looping), clumpy = 4.1
#ID02_buffer399 (18), clumpy = 1.4
#ID01_buffer399 (8), clumpy = 1.02
#ID45_buffer399 (68), clumpy = 1.02
# Todas essas paisagens tem apenas um fragmento e mais de 93% de cobertura florestal, com exceção do ID02_buffer399, que possui dois fragmento e ~78% de cobertura florestal.
#Decidimos substituir todos valores de clumpy maiores que 1 por "NA" (mais adiante no script)

hist(b688$clumpy)
hist(b977$clumpy)
hist(b1266$clumpy)
hist(b1555$clumpy)
hist(b1844$clumpy)
hist(b2133$clumpy)
hist(b2422$clumpy)
hist(b2711$clumpy)
hist(b3000$clumpy)

#Chamando os dados (ainda com valores de clumpy >1)
dados_cap2 <- read.csv("C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/dados_cap2.csv")
head(dados_cap2)
hist(dados_cap2$clumpy)
#criando a variavel resposta
dados_cap2$dens_groups <- dados_cap2$n_groups/dados_cap2$area_frag

#corrigindo valores de clumpy
names(dados_cap2)
dados_cap2$clumpy[dados_cap2$clumpy > 1] <- NA
hist(dados_cap2$clumpy)
#write.csv(dados_cap2, "G:/Bianca_mestrado/cap_2_versao_2/dados_cap2_NA_clumpy.csv")

dados_cap2 <- read.csv("C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/dados_cap2_NA_clumpy.csv")
nrow(dados_cap2)
hist(dados_cap2$clumpy)
#fazendo subsets (exclundo buffer 399) ----
b688 <- subset(dados_cap2, buffer== "buffer688")
b688 <- subset(b688, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"
b977 <- subset(dados_cap2, buffer== "buffer977")
b977 <- subset(b977, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"
b1266 <- subset(dados_cap2, buffer== "buffer1266")
b1266 <- subset(b1266, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"
b1555 <- subset(dados_cap2, buffer== "buffer1555")
b1555 <- subset(b1555, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"
b1844 <- subset(dados_cap2, buffer== "buffer1844")
b1844 <- subset(b1844, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"
b2133 <- subset(dados_cap2, buffer== "buffer2133")
b2133 <- subset(b2133, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"
b2422 <- subset(dados_cap2, buffer== "buffer2422")
b2422 <- subset(b2422, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"
b2711 <- subset(dados_cap2, buffer== "buffer2711")
b2711 <- subset(b2711, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"
b3000 <- subset(dados_cap2, buffer== "buffer3000")
b3000 <- subset(b3000, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"

#Selecionando apenas as colunas que importam para os modelos

names(b688)
b688 <- b688[, c(1, 6, 8, 15, 18, 23)]
names(b688)
table(is.na(b688))

names(b977)
b977 <- b977[, c(1, 6, 8, 15, 18, 23)]
names(b977)
table(is.na(b977))

names(b1266)
b1266 <- b1266[, c(1, 6, 8, 15, 18, 23)]
names(b1266)
table(is.na(b1266))

names(b1555)
b1555 <- b1555[, c(1, 6, 8, 15, 18, 23)]
names(b1555)
table(is.na(b1555))

names(b1844)
b1844 <- b1844[, c(1, 6, 8, 15, 18, 23)]
names(b1844)
table(is.na(b1844))

names(b2133)
b2133 <- b2133[, c(1, 6, 8, 15, 18, 23)]
names(b2133)
table(is.na(b2133))

names(b2422)
b2422 <- b2422[, c(1, 6, 8, 15, 18, 23)]
names(b2422)
table(is.na(b2422))

names(b2711)
b2711 <- b2711[, c(1, 6, 8, 15, 18, 23)]
names(b2711)
table(is.na(b2711))

names(b3000)
b3000 <- b3000[, c(1, 6, 8, 15, 18, 23)]
names(b3000)
table(is.na(b3000))

#ajustando os modelos (sem CDI e sem b399)

### Nulos
b688_null_model <- glm(log10(dens_groups+0.001) ~ 1, data = b688)
b977_null_model <- glm(log10(dens_groups+0.001) ~ 1, data = b977)
b1266_null_model <- glm(log10(dens_groups+0.001) ~ 1, data = b1266)
b1555_null_model <- glm(log10(dens_groups+0.001) ~ 1, data = b1555)
b1844_null_model <- glm(log10(dens_groups+0.001) ~ 1, data = b1844)
b2133_null_model <- glm(log10(dens_groups+0.001) ~ 1, data = b2133)
b2422_null_model <- glm(log10(dens_groups+0.001) ~ 1, data = b2422)
b2711_null_model <- glm(log10(dens_groups+0.001) ~ 1, data = b2711)
b3000_null_model <- glm(log10(dens_groups+0.001) ~ 1, data = b3000)

### Univariados
# scale(prop_florest)
b688_FC <- glm(log10(dens_groups+0.001) ~ scale(prop_florest), data = b688)
b977_FC <- glm(log10(dens_groups+0.001) ~ scale(prop_florest), data = b977)
b1266_FC <- glm(log10(dens_groups+0.001) ~ scale(prop_florest), data = b1266)
b1555_FC <- glm(log10(dens_groups+0.001) ~ scale(prop_florest), data = b1555)
b1844_FC <- glm(log10(dens_groups+0.001) ~ scale(prop_florest), data = b1844)
b2133_FC <- glm(log10(dens_groups+0.001) ~ scale(prop_florest), data = b2133)
b2422_FC <- glm(log10(dens_groups+0.001) ~ scale(prop_florest), data = b2422)
b2711_FC <- glm(log10(dens_groups+0.001) ~ scale(prop_florest), data = b2711)
b3000_FC <- glm(log10(dens_groups+0.001) ~ scale(prop_florest), data = b3000)

# scale(dens_frags)
b688_PD <- glm(log10(dens_groups+0.001) ~ scale(dens_frags), data = b688)
b977_PD <- glm(log10(dens_groups+0.001) ~ scale(dens_frags), data = b977)
b1266_PD <- glm(log10(dens_groups+0.001) ~ scale(dens_frags), data = b1266)
b1555_PD <- glm(log10(dens_groups+0.001) ~ scale(dens_frags), data = b1555)
b1844_PD <- glm(log10(dens_groups+0.001) ~ scale(dens_frags), data = b1844)
b2133_PD <- glm(log10(dens_groups+0.001) ~ scale(dens_frags), data = b2133)
b2422_PD <- glm(log10(dens_groups+0.001) ~ scale(dens_frags), data = b2422)
b2711_PD <- glm(log10(dens_groups+0.001) ~ scale(dens_frags), data = b2711)
b3000_PD <- glm(log10(dens_groups+0.001) ~ scale(dens_frags), data = b3000)

# scale(shape_mn)
b688_SH <- glm(log10(dens_groups+0.001) ~ scale(shape_mn), data = b688)
b977_SH <- glm(log10(dens_groups+0.001) ~ scale(shape_mn), data = b977)
b1266_SH <- glm(log10(dens_groups+0.001) ~ scale(shape_mn), data = b1266)
b1555_SH <- glm(log10(dens_groups+0.001) ~ scale(shape_mn), data = b1555)
b1844_SH <- glm(log10(dens_groups+0.001) ~ scale(shape_mn), data = b1844)
b2133_SH <- glm(log10(dens_groups+0.001) ~ scale(shape_mn), data = b2133)
b2422_SH <- glm(log10(dens_groups+0.001) ~ scale(shape_mn), data = b2422)
b2711_SH <- glm(log10(dens_groups+0.001) ~ scale(shape_mn), data = b2711)
b3000_SH <- glm(log10(dens_groups+0.001) ~ scale(shape_mn), data = b3000)

# scale(clumpy)
b688_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b688)
b977_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b977)
b1266_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b1266)
b1555_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b1555)
b1844_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b1844)
b2133_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b2133)
b2422_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b2422)
b2711_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b2711)
b3000_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b3000)

## Duas variáveis
# scale(prop_florest) + scale(dens_frags)
b688_FCePD <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags), data = b688)
b977_FCePD <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags), data = b977)
b1266_FCePD <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags), data = b1266)
b1555_FCePD <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags), data = b1555)
b1844_FCePD <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags), data = b1844)
b2133_FCePD <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags), data = b2133)
b2422_FCePD <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags), data = b2422)
b2711_FCePD <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags), data = b2711)
b3000_FCePD <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags), data = b3000)

# scale(prop_florest) + scale(shape_mn)
b688_FCeSH <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(shape_mn), data = b688)
b977_FCeSH <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(shape_mn), data = b977)
b1266_FCeSH <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(shape_mn), data = b1266)
b1555_FCeSH <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(shape_mn), data = b1555)
b1844_FCeSH <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(shape_mn), data = b1844)
b2133_FCeSH <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(shape_mn), data = b2133)
b2422_FCeSH <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(shape_mn), data = b2422)
b2711_FCeSH <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(shape_mn), data = b2711)
b3000_FCeSH <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(shape_mn), data = b3000)

# scale(prop_florest) + scale(clumpy)
b688_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b688)
b977_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b977)
b1266_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b1266)
b1555_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b1555)
b1844_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b1844)
b2133_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b2133)
b2422_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b2422)
b2711_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b2711)
b3000_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b3000)

# scale(dens_frags) + scale(shape_mn)
b688_PDeSH <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(shape_mn), data = b688)
b977_PDeSH <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(shape_mn), data = b977)
b1266_PDeSH <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(shape_mn), data = b1266)
b1555_PDeSH <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(shape_mn), data = b1555)
b1844_PDeSH <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(shape_mn), data = b1844)
b2133_PDeSH <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(shape_mn), data = b2133)
b2422_PDeSH <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(shape_mn), data = b2422)
b2711_PDeSH <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(shape_mn), data = b2711)
b3000_PDeSH <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(shape_mn), data = b3000)

# scale(dens_frags) + scale(clumpy)
b688_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b688)
b977_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b977)
b1266_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b1266)
b1555_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b1555)
b1844_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b1844)
b2133_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b2133)
b2422_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b2422)
b2711_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b2711)
b3000_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b3000)

# scale(shape_mn) + scale(clumpy)
b688_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b688)
b977_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b977)
b1266_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b1266)
b1555_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b1555)
b1844_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b1844)
b2133_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b2133)
b2422_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b2422)
b2711_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b2711)
b3000_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b3000)

## Tres variaveis
# scale(prop_florest) + scale(dens_frags) + scale(shape_mn)
b688_FCePDeSH<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn), data = b688)
b977_FCePDeSH<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn), data = b977)
b1266_FCePDeSH<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn), data = b1266)
b1555_FCePDeSH<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn), data = b1555)
b1844_FCePDeSH<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn), data = b1844)
b2133_FCePDeSH<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn), data = b2133)
b2422_FCePDeSH<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn), data = b2422)
b2711_FCePDeSH<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn), data = b2711)
b3000_FCePDeSH<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn), data = b3000)

# scale(prop_florest) + scale(dens_frags) + scale(clumpy)
b688_FCePDeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(clumpy), data = b688)
b977_FCePDeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(clumpy), data = b977)
b1266_FCePDeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(clumpy), data = b1266)
b1555_FCePDeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(clumpy), data = b1555)
b1844_FCePDeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(clumpy), data = b1844)
b2133_FCePDeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(clumpy), data = b2133)
b2422_FCePDeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(clumpy), data = b2422)
b2711_FCePDeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(clumpy), data = b2711)
b3000_FCePDeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(clumpy), data = b3000)

# scale(prop_florest) + scale(shape_mn) + scale(clumpy)
b688_FCeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(shape_mn)+scale(clumpy), data = b688)
b977_FCeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(shape_mn)+scale(clumpy), data = b977)
b1266_FCeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(shape_mn)+scale(clumpy), data = b1266)
b1555_FCeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(shape_mn)+scale(clumpy), data = b1555)
b1844_FCeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(shape_mn)+scale(clumpy), data = b1844)
b2133_FCeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(shape_mn)+scale(clumpy), data = b2133)
b2422_FCeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(shape_mn)+scale(clumpy), data = b2422)
b2711_FCeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(shape_mn)+scale(clumpy), data = b2711)
b3000_FCeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(shape_mn)+scale(clumpy), data = b3000)

# scale(dens_frags) + scale(shape_mn) + scale(clumpy)
b688_PDeSHeCL<- glm(log10(dens_groups+0.001) ~scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b688)
b977_PDeSHeCL<- glm(log10(dens_groups+0.001) ~scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b977)
b1266_PDeSHeCL<- glm(log10(dens_groups+0.001) ~scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b1266)
b1555_PDeSHeCL<- glm(log10(dens_groups+0.001) ~scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b1555)
b1844_PDeSHeCL<- glm(log10(dens_groups+0.001) ~scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b1844)
b2133_PDeSHeCL<- glm(log10(dens_groups+0.001) ~scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b2133)
b2422_PDeSHeCL<- glm(log10(dens_groups+0.001) ~scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b2422)
b2711_PDeSHeCL<- glm(log10(dens_groups+0.001) ~scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b2711)
b3000_PDeSHeCL<- glm(log10(dens_groups+0.001) ~scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b3000)

## Cheio

# scale(prop_florest) + scale(dens_frags) + scale(shape_mn) + scale(clumpy)
b688_FCePDeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b688)
b977_FCePDeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b977)
b1266_FCePDeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b1266)
b1555_FCePDeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b1555)
b1844_FCePDeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b1844)
b2133_FCePDeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b2133)
b2422_FCePDeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b2422)
b2711_FCePDeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b2711)
b3000_FCePDeSHeCL<- glm(log10(dens_groups+0.001) ~ scale(prop_florest)+scale(dens_frags)+scale(shape_mn)+scale(clumpy), data = b3000)

## Tabela AIC
#### Completa (todos buffers)
modelos <- list (b688_null_model, b977_null_model, b1266_null_model, b1555_null_model, b1844_null_model, b2133_null_model, b2422_null_model, b2711_null_model, b3000_null_model,
                 b688_FC, b977_FC, b1266_FC, b1555_FC, b1844_FC, b2133_FC, b2422_FC, b2711_FC, b3000_FC, 
                 b688_PD, b977_PD, b1266_PD, b1555_PD, b1844_PD, b2133_PD, b2422_PD, b2711_PD, b3000_PD, 
                 b688_SH, b977_SH, b1266_SH, b1555_SH, b1844_SH, b2133_SH, b2422_SH, b2711_SH, b3000_SH, 
                 b688_CL, b977_CL, b1266_CL, b1555_CL, b1844_CL, b2133_CL, b2422_CL, b2711_CL, b3000_CL, 
                 b688_FCePD, b977_FCePD, b1266_FCePD, b1555_FCePD, b1844_FCePD, b2133_FCePD, b2422_FCePD, b2711_FCePD, b3000_FCePD, 
                 b688_FCeSH, b977_FCeSH, b1266_FCeSH, b1555_FCeSH, b1844_FCeSH, b2133_FCeSH, b2422_FCeSH, b2711_FCeSH, b3000_FCeSH, 
                 b688_FCeCL, b977_FCeCL, b1266_FCeCL, b1555_FCeCL, b1844_FCeCL, b2133_FCeCL, b2422_FCeCL, b2711_FCeCL, b3000_FCeCL, 
                 b688_PDeSH, b977_PDeSH, b1266_PDeSH, b1555_PDeSH, b1844_PDeSH, b2133_PDeSH, b2422_PDeSH, b2711_PDeSH, b3000_PDeSH, 
                 b688_PDeCL, b977_PDeCL, b1266_PDeCL, b1555_PDeCL, b1844_PDeCL, b2133_PDeCL, b2422_PDeCL, b2711_PDeCL, b3000_PDeCL, 
                 b688_SHeCL, b977_SHeCL, b1266_SHeCL, b1555_SHeCL, b1844_SHeCL, b2133_SHeCL, b2422_SHeCL, b2711_SHeCL, b3000_SHeCL,
                 b688_FCePDeSH, b977_FCePDeSH, b1266_FCePDeSH, b1555_FCePDeSH, b1844_FCePDeSH, b2133_FCePDeSH, b2422_FCePDeSH, b2711_FCePDeSH, b3000_FCePDeSH, 
                 b688_FCePDeCL, b977_FCePDeCL, b1266_FCePDeCL, b1555_FCePDeCL, b1844_FCePDeCL, b2133_FCePDeCL, b2422_FCePDeCL, b2711_FCePDeCL, b3000_FCePDeCL, 
                 b688_FCeSHeCL, b977_FCeSHeCL, b1266_FCeSHeCL, b1555_FCeSHeCL, b1844_FCeSHeCL, b2133_FCeSHeCL, b2422_FCeSHeCL, b2711_FCeSHeCL, b3000_FCeSHeCL, 
                 b688_PDeSHeCL, b977_PDeSHeCL, b1266_PDeSHeCL, b1555_PDeSHeCL, b1844_PDeSHeCL, b2133_PDeSHeCL, b2422_PDeSHeCL, b2711_PDeSHeCL, b3000_PDeSHeCL, 
                 b688_FCePDeSHeCL, b977_FCePDeSHeCL, b1266_FCePDeSHeCL, b1555_FCePDeSHeCL, b1844_FCePDeSHeCL, b2133_FCePDeSHeCL, b2422_FCePDeSHeCL, b2711_FCePDeSHeCL, b3000_FCePDeSHeCL)

length(modelos)
mod_names <- c("b688_null_model", "b977_null_model", "b1266_null_model", "b1555_null_model", "b1844_null_model", "b2133_null_model", "b2422_null_model", "b2711_null_model", "b3000_null_model",
               "b688_FC", "b977_FC", "b1266_FC", "b1555_FC", "b1844_FC", "b2133_FC", "b2422_FC", "b2711_FC", "b3000_FC",
               "b688_PD", "b977_PD", "b1266_PD", "b1555_PD", "b1844_PD", "b2133_PD", "b2422_PD", "b2711_PD", "b3000_PD",
               "b688_SH", "b977_SH", "b1266_SH", "b1555_SH", "b1844_SH", "b2133_SH", "b2422_SH", "b2711_SH", "b3000_SH",
               "b688_CL", "b977_CL", "b1266_CL", "b1555_CL", "b1844_CL", "b2133_CL", "b2422_CL", "b2711_CL", "b3000_CL",
               "b688_FCePD", "b977_FCePD", "b1266_FCePD", "b1555_FCePD", "b1844_FCePD", "b2133_FCePD", "b2422_FCePD", "b2711_FCePD", "b3000_FCePD",
               "b688_FCeSH", "b977_FCeSH", "b1266_FCeSH", "b1555_FCeSH", "b1844_FCeSH", "b2133_FCeSH", "b2422_FCeSH", "b2711_FCeSH", "b3000_FCeSH",
               "b688_FCeCL", "b977_FCeCL", "b1266_FCeCL", "b1555_FCeCL", "b1844_FCeCL", "b2133_FCeCL", "b2422_FCeCL", "b2711_FCeCL", "b3000_FCeCL",
               "b688_PDeSH", "b977_PDeSH", "b1266_PDeSH", "b1555_PDeSH", "b1844_PDeSH", "b2133_PDeSH", "b2422_PDeSH", "b2711_PDeSH", "b3000_PDeSH",
               "b688_PDeCL", "b977_PDeCL", "b1266_PDeCL", "b1555_PDeCL", "b1844_PDeCL", "b2133_PDeCL", "b2422_PDeCL", "b2711_PDeCL", "b3000_PDeCL",
               "b688_SHeCL", "b977_SHeCL", "b1266_SHeCL", "b1555_SHeCL", "b1844_SHeCL", "b2133_SHeCL", "b2422_SHeCL", "b2711_SHeCL", "b3000_SHeCL",
               "b688_FCePDeSH", "b977_FCePDeSH", "b1266_FCePDeSH", "b1555_FCePDeSH", "b1844_FCePDeSH", "b2133_FCePDeSH", "b2422_FCePDeSH", "b2711_FCePDeSH", "b3000_FCePDeSH",
               "b688_FCePDeCL", "b977_FCePDeCL", "b1266_FCePDeCL", "b1555_FCePDeCL", "b1844_FCePDeCL", "b2133_FCePDeCL", "b2422_FCePDeCL", "b2711_FCePDeCL", "b3000_FCePDeCL",
               "b688_FCeSHeCL", "b977_FCeSHeCL", "b1266_FCeSHeCL", "b1555_FCeSHeCL", "b1844_FCeSHeCL", "b2133_FCeSHeCL", "b2422_FCeSHeCL", "b2711_FCeSHeCL", "b3000_FCeSHeCL",
               "b688_PDeSHeCL", "b977_PDeSHeCL", "b1266_PDeSHeCL", "b1555_PDeSHeCL", "b1844_PDeSHeCL", "b2133_PDeSHeCL", "b2422_PDeSHeCL", "b2711_PDeSHeCL", "b3000_PDeSHeCL",
               "b688_FCePDeSHeCL", "b977_FCePDeSHeCL", "b1266_FCePDeSHeCL", "b1555_FCePDeSHeCL", "b1844_FCePDeSHeCL", "b2133_FCePDeSHeCL", "b2422_FCePDeSHeCL", "b2711_FCePDeSHeCL", "b3000_FCePDeSHeCL")
length(mod_names)
#install.packages(AICcmodavg)
library("AICcmodavg")
modelos_AIC <- aictab(cand.set = modelos, modnames = mod_names)
modelos_AIC <- data.frame(modelos_AIC)
modelos_AIC

write.csv(modelos_AIC, "C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/modelos_aic_todos_dens.csv")


## Graficos para cada variavel ----
library(stringr)
### Null model ----
b688_nullmodel <- glm(log10(dens_groups+0.001) ~ 1, data = b688)
b977_nullmodel <- glm(log10(dens_groups+0.001) ~ 1, data = b977)
b1266_nullmodel <- glm(log10(dens_groups+0.001) ~ 1, data = b1266)
b1555_nullmodel <- glm(log10(dens_groups+0.001) ~ 1, data = b1555)
b1844_nullmodel <- glm(log10(dens_groups+0.001) ~ 1, data = b1844)
b2133_nullmodel <- glm(log10(dens_groups+0.001) ~ 1, data = b2133)
b2422_nullmodel <- glm(log10(dens_groups+0.001) ~ 1, data = b2422)
b2711_nullmodel <- glm(log10(dens_groups+0.001) ~ 1, data = b2711)
b3000_nullmodel <- glm(log10(dens_groups+0.001) ~ 1, data = b3000)

modelos_nulos <- list (b688_nullmodel, b977_nullmodel, b1266_nullmodel, b1555_nullmodel, b1844_nullmodel, b2133_nullmodel, b2422_nullmodel, b2711_nullmodel, b3000_nullmodel)
mod_names_nulos <- c("b688_nullmodel", "b977_nullmodel", "b1266_nullmodel", "b1555_nullmodel", "b1844_nullmodel", "b2133_nullmodel", "b2422_nullmodel", "b2711_nullmodel", "b3000_nullmodel")
nulos_AIC <- aictab(cand.set = modelos_nulos, modnames = mod_names_nulos)
nulos_AIC <- data.frame(nulos_AIC)
nulos_AIC[c('buffer', 'var')] <- str_split_fixed(nulos_AIC$Modnames, '_', 2)
nulos_AIC$buffer <- gsub("b","", nulos_AIC$buffer)
model_type <- c(rep("null model", times = nrow(nulos_AIC)))
nulos_AIC <- cbind(nulos_AIC, model_type)

### Forest Cover ----
FC_univariados <- list(b688_FC, b977_FC, b1266_FC, b1555_FC, b1844_FC, b2133_FC, b2422_FC, b2711_FC, b3000_FC)
mod_names <- c("b688_FC", "b977_FC", "b1266_FC", "b1555_FC", "b1844_FC", "b2133_FC", "b2422_FC", "b2711_FC", "b3000_FC")
FC_univariados_AIC <- aictab(cand.set = FC_univariados, modnames = mod_names)
FC_univariados_AIC <- data.frame(FC_univariados_AIC)
FC_univariados_AIC[c('buffer', 'var')] <- str_split_fixed(FC_univariados_AIC$Modnames, '_', 2)
FC_univariados_AIC$buffer <- gsub("b","", FC_univariados_AIC$buffer)
model_type <- c(rep("univariado", times = nrow(FC_univariados_AIC)))
FC_univariados_AIC <- cbind(FC_univariados_AIC, model_type)

FC_bivariados <- list(b688_FCePD, b977_FCePD, b1266_FCePD, b1555_FCePD, b1844_FCePD, b2133_FCePD, b2422_FCePD, b2711_FCePD, b3000_FCePD, 
                      b688_FCeSH, b977_FCeSH, b1266_FCeSH, b1555_FCeSH, b1844_FCeSH, b2133_FCeSH, b2422_FCeSH, b2711_FCeSH, b3000_FCeSH, 
                      b688_FCeCL, b977_FCeCL, b1266_FCeCL, b1555_FCeCL, b1844_FCeCL, b2133_FCeCL, b2422_FCeCL, b2711_FCeCL, b3000_FCeCL)
length(FC_bivariados)
mod_names <- c("b688_FCePD", "b977_FCePD", "b1266_FCePD", "b1555_FCePD", "b1844_FCePD", "b2133_FCePD", "b2422_FCePD", "b2711_FCePD", "b3000_FCePD", 
               "b688_FCeSH", "b977_FCeSH", "b1266_FCeSH", "b1555_FCeSH", "b1844_FCeSH", "b2133_FCeSH", "b2422_FCeSH", "b2711_FCeSH", "b3000_FCeSH", 
               "b688_FCeCL", "b977_FCeCL", "b1266_FCeCL", "b1555_FCeCL", "b1844_FCeCL", "b2133_FCeCL", "b2422_FCeCL", "b2711_FCeCL", "b3000_FCeCL")
length(mod_names)
FC_bivariados_AIC <- aictab(cand.set = FC_bivariados, modnames = mod_names)
FC_bivariados_AIC <- data.frame(FC_bivariados_AIC)
FC_bivariados_AIC[c('buffer', 'var')] <- str_split_fixed(FC_bivariados_AIC$Modnames, '_', 2)
FC_bivariados_AIC$buffer <- gsub("b","", FC_bivariados_AIC$buffer)
model_type <- c(rep("bivariado", times = nrow(FC_bivariados_AIC)))
FC_bivariados_AIC <- cbind(FC_bivariados_AIC, model_type)

FC_trivariados <- list(b688_FCePDeSH, b977_FCePDeSH, b1266_FCePDeSH, b1555_FCePDeSH, b1844_FCePDeSH, b2133_FCePDeSH, b2422_FCePDeSH, b2711_FCePDeSH, b3000_FCePDeSH, 
                       b688_FCePDeCL, b977_FCePDeCL, b1266_FCePDeCL, b1555_FCePDeCL, b1844_FCePDeCL, b2133_FCePDeCL, b2422_FCePDeCL, b2711_FCePDeCL, b3000_FCePDeCL, 
                       b688_FCeSHeCL, b977_FCeSHeCL, b1266_FCeSHeCL, b1555_FCeSHeCL, b1844_FCeSHeCL, b2133_FCeSHeCL, b2422_FCeSHeCL, b2711_FCeSHeCL, b3000_FCeSHeCL)
length(FC_trivariados)
mod_names <- c("b688_FCePDeSH", "b977_FCePDeSH", "b1266_FCePDeSH", "b1555_FCePDeSH", "b1844_FCePDeSH", "b2133_FCePDeSH", "b2422_FCePDeSH", "b2711_FCePDeSH", "b3000_FCePDeSH", 
               "b688_FCePDeCL", "b977_FCePDeCL", "b1266_FCePDeCL", "b1555_FCePDeCL", "b1844_FCePDeCL", "b2133_FCePDeCL", "b2422_FCePDeCL", "b2711_FCePDeCL", "b3000_FCePDeCL", 
               "b688_FCeSHeCL", "b977_FCeSHeCL", "b1266_FCeSHeCL", "b1555_FCeSHeCL", "b1844_FCeSHeCL", "b2133_FCeSHeCL", "b2422_FCeSHeCL", "b2711_FCeSHeCL", "b3000_FCeSHeCL")
length(mod_names)
FC_trivariados_AIC <- aictab(cand.set = FC_trivariados, modnames = mod_names)
FC_trivariados_AIC <- data.frame(FC_trivariados_AIC)
FC_trivariados_AIC[c('buffer', 'var')] <- str_split_fixed(FC_trivariados_AIC$Modnames, '_', 2)
FC_trivariados_AIC$buffer <- gsub("b","", FC_trivariados_AIC$buffer)
model_type <- c(rep("trivariado", times = nrow(FC_trivariados_AIC)))
FC_trivariados_AIC <- cbind(FC_trivariados_AIC, model_type)

FC_tetravariados <- list(b688_FCePDeSHeCL, b977_FCePDeSHeCL, b1266_FCePDeSHeCL, b1555_FCePDeSHeCL, b1844_FCePDeSHeCL, b2133_FCePDeSHeCL, b2422_FCePDeSHeCL, b2711_FCePDeSHeCL, b3000_FCePDeSHeCL)
length(FC_tetravariados)
mod_names <- c("b688_FCePDeSHeCL", "b977_FCePDeSHeCL", "b1266_FCePDeSHeCL", "b1555_FCePDeSHeCL", "b1844_FCePDeSHeCL", "b2133_FCePDeSHeCL", "b2422_FCePDeSHeCL", "b2711_FCePDeSHeCL", "b3000_FCePDeSHeCL")
length(mod_names)
FC_tetravariados_AIC <- aictab(cand.set = FC_tetravariados, modnames = mod_names)
FC_tetravariados_AIC <- data.frame(FC_tetravariados_AIC)
FC_tetravariados_AIC[c('buffer', 'var')] <- str_split_fixed(FC_tetravariados_AIC$Modnames, '_', 2)
FC_tetravariados_AIC$buffer <- gsub("b","", FC_tetravariados_AIC$buffer)
model_type <- c(rep("tetravariado", times = nrow(FC_tetravariados_AIC)))
FC_tetravariados_AIC <- cbind(FC_tetravariados_AIC, model_type)

FC_AIC <- rbind(nulos_AIC, FC_univariados_AIC, FC_bivariados_AIC, FC_trivariados_AIC, FC_tetravariados_AIC)
nrow(FC_AIC)
write.csv(FC_AIC, "G:/Bianca_mestrado/cap_2_versao_2/estrategia_5/FC_AIC_dens.csv")

cols <- c("null model" = "black", "univariado" = "#e41a1c", "bivariado" = "#377eb8", "trivariado" = "#4daf4a", "tetravariado" = "#984ea3")

library(ggplot2)
FC <- ggplot(FC_AIC, aes(x = as.numeric(buffer), y = AICc))+
  theme_classic(base_size = 16)+
  geom_vline(xintercept = c(688, 977, 1266, 1555, 1844, 2133, 2422, 2711, 3000), color = "gray")+
  geom_point(aes(colour = factor(model_type)), size = 2)+
  geom_line(aes(x = as.numeric(buffer), y = AICc, group = var, colour = factor(model_type)))+
  scale_x_continuous(breaks = c(688, 977, 1266, 1555, 1844, 2133, 2422, 2711, 3000))+
  scale_colour_manual(values = cols, name = "")+
  labs(x = "Buffer radius (m)", y = "AICc")
FC
FC <- FC + ylim (60, 80)
FC
FC_AIC[order(FC_AIC$AICc),]

### Patch density ----

PD_univariados <- list(b688_PD, b977_PD, b1266_PD, b1555_PD, b1844_PD, b2133_PD, b2422_PD, b2711_PD, b3000_PD)
length(PD_univariados)
mod_names <- c("b688_PD", "b977_PD", "b1266_PD", "b1555_PD", "b1844_PD", "b2133_PD", "b2422_PD", "b2711_PD", "b3000_PD")
length(mod_names)
PD_univariados_AIC <- aictab(cand.set = PD_univariados, modnames = mod_names)
PD_univariados_AIC <- data.frame(PD_univariados_AIC)
PD_univariados_AIC[c('buffer', 'var')] <- str_split_fixed(PD_univariados_AIC$Modnames, '_', 2)
PD_univariados_AIC$buffer <- gsub("b","", PD_univariados_AIC$buffer)
model_type <- c(rep("univariado", times = nrow(PD_univariados_AIC)))
PD_univariados_AIC <- cbind(PD_univariados_AIC, model_type)

PD_bivariados <- list(b688_FCePD, b977_FCePD, b1266_FCePD, b1555_FCePD, b1844_FCePD, b2133_FCePD, b2422_FCePD, b2711_FCePD, b3000_FCePD, 
                      b688_PDeSH, b977_PDeSH, b1266_PDeSH, b1555_PDeSH, b1844_PDeSH, b2133_PDeSH, b2422_PDeSH, b2711_PDeSH, b3000_PDeSH, 
                      b688_PDeCL, b977_PDeCL, b1266_PDeCL, b1555_PDeCL, b1844_PDeCL, b2133_PDeCL, b2422_PDeCL, b2711_PDeCL, b3000_PDeCL)
length(PD_bivariados)
mod_names <- c("b688_PDeFC", "b977_PDeFC", "b1266_PDeFC", "b1555_PDeFC", "b1844_PDeFC", "b2133_PDeFC", "b2422_PDeFC", "b2711_PDeFC", "b3000_PDeFC", 
               "b688_PDeSH", "b977_PDeSH", "b1266_PDeSH", "b1555_PDeSH", "b1844_PDeSH", "b2133_PDeSH", "b2422_PDeSH", "b2711_PDeSH", "b3000_PDeSH", 
               "b688_PDeCL", "b977_PDeCL", "b1266_PDeCL", "b1555_PDeCL", "b1844_PDeCL", "b2133_PDeCL", "b2422_PDeCL", "b2711_PDeCL", "b3000_PDeCL")
length(mod_names)
PD_bivariados_AIC <- aictab(cand.set = PD_bivariados, modnames = mod_names)
PD_bivariados_AIC <- data.frame(PD_bivariados_AIC)
PD_bivariados_AIC[c('buffer', 'var')] <- str_split_fixed(PD_bivariados_AIC$Modnames, '_', 2)
PD_bivariados_AIC$buffer <- gsub("b","", PD_bivariados_AIC$buffer)
model_type <- c(rep("bivariado", times = nrow(PD_bivariados_AIC)))
PD_bivariados_AIC <- cbind(PD_bivariados_AIC, model_type)

PD_trivariados <- list(b688_FCePDeSH, b977_FCePDeSH, b1266_FCePDeSH, b1555_FCePDeSH, b1844_FCePDeSH, b2133_FCePDeSH, b2422_FCePDeSH, b2711_FCePDeSH, b3000_FCePDeSH, 
                       b688_FCePDeCL, b977_FCePDeCL, b1266_FCePDeCL, b1555_FCePDeCL, b1844_FCePDeCL, b2133_FCePDeCL, b2422_FCePDeCL, b2711_FCePDeCL, b3000_FCePDeCL, 
                       b688_PDeSHeCL, b977_PDeSHeCL, b1266_PDeSHeCL, b1555_PDeSHeCL, b1844_PDeSHeCL, b2133_PDeSHeCL, b2422_PDeSHeCL, b2711_PDeSHeCL, b3000_PDeSHeCL)
length(PD_trivariados)
mod_names <- c("b688_FCePDeSH", "b977_FCePDeSH", "b1266_FCePDeSH", "b1555_FCePDeSH", "b1844_FCePDeSH", "b2133_FCePDeSH", "b2422_FCePDeSH", "b2711_FCePDeSH", "b3000_FCePDeSH", 
               "b688_FCePDeCL", "b977_FCePDeCL", "b1266_FCePDeCL", "b1555_FCePDeCL", "b1844_FCePDeCL", "b2133_FCePDeCL", "b2422_FCePDeCL", "b2711_FCePDeCL", "b3000_FCePDeCL", 
               "b688_PDeSHeCL", "b977_PDeSHeCL", "b1266_PDeSHeCL", "b1555_PDeSHeCL", "b1844_PDeSHeCL", "b2133_PDeSHeCL", "b2422_PDeSHeCL", "b2711_PDeSHeCL", "b3000_PDeSHeCL")
length(mod_names)
PD_trivariados_AIC <- aictab(cand.set = PD_trivariados, modnames = mod_names)
PD_trivariados_AIC <- data.frame(PD_trivariados_AIC)
PD_trivariados_AIC[c('buffer', 'var')] <- str_split_fixed(PD_trivariados_AIC$Modnames, '_', 2)
PD_trivariados_AIC$buffer <- gsub("b","", PD_trivariados_AIC$buffer)
model_type <- c(rep("trivariado", times = nrow(PD_trivariados_AIC)))
PD_trivariados_AIC <- cbind(PD_trivariados_AIC, model_type)

PD_tetravariados <- list(b688_FCePDeSHeCL, b977_FCePDeSHeCL, b1266_FCePDeSHeCL, b1555_FCePDeSHeCL, b1844_FCePDeSHeCL, b2133_FCePDeSHeCL, b2422_FCePDeSHeCL, b2711_FCePDeSHeCL, b3000_FCePDeSHeCL)
length(PD_tetravariados)
mod_names <- c("b688_FCePDeSHeCL", "b977_FCePDeSHeCL", "b1266_FCePDeSHeCL", "b1555_FCePDeSHeCL", "b1844_FCePDeSHeCL", "b2133_FCePDeSHeCL", "b2422_FCePDeSHeCL", "b2711_FCePDeSHeCL", "b3000_FCePDeSHeCL")
length(mod_names)
PD_tetravariados_AIC <- aictab(cand.set = PD_tetravariados, modnames = mod_names)
PD_tetravariados_AIC <- data.frame(PD_tetravariados_AIC)
PD_tetravariados_AIC[c('buffer', 'var')] <- str_split_fixed(PD_tetravariados_AIC$Modnames, '_', 2)
PD_tetravariados_AIC$buffer <- gsub("b","", PD_tetravariados_AIC$buffer)
model_type <- c(rep("tetravariado", times = nrow(PD_tetravariados_AIC)))
PD_tetravariados_AIC <- cbind(PD_tetravariados_AIC, model_type)

PD_AIC <- rbind(nulos_AIC, PD_univariados_AIC, PD_bivariados_AIC, PD_trivariados_AIC, PD_tetravariados_AIC)
write.csv(PD_AIC, "G:/Bianca_mestrado/cap_2_versao_2/estrategia_5/PD_AIC_dens.csv")

cols <- c("null model" = "black", "univariado" = "#e41a1c", "bivariado" = "#377eb8", "trivariado" = "#4daf4a", "tetravariado" = "#984ea3")
PD <- ggplot(PD_AIC, aes(x = as.numeric(buffer), y = AICc))+
  theme_classic(base_size = 16)+
  geom_vline(xintercept = c(688, 977, 1266, 1555, 1844, 2133, 2422, 2711, 3000), color = "gray")+
  geom_point(aes(colour = factor(model_type)), size = 2)+
  geom_line(aes(x = as.numeric(buffer), y = AICc, group = var, colour = factor(model_type)))+
  scale_x_continuous(breaks = c(688, 977, 1266, 1555, 1844, 2133, 2422, 2711, 3000))+
  scale_colour_manual(values = cols, name = "")+
  labs(x = "Buffer radius (m)", y = "AICc")
PD
PD <- PD + ylim (60, 80)
PD
PD_AIC[order(PD_AIC$AICc),]

### Shape ----

SH_univariados <- list(b688_SH, b977_SH, b1266_SH, b1555_SH, b1844_SH, b2133_SH, b2422_SH, b2711_SH, b3000_SH)
length(SH_univariados)
mod_names <- c("b688_SH", "b977_SH", "b1266_SH", "b1555_SH", "b1844_SH", "b2133_SH", "b2422_SH", "b2711_SH", "b3000_SH")
length(mod_names)
SH_univariados_AIC <- aictab(cand.set = SH_univariados, modnames = mod_names)
SH_univariados_AIC <- data.frame(SH_univariados_AIC)
SH_univariados_AIC[c('buffer', 'var')] <- str_split_fixed(SH_univariados_AIC$Modnames, '_', 2)
SH_univariados_AIC$buffer <- gsub("b","", SH_univariados_AIC$buffer)
model_type <- c(rep("univariado",  times = nrow(SH_univariados_AIC)))
SH_univariados_AIC <- cbind(SH_univariados_AIC, model_type)

SH_bivariados <- list(b688_FCeSH, b977_FCeSH, b1266_FCeSH, b1555_FCeSH, b1844_FCeSH, b2133_FCeSH, b2422_FCeSH, b2711_FCeSH, b3000_FCeSH, 
                      b688_PDeSH, b977_PDeSH, b1266_PDeSH, b1555_PDeSH, b1844_PDeSH, b2133_PDeSH, b2422_PDeSH, b2711_PDeSH, b3000_PDeSH,
                      b688_SHeCL, b977_SHeCL, b1266_SHeCL, b1555_SHeCL, b1844_SHeCL, b2133_SHeCL, b2422_SHeCL, b2711_SHeCL, b3000_SHeCL)
length(SH_bivariados)
mod_names <- c("b688_SHeFC", "b977_SHeFC", "b1266_SHeFC", "b1555_SHeFC", "b1844_SHeFC", "b2133_SHeFC", "b2422_SHeFC", "b2711_SHeFC", "b3000_SHeFC",
               "b688_SHePD", "b977_SHePD", "b1266_SHePD", "b1555_SHePD", "b1844_SHePD", "b2133_SHePD", "b2422_SHePD", "b2711_SHePD", "b3000_SHePD",
               "b688_SHeCL", "b977_SHeCL", "b1266_SHeCL", "b1555_SHeCL", "b1844_SHeCL", "b2133_SHeCL", "b2422_SHeCL", "b2711_SHeCL", "b3000_SHeCL")
length(mod_names)
SH_bivariados_AIC <- aictab(cand.set = SH_bivariados, modnames = mod_names)
SH_bivariados_AIC <- data.frame(SH_bivariados_AIC)
SH_bivariados_AIC[c('buffer', 'var')] <- str_split_fixed(SH_bivariados_AIC$Modnames, '_', 2)
SH_bivariados_AIC$buffer <- gsub("b","", SH_bivariados_AIC$buffer)
model_type <- c(rep("bivariado", times = nrow(SH_bivariados_AIC)))
SH_bivariados_AIC <- cbind(SH_bivariados_AIC, model_type)

SH_trivariados <- list(b688_FCePDeSH, b977_FCePDeSH, b1266_FCePDeSH, b1555_FCePDeSH, b1844_FCePDeSH, b2133_FCePDeSH, b2422_FCePDeSH, b2711_FCePDeSH, b3000_FCePDeSH,
                       b688_FCeSHeCL, b977_FCeSHeCL, b1266_FCeSHeCL, b1555_FCeSHeCL, b1844_FCeSHeCL, b2133_FCeSHeCL, b2422_FCeSHeCL, b2711_FCeSHeCL, b3000_FCeSHeCL,
                       b688_PDeSHeCL, b977_PDeSHeCL, b1266_PDeSHeCL, b1555_PDeSHeCL, b1844_PDeSHeCL, b2133_PDeSHeCL, b2422_PDeSHeCL, b2711_PDeSHeCL, b3000_PDeSHeCL)
length(SH_trivariados)
mod_names <- c("b688_FCePDeSH", "b977_FCePDeSH", "b1266_FCePDeSH", "b1555_FCePDeSH", "b1844_FCePDeSH", "b2133_FCePDeSH", "b2422_FCePDeSH", "b2711_FCePDeSH", "b3000_FCePDeSH",
               "b688_FCeSHeCL", "b977_FCeSHeCL", "b1266_FCeSHeCL", "b1555_FCeSHeCL", "b1844_FCeSHeCL", "b2133_FCeSHeCL", "b2422_FCeSHeCL", "b2711_FCeSHeCL", "b3000_FCeSHeCL",
               "b688_PDeSHeCL", "b977_PDeSHeCL", "b1266_PDeSHeCL", "b1555_PDeSHeCL", "b1844_PDeSHeCL", "b2133_PDeSHeCL", "b2422_PDeSHeCL", "b2711_PDeSHeCL", "b3000_PDeSHeCL")
length(mod_names)
SH_trivariados_AIC <- aictab(cand.set = SH_trivariados, modnames = mod_names)
SH_trivariados_AIC <- data.frame(SH_trivariados_AIC)
SH_trivariados_AIC[c('buffer', 'var')] <- str_split_fixed(SH_trivariados_AIC$Modnames, '_', 2)
SH_trivariados_AIC$buffer <- gsub("b","", SH_trivariados_AIC$buffer)
model_type <- c(rep("trivariado", times = nrow(SH_trivariados_AIC)))
SH_trivariados_AIC <- cbind(SH_trivariados_AIC, model_type)

SH_tetravariados <- list(b688_FCePDeSHeCL, b977_FCePDeSHeCL, b1266_FCePDeSHeCL, b1555_FCePDeSHeCL, b1844_FCePDeSHeCL, b2133_FCePDeSHeCL, b2422_FCePDeSHeCL, b2711_FCePDeSHeCL, b3000_FCePDeSHeCL)
length(SH_tetravariados)
mod_names <- c("b688_FCePDeSHeCL", "b977_FCePDeSHeCL", "b1266_FCePDeSHeCL", "b1555_FCePDeSHeCL", "b1844_FCePDeSHeCL", "b2133_FCePDeSHeCL", "b2422_FCePDeSHeCL", "b2711_FCePDeSHeCL", "b3000_FCePDeSHeCL")
length(mod_names)
SH_tetravariados_AIC <- aictab(cand.set = SH_tetravariados, modnames = mod_names)
SH_tetravariados_AIC <- data.frame(SH_tetravariados_AIC)
SH_tetravariados_AIC[c('buffer', 'var')] <- str_split_fixed(SH_tetravariados_AIC$Modnames, '_', 2)
SH_tetravariados_AIC$buffer <- gsub("b","", SH_tetravariados_AIC$buffer)
model_type <- c(rep("tetravariado", times = nrow(SH_tetravariados_AIC)))
SH_tetravariados_AIC <- cbind(SH_tetravariados_AIC, model_type)

SH_AIC <- rbind(nulos_AIC, SH_univariados_AIC, SH_bivariados_AIC, SH_trivariados_AIC, SH_tetravariados_AIC)
write.csv(SH_AIC, "G:/Bianca_mestrado/cap_2_versao_2/estrategia_5/SH_AIC_dens.csv")

cols <- c("null model" = "black", "univariado" = "#e41a1c", "bivariado" = "#377eb8", "trivariado" = "#4daf4a", "tetravariado" = "#984ea3")
SH <- ggplot(SH_AIC, aes(x = as.numeric(buffer), y = AICc))+
  theme_classic(base_size = 16)+
  geom_vline(xintercept = c(688, 977, 1266, 1555, 1844, 2133, 2422, 2711, 3000), color = "gray")+
  geom_point(aes(colour = factor(model_type)), size = 2)+
  geom_line(aes(x = as.numeric(buffer), y = AICc, group = var, colour = factor(model_type)))+
  scale_x_continuous(breaks = c(688, 977, 1266, 1555, 1844, 2133, 2422, 2711, 3000))+
  scale_colour_manual(values = cols, name = "")+
  labs(x = "Buffer radius (m)", y = "AICc")
SH
SH <- SH + ylim (60, 80)
SH
SH_AIC[order(SH_AIC$AICc),]

### Clumpy ----
CL_univariados <- list(b688_CL, b977_CL, b1266_CL, b1555_CL, b1844_CL, b2133_CL, b2422_CL, b2711_CL, b3000_CL)
length(CL_univariados)
mod_names <- c("b688_CL", "b977_CL", "b1266_CL", "b1555_CL", "b1844_CL", "b2133_CL", "b2422_CL", "b2711_CL", "b3000_CL")
length(mod_names)
CL_univariados_AIC <- aictab(cand.set = CL_univariados, modnames = mod_names)
CL_univariados_AIC <- data.frame(CL_univariados_AIC)
CL_univariados_AIC[c('buffer', 'var')] <- str_split_fixed(CL_univariados_AIC$Modnames, '_', 2)
CL_univariados_AIC$buffer <- gsub("b","", CL_univariados_AIC$buffer)
model_type <- c(rep("univariado", times = nrow(CL_univariados_AIC)))
CL_univariados_AIC <- cbind(CL_univariados_AIC, model_type)

CL_bivariados <- list(b688_FCeCL, b977_FCeCL, b1266_FCeCL, b1555_FCeCL, b1844_FCeCL, b2133_FCeCL, b2422_FCeCL, b2711_FCeCL, b3000_FCeCL,
                      b688_PDeCL, b977_PDeCL, b1266_PDeCL, b1555_PDeCL, b1844_PDeCL, b2133_PDeCL, b2422_PDeCL, b2711_PDeCL, b3000_PDeCL,
                      b688_SHeCL, b977_SHeCL, b1266_SHeCL, b1555_SHeCL, b1844_SHeCL, b2133_SHeCL, b2422_SHeCL, b2711_SHeCL, b3000_SHeCL)
length(CL_bivariados)
mod_names <- c("b688_CLeFC", "b977_CLeFC", "b1266_CLeFC", "b1555_CLeFC", "b1844_CLeFC", "b2133_CLeFC", "b2422_CLeFC", "b2711_CLeFC", "b3000_CLeFC",
               "b688_CLePD", "b977_CLePD", "b1266_CLePD", "b1555_CLePD", "b1844_CLePD", "b2133_CLePD", "b2422_CLePD", "b2711_CLePD", "b3000_CLePD",
               "b688_CLeSH", "b977_CLeSH", "b1266_CLeSH", "b1555_CLeSH", "b1844_CLeSH", "b2133_CLeSH", "b2422_CLeSH", "b2711_CLeSH", "b3000_CLeSH")
length(mod_names)
CL_bivariados_AIC <- aictab(cand.set = CL_bivariados, modnames = mod_names)
CL_bivariados_AIC <- data.frame(CL_bivariados_AIC)
CL_bivariados_AIC[c('buffer', 'var')] <- str_split_fixed(CL_bivariados_AIC$Modnames, '_', 2)
CL_bivariados_AIC$buffer <- gsub("b","", CL_bivariados_AIC$buffer)
model_type <- c(rep("bivariado", times = nrow(CL_bivariados_AIC)))
CL_bivariados_AIC <- cbind(CL_bivariados_AIC, model_type)

CL_trivariados <- list(b688_FCePDeCL, b977_FCePDeCL, b1266_FCePDeCL, b1555_FCePDeCL, b1844_FCePDeCL, b2133_FCePDeCL, b2422_FCePDeCL, b2711_FCePDeCL, b3000_FCePDeCL,
                       b688_FCeSHeCL, b977_FCeSHeCL, b1266_FCeSHeCL, b1555_FCeSHeCL, b1844_FCeSHeCL, b2133_FCeSHeCL, b2422_FCeSHeCL, b2711_FCeSHeCL, b3000_FCeSHeCL,
                       b688_PDeSHeCL, b977_PDeSHeCL, b1266_PDeSHeCL, b1555_PDeSHeCL, b1844_PDeSHeCL, b2133_PDeSHeCL, b2422_PDeSHeCL, b2711_PDeSHeCL, b3000_PDeSHeCL)
length(CL_trivariados)
mod_names <- c("b688_FCePDeCL", "b977_FCePDeCL", "b1266_FCePDeCL", "b1555_FCePDeCL", "b1844_FCePDeCL", "b2133_FCePDeCL", "b2422_FCePDeCL", "b2711_FCePDeCL", "b3000_FCePDeCL",
               "b688_FCeSHeCL", "b977_FCeSHeCL", "b1266_FCeSHeCL", "b1555_FCeSHeCL", "b1844_FCeSHeCL", "b2133_FCeSHeCL", "b2422_FCeSHeCL", "b2711_FCeSHeCL", "b3000_FCeSHeCL",
               "b688_PDeSHeCL", "b977_PDeSHeCL", "b1266_PDeSHeCL", "b1555_PDeSHeCL", "b1844_PDeSHeCL", "b2133_PDeSHeCL", "b2422_PDeSHeCL", "b2711_PDeSHeCL", "b3000_PDeSHeCL")
length(mod_names)
CL_trivariados_AIC <- aictab(cand.set = CL_trivariados, modnames = mod_names)
CL_trivariados_AIC <- data.frame(CL_trivariados_AIC)
CL_trivariados_AIC[c('buffer', 'var')] <- str_split_fixed(CL_trivariados_AIC$Modnames, '_', 2)
CL_trivariados_AIC$buffer <- gsub("b","", CL_trivariados_AIC$buffer)
model_type <- c(rep("trivariado", times = nrow(CL_trivariados_AIC)))
CL_trivariados_AIC <- cbind(CL_trivariados_AIC, model_type)

CL_tetravariados <- list(b688_FCePDeSHeCL, b977_FCePDeSHeCL, b1266_FCePDeSHeCL, b1555_FCePDeSHeCL, b1844_FCePDeSHeCL, b2133_FCePDeSHeCL, b2422_FCePDeSHeCL, b2711_FCePDeSHeCL, b3000_FCePDeSHeCL)
length(CL_tetravariados)
mod_names <- c("b688_FCePDeSHeCL", "b977_FCePDeSHeCL", "b1266_FCePDeSHeCL", "b1555_FCePDeSHeCL", "b1844_FCePDeSHeCL", "b2133_FCePDeSHeCL", "b2422_FCePDeSHeCL", "b2711_FCePDeSHeCL", "b3000_FCePDeSHeCL")
length(mod_names)
CL_tetravariados_AIC <- aictab(cand.set = CL_tetravariados, modnames = mod_names)
CL_tetravariados_AIC <- data.frame(CL_tetravariados_AIC)
CL_tetravariados_AIC[c('buffer', 'var')] <- str_split_fixed(CL_tetravariados_AIC$Modnames, '_', 2)
CL_tetravariados_AIC$buffer <- gsub("b","", CL_tetravariados_AIC$buffer)
model_type <- c(rep("tetravariado", times = nrow(CL_tetravariados_AIC)))
CL_tetravariados_AIC <- cbind(CL_tetravariados_AIC, model_type)

CL_AIC <- rbind(nulos_AIC, CL_univariados_AIC, CL_bivariados_AIC, CL_trivariados_AIC, CL_tetravariados_AIC)
write.csv(CL_AIC, "G:/Bianca_mestrado/cap_2_versao_2/estrategia_5/CL_AIC_dens.csv")

CL <- ggplot(CL_AIC, aes(x = as.numeric(buffer), y = AICc))+
  theme_classic(base_size = 16)+
  geom_vline(xintercept = c(688, 977, 1266, 1555, 1844, 2133, 2422, 2711, 3000), color = "gray")+
  geom_point(aes(colour = factor(model_type)), size = 2)+
  geom_line(aes(x = as.numeric(buffer), y = AICc, group = var, colour = factor(model_type)))+
  scale_x_continuous(breaks = c(688, 977, 1266, 1555, 1844, 2133, 2422, 2711, 3000))+
  scale_colour_manual(values = cols, name = "")+
  labs(x = "Buffer radius (m)", y = "AICc")
CL
CL <- CL + ylim (60, 80)
CL
CL_AIC[order(CL_AIC$AICc),]

#Salvando os plots

tiff(file="D:/Bianca_mestrado/cap_2_versao_2/estrategia_5/FC_AIC.tiff", width=8, height=4, units="in", res=100)
FC
dev.off()

tiff(file="D:/Bianca_mestrado/cap_2_versao_2/estrategia_5/PD_AIC.tiff", width=8, height=4, units="in", res=100)
PD
dev.off()

tiff(file="D:/Bianca_mestrado/cap_2_versao_2/estrategia_5/SH_AIC.tiff", width=8, height=4, units="in", res=100)
SH
dev.off()

tiff(file="D:/Bianca_mestrado/cap_2_versao_2/estrategia_5/CL_AIC.tiff", width=8, height=4, units="in", res=100)
CL
dev.off()

## Salvando valores de beta ----
### b1844----

#### Para FC
FC <- broom::tidy(b1844_FC, conf.int = TRUE)
FCePD <- broom::tidy(b1844_FCePD, conf.int = TRUE)
FCeSH <- broom::tidy(b1844_FCeSH, conf.int = TRUE)
FCeCL <- broom::tidy(b1844_FCeCL, conf.int = TRUE)
FCePDeSH <- broom::tidy(b1844_FCePDeSH, conf.int = TRUE)
FCePDeCL <- broom::tidy(b1844_FCePDeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b1844_FCeSHeCL, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b1844_FCePDeSHeCL, conf.int = TRUE)

estimates_FC <- rbind(FC, FCeCL, FCePD, FCePDeCL, FCePDeSH, FCePDeSHeCL, FCeSH, FCeSHeCL)
estimates_FC_onlyFC <- estimates_FC[which(estimates_FC$term == "scale(prop_florest)"),]
estimates_FC_onlyFC$term <- c("FC", "FCeCL", "FCePD", "FCePDeCL", "FCePDeSH", "FCePDeSHeCL", "FCeSH", "FCeSHeCL")
estimates_FC_onlyFC
estimates_FC_onlyFC$variable <- c("univariate", rep("other", 7))
FC_AIC[order(FC_AIC$AICc),]#acho que isso n?o t? certo, n?o deveria ser o melhor por buffer (que tem mais de um?)
estimates_FC_onlyFC[which(estimates_FC_onlyFC$term == "FCeCL"), 8] <- "better"
estimates_FC_onlyFC
library(GGally)
estimates_FC_plot <- ggcoef(estimates_FC_onlyFC, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_FC_plot

estimates_FC_plot <- estimates_FC_plot + xlim(-1, 1) 
estimates_FC_plot

#salvando plot
tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b1844_estimates_FC_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_FC_plot
dev.off()

## Para PD

PD<- broom::tidy(b1844_PD, conf.int = TRUE)
FCePDeCL<- broom::tidy(b1844_FCePDeCL, conf.int = TRUE)
FCePDeSH<- broom::tidy(b1844_FCePDeSH, conf.int = TRUE)
FCePDeSHeCL<- broom::tidy(b1844_FCePDeSHeCL, conf.int = TRUE)
PDeCL<- broom::tidy(b1844_PDeCL, conf.int = TRUE)
PDeFC<- broom::tidy(b1844_FCePD, conf.int = TRUE)
PDeSH<- broom::tidy(b1844_PDeSH, conf.int = TRUE)
PDeSHeCL<- broom::tidy(b1844_PDeSHeCL, conf.int = TRUE)

estimates_PD <- rbind(PD, FCePDeCL, FCePDeSH, FCePDeSHeCL, PDeCL, PDeFC, PDeSH, PDeSHeCL)
estimates_PD_onlyPD <- estimates_PD[which(estimates_PD$term == "scale(dens_frags)"),]
estimates_PD_onlyPD$term <- c("PD", "FCePDeCL", "FCePDeSH", "FCePDeSHeCL", "PDeCL", "PDeFC", "PDeSH", "PDeSHeCL")
estimates_PD_onlyPD
estimates_PD_onlyPD$variable <- c("univariate", rep("other", 7))
PD_AIC[order(PD_AIC$AICc),]
estimates_PD_onlyPD[which(estimates_PD_onlyPD$term == "PDeCL"), 8] <- "better"
estimates_PD_onlyPD

estimates_PD_plot <- ggcoef(estimates_PD_onlyPD, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_PD_plot

estimates_PD_plot <- estimates_PD_plot + xlim(-1, 1) 
estimates_PD_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b1844_estimates_PD_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_PD_plot
dev.off()


## Para SH
SH <- broom::tidy(b1844_SH, conf.int = TRUE)
FCePDeSH <- broom::tidy(b1844_FCePDeSH, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b1844_FCePDeSHeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b1844_FCeSHeCL, conf.int = TRUE)
PDeSHeCL <- broom::tidy(b1844_PDeSHeCL, conf.int = TRUE)
SHeCL <- broom::tidy(b1844_SHeCL, conf.int = TRUE)
SHeFC <- broom::tidy(b1844_FCeSH, conf.int = TRUE)
SHePD <- broom::tidy(b1844_PDeSH, conf.int = TRUE)

estimates_SH <- rbind(SH, FCePDeSH, FCePDeSHeCL, FCeSHeCL, PDeSHeCL, SHeCL, SHeFC, SHePD)
estimates_SH_onlySH <- estimates_SH[which(estimates_SH$term == "scale(shape_mn)"),]
estimates_SH_onlySH$term <- c("SH", "FCePDeSH", "FCePDeSHeCL", "FCeSHeCL", "PDeSHeCL", "SHeCL", "SHeFC", "SHePD")
estimates_SH_onlySH
estimates_SH_onlySH$variable <- c("univariate", rep("other", 7))
SH_AIC[order(SH_AIC$AICc),]
estimates_SH_onlySH[which(estimates_SH_onlySH$term == "SHeCL"), 8] <- "better"
estimates_SH_onlySH


estimates_SH_plot <- ggcoef(estimates_SH_onlySH, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_SH_plot

estimates_SH_plot <- estimates_SH_plot + xlim(-1, 1) 
estimates_SH_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b1844_estimates_SH_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_SH_plot
dev.off()

## Para CL
CL <- broom::tidy(b1844_CL, conf.int = TRUE)
CLeFC <- broom::tidy(b1844_FCeCL, conf.int = TRUE)
CLePD <- broom::tidy(b1844_PDeCL, conf.int = TRUE)
CLeSH <- broom::tidy(b1844_SHeCL, conf.int = TRUE)
FCePDeCL <- broom::tidy(b1844_FCePDeCL, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b1844_FCePDeSHeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b1844_FCeSHeCL, conf.int = TRUE)
PDeSHeCL <- broom::tidy(b1844_PDeSHeCL, conf.int = TRUE)

estimates_CL <- rbind(CL, CLeFC, CLePD, CLeSH, FCePDeCL, FCePDeSHeCL, FCeSHeCL, PDeSHeCL)
estimates_CL_onlyCL <- estimates_CL[which(estimates_CL$term == "scale(clumpy)"),]
estimates_CL_onlyCL$term <- c("CL", "CLeFC", "CLePD", "CLeSH", "FCePDeCL", "FCePDeSHeCL", "FCeSHeCL", "PDeSHeCL")
estimates_CL_onlyCL
estimates_CL_onlyCL$variable <- c("univariate", rep("other", 7))
CL_AIC[order(CL_AIC$AICc),]
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CL"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLeFC"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLePD"), 8] <- "better"
estimates_CL_onlyCL

estimates_CL_plot <- ggcoef(estimates_CL_onlyCL, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_CL_plot

estimates_CL_plot <- estimates_CL_plot + xlim(-1,1) 
estimates_CL_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b1844_estimates_CL_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_CL_plot
dev.off()

### b2133----

#### Para FC
FC <- broom::tidy(b2133_FC, conf.int = TRUE)
FCePD <- broom::tidy(b2133_FCePD, conf.int = TRUE)
FCeSH <- broom::tidy(b2133_FCeSH, conf.int = TRUE)
FCeCL <- broom::tidy(b2133_FCeCL, conf.int = TRUE)
FCePDeSH <- broom::tidy(b2133_FCePDeSH, conf.int = TRUE)
FCePDeCL <- broom::tidy(b2133_FCePDeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b2133_FCeSHeCL, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b2133_FCePDeSHeCL, conf.int = TRUE)

estimates_FC <- rbind(FC, FCeCL, FCePD, FCePDeCL, FCePDeSH, FCePDeSHeCL, FCeSH, FCeSHeCL)
estimates_FC_onlyFC <- estimates_FC[which(estimates_FC$term == "scale(prop_florest)"),]
estimates_FC_onlyFC$term <- c("FC", "FCeCL", "FCePD", "FCePDeCL", "FCePDeSH", "FCePDeSHeCL", "FCeSH", "FCeSHeCL")
estimates_FC_onlyFC
estimates_FC_onlyFC$variable <- c("univariate", rep("other", 7))
FC_AIC[order(FC_AIC$AICc),]
estimates_FC_onlyFC[which(estimates_FC_onlyFC$term == "FCePDeCL"), 8] <- "better"
estimates_FC_onlyFC[which(estimates_FC_onlyFC$term == "FCeCL"), 8] <- "better"
estimates_FC_onlyFC
library(GGally)
estimates_FC_plot <- ggcoef(estimates_FC_onlyFC, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_FC_plot

estimates_FC_plot <- estimates_FC_plot + xlim(-1, 1) 
estimates_FC_plot

#salvando plot
tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2133_estimates_FC_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_FC_plot
dev.off()

## Para PD
PD<- broom::tidy(b2133_PD, conf.int = TRUE)
FCePDeCL<- broom::tidy(b2133_FCePDeCL, conf.int = TRUE)
FCePDeSH<- broom::tidy(b2133_FCePDeSH, conf.int = TRUE)
FCePDeSHeCL<- broom::tidy(b2133_FCePDeSHeCL, conf.int = TRUE)
PDeCL<- broom::tidy(b2133_PDeCL, conf.int = TRUE)
PDeFC<- broom::tidy(b2133_FCePD, conf.int = TRUE)
PDeSH<- broom::tidy(b2133_PDeSH, conf.int = TRUE)
PDeSHeCL<- broom::tidy(b2133_PDeSHeCL, conf.int = TRUE)

estimates_PD <- rbind(PD, FCePDeCL, FCePDeSH, FCePDeSHeCL, PDeCL, PDeFC, PDeSH, PDeSHeCL)
estimates_PD_onlyPD <- estimates_PD[which(estimates_PD$term == "scale(dens_frags)"),]
estimates_PD_onlyPD$term <- c("PD", "FCePDeCL", "FCePDeSH", "FCePDeSHeCL", "PDeCL", "PDeFC", "PDeSH", "PDeSHeCL")
estimates_PD_onlyPD
estimates_PD_onlyPD$variable <- c("univariate", rep("other", 7))
PD_AIC[order(PD_AIC$AICc),]
estimates_PD_onlyPD[which(estimates_PD_onlyPD$term == "FCePDeCL"), 8] <- "better"
estimates_PD_onlyPD[which(estimates_PD_onlyPD$term == "PDeCL"), 8] <- "better"
estimates_PD_onlyPD

estimates_PD_plot <- ggcoef(estimates_PD_onlyPD, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_PD_plot

estimates_PD_plot <- estimates_PD_plot + xlim(-1, 1) 
estimates_PD_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2133_estimates_PD_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_PD_plot
dev.off()


## Para SH
SH <- broom::tidy(b2133_SH, conf.int = TRUE)
FCePDeSH <- broom::tidy(b2133_FCePDeSH, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b2133_FCePDeSHeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b2133_FCeSHeCL, conf.int = TRUE)
PDeSHeCL <- broom::tidy(b2133_PDeSHeCL, conf.int = TRUE)
SHeCL <- broom::tidy(b2133_SHeCL, conf.int = TRUE)
SHeFC <- broom::tidy(b2133_FCeSH, conf.int = TRUE)
SHePD <- broom::tidy(b2133_PDeSH, conf.int = TRUE)

estimates_SH <- rbind(SH, FCePDeSH, FCePDeSHeCL, FCeSHeCL, PDeSHeCL, SHeCL, SHeFC, SHePD)
estimates_SH_onlySH <- estimates_SH[which(estimates_SH$term == "scale(shape_mn)"),]
estimates_SH_onlySH$term <- c("SH", "FCePDeSH", "FCePDeSHeCL", "FCeSHeCL", "PDeSHeCL", "SHeCL", "SHeFC", "SHePD")
estimates_SH_onlySH
estimates_SH_onlySH$variable <- c("univariate", rep("other", 7))
SH_AIC[order(SH_AIC$AICc),]
estimates_SH_onlySH[which(estimates_SH_onlySH$term == "SHeCL"), 8] <- "better"
estimates_SH_onlySH[which(estimates_SH_onlySH$term == "SH"), 8] <- "better"
estimates_SH_onlySH

estimates_SH_plot <- ggcoef(estimates_SH_onlySH, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_SH_plot

estimates_SH_plot <- estimates_SH_plot + xlim(-1, 1) 
estimates_SH_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2133_estimates_SH_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_SH_plot
dev.off()

## Para CL
CL <- broom::tidy(b2133_CL, conf.int = TRUE)
CLeFC <- broom::tidy(b2133_FCeCL, conf.int = TRUE)
CLePD <- broom::tidy(b2133_PDeCL, conf.int = TRUE)
CLeSH <- broom::tidy(b2133_SHeCL, conf.int = TRUE)
FCePDeCL <- broom::tidy(b2133_FCePDeCL, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b2133_FCePDeSHeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b2133_FCeSHeCL, conf.int = TRUE)
PDeSHeCL <- broom::tidy(b2133_PDeSHeCL, conf.int = TRUE)

estimates_CL <- rbind(CL, CLeFC, CLePD, CLeSH, FCePDeCL, FCePDeSHeCL, FCeSHeCL, PDeSHeCL)
estimates_CL_onlyCL <- estimates_CL[which(estimates_CL$term == "scale(clumpy)"),]
estimates_CL_onlyCL$term <- c("CL", "CLeFC", "CLePD", "CLeSH", "FCePDeCL", "FCePDeSHeCL", "FCeSHeCL", "PDeSHeCL")
estimates_CL_onlyCL
estimates_CL_onlyCL$variable <- c("univariate", rep("other", 7))
CL_AIC[order(CL_AIC$AICc),]
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "FCePDeCL"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CL"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLePD"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLeFC"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLeSH"), 8] <- "better"
estimates_CL_onlyCL

estimates_CL_plot <- ggcoef(estimates_CL_onlyCL, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_CL_plot

estimates_CL_plot <- estimates_CL_plot + xlim(-1,1) 
estimates_CL_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2133_estimates_CL_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_CL_plot
dev.off()

### b2422----

#### Para FC
FC <- broom::tidy(b2422_FC, conf.int = TRUE)
FCePD <- broom::tidy(b2422_FCePD, conf.int = TRUE)
FCeSH <- broom::tidy(b2422_FCeSH, conf.int = TRUE)
FCeCL <- broom::tidy(b2422_FCeCL, conf.int = TRUE)
FCePDeSH <- broom::tidy(b2422_FCePDeSH, conf.int = TRUE)
FCePDeCL <- broom::tidy(b2422_FCePDeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b2422_FCeSHeCL, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b2422_FCePDeSHeCL, conf.int = TRUE)

estimates_FC <- rbind(FC, FCeCL, FCePD, FCePDeCL, FCePDeSH, FCePDeSHeCL, FCeSH, FCeSHeCL)
estimates_FC_onlyFC <- estimates_FC[which(estimates_FC$term == "scale(prop_florest)"),]
estimates_FC_onlyFC$term <- c("FC", "FCeCL", "FCePD", "FCePDeCL", "FCePDeSH", "FCePDeSHeCL", "FCeSH", "FCeSHeCL")
estimates_FC_onlyFC
estimates_FC_onlyFC$variable <- c("univariate", rep("other", 7))
FC_AIC[order(FC_AIC$AICc),]
estimates_FC_onlyFC[which(estimates_FC_onlyFC$term == "FCePDeCL"), 8] <- "better"
estimates_FC_onlyFC[which(estimates_FC_onlyFC$term == "FCeCL"), 8] <- "better"
estimates_FC_onlyFC
library(GGally)
estimates_FC_plot <- ggcoef(estimates_FC_onlyFC, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_FC_plot

estimates_FC_plot <- estimates_FC_plot + xlim(-1, 1) 
estimates_FC_plot

#salvando plot
tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2422_estimates_FC_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_FC_plot
dev.off()

## Para PD

PD<- broom::tidy(b2422_PD, conf.int = TRUE)
FCePDeCL<- broom::tidy(b2422_FCePDeCL, conf.int = TRUE)
FCePDeSH<- broom::tidy(b2422_FCePDeSH, conf.int = TRUE)
FCePDeSHeCL<- broom::tidy(b2422_FCePDeSHeCL, conf.int = TRUE)
PDeCL<- broom::tidy(b2422_PDeCL, conf.int = TRUE)
PDeFC<- broom::tidy(b2422_FCePD, conf.int = TRUE)
PDeSH<- broom::tidy(b2422_PDeSH, conf.int = TRUE)
PDeSHeCL<- broom::tidy(b2422_PDeSHeCL, conf.int = TRUE)

estimates_PD <- rbind(PD, FCePDeCL, FCePDeSH, FCePDeSHeCL, PDeCL, PDeFC, PDeSH, PDeSHeCL)
estimates_PD_onlyPD <- estimates_PD[which(estimates_PD$term == "scale(dens_frags)"),]
estimates_PD_onlyPD$term <- c("PD", "FCePDeCL", "FCePDeSH", "FCePDeSHeCL", "PDeCL", "PDeFC", "PDeSH", "PDeSHeCL")
estimates_PD_onlyPD
estimates_PD_onlyPD$variable <- c("univariate", rep("other", 7))
PD_AIC[order(PD_AIC$AICc),]
estimates_PD_onlyPD[which(estimates_PD_onlyPD$term == "FCePDeCL"), 8] <- "better"
estimates_PD_onlyPD[which(estimates_PD_onlyPD$term == "PDeCL"), 8] <- "better"
estimates_PD_onlyPD

estimates_PD_plot <- ggcoef(estimates_PD_onlyPD, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_PD_plot

estimates_PD_plot <- estimates_PD_plot + xlim(-1, 1) 
estimates_PD_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2422_estimates_PD_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_PD_plot
dev.off()


## Para SH
SH <- broom::tidy(b2422_SH, conf.int = TRUE)
FCePDeSH <- broom::tidy(b2422_FCePDeSH, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b2422_FCePDeSHeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b2422_FCeSHeCL, conf.int = TRUE)
PDeSHeCL <- broom::tidy(b2422_PDeSHeCL, conf.int = TRUE)
SHeCL <- broom::tidy(b2422_SHeCL, conf.int = TRUE)
SHeFC <- broom::tidy(b2422_FCeSH, conf.int = TRUE)
SHePD <- broom::tidy(b2422_PDeSH, conf.int = TRUE)

estimates_SH <- rbind(SH, FCePDeSH, FCePDeSHeCL, FCeSHeCL, PDeSHeCL, SHeCL, SHeFC, SHePD)
estimates_SH_onlySH <- estimates_SH[which(estimates_SH$term == "scale(shape_mn)"),]
estimates_SH_onlySH$term <- c("SH", "FCePDeSH", "FCePDeSHeCL", "FCeSHeCL", "PDeSHeCL", "SHeCL", "SHeFC", "SHePD")
estimates_SH_onlySH
estimates_SH_onlySH$variable <- c("univariate", rep("other", 7))
SH_AIC[order(SH_AIC$AICc),]
estimates_SH_onlySH[which(estimates_SH_onlySH$term == "SH"), 8] <- "better"
estimates_SH_onlySH[which(estimates_SH_onlySH$term == "SHeCL"), 8] <- "better"
estimates_SH_onlySH

estimates_SH_plot <- ggcoef(estimates_SH_onlySH, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_SH_plot

estimates_SH_plot <- estimates_SH_plot + xlim(-1, 1) 
estimates_SH_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2422_estimates_SH_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_SH_plot
dev.off()

## Para CL
CL <- broom::tidy(b2422_CL, conf.int = TRUE)
CLeFC <- broom::tidy(b2422_FCeCL, conf.int = TRUE)
CLePD <- broom::tidy(b2422_PDeCL, conf.int = TRUE)
CLeSH <- broom::tidy(b2422_SHeCL, conf.int = TRUE)
FCePDeCL <- broom::tidy(b2422_FCePDeCL, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b2422_FCePDeSHeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b2422_FCeSHeCL, conf.int = TRUE)
PDeSHeCL <- broom::tidy(b2422_PDeSHeCL, conf.int = TRUE)

estimates_CL <- rbind(CL, CLeFC, CLePD, CLeSH, FCePDeCL, FCePDeSHeCL, FCeSHeCL, PDeSHeCL)
estimates_CL_onlyCL <- estimates_CL[which(estimates_CL$term == "scale(clumpy)"),]
estimates_CL_onlyCL$term <- c("CL", "CLeFC", "CLePD", "CLeSH", "FCePDeCL", "FCePDeSHeCL", "FCeSHeCL", "PDeSHeCL")
estimates_CL_onlyCL
estimates_CL_onlyCL$variable <- c("univariate", rep("other", 7))
CL_AIC[order(CL_AIC$AICc),]
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CL"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "FCePDeCL"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLePD"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLeFC"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLeSH"), 8] <- "better"
estimates_CL_onlyCL

estimates_CL_plot <- ggcoef(estimates_CL_onlyCL, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_CL_plot

estimates_CL_plot <- estimates_CL_plot + xlim(-1,1) 
estimates_CL_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2422_estimates_CL_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_CL_plot
dev.off()

### b2711----

#### Para FC
FC <- broom::tidy(b2711_FC, conf.int = TRUE)
FCePD <- broom::tidy(b2711_FCePD, conf.int = TRUE)
FCeSH <- broom::tidy(b2711_FCeSH, conf.int = TRUE)
FCeCL <- broom::tidy(b2711_FCeCL, conf.int = TRUE)
FCePDeSH <- broom::tidy(b2711_FCePDeSH, conf.int = TRUE)
FCePDeCL <- broom::tidy(b2711_FCePDeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b2711_FCeSHeCL, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b2711_FCePDeSHeCL, conf.int = TRUE)

estimates_FC <- rbind(FC, FCeCL, FCePD, FCePDeCL, FCePDeSH, FCePDeSHeCL, FCeSH, FCeSHeCL)
estimates_FC_onlyFC <- estimates_FC[which(estimates_FC$term == "scale(prop_florest)"),]
estimates_FC_onlyFC$term <- c("FC", "FCeCL", "FCePD", "FCePDeCL", "FCePDeSH", "FCePDeSHeCL", "FCeSH", "FCeSHeCL")
estimates_FC_onlyFC
estimates_FC_onlyFC$variable <- c("univariate", rep("other", 7))
FC_AIC[order(FC_AIC$AICc),]
estimates_FC_onlyFC[which(estimates_FC_onlyFC$term == "FCePDeCL"), 8] <- "better"
estimates_FC_onlyFC[which(estimates_FC_onlyFC$term == "FCeCL"), 8] <- "better"
estimates_FC_onlyFC
library(GGally)
estimates_FC_plot <- ggcoef(estimates_FC_onlyFC, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_FC_plot

estimates_FC_plot <- estimates_FC_plot + xlim(-1, 1) 
estimates_FC_plot

#salvando plot
tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2711_estimates_FC_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_FC_plot
dev.off()

## Para PD

PD<- broom::tidy(b2711_PD, conf.int = TRUE)
FCePDeCL<- broom::tidy(b2711_FCePDeCL, conf.int = TRUE)
FCePDeSH<- broom::tidy(b2711_FCePDeSH, conf.int = TRUE)
FCePDeSHeCL<- broom::tidy(b2711_FCePDeSHeCL, conf.int = TRUE)
PDeCL<- broom::tidy(b2711_PDeCL, conf.int = TRUE)
PDeFC<- broom::tidy(b2711_FCePD, conf.int = TRUE)
PDeSH<- broom::tidy(b2711_PDeSH, conf.int = TRUE)
PDeSHeCL<- broom::tidy(b2711_PDeSHeCL, conf.int = TRUE)

estimates_PD <- rbind(PD, FCePDeCL, FCePDeSH, FCePDeSHeCL, PDeCL, PDeFC, PDeSH, PDeSHeCL)
estimates_PD_onlyPD <- estimates_PD[which(estimates_PD$term == "scale(dens_frags)"),]
estimates_PD_onlyPD$term <- c("PD", "FCePDeCL", "FCePDeSH", "FCePDeSHeCL", "PDeCL", "PDeFC", "PDeSH", "PDeSHeCL")
estimates_PD_onlyPD
estimates_PD_onlyPD$variable <- c("univariate", rep("other", 7))
PD_AIC[order(PD_AIC$AICc),]
estimates_PD_onlyPD[which(estimates_PD_onlyPD$term == "FCePDeCL"), 8] <- "better"
estimates_PD_onlyPD[which(estimates_PD_onlyPD$term == "PDeCL"), 8] <- "better"
estimates_PD_onlyPD

estimates_PD_plot <- ggcoef(estimates_PD_onlyPD, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_PD_plot

estimates_PD_plot <- estimates_PD_plot + xlim(-1, 1) 
estimates_PD_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2711_estimates_PD_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_PD_plot
dev.off()


## Para SH
SH <- broom::tidy(b2711_SH, conf.int = TRUE)
FCePDeSH <- broom::tidy(b2711_FCePDeSH, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b2711_FCePDeSHeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b2711_FCeSHeCL, conf.int = TRUE)
PDeSHeCL <- broom::tidy(b2711_PDeSHeCL, conf.int = TRUE)
SHeCL <- broom::tidy(b2711_SHeCL, conf.int = TRUE)
SHeFC <- broom::tidy(b2711_FCeSH, conf.int = TRUE)
SHePD <- broom::tidy(b2711_PDeSH, conf.int = TRUE)

estimates_SH <- rbind(SH, FCePDeSH, FCePDeSHeCL, FCeSHeCL, PDeSHeCL, SHeCL, SHeFC, SHePD)
estimates_SH_onlySH <- estimates_SH[which(estimates_SH$term == "scale(shape_mn)"),]
estimates_SH_onlySH$term <- c("SH", "FCePDeSH", "FCePDeSHeCL", "FCeSHeCL", "PDeSHeCL", "SHeCL", "SHeFC", "SHePD")
estimates_SH_onlySH
estimates_SH_onlySH$variable <- c("univariate", rep("other", 7))
SH_AIC[order(SH_AIC$AICc),]
estimates_SH_onlySH[which(estimates_SH_onlySH$term == "SHeCL"), 8] <- "better"
estimates_SH_onlySH[which(estimates_SH_onlySH$term == "SH"), 8] <- "better"
estimates_SH_onlySH

estimates_SH_plot <- ggcoef(estimates_SH_onlySH, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_SH_plot

estimates_SH_plot <- estimates_SH_plot + xlim(-1, 1) 
estimates_SH_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2711_estimates_SH_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_SH_plot
dev.off()

## Para CL
CL <- broom::tidy(b2711_CL, conf.int = TRUE)
CLeFC <- broom::tidy(b2711_FCeCL, conf.int = TRUE)
CLePD <- broom::tidy(b2711_PDeCL, conf.int = TRUE)
CLeSH <- broom::tidy(b2711_SHeCL, conf.int = TRUE)
FCePDeCL <- broom::tidy(b2711_FCePDeCL, conf.int = TRUE)
FCePDeSHeCL <- broom::tidy(b2711_FCePDeSHeCL, conf.int = TRUE)
FCeSHeCL <- broom::tidy(b2711_FCeSHeCL, conf.int = TRUE)
PDeSHeCL <- broom::tidy(b2711_PDeSHeCL, conf.int = TRUE)

estimates_CL <- rbind(CL, CLeFC, CLePD, CLeSH, FCePDeCL, FCePDeSHeCL, FCeSHeCL, PDeSHeCL)
estimates_CL_onlyCL <- estimates_CL[which(estimates_CL$term == "scale(clumpy)"),]
estimates_CL_onlyCL$term <- c("CL", "CLeFC", "CLePD", "CLeSH", "FCePDeCL", "FCePDeSHeCL", "FCeSHeCL", "PDeSHeCL")
estimates_CL_onlyCL
estimates_CL_onlyCL$variable <- c("univariate", rep("other", 7))
CL_AIC[order(CL_AIC$AICc),]
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CL"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "FCePDeCL"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLeFC"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLePD"), 8] <- "better"
estimates_CL_onlyCL[which(estimates_CL_onlyCL$term == "CLeSH"), 8] <- "better"
estimates_CL_onlyCL

estimates_CL_plot <- ggcoef(estimates_CL_onlyCL, 
                            mapping = aes(x = estimate, y = term, colour = variable),
                            size = 5, sort = "ascending")+
  scale_color_manual(values = c("better" = "#f8766d", "other" = "#00ba38", "univariate" = "#619cff")) +
  theme_classic()
estimates_CL_plot

estimates_CL_plot <- estimates_CL_plot + xlim(-1,1) 
estimates_CL_plot

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/betas_versao_atualizada/b2711_estimates_CL_plot_dens.tiff", width=8, height=4, units="in", res=100)
estimates_CL_plot
dev.off()

## Graficos parciais ----

### b1844 ----
dados_cap2 <- read.csv("G:/Bianca_mestrado/cap_2_versao_2/dados_cap2_NA_clumpy.csv")
nrow(dados_cap2)
hist(dados_cap2$clumpy)
#fazendo subset
b1844 <- subset(dados_cap2, buffer== "buffer1844")
b1844 <- subset(b1844, select = c(-X, -X.1)) #removendo a colunas "X" e "X.1"

names(b1844)
b1844 <- b1844[, c(1, 6, 8, 15, 18, 23)]
names(b1844)
table(is.na(b1844))


#### Parciais b1844_CL ----
ggplot(data = b1844, aes(x = clumpy, y = dens_groups)) + 
  labs(x = "Clumpiness index", y = "Group density") +
  geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_classic()

tiff(file="G:/Bianca_mestrado/cap_2_versao_2/estrategia_5/figuras/b1844_parcial_CL.tiff", width=6, height=5, units="in", res=100)
ggplot(data = b1844, aes(x = clumpy, y = dens_groups)) + 
  labs(x = "Clumpiness index", y = "Group density") +
  geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_classic()
dev.off()

#rascunhos
#b1844_CL <- glm(log10(dens_groups+0.001) ~ scale(clumpy), data = b1844)
#log10(b1844$dens_groups+0.001)

#tiff(file="G:/Bianca_mestrado/cap_2_versao_2/estrategia_5/figuras/b1844_parcial_CL_scale.tiff", width=6, height=5, units="in", res=100)
#ggplot(data = b1844, aes(x = scale(clumpy), y = log10(dens_groups+0.001))) + 
#labs(x = "Clumpiness index", y = "Group density") +
#geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
#geom_smooth(method = lm, se = FALSE, color = "black") +
#theme_classic()
#dev.off()

#tiff(file="G:/Bianca_mestrado/cap_2_versao_2/estrategia_5/figuras/b1844_parcial_CL_logdens.tiff", width=6, height=5, units="in", res=100)
#ggplot(data = b1844, aes(x = clumpy, y =log10(dens_groups+0.001))) + 
# labs(x = "Clumpiness index", y = "Group density") +
#geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
#geom_smooth(method = lm, se = FALSE, color = "black") +
#theme_classic()
#dev.off()

#### Parciais b1844_FCeCL ----
library(pdp)
b1844_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b1844)
pd <- partial(b1844_FCeCL, pred.var = c("prop_florest", "clumpy"))
# Plotando
pdp1_a <- plotPartial(pd)
pdp1_a

tiff(file="D:/Bianca_mestrado/cap_2_versao_2/estrategia_5/b1844_parcial_FCeCL.tiff", width=6, height=5, units="in", res=100)
pdp1_a
dev.off()

#### Parciais b1844_PDeCL ----
b1844_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b1844)
pd <- partial(b1844_PDeCL, pred.var = c("dens_frags", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="D:/Bianca_mestrado/cap_2_versao_2/estrategia_5/b1844_parcial_PDeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()

### b2133 ----
b2133 <- subset(dados_cap2, buffer== "buffer2133")
b2133 <- subset(b2133, select = c(-X, -X.1))
#### Parciais b2133_FCePDeCL ----
b2133_FCePDeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags) + scale(clumpy), data = b2133)
pd <- partial(b2133_FCePDeCL, pred.var = c("prop_florest", "dens_frags", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="D:/Bianca_mestrado/cap_2_versao_2/estrategia_5/b2133_parcial_FCePDeCL.tiff", width=8, height=4, units="in", res=100)
pdp1
dev.off()

#### Parciais b2133_CL ----

ggplot(data = b2133, aes(x = clumpy, y = dens_groups)) + 
  labs(x = "Clumpiness index", y = "Group density") +
  geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_classic()

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2133_parcial_CL.tiff", width=6, height=5, units="in", res=100)
ggplot(data = b2133, aes(x = clumpy, y = dens_groups)) + 
  labs(x = "Clumpiness index", y = "Group density") +
  geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_classic()
dev.off()

#### Parciais b2133_PDeCL ----
b2133_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b2133)
pd <- partial(b2133_PDeCL, pred.var = c("dens_frags", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2133_parcial_PDeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()


#### Parciais b2133_FCeCL ----
b2133_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b2133)
pd <- partial(b2133_FCeCL, pred.var = c("prop_florest", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2133_parcial_FCeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()

#### Parciais b2133_SHeCL ----
b2133_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b2133)
pd <- partial(b2133_SHeCL, pred.var = c("shape_mn", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2133_parcial_SHeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()


#### Parciais b2133_SH ----
a <- ggplot(data = b2133, aes(x = shape_mn, y = dens_groups)) + 
  labs(x = "Shape index", y = "Group density") +
  geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_classic()
a

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2133_parcial_SH.tiff", width=6, height=5, units="in", res=100)
a
dev.off()


### b2422 ----
b2422 <- subset(dados_cap2, buffer== "buffer2422")
b2422 <- subset(b2422, select = c(-X, -X.1))

#### Parciais b2422_CL ----
a <- ggplot(data = b2422, aes(x = clumpy, y = dens_groups)) + 
  labs(x = "Clumpiness index", y = "Group density") +
  geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_classic()
a
tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2422_parcial_CL.tiff", width=6, height=5, units="in", res=100)
a
dev.off()

#### Parciais b2422_FCePDeCL ----
b2422_FCePDeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags) + scale(clumpy), data = b2422)
pd <- partial(b2422_FCePDeCL, pred.var = c("prop_florest", "dens_frags", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="D:/Bianca_mestrado/cap_2_versao_2/estrategia_5/b2422_parcial_FCePDeCL.tiff", width=8, height=4, units="in", res=100)
pdp1
dev.off()

#### Parciais b2422_PDeCL ----
b2422_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b2422)
pd <- partial(b2422_PDeCL, pred.var = c("dens_frags", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2422_parcial_PDeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()

#### Parciais b2422_FCeCL ----
b2422_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b2422)
pd <- partial(b2422_FCeCL, pred.var = c("prop_florest", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2422_parcial_FCeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()

#### Parciais b2422_SH ----
a <- ggplot(data = b2422, aes(x = shape_mn, y = dens_groups)) + 
  labs(x = "Shape index", y = "Group density") +
  geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_classic()
a

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2422_parcial_SH.tiff", width=6, height=5, units="in", res=100)
a
dev.off()

#### Parciais b2422_ null: nao faz sentido ter grafico----

#### Parciais b2422_SHeCL ----
b2422_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b2422)
pd <- partial(b2422_SHeCL, pred.var = c("shape_mn", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2422_parcial_SHeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()

### b2711 ----
b2711 <- subset(dados_cap2, buffer== "buffer2711")
b2711 <- subset(b2711, select = c(-X, -X.1))

#### Parciais b2711_CL ----

a <- ggplot(data = b2711, aes(x = clumpy, y = dens_groups)) + 
  labs(x = "Clumpiness index", y = "Group density") +
  geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_classic()
a

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2711_parcial_CL.tiff", width=6, height=5, units="in", res=100)
a
dev.off()

#### Parciais b2711_FCePDeCL ----
b2711_FCePDeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(dens_frags) + scale(clumpy), data = b2711)
pd <- partial(b2711_FCePDeCL, pred.var = c("prop_florest", "dens_frags", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="D:/Bianca_mestrado/cap_2_versao_2/estrategia_5/b2711_parcial_FCePDeCL.tiff", width=8, height=4, units="in", res=100)
pdp1
dev.off()

#### Parciais b2711_FCeCL ----
b2711_FCeCL <- glm(log10(dens_groups+0.001) ~ scale(prop_florest) + scale(clumpy), data = b2711)
pd <- partial(b2711_FCeCL, pred.var = c("prop_florest", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2711_parcial_FCeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()

#### Parciais b2711_PDeCL ----
b2711_PDeCL <- glm(log10(dens_groups+0.001) ~ scale(dens_frags) + scale(clumpy), data = b2711)
pd <- partial(b2711_PDeCL, pred.var = c("dens_frags", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2711_parcial_PDeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()

#### Parciais b2711_SHeCL ----
b2711_SHeCL <- glm(log10(dens_groups+0.001) ~ scale(shape_mn) + scale(clumpy), data = b2711)
pd <- partial(b2711_SHeCL, pred.var = c("shape_mn", "clumpy"))
# Plotando
pdp1 <- plotPartial(pd)
pdp1

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2711_parcial_SHeCL.tiff", width=6, height=5, units="in", res=100)
pdp1
dev.off()

#### Parciais b2711_SH ----
a <- ggplot(data = b2711, aes(x = shape_mn, y = dens_groups)) + 
  labs(x = "Shape index", y = "Group density") +
  geom_point(size = 4, shape = 21, fill = "grey", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_classic()
a
tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2711_parcial_SH.tiff", width=6, height=5, units="in", res=100)
a
dev.off()


### Escrevendo os resultados ----

#### valores de densidade - sem densidade 0 ----
b3000$dens_groups
b3000["dens_groups"][b3000["dens_groups"] == 0] <- NA
sd(b3000$dens_groups)
b3000$dens_groups_km2 <- (b3000$n_groups/(b3000$area_frag/100))
summary(b3000$dens_groups)

apply(b3000$dens_groups,function(x) sum(x > 0))

write.csv(b977, "G:/Bianca_mestrado/capitulo_2/executando/densidade/b977.csv")

#valores de densidade em fragmentos maiores que 50ha ----
b3000["area_frag"][b3000["area_frag"] > 49.9999] <- NA
b3000$dens_groups_50ha <- b3000$n_groups/b3000$area_frag
b3000["dens_groups_50ha"][b3000["dens_groups_50ha"] == 0] <- NA
summary(b3000$dens_groups_50ha)

b977 <- read.csv("D:/Bianca_mestrado/capitulo_2/executando/densidade/b977.csv")
names(b977)
b977 <- subset(b977, select = -(c(X, area, n_frags, media_area_frags, cv_area_frags, 
                                  cv_dist_eucl, pcore, dens_borda, shape_cv, iagreg,
                                  cv_cdi, prop_cdi))) #removendo essas colunas que não vão importar
b977_modelos <- b977[complete.cases(b977),]

### histogramas----

b1844$prop_florest
summary(b1844$prop_florest)
trasd(b1844$prop_florest)
hist(b1844$prop_florest, col ="#1a9850" , border= "white", ylim = range(0, 15), xlim = range(0, 60), breaks = 5)


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b1844_hist_FC.tiff", width=6, height=5, units="in", res=300)
hist(b1844$prop_florest, col ="#1a9850" , border= "white", ylim = range(0, 15), xlim = range(0, 60), breaks = 5)
dev.off()

b1844$dens_frags
summary(b1844$dens_frags)
mean(b1844$dens_frags)
sd(b1844$dens_frags)
hist(b1844$dens_frags, col = "#91cf60" , border= "white" , ylim = range(0, 15))

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b1844_hist_PD.tiff", width=6, height=5, units="in", res=300)
hist(b1844$dens_frags, col = "#91cf60" , border= "white" , ylim = range(0, 15))
dev.off()


b1844$shape_mn
summary(b1844$shape_mn)
mean(b1844$shape_mn)
sd(b1844$shape_mn)
hist(b1844$shape_mn,col = "#fee08b" , border= "white" , ylim = range(0, 15), xlim = range(1.5, 2.7), breaks= 10)


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b1844_hist_SH.tiff", width=6, height=5, units="in", res=300)
hist(b1844$shape_mn,col = "#fee08b" , border= "white" , ylim = range(0, 15), xlim = range(1.5, 2.7), breaks= 10)
dev.off()

b1844$clumpy
summary(b1844$clumpy)
mean(b1844$clumpy)
sd(b1844$clumpy)
hist(b1844$clumpy, col = "#fc8d59", border= "white", ylim = range(0, 15))


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b1844_hist_CL.tiff", width=6, height=5, units="in", res=300)
hist(b1844$clumpy, col = "#fc8d59", border= "white", ylim = range(0, 15))
dev.off()

#b2133

b2133$prop_florest
summary(b2133$prop_florest)
sd(b2133$prop_florest)
hist(b2133$prop_florest, col ="#1a9850" , border= "white", ylim = range(0, 15), xlim = range(0, 60), breaks = 5)


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2133_hist_FC.tiff", width=6, height=5, units="in", res=300)
hist(b2133$prop_florest, col ="#1a9850" , border= "white", ylim = range(0, 15), xlim = range(0, 60), breaks = 5)
dev.off()

b2133$dens_frags
summary(b2133$dens_frags)
mean(b2133$dens_frags)
sd(b2133$dens_frags)
hist(b2133$dens_frags, col = "#91cf60" , border= "white" , ylim = range(0, 15))

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2133_hist_PD.tiff", width=6, height=5, units="in", res=300)
hist(b2133$dens_frags, col = "#91cf60" , border= "white" , ylim = range(0, 15))
dev.off()


b2133$shape_mn
summary(b2133$shape_mn)
mean(b2133$shape_mn)
sd(b2133$shape_mn)
hist(b2133$shape_mn,col = "#fee08b" , border= "white" , ylim = range(0, 15), xlim = range(1.5, 2.7), breaks= 10)


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2133_hist_SH.tiff", width=6, height=5, units="in", res=300)
hist(b2133$shape_mn,col = "#fee08b" , border= "white" , ylim = range(0, 15), xlim = range(1.5, 2.7), breaks= 10)
dev.off()

b2133$clumpy
summary(b2133$clumpy)
mean(b2133$clumpy)
sd(b2133$clumpy)
hist(b2133$clumpy, col = "#fc8d59", border= "white", ylim = range(0, 15))


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2133_hist_CL.tiff", width=6, height=5, units="in", res=300)
hist(b2133$clumpy, col = "#fc8d59", border= "white", ylim = range(0, 15))
dev.off()

#b2422
b2422$prop_florest
summary(b2422$prop_florest)
sd(b2422$prop_florest)
hist(b2422$prop_florest, col ="#1a9850" , border= "white", ylim = range(0, 15), xlim = range(0, 60), breaks = 5)


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2422_hist_FC.tiff", width=6, height=5, units="in", res=300)
hist(b2422$prop_florest, col ="#1a9850" , border= "white", ylim = range(0, 15), xlim = range(0, 60), breaks = 5)
dev.off()

b2422$dens_frags
summary(b2422$dens_frags)
mean(b2422$dens_frags)
sd(b2422$dens_frags)
hist(b2422$dens_frags, col = "#91cf60" , border= "white" , ylim = range(0, 15))

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2422_hist_PD.tiff", width=6, height=5, units="in", res=300)
hist(b2422$dens_frags, col = "#91cf60" , border= "white" , ylim = range(0, 15))
dev.off()


b2422$shape_mn
summary(b2422$shape_mn)
mean(b2422$shape_mn)
sd(b2422$shape_mn)
hist(b2422$shape_mn,col = "#fee08b" , border= "white" , ylim = range(0, 15), xlim = range(1.5, 2.7), breaks= 10)


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2422_hist_SH.tiff", width=6, height=5, units="in", res=300)
hist(b2422$shape_mn,col = "#fee08b" , border= "white" , ylim = range(0, 15), xlim = range(1.5, 2.7), breaks= 10)
dev.off()

b2422$clumpy
summary(b2422$clumpy)
mean(b2422$clumpy)
sd(b2422$clumpy)
hist(b2422$clumpy, col = "#fc8d59", border= "white", ylim = range(0, 15))


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2422_hist_CL.tiff", width=6, height=5, units="in", res=300)
hist(b2422$clumpy, col = "#fc8d59", border= "white", ylim = range(0, 15))
dev.off()

#b2711
b2711$prop_florest
summary(b2711$prop_florest)
sd(b2711$prop_florest)
hist(b2711$prop_florest, col ="#1a9850" , border= "white", ylim = range(0, 15), xlim = range(0, 60), breaks = 5)


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2711_hist_FC.tiff", width=6, height=5, units="in", res=300)
hist(b2711$prop_florest, col ="#1a9850" , border= "white", ylim = range(0, 15), xlim = range(0, 60), breaks = 5)
dev.off()

b2711$dens_frags
summary(b2711$dens_frags)
mean(b2711$dens_frags)
sd(b2711$dens_frags)
hist(b2711$dens_frags, col = "#91cf60" , border= "white" , ylim = range(0, 15))

tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2711_hist_PD.tiff", width=6, height=5, units="in", res=300)
hist(b2711$dens_frags, col = "#91cf60" , border= "white" , ylim = range(0, 15))
dev.off()


b2711$shape_mn
summary(b2711$shape_mn)
mean(b2711$shape_mn)
sd(b2711$shape_mn)
hist(b2711$shape_mn,col = "#fee08b" , border= "white" , ylim = range(0, 15), xlim = range(1.5, 2.7), breaks= 10)


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2711_hist_SH.tiff", width=6, height=5, units="in", res=300)
hist(b2711$shape_mn,col = "#fee08b" , border= "white" , ylim = range(0, 15), xlim = range(1.5, 2.7), breaks= 10)
dev.off()

b2711$clumpy
summary(b2711$clumpy)
mean(b2711$clumpy)
sd(b2711$clumpy)
hist(b2711$clumpy, col = "#fc8d59", border= "white", ylim = range(0, 15), breaks=5)


tiff(file="C:/Users/Usuario/OneDrive/?rea de Trabalho/cap_2_versao_2/estrategia_5/b2711_hist_CL.tiff", width=6, height=5, units="in", res=300)
hist(b2711$clumpy, col = "#fc8d59", border= "white", ylim = range(0, 15), breaks=5)
dev.off()

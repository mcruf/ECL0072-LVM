#########################################
#                                       #
#         Prática da aula 05            #
#      - GLMM: dados binários -         #
#                                       #
#########################################


# Contexto:
#   - Análise da distribuição espacial de coalas em uma paisagem fragmentada (adaptado de Zuur et al., 2009; Cap. 21).

# Objetivo:
#   - Identificar os fatores ambientais e espaciais que determinam a ocorrência (presença/ausência) do coala em uma paisagem fragmentada

# Estrutura dos dados:
#   - Variável resposta: presença/ausência (binária, 0/1)
#   - Covariáveis: atributos locais (vegetação primária/secundária) e métricas de paisagem calculadas em múltiplos buffers concêntricos (1 km, 2.5 km e 5 km)
#   - Amostragem: 100 fragmentos (sites), com 3 pontos de amostragem por fragmento (subsites; N_total = 300)


# Abordagem analítica:
# - Ajuste de GLMM com distribuição Bernoulli (link logit) 
#   - 3 modelos: (i) modelo nulo, (ii) modelo com efeitos fixos, (iii) modelo com efeitos fixos & aleatórios



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) #Manipulação de dados
library(ggplot2) #Visualização de dados
library(corrplot) #Visualização de dados
library(FactoMineR) #Análises multivariadas
library(factoextra) #Visualização das análises multivariadas
library(sf) #Manipulação de dados espacializados
library(mapview) #Visualização de dados espacializados
library(ncf) #Correlograma
library(brms) #Modelos bayesianos
library(performance)



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~
df <- readRDS("Dados/koalas.rds")
head(df) # oops! Reposicionar o nome das colunas



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Tratamento dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.1) Ajustando o nome das colunas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colnames(df) <- df[1, ] 
df <- df[-1,]
head(df)

# 2.2) Ajustando as variáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Todas as variáveis foram importadas como character
str(df) 

## Convertendo para fator
df[, c("site", "subsite")] <- lapply(df[, c("site", "subsite")], as.factor)


## Convertendo para numérico
df[, 3:ncol(df)] <- lapply(df[, 3:ncol(df)], as.numeric)

## Sanity check
str(df) 


# 2.3) NA screening
#~~~~~~~~~~~~~~~~~~~~
table(is.na(df))



#~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~
nlevels(df$site) 
nlevels(df$subsite)


# 3.1) Ocupação espacial dos coalas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Proporção de presença em toda a área
table(df$presence) #Coalas estiveram presentes em 38% dos subsites amostrados

## Proporção de presença por fragmento (site)
df %>%
  group_by(site) %>%
  summarise(n_presences = sum(presence), total_subsites = n()) %>%
  count(n_presences)

df %>%
  group_by(site) %>%
  summarise(n_presences = sum(presence))


## Visualizando em um mapa

### Agrupando os dados - no. total de presença por site
df_site <- df %>%
           group_by(site) %>%
           summarise(easting  = first(easting), 
                     northing = first(northing), 
                     n_presence = sum(presence))


### Tornando os dados um objeto espacial
dfs <- st_as_sf(df_site,
                coords = c("easting", "northing"), #Indicando as colunas das coordenadas
                crs = 20256) #Indicando o sistema de referência; https://epsg.io/20256


### Visualizando no mapa
mapview(dfs,
        zcol = "n_presence",
        cex = "n_presence",
        layer.name = "Ocupação dos fragmentos")



# 3.2) Avaliando a multicolinearidade entre as covariáveis 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
covars <- df %>% select("pprim_ssite","psec_ssite", starts_with(c("phss", "pm", "pdens", "edens", "rdens")))
covars_cor <- cor(covars, method = 'spearman')
corrplot(covars_cor, method = "color", type = "lower", addCoef.col = "black")

## As variáveis da paisagem que medem a mesma característica em diferentes extensões da paisagem (buffers)
# tendem a ter uma alta correlação positiva

# As variáveis de paisagem foram quantificadas em diferentes escalas espaciais (buffers).
# Como os buffers são aninhados (1 km ⊂ 2.5 km ⊂ 5 km), as medidas tendem a ser altamente correlacionadas.

df %>% 
  select(starts_with(c("phss"))) %>%  #pm, pdens, edens, rdens
  pairs(lower.panel = panel.smooth, upper.panel = NULL)

df %>% 
  select(starts_with(c("phss"))) %>% 
  cor()


# Há algumas abordagens para tratar a multicolinearidade imposta pelos buffers:
# 1) Seleção da escala de efeito [  ]
#    - Ajustar um modelo para cada buffer e compará-los (AIC, WAIC, LOO, etc.)
#    - Permite identificar a escala espacial mais importante para o processo ecológico
# 2) Transformação linear [ X ]
#    - Reexpressar os buffers como desvios entre escalas sucessivas
#    - Permite incluir todas as escalas no mesmo modelo
# 3) Buffers concêntricos [ X ]
#    - Calcula-se a densidade real de habitat em anéis ortogonais não sobrepostos
#    - Remove a sobreposição espacial dos buffers
# 4) Redução de dimensionalidade (PCA) [ X ]
#    - Resumir as variáveis em componentes ortogonais (eg., PC1 e PC2)
#    - Elimina a colinearidade, mas dificulta a interpretação ecológica



# 3.2.1) Transformação linear
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- df %>% 
      mutate(## phss
             phss_Z5 = phss_5km, 
             phss_Z2.5 = phss_2.5km - phss_5km, 
             phss_Z1 = phss_1km - phss_2.5km,
             
             ## pm
             pm_Z5 = pm_5km, 
             pm_Z2.5 = pm_2.5km - pm_5km, 
             pm_Z1 = pm_1km - pm_2.5km,
             
             ## pdens
             pdens_Z5 = pdens_5km, 
             pdens_Z2.5 = pdens_2.5km - pdens_5km, 
             pdens_Z1 = pdens_1km - pdens_2.5km,
             
             ## edens
             edens_Z5 = edens_5km, 
             edens_Z2.5 = edens_2.5km - edens_5km, 
             edens_Z1 = edens_1km - edens_2.5km,
             
             ## rdens
             rdens_Z5 = rdens_5km, 
             rdens_Z2.5 = rdens_2.5km - rdens_5km, 
             rdens_Z1 = rdens_1km - rdens_2.5km)

## Verificando a multicolinearidade
df %>% 
  select(starts_with(c("phss"))) %>% #pm, pdens, edens, rdens
  cor()

df %>% 
  select(starts_with(c("phss"))) %>%  #pm, pdens, edens, rdens
  pairs(lower.panel = panel.smooth, upper.panel = NULL)



# 3.2.2) Buffers concêntricos (donut)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Área de um círculo: A = pi * r^2

## Calcular a área de cada buffer
A1 <- pi * 1^2
A2.5 <- pi * (2.5)^2
A5 <- pi * 5^2

c(A1, A2.5, A5)


## Ponderar as covariáveis 
# Anel A1 = X1 * A1 
# Anel A1-2.5 = (X2.5 * A2.5) - (X1 * A1) / A2.5 - A1
# Anel A2.5-5 = (X5 * A5) - (X2.5 * A2.5) / A5 - A2.5


df <- df %>% 
      mutate(## phss
             phss_A1   = phss_1km * A1, 
             phss_A2.5 = (phss_2.5km * A2.5) - (phss_1km * A1) / A2.5 - A1,
             phss_A5   = (phss_5km * A5) - (phss_2.5km * A2.5) / A5 - A2.5,
             
             ## pm
             pm_A1   = pm_1km * A1, 
             pm_A2.5 = (pm_2.5km * A2.5) - (pm_1km * A1) / A2.5 - A1,
             pm_A5   = (pm_5km * A5) - (pm_2.5km * A2.5) / A5 - A2.5,
             
             ## pdens
             pdens_A1   = pdens_1km * A1, 
             pdens_A2.5 = (pdens_2.5km * A2.5) - (pdens_1km * A1) / A2.5 - A1,
             pdens_A5   = (pdens_5km * A5) - (pdens_2.5km * A2.5) / A5 - A2.5,
             
             ## edens
             edens_A1   = edens_1km * A1, 
             edens_A2.5 = (edens_2.5km * A2.5) - (edens_1km * A1) / A2.5 - A1,
             edens_A5   = (edens_5km * A5) - (edens_2.5km * A2.5) / A5 - A2.5,
             
             ## rdens
             rdens_A1   = rdens_1km * A1, 
             rdens_A2.5 = (rdens_2.5km * A2.5) - (rdens_1km * A1) / A2.5 - A1,
             rdens_A5   = (rdens_5km * A5) - (rdens_2.5km * A2.5) / A5 - A2.5 )



## Verificando a multicolinearidade
df %>% 
  select(starts_with(c("phss"))) %>% #pm, pdens, edens, rdens
  cor()

df %>% 
  select(starts_with(c("phss"))) %>%  #pm, pdens, edens, rdens
  pairs(lower.panel = panel.smooth, upper.panel = NULL)



# 3.2.3) PCA
#~~~~~~~~~~~~~
covars_std <- scale(covars, center = T, scale = T) #Padronizando as covariáveis

pca_res <- PCA(covars_std, scale.unit = F, graph = FALSE, ncp = ncol(covars_std)) #Rodando a PCA
fviz_eig(pca_res, addlabels = TRUE, ncp = ncol(covars_std)) #scree plot


## Identificando as variáveis com maior contribuição para os eixos
fviz_pca_var(pca_res, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) # Indica a porcentagem da variância do eixo que é explicada por aquela variável específica. A soma da contribuição de todas as variáveis em um eixo é igual a $100\%$.

varpca <- get_pca_var(pca_res)
corrplot(varpca$contrib[, 1:5], is.corr = FALSE) #Plot dimensões 1-5
# As variáveis medidas diretamente no nível do subsite (porcentagem de árvores primárias e secundárias) 
# têm uma variação quase ortogonal às variáveis de paisagem (buffers). 
# Isso justifica o uso de um GLMM com efeitos aleatórios por site/subsite, já que há processos atuando em níveis espaciais distintos!

fviz_pca_var(pca_res, 
             col.var = "contrib",
             axes = c(4,5),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


## Retendo os scores da PCA
scores <- pca_res$ind$coord
head(scores)

df$PC1 <- scores[ , 1] #Primeiro componente
df$PC2 <- scores[ , 2] #Segundo componente



# 3.3) Avaliando a relação entre ocorrência e as covariáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df %>% 
  select(ends_with(c("Z5", "Z2.5", "Z1")),"pprim_ssite", "psec_ssite", "presence") %>% 
  pivot_longer(names_to = "Variable", values_to = "Value", cols = phss_Z5:psec_ssite) %>% #Transformando para o formato largo; necessário para usar a função facet_wrap()
  #filter(Variable %in% c("pprim_ssite", "psec_ssite")) %>% #Close para variáveis específicas
  ggplot(aes(x = as.factor(presence), y = Value)) +
  geom_boxplot() +
  facet_wrap(Variable ~ ., scales = 'free_y', ncol = 3) +
  theme_light()



# 3.4) Avaliando a autocorrelação espacial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Há dois motivos pelos quais devemos suspeitar de correlação espacial nos dados em questão.
# 1) Autocorrelação espacial na escala local: pode ocorrer porque as distâncias entre os subsites dentro de site são pequenas em relação ao tamanho das áreas de vida dos coalas.
# Isso também pode ser configurado como pseudo-réplica, uma vez que valores coletados em subsites dentro do mesmo site serão mais similares entre si, do que entre sites distintos.
# 2) Autocorrelação espacial na escala regional: pode ocorrer devido à dispersão restrita dos coalas. Se assumirmos que as distâncias de dispersão são substancialmente menores
# do que a extensão espacial da área de estudo, pode ocorrer autocorrelação espacial entre os sites.


sp_cor <- spline.correlog(x = df[, "easting"], 
                          y = df[, "northing"], 
                          z = df[, "presence"], 
                          xmax = 10000) #Distância máxima permitida (=10 km)

plot(sp_cor) #Correlação é significativa quando os valores encontram-se totalmente acima/abaixo da linha



#~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Modelando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~

# Os dados aqui tratados possuem uma estruturação hierárquica clara: sites | subsites (pseudo-réplica)
# Podemos, desta forma, especificar um efeito aleatório com pontos amostrais (subsites) aninhados em seus respectivos fragmentos (sites)


# 4.1) Definindo inputs para as cadeias MCMC
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Faster cmputation
options(mc.cores = 4, brms.backend = "cmdstanr")


## Set global Stan options
SEED <- 1234
CHAINS <- 4
ITER <- 2000
WARMUP <- 1000
THIN <- 1


# 4.2) Definindo os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Modelo nulo
f0 <- bf(presence ~ 1)

## Modelo com efeitos fixos
f1 <- bf(presence ~ 1 + pprim_ssite + psec_ssite + phss_Z5 + phss_Z2.5 + phss_Z1 + rdens_Z5 + rdens_Z2.5 + rdens_Z1)

## Modelo com efeitos fixos e aleatórios
f2 <- bf(presence ~ 1 + pprim_ssite + psec_ssite + phss_Z5 + phss_Z2.5 + phss_Z1 + rdens_Z5 + rdens_Z2.5 + rdens_Z1 + (1 | site))


formulas <- list(f0, f1, f2)



# 4.3) Ajustando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
res <- list()

for(i in 1:length(formulas)){
  
res[[i]] <- brm(formula = formulas[[i]],
                data    = df,
                family  = bernoulli(link = "logit"),
                #prior   = priors,
                chains  = CHAINS,
                iter    = ITER,
                warmup  = WARMUP,
                seed    = SEED,
                thin = THIN,
                control = list(adapt_delta = 0.95, max_treedepth = 12))
  }

res2 <- lapply(res, function(x) loo(x, compare=T))


# 4.4) Comparando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_comparison <- loo(res[[1]], res[[2]], res[[3]],
                      compare = TRUE)

mod_comparison <- data.frame(mod_comparison$diffs)
#Incluir a estrutura espacial/agrupamento por site melhora drasticamente o ajuste do modelo, 
# o que confirma que existe uma forte variação/dependência espacial entre os sites que os efeitos fixos 
# isoladamente não conseguem explicar.



# 4.5) Avaliando o modelo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Convergência das cadeias
mcmc_plot(res[[3]], type = "trace") + theme_light()

## Convergência das cadeias
mcmc_plot(res[[3]], type = "trace") + theme_light() #via traceplot
summary(res[[3]]) #via Rhat; convergência quando Rhat <= 1.01

## Posetrior predictive check:
pp_check(res[[3]], type = "stat", stat = "mean") # Verifica se o modelo reproduz corretamente a proporção de ocorrência observada.


## Correlação intra-classe (icc)
icc(res[[3]]) #Adjusted: representa a correlação entre observações do mesmo site após controlar pelo efeito das covariáveis fixas



# 4.6) Avaliando os resultados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Resultado numérico
summary(res[[3]])


## Visualização - efeitos fixos
res[[3]] %>%
  gather_draws(`b_.*`, regex = TRUE) %>% #Extraindo os resultados
  filter(.variable != "b_Intercept") %>% #Removendo o intercepto do gráfico (melhor visualização)
  mutate(.value = exp(.value), #Transformando valores para escala original
         .variable = gsub("b_", "", .variable)) %>% #Removendo o label 'b' do nome da variável
  ggplot(aes(y = .variable, x = .value)) +
  stat_pointinterval(.width = c(0.80, 0.95), point_interval = median_qi) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(x = "Razão de chances", y = "Covariável") +
  theme_light()

exp(fixef(res[[3]])) #Convertendo para escala original ()


## Visualização - efeitos aleatórios
res[[3]] %>%
  spread_draws(r_site[site, term]) %>%
  ggplot(aes(y = reorder(site, r_site), x = r_site)) +
  stat_pointinterval(.width = c(0.80, 0.95), point_interval = median_qi, color = "#457b9d") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "firebrick") +
  labs(x = "", y = "Site") +
  theme_minimal()
## Pontos {67, 2, 3, 1, 20} apresentam uma probabilidade de ocorrência substancialmente maior que a média regional; candidatos prioritários para conservação


### Outra forma de visualizar
mcmc_areas(as.array(res[[3]]), regex_pars = "^r_site") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "firebrick") + 
  theme_minimal() 



mapview(filter(dfs, site %in% c("67","2","3","1", "20")), #55, 83, 84, 85, 93
        zcol = "n_presence",
        cex = "n_presence",
        layer.name = "Ocupação dos fragmentos")



## Predições marginais do modelo
conditional_effects(res[[3]]) #O efeito aleatório de fragmento apresentou variância significativamente diferente de zero




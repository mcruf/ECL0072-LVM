#### Relatório 06 Distribuição Multinomial

## Caregando os pacotes
library(VGAM) #Modelo multinomial
library(nnet) #Modelo multinomial (pacote alternativo)
library(ggplot2)

library(dplyr)
library(DHARMa)
library(ggeffects) #para plotar os resultados
library(performance) #avaliação dos residuos

# Definindo Diretório
setwd("~/Doutorado/Disciplinas/GLM/ECL0072-GLM_GAM-gh-pages/ECL0072-GLM_GAM-gh-pages")



# Carregando script de funções auxiliares

source("~/Doutorado/Disciplinas/GLM/ECL0072-GLM_GAM-gh-pages/ECL0072-GLM_GAM-gh-pages/Scripts/funcoes_auxiliares.R") 

## Importando os dados

dados <- readRDS("~/Doutorado/Disciplinas/GLM/ECL0072-GLM_GAM-gh-pages/ECL0072-GLM_GAM-gh-pages/Dados/palmerpenguins_extended.rds")

# Transformar as variáveis que estão como character para factor
cols <- c("species", "island", "sex", "diet", "life_stage", "health_metrics")
dados[, cols] <- lapply(dados[, cols], factor)

View(dados)
# Analisar os dados visualmente
# Primeiramente vamos colocar os estágios de vida em ordem
dados$life_stage <- factor(dados$life_stage, levels = c("chick","juvenile","adult"), ordered = TRUE)

# Relação de métrica de saúde (acima, abaixo ou peso normal) em função do estágio de vida
# Observamos que quando tem mais proporção de pinguins filhotes com métrica de saúde "saudável" comparado aos outros estágios de vida, isso se aplica pras três espécies. Chegando na fase juvenil, a proporção de "saudáveis" diminui. E nos adultos a proporção de acima do peso aumenta, porém para Chinstrap esse aumento não é tão visualmente maior que pras outras duas espécies. Então a métrica de saúde parece estar relacionada ao estágio de vida dos pinguins.

ggplot(dados, aes(x = life_stage, fill = health_metrics)) + #a variável life_stage vai para o eixo x. Cada nível de health_metrics vai ter uma cor diferente.
  geom_bar(position = "fill", alpha = 0.8) + # Barras empilhadas em proporção
  scale_y_continuous(labels = scales::percent) + # Transformar em porcentagem as proporções
  labs(y = "Proporção", x = "Estágio de vida", fill = "Métrica de saúde") +
  facet_wrap(~species) + # Gráfico por espécie
  theme_minimal(base_size = 15)
  
### Veremos como as métricas de saúde se comportam em relação a espécie no geral 
# Não parece ter uma diferença tão evidente assim, mas podemos observar que o que tem menor proporção de sobrepeso é o Chinstrap e eles apresentam maior proporção de saudáveis. Entre Adelie e Gentoo, o segundo tem menor proporção de abaixo do peso.

ggplot(dados, aes(x = species, fill = health_metrics)) + #a variável life_stage vai para o eixo x. Cada nível de health_metrics vai ter uma cor diferente.
  geom_bar(position = "fill", alpha = 0.8) + # Barras empilhadas em proporção
  scale_y_continuous(labels = scales::percent) + # Transformar em porcentagem as proporções
  labs(y = "Proporção", x = "Espécie", fill = "Métrica de saúde") + 
  theme_minimal(base_size = 15)


## Será que entre os sexos a diferença na métrica de saúde?
# Visualmente não aparenta ter grandes diferenças, só que fêmea tem uma proporção maior de saudáveis, enquanto que machos apresentam ter, ligeiramente, maiores proporções de sobrepeso e abaixo do peso

ggplot(dados, aes(x = sex, fill = health_metrics)) + 
  geom_bar(position = "fill", alpha = 0.8) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Proporção", x = "Sexo", fill = "Métrica de saúde") + 
  theme_minimal(base_size = 15)


#### Aplicando modelos


levels(dados$health_metrics)
dados$health_metrics <- factor(dados$health_metrics, levels = c("underweight", "healthy", "overweight"), ordered = TRUE) # Essa parte aqui é pra colocar na ordem certa, e transformar em um fator ORDINAL
levels(dados$health_metrics)

modelo1 <- vglm(health_metrics ~ life_stage, # o vglm permite multinomial ou ordinal, ou seja, mais de 2 categorias e ordens nos níveis.
                family = cumulative(parallel = TRUE, link = "logitlink"), #Aqui não colocamos multinomial, colocamos cumulative que é da multinomial, mas agora especifico que quero multinomial ordenal. Esse parallel = TRUE, e colocamos a função de ligação que é logit
                data = dados)

summary(modelo1) # Lembrando que os valores estão em logit. Como podemos ver só tem 2 intercept, (Intercept):1 e (Intercept):2 são os limiares acumulativos de probabilidade eles não correspondem a uma categoria isolada, mas à probabilidade acumulada de estar em uma categoria até aquele limiar. Ou seja, intercept 1 a probabilidade acumulada de estar em underweight, intercet 2 a probabilidade acumulada de estar em underweight ou healthy, e o próximo seria a probabilidade de estar em overweight ou healthy ou underweight dessa forma ele vale 1, porque inclui todas as categorias. O overweight não aparece porque ele é calculado indiretamente baseado nos outros. E nas variáveis preditoras (life_stage) não vemos chick porque O R precisa de um nível de referência. Por padrão, ele usa o primeiro nível, que é chick. O R precisa de um nível de referência. Por padrão, ele usa o primeiro nível, que é chick, ou seja, os coeficientes dos outros níveis (life_stagejuvenile e life_stageadult) mostram a diferença em log-odds acumuladas comparado a chick. Esses coeficientes indicam quanto o logit acumulado de health_metrics muda quando você passa de chick para juvenile ou adult. O baseline “chick” não tem coeficiente porque ele é a referência. Mas os efeitos que você vê em juvenile e adult só fazem sentido junto com a interpretação da variável resposta health_metrics. Então os coeficientes do preditor (life_stage) indicam como a mudança de categoria do preditor altera a chance acumulada da variável resposta (health_metrics). O que o modelo faz é calcular as probabilidades acumuladas de cada categoria de health_metrics para cada nível de life_stage, usando chick como referência para comparar os efeitos.
# Então pelo coeficiente do intercept: 1 = -1.04704 quer dizer que é o logit da probabilidade acumulada de que um chick esteja em health_metrics ≤ underweight. e o intercept 2 = 0.99330 quer dizer que a probabilidade acumulada de chick ser healthy ou underweight é essa. No caso de life_stagejuvenile = -0.21959 o coeficiente negativo significa que juveniles têm menos chance de estar nas categorias mais baixas comparados a chicks. Ou seja esse valor encontrado comparando de juvenil pra chick diz que a probabilidade acumlativa de estar em categorias mais baixas (underweight e healthy) é -0.22 logit. E isso vai se aplicar aos coeficientes de health_metrics, no caso tanto pra undereweight quanto para healthy juvenis tem -0.22 chance (menos chance) de estar em categorias de underweight e healthy. Para life_stageadult = -0.77444 significa que adultos tem menos chances de estar nas categorias mais baixas comparado a chick



### Modelo com outras variáveis 

modelo2 <- vglm(health_metrics ~ life_stage + sex + species, 
                family = cumulative(parallel = TRUE, link = "logitlink"), 
                data = dados)

summary(modelo2) # o intercept 1  -1.03247 é o logit acumulado da probabilidade de estar em health_metrics underweight para todas as variáveis que fazem parte desse intercept: filhote/chick, fêmea, Adelie. Então para a categoria underweight, o intercepto negativo mostra que a chance acumulada de estar nessa categoria é menor que a chance de estar acima dela (healthy ou overweight). (Intercept):2 = 1.03205 é o logit acumulado até o segundo nível (saudável), para a combinação de referência das preditoras (chick, fêmea, Adelie). O valor é positivo, o que indica que que a probabilidade acumulada de estar na categoria saudável ou abaixo do peso para a combinação de referência das variáveis preditoras (chick, fêmea, Adelie) é maior.
#Para parte de life_stage, pegamos por exemplo: life_stagejuvenile = -0.22794, é o efeito da variável “life_stage” ao passar de chick (referência) para juvenile no logit acumulado, por ser negativo diminui a probabilidade acumulada de estar nas categorias mais baixas (underweight e healthy) comparado a chick.
# Para a parte de sexo, como o coeficiente é positivo Ele indica um pequeno aumento na probabilidade acumulada de estar nas categorias mais baixas (underweight e healthy) para machos em comparação às fêmeas, mas esse efeito é muito fraco e não significativo
# O coeficiente no de speciesChinstrap mostra o efeito no logit da probabilidade acumulada de estar nas categorias mais baixas de health_metrics (underweight e healthy) comparado a Adelie, já que é um valor positivo. Para speciesGentoo o valor negativo diminui o logit da probabilidade acumulada de estar nas categorias mais baixas, ou seja, ele tem maior chance de estar overweight comparado a Adelie.

#install.packages("sjPlot")
library(sjPlot)
plot_model(modelo2, type = 'pred',  terms = c("life_stage", "sex", "species"), data = dados)



# Gerar predições para todas as combinações
pred <- ggpredict(modelo2, terms = c("life_stage", "sex", "species"))

# Plotando

pred <- ggpredict(modelo2, terms = c("life_stage", "sex", "species"))

ggplot(pred, aes(x = x, y = predicted, fill = group)) + # Aqui x é life_stage que foi o primeiro que colocamos no terms do "pred", predicted = probabilidade prevista para cada categoria de health_metricspor, group é referente aos sexos 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) + # Garante que serão usados as probabilidades previstas para definir as alturas das barras. Position = Coloca barras lado a lado para machos e fêmeas, alpha = 0.8 = questão de transparência
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), #adiciona o intervalo de confiança (IC) para a estimativa da probabilidade prevista
                position = position_dodge(width = 0.8)) + # para deixar o erro lado a lado 
  facet_wrap(~facet) + # cria um gráfico separado por species
  labs(
    x = "Estágio de Vida",
    y = "Probabilidade de Health Metrics",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("female" = "pink", "male" = "cyan4"))

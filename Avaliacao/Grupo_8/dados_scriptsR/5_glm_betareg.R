#glm betareg
#proporcao

library(glmmTMB) #Para rodar modelo com distribuição beta

library(ggplot2) #Visualização
library(patchwork) #Para visualização de gráficos
library(GGally) #Complemento ao ggplot2
library(ggpubr) #Visualização
library(dplyr) #Manipulação dos dados
library(DHARMa) #Avaliação de modelos
library(performance) #Avaliação de modelos
library(ggeffects)

setwd("D:/Documentos/Doutorado/Disciplinas/GLMM/ECL0072-GLM_GAM-gh-pages")

#abrir meu banco de dados
micro_sid <- read.csv("Relatorio/dados_scriptsR/geral_microhabitat_sid.csv", header = T, sep =";", dec = ".")

#começar a explorar os dados
str(micro_sid)

#converter em fator o que está como character
cols1 <- c("trat", "id", "cor", "habitat", "colonia", "fragmento")
micro_sid[, cols1] <- lapply(micro_sid[, cols1], factor)
micro_sid$trat <- factor(micro_sid$trat, levels = c("C","A"))
micro_sid$habitat <- factor(micro_sid$habitat, levels = c("T", "B"))

## Distribuição da variável resposta
p1 <- ggplot(micro_sid, aes(x = yii)) +
  geom_histogram(fill = 'coral',col = 'coral', alpha = 0.4,
                 bins = 10) 
p1

#tentar ver se com linha fica melhor de enxergar algo
ggplot(micro_sid, aes(y=yii, x=dia_exp, color=interaction(trat, habitat), group=interaction(trat, habitat)))+
  stat_summary(geom="errorbar")+
  stat_summary(geom = "line", linetype=2)+
  stat_summary(geom = "point")+
  xlab("Dias de experimento")+
  ylab("Eficiencia fotossintetica (YII ± SE)")+
  theme_minimal()+
  theme(legend.position="bottom")+
  scale_color_manual(values=c("#25e4bc","#f96a00","#03453d","#c7003f"), name="Tratamento")


#FAZENDO O MODELO
#ajustar os zeros pra poder usar betareg
summary(micro_sid$yii)

micro_sid$yii <- (micro_sid$yii + (median(micro_sid$yii, na.rm = TRUE)*0.1))

summary(micro_sid$yii)

#Modelo 1: testar o efeito do tratamento com o tempo pra ver se o experimento funcionou
mod_beta1 <- glmmTMB(yii ~ trat + dia_exp,
                     family=beta_family(link="logit"),
                     data=micro_sid)

#modelo 2: agora vendo se teve diferença entre habitats
mod_beta2 <- glmmTMB (yii ~ trat + dia_exp + habitat,
                      family = beta_family(link="logit"),
                      data = micro_sid)

#modelo 3: vou aproveitar que essa funçao aceita modelos mistos e vou colocar o id dos fragmentos como fator aleatorio
mod_beta3 <- glmmTMB (yii ~ trat + dia_exp + habitat +
                      (1|id), #porque tenho medidas repetidas
                      family = beta_family(link="logit"),
                      data = micro_sid)

#modelo 4: testar agora com interaçao
mod_beta4 <- glmmTMB (yii ~ trat*dia_exp*habitat +
                        (1|id),
                      family = beta_family(link="logit"),
                      data = micro_sid)


#agora vou comparar os modelos
AIC(mod_beta1, mod_beta2, mod_beta3, mod_beta4)
#o modelo que parece mais bem ajustado é o 4, com o efeito aleatorio e as interaçoes

#checando o ajuste do modelo visualmente
simres_mod4 <- simulateResiduals(mod_beta4, n=1000)
plot(simres_mod4) #nao ta parecendo muito bom
check_model(simres_mod4) #ta quase bom, mas o grafico da homogeneidade de variancia ainda não ta muito reto e horizontal

#### Interpretando os resultados
summary(mod_beta4) #teve efeito da interaçao entre tratamento e tempo. Mostra que o aquecido teve diminuição do yii com o aumento dos dias de experimento
#nao teve efeito do habitat
summary(mod_beta3)

### visualizar os resultados
pred <- ggpredict(mod_beta4, terms = c("dia_exp", "trat","habitat"))
View(pred)

ggplot(pred, aes(x = x, y = predicted, fill = interaction(group, facet), color=interaction(group, facet), group=interaction(group, facet))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Dias de experimento", y = "YII") +
  scale_color_manual(values=c("#25e4bc","#f96a00","#03453d","#c7003f"), name="Tratamento")+
  scale_fill_manual(values=c("#25e4bc","#f96a00","#03453d","#c7003f"), name="Tratamento")+
  theme_minimal()





 



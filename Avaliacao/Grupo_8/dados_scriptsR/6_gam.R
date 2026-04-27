#GAM FINALMENTE

#ja tinha rodado um glmm com esses dados e percebi que não apresentam uma relaçao linear

library(mgcv) #rodar gam
library(mgcViz) #visualizar resultados do gam
library(gratia) #visualizar resultados do gam
library(ggplot2) #plotar
library(dplyr) #manipular dados
library(DHARMa) #avaliação dos residuos
library(performance) #avaliação dos residuos
library(ggeffects) #para plotar os resultados
library(glmmTMB)

setwd("D:/Documentos/Doutorado/Disciplinas/GLMM/ECL0072-GLM_GAM-gh-pages")

micro_porites <- read.csv("Relatorio/dados_scriptsR/PAM_porites.csv", sep=";", dec=".")
View(micro_porites)
str(micro_porites)

cols_p <- c("habitat", "treatment", "colony", "ID")
micro_porites[, cols_p] <- lapply(micro_porites[, cols_p], factor) #Transformando para fator

#visualizando
ggplot(micro_porites, aes(x=exp_day, y = YII)) +
  geom_point(col = 'gray40', pch = 19, alpha =0.5, size = 3) +
  geom_smooth(method = "gam", fill = 'gray60', col = 'coral') +
  labs(x = "Dias de experimento",
       y = "YII") +
  theme_minimal(base_size = 14)

#dados parecem ter forte assimetria a direita e tem muitos zeros
ggplot(micro_porites, aes(x = YII)) +
  geom_histogram(bins = 35, fill = 'coral', col = 'white', alpha = 0.6)

#plotando os dados
ggplot(micro_porites, aes(x = factor(exp_day) , y=YII)) +
  geom_boxplot(fill = 'coral', col = 'white', alpha = 0.6)

#separando por tratamento
ggplot(micro_porites, aes(x = factor(exp_day) , y=YII, fill=treatment)) +
  geom_boxplot(alpha = 0.8)+
  scale_fill_manual(values=c("coral","cyan4"))+
  theme(legend.position = "bottom")

#separando agora por habitat também
ggplot(micro_porites, aes(x = factor(exp_day) , y=YII, fill=interaction (treatment, habitat)))+
  geom_boxplot(alpha = 0.8)+
  scale_fill_manual(values=c("coral","cyan4", "orange", "cyan"))+
  theme(legend.position = "bottom")

  
##### VOU RODAR UM GLMM QUE NEM O DE SID
summary(micro_porites$YII)

micro_porites$YII <- (micro_porites$YII + (median(micro_porites$YII, na.rm = TRUE)*0.1))

#fazendo um modelo com tudo que tem direito
glmm_porites <- glmmTMB (YII ~ treatment*exp_day*habitat +
                        (1|ID),
                      family = beta_family(link="logit"),
                      data = micro_porites)

#avaliando o ajuste do modelo
simres_glmm_por <- simulateResiduals(glmm_porites, n=1000)
plot(simres_glmm_por) #bem ruim, não parece linear
check_model(simres_glmm_por) #o modelo não tá prevendo bem a distribuição dos residuos


###USAR GAMM AGORA
### GAM com distribuição Normal
gam_porites1 <- gam(YII ~ s(treatment, bs = "cr") + (), #outros suavizadores: tp, ts, cc
              data = dados2,
              family = gaussian())

gam_porites <- gam(YII ~ s(exp_day, bs = "tp") + treatment + habitat +
                     s(ID, bs="re"), #colocando o id dos corais como efeito aleatório
                   data = micro_porites,
                   family = betar(link="logit"))

summary(gam_porites)

par(mfrow = c(2,2))
gam.check(gam_porites)
check_model(gam_porites)

#fazer um modelo com by=treatment e um sem 
#Estou supondo que o controle e o aquecido não tem o mesmo formato de relaçao com o tempo, ja que o controle nao deve mudar


#pro glmm:
#"re"
#Random effect smoother
#Para efeitos aleatórios (como `(1
#                               id)` nos GLMMs)
AIC(m_glm5, m_gam1) #glm5 usa distribuição GAMA, m_gam1 usa distribuição normal

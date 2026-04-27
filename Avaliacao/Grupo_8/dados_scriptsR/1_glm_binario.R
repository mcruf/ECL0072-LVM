#RODANDO OS MODELOS COM OS MEUS DADOS
#RELATORIO DISCIPLINA GLM
#GLM PARA DADOS BINARIOS

setwd("D:/Documentos/Doutorado/Disciplinas/GLMM/ECL0072-GLM_GAM-gh-pages")

# carregando os pacotes necessarios
library(ggplot2) # para plotar
library(dplyr) #manipular os dados
library(DHARMa) #avaliação dos residuos
library(performance) #avaliação dos residuos

#abrir meu banco de dados
micro_sid <- read.csv("Relatorio/dados_scriptsR/geral_microhabitat_sid.csv", header = T, sep =";", dec = ".")

#começar a explorar os dados
str(micro_sid)

#converter em fator o que está como character
cols1 <- c("dia_exp","trat", "id", "cor", "habitat", "colonia", "fragmento")
micro_sid[, cols1] <- lapply(micro_sid[, cols1], factor)

#quero que meu y seja a incidencia de branqueamento nos corais (branqueado (1) x saudavel (0)). Pra isso, vou usar a coluna "cor" e considerar as cores D1 e D2 como branqueamento
table(micro_sid$cor)

micro_sid <- micro_sid %>% 
  filter(cor != "morto") %>% 
  mutate (branq = if_else(cor %in% c("D1", "D2"), 1, 0))

micro_sid$branq <- as.factor(micro_sid$branq)

micro_sid %>%
  group_by(dia_exp, branq) %>% 
  summarise(Ncorais = n()) %>%
  mutate(Proporcao = Ncorais / sum(Ncorais)) #a proporçao de branqueamento aumenta ao longo do tempo

#proporcao de branqueamento ao longo do tempo
ggplot(micro_sid, aes(x = dia_exp, fill = branq)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("coral", "#dfc27d"))+
  theme_minimal()

#agora separando por tratamento e micro-habitat
ggplot(micro_sid, aes(x = dia_exp, fill = branq)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("coral", "#dfc27d"))+
  facet_wrap(trat ~ habitat) +
  theme_minimal()

#provavelmente existe uma forte autocorrelacao temporal, então por enquanto vou comparar apenas o dia 32 (1 semana após o fim do estresse termico)
d32 <- micro_sid %>% 
  filter(dia_exp==32)

#parece que os corais do topo do recife branquearam mais que os corais da base e que houve um efeito do tratamento de aquecimento
ggplot(d32, aes(x=trat, fill = branq))+
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("coral", "#dfc27d"))+
  facet_wrap(.~ habitat) +
  theme_minimal()

#vou testar modelos comparando primeiro so esse dia que escolhi pra fazer um glm mesmo

# modelo nulo
mod0 <- glm(branq ~ 1,
            family = binomial(link = "logit"),
            data = d32)

# so com o efeito do tratamento
mod1 <- glm(branq ~ trat,
            family = binomial(link = "logit"),
            data = d32)

# com o efeito do tratamento + micro-habitat
mod2 <- glm(branq ~ trat + habitat,
            family = binomial(link = "logit"),
            data = d32)

# agora com o efeito da interacao entre tratamento e micro-habitat
mod3 <- glm(branq ~ trat*habitat,
            family = binomial(link = "logit"),
            data = d32)

#comparar os modelos usando AIC
aic <- AIC(mod0, mod1, mod2, mod3)
aic %>% arrange(-AIC) #o modelo com a interação explicou melhor

#lmtest::lrtest(mod3, mod2) # vou testar so porque tinha na pratica
anova(mod3, mod1, test = "Chisq") #testei o 3 e o 1 que foram os melhores
# o teste deu significativo, entao o modelo mais complexo (3) é o mais ajustado

#vou validar o modelo escolhido, que foi o 3

resid_sim <- simulateResiduals(mod3, n=1000) #simulando os residuos
plot(resid_sim) #parece estar tudo ok, nada deu significativo

windows()
check_model(mod3) #nao sei interpretar esses graficos

summary(mod3) #confusa porque deu significativo só os efeitos principais e nao a interacao

#AGORA VOU CONSIDERAR O TEMPO 

# modelo nulo
mod00 <- glm(branq ~ 1,
            family = binomial(link = "logit"),
            data = micro_sid)

#agora com o efeito do tratamento
mod4 <- glm(branq ~ trat,
             family = binomial(link = "logit"),
             data = micro_sid)

#efeito do tratamento + habitat
mod5 <- glm(branq ~ trat + habitat,
             family = binomial(link = "logit"),
             data = micro_sid)

#efeito do tratamento interagindo com o habitat
mod6 <- glm(branq ~ trat * habitat,
             family = binomial(link = "logit"),
             data = micro_sid)

#efeito do tratamento + habitat + tempo
mod7 <- glm(branq ~ trat + habitat + dia_exp,
             family = binomial(link = "logit"),
             data = micro_sid)

#agora com interaçao de tudo
mod8 <- glm(branq ~ trat*habitat*dia_exp,
             family = binomial(link = "logit"),
             data = micro_sid)

#interaçao so de tratamento e habitat
mod9 <- glm(branq ~ trat*habitat + dia_exp,
            family = binomial(link = "logit"),
            data = micro_sid)

#agora sem interaçao com o tratamento
mod10 <- glm(branq ~ trat + habitat*dia_exp,
            family = binomial(link = "logit"),
            data = micro_sid)

#comparar esses modelos usando AIC
aic2 <- AIC(mod00, mod4, mod5, mod6, mod7, mod8, mod9, mod10)
aic2 %>% arrange(-AIC) #o modelo 9, com a interação entre tratamento e habitat, explicou melhor

#validar o modelo escolhido, que foi o 9

resid_sim9 <- simulateResiduals(mod9, n=1000) #simulando os residuos
plot(resid_sim9) # o modelo ainda nao parece muito adequado e pode ser por causa da autocorrelação temporal

windows()
check_model(mod9) #nao sei interpretar esses graficos

summary (mod9) #nesse aqui a interacao entre tratamento e habitat foi significativa, mas nao deu efeito significativo do tempo


#SOLUÇÃO PRA ESSE SCRIPT: ESCOLHER 3 TEMPOS DIFERENTES APENAS, QUE AI EU DIMINUO A AUTOCORRELAÇAO TEMPORAL E DEIXO COMO PROBLEMA SO AS MEDIDAS REPETIDAS
# MAS AI EU POSSO COLOCAR O FRAGMENTO COMO UM FATOR E VER SE EXPLICA ALGO. O MODELO VAI FICAR RUIM MAS PACIENCIA, AI EU FALO QUE DEVERIA USAR GLMM EM VEZ DE GLM

  
### Avaliando a correlação temporal
res <- recalculateResiduals(resG, group = dados$Mes)
testTemporalAutocorrelation(res, time = unique(dados$Mes))
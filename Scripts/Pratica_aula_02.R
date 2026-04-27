####################################
#                                  #
#         Aula prática 02          #
#     - Métodos de Estimação -     #
#                                  #
####################################

# Este script abordará dois métodos de estimação de parâmetros:
# 1) mínimos quadrados (OLS - ordinary least squares)
# 2) máxima verossimilhança (ML - maximum likelihood)
# Para cada um dos métodos, será mostrado como se pode esimtar os 
# parâmetros manualmente e via funções específicas (lm/glm).

# Para vias de exemplo prático, usaremos o banco de dados
# dos pinguins de palmer.



#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)


#~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("Dados/palmerpenguins_extended.rds")



#~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Métodos de estimação
#~~~~~~~~~~~~~~~~~~~~~~~~~


# 2.1) Método dos mínimos quadrados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# O método dos mínimos quadrados permite estimar os parâmetros analíticamente.
# Pode ser realizado via função lm() ou manualmente.


# 2.1.1) Método manual
#~~~~~~~~~~~~~~~~~~~~~
# Lembrando que:
# XBeta = (X'X)^(-1) X'y


## Definindo a matriz de design e a variável resposta
y <- dados$body_mass_g
X <- cbind(1, dados$bill_length_mm) #Inclui o intercepto (1) e a(s) covariável(eis)


## Calculando os parâmetros
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat


## Calculando os valores ajustados e os resíduos
y_hat <- X %*% beta_hat  #valores ajustados     
res <- y - y_hat #resíduos


## Calculando os erros padrões e variância residual

### variância residual
n <- dim(X)[1] #No. de observações (linhas)
nparams <- ncol(X) #No. parâmetros
gl <- n - nparams #graus de liberadade
sigma2_hat <- sum(res^2) / gl #variância residual

### erros padrões
var_beta_hat <- sigma2_hat * solve(t(X) %*% X) #matriz de variância-covariância
erros_pad <- sqrt(diag(var_beta_hat))


# 2.1.1) Método via função lm()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_lm <- lm(body_mass_g~bill_length_mm, dados)
summary(mod_lm)


#### Comparando...
data.frame(Estimate = beta_hat,
           Std.Error = erros_pad)




# 2.2) Método de Máxima Verossimilhança
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pode ser realizado via função glm() ou manualmente.


# 2.2.1) Ideias gerais
#~~~~~~~~~~~~~~~~~~~~~~~

## Histograma com curva de densidade ##
ggplot(data=dados, aes(body_mass_g)) + 
  geom_histogram(aes(y = ..density..),col="white", fill="gray85") +
  geom_density(lwd=1.2, col = 'darkorange') +
  xlab("Peso (g)") + ylab("Densidade") +
  theme_bw() 



## Qual a média e desvio padrão dos dados? ##
mean(dados$body_mass_g)
sd(dados$body_mass_g)


# 2.2.2) Calculando manualmente
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aqui vamos calcular a verossimilhança assumindo a média e o desivo padrão dos dados.
# A interpretação, portanto, se dará sob a forma de:
# Dado uma distribuição Normal com parâmetros mu = mean(dados$body_mass_g) e sigma = sd(dados$body_mass_g),
# qual a verossimilhança de observar um pinguim com peso x?

## Calculando a média e desvio padrão dos dados ##
mu_obs <- mean(dados$body_mass_g)
sd_obs <- sd(dados$body_mass_g)


# Vamos calcular os valores de verossimilhança para os seguintes pinguins:
# pinguim 1: 2500 g (linha azul)
# pinguim 2: 4000 g (linha preto)
# pinguim 3: 9000 g (linha vermelho)

ggplot(data=dados, aes(body_mass_g)) + 
  geom_histogram(aes(y = ..density..),col="white", fill="gray92") +
  geom_density(lwd=1.2, col = 'darkorange') +
  geom_vline(xintercept=2500,colour='cyan4',lwd=1) +
  geom_vline(xintercept=4000,colour='black',lwd=1) +
  geom_vline(xintercept=9000,colour='coral1',lwd=1) +
  xlab("Peso (g)") + ylab("Densidade") +
  theme_bw() 


# para calulcar, pode-se usar a função 'dnorm' conforme:
pinguim_1 <- dnorm(2500, mean = mu_obs, sd = sd_obs)
pinguim_2 <- dnorm(4000, mean = mu_obs, sd = sd_obs)
pinguim_3 <- dnorm(9000, mean = mu_obs, sd = sd_obs)

rbind(pinguim_1, pinguim_2, pinguim_3)


# Precisamos repetir o passo anterior para todos os n pinguins amostrados para, em sequência,
# calcular a verossimilhança dos dados observados dado os parâmetros especificados.
# Lembrando que para minimizar (maximizar) a função L(), é necessário logaritimizar
# a função Normal e me sequência somar os valores de verossimilhança que foram calculados
# para cada pinguim.


sum(dnorm(dados$body_mass_g, mean = mu_obs, sd = sd_obs, log = T))


# Lembrando que o valor que obtemos é referente à uma distribuição Normal cujos
# parâmetros do modelo estão centrados em cima dos parâmetros populacionais.
# De acordo com o método MLE, precisamos achar o conjunto de parâmetros que
# resulta no menor valor de log-verossimilhança. Precisamos, por tanto, testar
# esse conjunto de valores usando uma função de otimização, conforme destacado na sequência


# A estimação "manual" dos parâmetros pode ser dividida em duas etapas:
# 1) Definição da função de verossimilhança (no caso, a função logaritimizada e negativa)
# 2) Otimização da função (i.e., achar o conjunto de parâmetros que resulta no menor valor de log-verossimilhança)


## 1) Função de log-verossimilhança negativa (joint neg. log-likelihood) ##
fun_lvn <- function(parametros, y, x) {
  
  ## Definindo os parametros
  beta0 <- parametros[1]
  beta1 <- parametros[2]
  sd <- exp(parametros[3])  #garante que sd > 0
  
  ## Média para cada observação
  mu <- beta0 + beta1*x
  
  ## Calculando a verossimilhanca (para cada amostragem/observação)
  vetor_logv <- dnorm(y, mean = mu, sd = sd, log = TRUE)
  lvn <- -sum(vetor_logv) #somando as verossimilhanças 
  
  return(lvn)
}



## 2) Otimização da função ##

### Valores iniciais para os parâmetros
summary(mod_lm)$coeff

beta0_init <- summary(mod_lm)$coeff[1,1]
beta1_init <- summary(mod_lm)$coeff[2,1]
sd_init <- log(sd(dados$body_mass_g, na.rm = TRUE))


estim_mle <- optim(par = c(beta0_init, beta1_init, sd_init),
                   fn = fun_lvn,
                   y = dados$body_mass_g,
                   x = dados$bill_length_mm,
                   method = "BFGS",
                   hessian = TRUE)

names(estim_mle)

estim_mle$par #
estim_mle$value #valor de log-verossimilhança negativa


### Calculando erros padrões
sqrt(diag(solve(estim_mle$hessian))) #solve: retorna matriz de covarianca; diag: retorna os valores diagonais, ie., variancas; sqrt - converte as variancias em desvios padroes

#### Comparando....
summary(mod_lm)$coeff
estim_mle$par
sqrt(diag(solve(estim_mle$hessian))) 




# 2.2.3) Calculando via função glm()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### LM ###
estim_glm <- glm(body_mass_g ~ bill_length_mm, 
                 data = dados,
                 family = gaussian)

summary(estim_glm)$coeff
summary(mod_lm)$coeff

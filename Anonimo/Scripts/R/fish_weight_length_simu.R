###############################################################################
#                                                                             #
#       Simulação da relação peso-comprimento de peixes em 5 recifes          #
#                                                                             #
###############################################################################

# Script simula uma coleta de uma espécie de peixes em 5 recifes ao longo de 4 anos.

#~~~~~~~~~~~~~~~~~~
# Carregando pacotes
#~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(lme4)

set.seed(1234)

recifes <- paste0("Recife_", LETTERS[1:5])
recifes <- LETTERS[1:5]

anos    <- factor(2023:2026)

# 1. Tabela com distribuições de comprimento e ruído residual específico
params_recife <- data.frame(
  recife       = recifes,
  mean_comp    = c(32, 36, 40, 43, 46),
  sd_comp      = c(2.5, 4.2, 6.5, 3.8, 5.5),
  sd_residual  = c(0.21, 0.35, 0.25, 0.38, 0.28)) # Ruídos residuais distintos



# 2. Grade de amostragem
desenho_amostral <- expand.grid(recife = recifes, ano = anos) %>%
  rowwise() %>%
  mutate(n_peixes = rpois(1, lambda = 50)) %>%
  ungroup()


dados <- desenho_amostral %>%
  uncount(n_peixes) %>%
  left_join(params_recife, by = "recife") %>%
  group_by(recife, ano) %>%
  mutate(id_peixe = row_number()) %>%
  ungroup() %>%
  mutate(
    id_unico = row_number(),
    sexo = sample(c("Macho", "Fêmea"), size = n(), replace = TRUE),
    comprimento_cm = round(rnorm(n(), mean = mean_comp, sd = sd_comp), 1)
  )

# 3. Parâmetros Populacionais e Efeitos Aleatórios Controlados
beta_0    <- -3.8
beta_1    <-  3.0
beta_sexo <-  0.06

# Redução no sd do slope aleatório e controle do intercepto
sd_recife_intercepto <- 0.15 
sd_recife_slope      <- 0.08 # Reduzido para evitar que o slope fique negativo ou achate a curva global
sd_ano               <- 0.03

# Definir efeitos aleatórios sorteados mantendo consistência biológica
efeitos_recife <- data.frame(
  recife = recifes,
  u_0i   = rnorm(length(recifes), mean = 0, sd = sd_recife_intercepto),
  u_1i   = rnorm(length(recifes), mean = 0, sd = sd_recife_slope)
)

efeitos_ano <- data.frame(
  ano = anos,
  v_k = rnorm(length(anos), mean = 0, sd = sd_ano)
)

# 4. Simulação com erro residual individualizado por recife
dados_simulados <- dados %>%
  left_join(efeitos_recife, by = "recife") %>%
  left_join(efeitos_ano, by = "ano") %>%
  rowwise() %>%
  mutate(
    log_comprimento = log(comprimento_cm),
    sexo_dummy      = ifelse(sexo == "Fêmea", 1, 0),
    erro_residual   = rnorm(1, mean = 0, sd = sd_residual),
    
    log_peso = (beta_0 + u_0i + v_k) + 
      (beta_1 + u_1i) * log_comprimento + 
      beta_sexo * sexo_dummy + 
      erro_residual,
    
    peso_g = round(exp(log_peso), 2)
  ) %>%
  ungroup()


# 5. Selecionar apenas colunas relevantes
dados_simulados2 <- dados_simulados %>% select(recife, ano, id_unico, sexo, comprimento_cm, peso_g)


# 6. Salvando os dados
saveRDS(dados_simulados2, "~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-LVM/Dados/peixes.rds")




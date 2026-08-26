###############################################################################
#                                                                             #
#     Simulação de Biomassa de Peixes Recifais em Estrutura de LMM            #
#                                                                             #
###############################################################################

library(tidyverse)
library(lme4)

set.seed(42) # Semente para reprodutibilidade

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Definição do Desenho Amostral
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

recifes      <- paste0("Recife_", LETTERS[1:5])
anos         <- 2021:2023
meses        <- 1:12
mergulhadores <- c("Mergulhador_1", "Mergulhador_2", "Mergulhador_3")

# Grade completa de amostragem (Recife x Ano x Mês)
grade_base <- expand.grid(
  recife = recifes,
  ano    = anos,
  mes    = meses
)

# Simulando perda eventual de amostragem em alguns recifes/meses (probabilidade de 85% de amostragem)
amostragens_efetivas <- grade_base %>%
  filter(rbinom(n(), size = 1, prob = 0.85) == 1)

# Para cada evento de amostragem no recife/mês, geramos exatamente 5 transectos.
# A alocação dos mergulhadores é totalmente aleatória por transecto.
dados_amostrais <- amostragens_efetivas %>%
  group_by(recife, ano, mes) %>%
  reframe(
    transecto = 1:5,
    mergulhador = sample(mergulhadores, size = 5, replace = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    id_transecto = row_number(),
    # Simulação da covariável ambiental SST (SST média de 27°C com variação sazonal)
    sst = round(27 + 1.8 * sin(2 * pi * mes / 12) + rnorm(n(), mean = 0, sd = 0.4), 2)
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Definição de Parâmetros e Efeitos Aleatórios
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Efeitos Fixos
beta_0   <- 4.5    # Intercepto global (log-biomassa média)
beta_sst <- 0.12   # Efeito positivo da SST na biomassa

# Efeitos Aleatórios
sd_recife       <- 0.45  # Variabilidade estrutural ENTRE recifes (inter-classe)
sd_mergulhador  <- 0.15  # Incerteza/viés entre observadores
sd_ano          <- 0.08  # Variação interanual
sd_residual     <- 0.25  # Ruído residual (variância intra-classe)

# Sorteio dos efeitos aleatórios (não ordenados de forma crescente)
efeitos_recife <- data.frame(
  recife = recifes,
  u_recife = rnorm(length(recifes), mean = 0, sd = sd_recife)
)

efeitos_mergulhador <- data.frame(
  mergulhador = mergulhadores,
  u_mergulhador = rnorm(length(mergulhadores), mean = 0, sd = sd_mergulhador)
)

efeitos_ano <- data.frame(
  ano = anos,
  u_ano = rnorm(length(anos), mean = 0, sd = sd_ano)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Simulação da Biomassa (Processo Estocástico)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dados_simulados <- dados_amostrais %>%
  left_join(efeitos_recife, by = "recife") %>%
  left_join(efeitos_mergulhador, by = "mergulhador") %>%
  left_join(efeitos_ano, by = "ano") %>%
  mutate(
    # Padronização simples da SST para estabilidade do modelo
    sst_std = as.vector(scale(sst)),
    
    # Modelo Linear Misto no espaço Log:
    # log(y) = (Efeitos Fixos) + (Efeitos Aleatórios) + Erro Residual
    log_biomassa = (beta_0 + beta_sst * sst_std) + 
      u_recife + u_mergulhador + u_ano + 
      rnorm(n(), mean = 0, sd = sd_residual),
    
    # Conversão para a escala original de biomassa (kg / transecto)
    biomassa_kg = round(exp(log_biomassa), 2)
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Seleção e Organização Final das Colunas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dados_finais <- dados_simulados %>%
  select(
    id_transecto,
    recife,
    ano,
    mes,
    transecto,
    mergulhador,
    sst,
    biomassa_kg
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Inspeção dos Dados Gerados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# head(dados_finais, 10)
# 
# ggplot(dados_finais, aes(x = recife, y = biomassa_kg, fill = recife, col = recife)) +
#   geom_boxplot(alpha = 0.5, notch=T) +
#   scale_fill_manual(values = c("#846267", "#588157","#e07a5f", "#457b9d", "#f6bd60")) +
#   scale_color_manual(values = c("#846267", "#588157","#e07a5f", "#457b9d", "#f6bd60")) +
#   labs(x = "Recife",y = "X") +
#   theme_minimal(base_size = 14) +
#   my_theme



# 6. Salvando os dados
saveRDS(dados_finais, "~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/Graduation/ECL0072-LVM/Dados/biomass_reef.rds")




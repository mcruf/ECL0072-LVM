#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reorganizando os dados de mamíferos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dados extraídos do pacote (faraway)


# Carregando o(s) pacotes(s)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(faraway)


# Carregando os dados
#~~~~~~~~~~~~~~~~~~~~~~~
data("mammalsleep")

df <- mammalsleep; rm(mammalsleep)


# Reorganizando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~
df$mammals <- rownames(df) 

rownames(df) <- 1:nrow(df)


df <- df[,c(11, 1:10)]


saveRDS(df, "Dados/mammal_sleep.rds")

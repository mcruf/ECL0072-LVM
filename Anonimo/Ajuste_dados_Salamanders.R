
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reorganizando os dados de salamandras
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(glmmTMB)

data("Salamanders")

df <- Salamanders %>%
      spread(spp, count) %>%
      rename("Gyrinophilus_porphyriticus" = "GP",
             "Pseudotriton_ruber" = "PR",
             "Desmognathus_monticola" = "DM",
             "Eurycea_cirrigera_Adult" = "EC-A",
             "Eurycea_cirrigera_Larval" = "EC-L",
             "Desmognathus_spp_Larval" = "DES-L",
             "Desmognathus_fuscus" = "DF")

saveRDS(df, "Dados/Salamanders.rds")


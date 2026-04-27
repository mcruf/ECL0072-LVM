#################################
#                               #
#     Funções auxiliares        #
#                               #
#################################



#~~~~~~~~~~~~~~~~~~~
# Função softmax
#~~~~~~~~~~~~~~~~~~~
softmax <- function(eta) {
  exp_eta <- c(1, exp(eta)) #Add categoria de referência
  return(exp_eta / sum(exp_eta))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Função softmax jacobiana
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
softmax_jacobian <- function(probs) {
  k <- length(probs)
  J <- matrix(NA, nrow = k, ncol = k-1) # k linhas, k-1 colunas
  for (i in 1:k) {
    for (j in 1:(k-1)) {
      J[i, j] <- probs[i] * (ifelse(i == (j+1), 1, 0) - probs[j+1])
    }
  }
  return(J)
}
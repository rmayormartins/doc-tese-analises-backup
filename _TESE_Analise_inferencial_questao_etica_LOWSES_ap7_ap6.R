# Dados para a Aplicação 7 (2023)
dados_2023 <- matrix(c(24, 20, 13, 6, 11, 22), nrow = 3, byrow = TRUE)
dimnames(dados_2023) <- list(c("Não Percebe Risco", "Incerteza", "Percebe Risco"), c("Pre", "Pos"))

# Realizando o teste Qui-Quadrado
chi2_teste_2023 <- chisq.test(dados_2023)
print(chi2_teste_2023)

# Dados para a Aplicação 6 (2022)
dados_2022 <- matrix(c(47, 30, 20, 18, 13, 32), nrow = 3, byrow = TRUE)
dimnames(dados_2022) <- list(c("Não Percebe Risco", "Incerteza", "Percebe Risco"), c("Pre", "Pos"))

# Realizando o teste Qui-Quadrado
chi2_teste_2022 <- chisq.test(dados_2022)
print(chi2_teste_2022)


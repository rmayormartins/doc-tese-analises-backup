# Quest�o 1: "O curso foi" (Parte 1)
dados_curso_foi_1 <- matrix(c(62, 71, 27, 18), nrow = 2)
colnames(dados_curso_foi_1) <- c("Percebe divers�o", "N�o percebe divers�o")
rownames(dados_curso_foi_1) <- c("Ensino Fundamental", "Ensino M�dio")

# Quest�o 2: "O curso foi" (Parte 2)
dados_curso_foi_2 <- matrix(c(29, 43, 60, 46), nrow = 2)
colnames(dados_curso_foi_2) <- c("Percebe facilidade", "N�o percebe facilidade")
rownames(dados_curso_foi_2) <- c("Ensino Fundamental", "Ensino M�dio")

# Quest�o 3: "O tempo das aulas passou"
dados_tempo_aulas <- matrix(c(40, 49, 49, 40), nrow = 2)
colnames(dados_tempo_aulas) <- c("Percebe o tempo passar r�pido", "N�o percebe o tempo passar r�pido")
rownames(dados_tempo_aulas) <- c("Ensino Fundamental", "Ensino M�dio")

# Quest�o 4: "Em geral, o curso foi"
dados_curso_geral <- matrix(c(80, 81, 9, 8), nrow = 2)
colnames(dados_curso_geral) <- c("Percebe ser bom", "N�o percebe ser bom")
rownames(dados_curso_geral) <- c("Ensino Fundamental", "Ensino M�dio")

# Quest�o 5: "Eu entendi o que � Machine Learning"
dados_ml_entendimento <- matrix(c(76, 79, 13, 10), nrow = 2)
colnames(dados_ml_entendimento) <- c("Sim", "N�o")
rownames(dados_ml_entendimento) <- c("Ensino Fundamental", "Ensino M�dio")

# Quest�o 6: "Computadores ou sistemas podem aprender?"
dados_computadores_aprender <- matrix(c(86, 88, 2, 1), nrow = 2)
colnames(dados_computadores_aprender) <- c("Sim", "N�o")
rownames(dados_computadores_aprender) <- c("Ensino Fundamental", "Ensino M�dio")

# Quest�o 7: "Eu posso aprender a fazer solu��es com IA/ML?"
dados_aprender_solucoes <- matrix(c(86, 88, 3, 1), nrow = 2)
colnames(dados_aprender_solucoes) <- c("Sim", "N�o")
rownames(dados_aprender_solucoes) <- c("Ensino Fundamental", "Ensino M�dio")

# Quest�o 8: "Eu consigo desenvolver um modelo de ML para reconhecimento de imagens"
dados_desenvolver_ml <- matrix(c(79, 81, 10, 7), nrow = 2)
colnames(dados_desenvolver_ml) <- c("Sim", "N�o")
rownames(dados_desenvolver_ml) <- c("Ensino Fundamental", "Ensino M�dio")

# Quest�o 9: "Desenvolver um modelo de ML �"
dados_ml_facilidade <- matrix(c(12, 28, 77, 61), nrow = 2)
colnames(dados_ml_facilidade) <- c("Percebe facilidade", "N�o Percebe facilidade")
rownames(dados_ml_facilidade) <- c("Ensino Fundamental", "Ensino M�dio")

#Quest�o 10: "Consigo explicar para um amigo(a) o que � ML"
dados_explicar_ml <- matrix(c(46, 61, 43, 28), nrow = 2)
colnames(dados_explicar_ml) <- c("Sim", "N�o")
rownames(dados_explicar_ml) <- c("Ensino Fundamental", "Ensino M�dio")

#Quest�o 11: "Quero aprender mais sobre ML"
dados_quero_aprender_ml <- matrix(c(61, 68, 28, 21), nrow = 2)
colnames(dados_quero_aprender_ml) <- c("Sim", "N�o")
rownames(dados_quero_aprender_ml) <- c("Ensino Fundamental", "Ensino M�dio")

# Realizando o teste qui-quadrado para cada quest�o

# Quest�o 1
teste_q1 <- chisq.test(dados_curso_foi_1)
print("Teste Qui-Quadrado - Quest�o 1:")
print(teste_q1)

# Quest�o 2
teste_q2 <- chisq.test(dados_curso_foi_2)
print("Teste Qui-Quadrado - Quest�o 2:")
print(teste_q2)

# Quest�o 3
teste_q3 <- chisq.test(dados_tempo_aulas)
print("Teste Qui-Quadrado - Quest�o 3:")
print(teste_q3)

# Quest�o 4
teste_q4 <- chisq.test(dados_curso_geral)
print("Teste Qui-Quadrado - Quest�o 4:")
print(teste_q4)

# Quest�o 5
teste_q5 <- chisq.test(dados_ml_entendimento)
print("Teste Qui-Quadrado - Quest�o 5:")
print(teste_q5)

# Quest�o 6
teste_q6 <- chisq.test(dados_computadores_aprender)
print("Teste Qui-Quadrado - Quest�o 6:")
print(teste_q6)

# Quest�o 7
teste_q7 <- chisq.test(dados_aprender_solucoes)
print("Teste Qui-Quadrado - Quest�o 7:")
print(teste_q7)

# Quest�o 8
teste_q8 <- chisq.test(dados_desenvolver_ml)
print("Teste Qui-Quadrado - Quest�o 8:")
print(teste_q8)

# Quest�o 9
teste_q9 <- chisq.test(dados_ml_facilidade)
print("Teste Qui-Quadrado - Quest�o 9:")
print(teste_q9)

# Quest�o 10
teste_q10 <- chisq.test(dados_explicar_ml)
print("Teste Qui-Quadrado - Quest�o 10:")
print(teste_q10)

# Quest�o 11
teste_q11 <- chisq.test(dados_quero_aprender_ml)
print("Teste Qui-Quadrado - Quest�o 11:")
print(teste_q11)

#--------------posso optar pelo fisher se as amostras forem pequenas, mas o chi-quadrado resolve-------#
teste_q6_fisher <- fisher.test(dados_computadores_aprender)
print("Teste Exato de Fisher - Quest�o 6:")
print(teste_q6_fisher)

teste_q6 <- chisq.test(dados_computadores_aprender)
print("Teste Qui-Quadrado - Quest�o 6:")
print(teste_q6)

#----------Percepcao de aprendizagem--------------#
# Quest�o 5
teste_q5 <- chisq.test(dados_ml_entendimento)
print("Teste Qui-Quadrado - Quest�o 5:")
print(teste_q5)

# Quest�o 7
teste_q7 <- chisq.test(dados_aprender_solucoes)
print("Teste Qui-Quadrado - Quest�o 7:")
print(teste_q7)

# Quest�o 8
teste_q8 <- chisq.test(dados_desenvolver_ml)
print("Teste Qui-Quadrado - Quest�o 8:")
print(teste_q8)

# Quest�o 9
teste_q9 <- chisq.test(dados_ml_facilidade)
print("Teste Qui-Quadrado - Quest�o 9:")
print(teste_q9)

# Quest�o 10
teste_q10 <- chisq.test(dados_explicar_ml)
print("Teste Qui-Quadrado - Quest�o 10:")
print(teste_q10)

#----------Percepcao de experiencia de aprendizagem--------------#
# Quest�o 1
teste_q1 <- chisq.test(dados_curso_foi_1)
print("Teste Qui-Quadrado - Quest�o 1:")
print(teste_q1)

# Quest�o 2
teste_q2 <- chisq.test(dados_curso_foi_2)
print("Teste Qui-Quadrado - Quest�o 2:")
print(teste_q2)

# Quest�o 3
teste_q3 <- chisq.test(dados_tempo_aulas)
print("Teste Qui-Quadrado - Quest�o 3:")
print(teste_q3)

# Quest�o 4
teste_q4 <- chisq.test(dados_curso_geral)
print("Teste Qui-Quadrado - Quest�o 4:")
print(teste_q4)

# Quest�o 11
teste_q11 <- chisq.test(dados_quero_aprender_ml)
print("Teste Qui-Quadrado - Quest�o 11:")
print(teste_q11)

# Questão 1: "O curso foi" (Parte 1)
dados_curso_foi_1 <- matrix(c(46, 87, 10, 35), nrow = 2)
colnames(dados_curso_foi_1) <- c("Percebe diversão", "Não percebe diversão")
rownames(dados_curso_foi_1) <- c("Aplicação 7", "Aplicação 6")

# Questão 2: "O curso foi" (Parte 2)
dados_curso_foi_2 <- matrix(c(16, 56, 40, 66), nrow = 2)
colnames(dados_curso_foi_2) <- c("Percebe facilidade", "Não percebe facilidade")
rownames(dados_curso_foi_2) <- c("Aplicação 7", "Aplicação 6")

# Questão 3: "O tempo das aulas passou"
dados_tempo_aulas <- matrix(c(31, 58, 25, 64), nrow = 2)
colnames(dados_tempo_aulas) <- c("Percebe o tempo passar rápido", "Não percebe o tempo passar rápido")
rownames(dados_tempo_aulas) <- c("Aplicação 7", "Aplicação 6")

# Questão 4: "Em geral, o curso foi"
dados_curso_geral <- matrix(c(51, 110, 5, 12), nrow = 2)
colnames(dados_curso_geral) <- c("Percebe ser bom", "Não percebe ser bom")
rownames(dados_curso_geral) <- c("Aplicação 7", "Aplicação 6")

# Questão 5: "Eu entendi o que é Machine Learning"
dados_ml_entendimento <- matrix(c(52, 103, 4, 19), nrow = 2)
colnames(dados_ml_entendimento) <- c("Sim", "Não")
rownames(dados_ml_entendimento) <- c("Aplicação 7", "Aplicação 6")

#Questão 6: "Computadores ou sistemas podem aprender?"
dados_computadores_aprender <- matrix(c(55, 119, 1, 2), nrow = 2)
colnames(dados_computadores_aprender) <- c("Sim", "Não")
rownames(dados_computadores_aprender) <- c("Aplicação 7", "Aplicação 6")

#Questão 7: "Eu posso aprender a fazer soluções com IA/ML?"
dados_aprender_solucoes <- matrix(c(55, 119, 1, 3), nrow = 2)
colnames(dados_aprender_solucoes) <- c("Sim", "Não")
rownames(dados_aprender_solucoes) <- c("Aplicação 7", "Aplicação 6")

#Questão 8: "Eu consigo desenvolver um modelo de ML para reconhecimento de imagens"
dados_desenvolver_ml <- matrix(c(50, 110, 6, 11), nrow = 2)
colnames(dados_desenvolver_ml) <- c("Sim", "Não")
rownames(dados_desenvolver_ml) <- c("Aplicação 7", "Aplicação 6")

#Questão 9: "Desenvolver um modelo de ML é"
dados_ml_facilidade <- matrix(c(7, 33, 49, 89), nrow = 2)
colnames(dados_ml_facilidade) <- c("Percebe facilidade", "Não Percebe facilidade")
rownames(dados_ml_facilidade) <- c("Aplicação 7", "Aplicação 6")

#Questão 10: "Consigo explicar para um amigo(a) o que é ML"
dados_explicar_ml <- matrix(c(30, 77, 26, 45), nrow = 2)
colnames(dados_explicar_ml) <- c("Sim", "Não")
rownames(dados_explicar_ml) <- c("Aplicação 7", "Aplicação 6")

#Questão 11: "Quero aprender mais sobre ML"
dados_quero_aprender_ml <- matrix(c(37, 92, 19, 30), nrow = 2)
colnames(dados_quero_aprender_ml) <- c("Sim", "Não")
rownames(dados_quero_aprender_ml) <- c("Aplicação 7", "Aplicação 6")
                                

# Realizando o teste qui-quadrado para cada questão

# Questão 1
teste_q1 <- chisq.test(dados_curso_foi_1)
print("Teste Qui-Quadrado - Questão 1:")
print(teste_q1)

# Questão 2
teste_q2 <- chisq.test(dados_curso_foi_2)
print("Teste Qui-Quadrado - Questão 2:")
print(teste_q2)

# Questão 3
teste_q3 <- chisq.test(dados_tempo_aulas)
print("Teste Qui-Quadrado - Questão 3:")
print(teste_q3)

# Questão 4
teste_q4 <- chisq.test(dados_curso_geral)
print("Teste Qui-Quadrado - Questão 4:")
print(teste_q4)

# Questão 5
teste_q5 <- chisq.test(dados_ml_entendimento)
print("Teste Qui-Quadrado - Questão 5:")
print(teste_q5)

# Questão 6
teste_q6 <- chisq.test(dados_computadores_aprender)
print("Teste Qui-Quadrado - Questão 6:")
print(teste_q6)

# Questão 7
teste_q7 <- chisq.test(dados_aprender_solucoes)
print("Teste Qui-Quadrado - Questão 7:")
print(teste_q7)

# Questão 8
teste_q8 <- chisq.test(dados_desenvolver_ml)
print("Teste Qui-Quadrado - Questão 8:")
print(teste_q8)

# Questão 9
teste_q9 <- chisq.test(dados_ml_facilidade)
print("Teste Qui-Quadrado - Questão 9:")
print(teste_q9)

# Questão 10
teste_q10 <- chisq.test(dados_explicar_ml)
print("Teste Qui-Quadrado - Questão 10:")
print(teste_q10)

# Questão 11
teste_q11 <- chisq.test(dados_quero_aprender_ml)
print("Teste Qui-Quadrado - Questão 11:")
print(teste_q11)

#--------------posso optar pelo fisher se as amostras forem pequenas, mas o chi-quadrado resolve-------#
teste_q6_fisher <- fisher.test(dados_computadores_aprender)
print("Teste Exato de Fisher - Questão 6:")
print(teste_q6_fisher)

teste_q6 <- chisq.test(dados_computadores_aprender)
print("Teste Qui-Quadrado - Questão 6:")
print(teste_q6)
#----------Percepcao de aprendizagem--------------#
# Questão 5
teste_q5 <- chisq.test(dados_ml_entendimento)
print("Teste Qui-Quadrado - Questão 5:")
print(teste_q5)

# Questão 7
teste_q7 <- chisq.test(dados_aprender_solucoes)
print("Teste Qui-Quadrado - Questão 7:")
print(teste_q7)

# Questão 8
teste_q8 <- chisq.test(dados_desenvolver_ml)
print("Teste Qui-Quadrado - Questão 8:")
print(teste_q8)

# Questão 9
teste_q9 <- chisq.test(dados_ml_facilidade)
print("Teste Qui-Quadrado - Questão 9:")
print(teste_q9)

# Questão 10
teste_q10 <- chisq.test(dados_explicar_ml)
print("Teste Qui-Quadrado - Questão 10:")
print(teste_q10)

#----------Percepcao de experiencia de aprendizagem--------------#
# Questão 1
teste_q1 <- chisq.test(dados_curso_foi_1)
print("Teste Qui-Quadrado - Questão 1:")
print(teste_q1)

# Questão 2
teste_q2 <- chisq.test(dados_curso_foi_2)
print("Teste Qui-Quadrado - Questão 2:")
print(teste_q2)

# Questão 3
teste_q3 <- chisq.test(dados_tempo_aulas)
print("Teste Qui-Quadrado - Questão 3:")
print(teste_q3)

# Questão 4
teste_q4 <- chisq.test(dados_curso_geral)
print("Teste Qui-Quadrado - Questão 4:")
print(teste_q4)

# Questão 11
teste_q11 <- chisq.test(dados_quero_aprender_ml)
print("Teste Qui-Quadrado - Questão 11:")
print(teste_q11)


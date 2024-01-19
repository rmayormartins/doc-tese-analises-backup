library(dplyr)
library(psych)

#todos low ses 23 (ap7)
todos_low_ses_ap7 <- c(5.8, 6.9, 8.1, 7.8, 7.5, 7.8, 8.1, 2.2, 8.3, 7.5, 7.5, 7.5, 7.5, 6.9, 5.3, 7.8, 6.7, 8.3, 7.2, 7.2, 7.2, 7.5, 8.3, 6.7, 8.1, 6.1, 6.1, 7.2, 9.4, 8.1, 6.7, 3.3, 7.5, 8.9, 3.9, 8.6, 2.2, 7.5, 7.8, 8.3, 6.1, 5.3, 6.9, 7.8, 8.9, 5.8, 6.1, 6.9, 7.2, 8.3, 5, 9.7, 7.2, 3.9, 1.9, 1.9)
#

#todos low ses 22 (ap6)
todos_low_ses_ap6 <- c(5, 2.8, 7.2, 7.5, 6.7, 5.8, 8.3, 8.6, 8.3, 8.9, 7.2, 8.3, 4.2, 7.5, 8.1, 6.7, 7.5, 5.8, 8.1, 6.9, 6.7, 8.1, 7.5, 5.3, 6.4, 7.8, 7.8, 7.2, 9.4, 8.3, 7.8, 8.1, 8.1, 8.1, 7.8, 5.3, 8.9, 8.9, 9.2, 7.5, 8.6, 7.8, 6.4, 6.4, 5.3, 5.3, 7.8, 8.3, 7.8, 7.8, 7.5, 8.1, 8.9, 8.3, 8.1, 7.8, 8.6, 9.2, 6.7, 7.2, 8.6, 2.5, 7.5, 7.8, 8.1, 8.6, 6.1, 9.4, 8.9, 8.3, 8.3, 8.3, 8.1, 7.5, 8.1, 7.5, 7.5, 7.5, 8.3)
#

# Teste de Shapiro-Wilk
shapiro_ap7 <- shapiro.test(todos_low_ses_ap7)
shapiro_ap6 <- shapiro.test(todos_low_ses_ap6)

# Exibir resultados
print(shapiro_ap7)
print(shapiro_ap6)

# Teste de Mann-Whitney
mann_whitney <- wilcox.test(todos_low_ses_ap7, todos_low_ses_ap6)

# Exibir resultados
print(mann_whitney)

#Como achei diferença significativa com o Mann-Whitney, 
#o Mann-Whitney so indica que tem diferença

#--------------------------TESTE de Levene--------------------#
#verificar se as variâncias dos dois grupos são semelhantes: Descobrir a igualdade das variâncias
# Combina as notas em um único vetor
notas <- c(todos_low_ses_ap7, todos_low_ses_ap6)

#vetor de grupos correspondente
grupos <- factor(c(rep("AP7", length(todos_low_ses_ap7)), 
                   rep("AP6", length(todos_low_ses_ap6))))

#dataframe combinando notas e grupos
dados <- data.frame(notas, grupos)

#teste de Levene
leveneTest(notas ~ grupos, data = dados)

#-----------------------d de Cohen----------------------------#
#Análise de Efeito Prático: medir o tamanho do efeito (como o d de Cohen) para entender a magnitude da diferença entre os grupos.
#install.packages("effsize")

library(effsize)
d <- cohen.d(todos_low_ses_ap7, todos_low_ses_ap6)
print(d)



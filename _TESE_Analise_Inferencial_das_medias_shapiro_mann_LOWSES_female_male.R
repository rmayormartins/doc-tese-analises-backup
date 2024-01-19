library(dplyr)
library(psych)

female_low_ses <- c(
  7.8, 7.5, 7.8, 2.2, 8.3, 7.5, 7.5, 6.9, 5.3, 7.8, 6.7, 8.3, 7.2, 7.2, 8.1,
  6.7, 3.9, 7.5, 7.8, 8.3, 5.3, 6.9, 7.8, 5.8, 6.1, 7.2, 8.3, 5, 9.7, 7.2, 3.9, 
  1.9, 5, 2.8, 7.5, 8.3, 8.6, 8.9, 4.2, 7.5, 8.1, 6.7, 7.5, 7.8, 7.8, 8.3, 
  8.1, 8.9, 7.8, 6.4, 6.4, 5.3, 5.3, 8.3, 7.8, 8.9, 7.8, 8.6, 8.6, 2.5, 7.8, 
  8.9, 8.3, 8.3, 8.1, 8.1, 7.5, 8.3
)
male_low_ses <- c(
  5.8, 6.9, 8.1, 8.1, 7.5, 7.5, 7.2, 7.5, 8.3, 6.7, 8.1, 6.1, 6.1, 7.2, 9.4, 
  3.3, 7.5, 8.9, 8.6, 2.2, 6.1, 8.9, 6.9, 1.9, 7.2, 6.7, 5.8, 8.3, 7.2, 8.3, 
  7.5, 5.8, 8.1, 6.9, 6.7, 8.1, 5.3, 6.4, 7.2, 9.4, 7.8, 8.1, 8.1, 7.8, 5.3, 
  8.9, 9.2, 7.5, 8.6, 7.8, 7.8, 7.5, 8.1, 8.3, 8.1, 9.2, 6.7, 7.2, 7.5, 8.1, 
  8.6, 6.1, 9.4, 8.3, 7.5, 7.5, 7.5
)

# Teste de Shapiro-Wilk
shapiro_female <- shapiro.test(female_low_ses)
shapiro_male <- shapiro.test(male_low_ses)

# Exibir resultados
print(shapiro_female)
print(shapiro_male)

# Teste de Mann-Whitney
mann_whitney <- wilcox.test(female_low_ses, male_low_ses)

# Exibir resultados
print(mann_whitney)

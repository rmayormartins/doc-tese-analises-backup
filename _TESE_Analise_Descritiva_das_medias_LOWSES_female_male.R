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

##########SO FEMALE#####################
# Criar data frames separados para female
df_female <- data.frame(grupo = "Female", valores = female_low_ses)

# Criar um data frame somente para todos_geral
df_female <- data.frame(
  grupo = "Female",
  valores = female_low_ses
)

# Calcule estatísticas descritivas para todos_geral
df_female %>% 
  group_by(grupo) %>% 
  summarise(
    count = n(),
    mean = mean(valores, na.rm = TRUE),
    sd = sd(valores, na.rm = TRUE),
    min = min(valores, na.rm = TRUE),
    q1 = quantile(valores, 0.25, na.rm = TRUE),
    median = median(valores, na.rm = TRUE),
    q3 = quantile(valores, 0.75, na.rm = TRUE),
    max = max(valores, na.rm = TRUE),
    IQR = IQR(valores, na.rm = TRUE),
    skew = psych::skew(valores),
    kurtosis = psych::kurtosi(valores),
    cv = sd(valores, na.rm = TRUE) / mean(valores, na.rm = TRUE)
  )

##########SO MALE#####################
# Criar data frames separados para male_low_ses
df_male <- data.frame(grupo = "male", valores = male_low_ses)

# Criar um data frame somente para male
df_male <- data.frame(
  grupo = "male",
  valores = male_low_ses
)

# Calcule estatísticas descritivas para male_low_ses
df_male %>% 
  group_by(grupo) %>% 
  summarise(
    count = n(),
    mean = mean(valores, na.rm = TRUE),
    sd = sd(valores, na.rm = TRUE),
    min = min(valores, na.rm = TRUE),
    q1 = quantile(valores, 0.25, na.rm = TRUE),
    median = median(valores, na.rm = TRUE),
    q3 = quantile(valores, 0.75, na.rm = TRUE),
    max = max(valores, na.rm = TRUE),
    IQR = IQR(valores, na.rm = TRUE),
    skew = psych::skew(valores),
    kurtosis = psych::kurtosi(valores),
    cv = sd(valores, na.rm = TRUE) / mean(valores, na.rm = TRUE)
  )

#------------------------Boxplot e Histograma/Densidade arrumando-----------------------------#
# Primeiro, combine os data frames df_female e df_male
df_combined <- rbind(df_female, df_male)

# Carregar as bibliotecas necessárias
library(ggplot2)
library(gridExtra)

# Código para os boxplots
boxplot_graph <- ggplot(df_combined, aes(x=grupo, y=valores, fill=grupo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +
  labs(x="Grupo", y="Notas") +
  theme_bw() +
  coord_flip()

# Código para o gráfico de densidade sobreposto
density_plot <- ggplot(df_combined, aes(x=valores, fill=grupo)) +
  geom_density(alpha=0.7) +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +
  labs(x = "Notas", y = "Densidade") +
  theme_light() +
  theme(legend.position="top")

# Combinar os gráficos em uma única figura
grid.arrange(boxplot_graph, density_plot, ncol = 2)

#------------------------Boxplot e Histograma/Densidade arrumando-----------------------------#
# Carregar pacotes necessários
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)

# Combinar os dataframes df_female e df_male
df_combined <- rbind(df_female, df_male)

# Definir a ordem dos níveis do fator
df_combined$grupo <- factor(df_combined$grupo, levels = c("Female", "Male"))

# Criar os boxplots com cores diferenciadas e jitter para Female e Male
plot1 <- ggplot(df_combined, aes(x=grupo, y=valores, fill=grupo)) +
  geom_boxplot(color="black", outlier.shape = NA) +
  geom_jitter(width=0.3, size=1, color="black") +
  labs(x="Grupo", y="Notas", fill="") +
  theme_light() +
  theme(legend.position="none") +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +
  coord_flip()

# Criar o dataframe para o histograma
df_histogram <- data.frame(
  valores = c(female_low_ses, male_low_ses),
  grupo = c(rep("Female", length(female_low_ses)), 
            rep("Male", length(male_low_ses)))
)

# Criar o histograma e a densidade
plot2 <- ggplot(df_histogram, aes(x = valores, fill = grupo)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +
  labs(x = "Notas", y = "Densidade", fill = "") +
  theme_light() +
  theme(legend.position="none")

# Combinar os gráficos em uma grade
grid_plot <- grid.arrange(plot1, plot2, ncol=2)

#------------------------Boxplot e Histograma/Densidade arrumando-----------------------------#
library(ggplot2)
library(dplyr)

# Preparar os dados
df_female_low_ses <- data.frame(sexo = "Female", notas = female_low_ses)
df_male_low_ses <- data.frame(sexo = "Male", notas = male_low_ses)
df_combined <- rbind(df_female_low_ses, df_male_low_ses)

# Criar o boxplot
plot1 <- ggplot(df_combined, aes(x = sexo, y = notas, fill = sexo)) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.3, size = 1, color = "black") +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +
  coord_flip()

# Criar o histograma/densidade
plot2 <- ggplot(df_combined, aes(x = notas, fill = sexo)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue"))

# Combinar os gráficos
grid.arrange(plot1, plot2, ncol = 2)


#------------------------Boxplot e Histograma/Densidade arrumando-----------------------------#
library(ggplot2)
library(dplyr)

# Preparar os dados
df_female_low_ses <- data.frame(sexo = "Female", notas = female_low_ses)
df_male_low_ses <- data.frame(sexo = "Male", notas = male_low_ses)
df_combined <- rbind(df_female_low_ses, df_male_low_ses)

# Ajustar a ordem dos fatores
df_combined$sexo <- factor(df_combined$sexo, levels = c("Male", "Female"))

# Criar o boxplot
plot1 <- ggplot(df_combined, aes(x = sexo, y = notas, fill = sexo)) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.3, size = 1, color = "black") +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +
  coord_flip() +
  theme_minimal()  # Aplicar um tema com fundo branco

# Criar o histograma/densidade
plot2 <- ggplot(df_combined, aes(x = notas, fill = sexo)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "lightblue")) +
  theme_minimal()  # Aplicar um tema com fundo branco

# Combinar os gráficos
grid.arrange(plot1, plot2, ncol = 2)
#############Arrumado, embaixo desse o oficial###############
#------------------------Boxplot e Histograma/Densidade arrumando-----------------------------#
library(ggplot2)
library(dplyr)

# Preparar os dados
df_female_low_ses <- data.frame(sexo = "Feminino", notas = female_low_ses)
df_male_low_ses <- data.frame(sexo = "Masculino", notas = male_low_ses)
df_combined <- rbind(df_female_low_ses, df_male_low_ses)

# Ajustar a ordem dos fatores
df_combined$sexo <- factor(df_combined$sexo, levels = c("Masculino", "Feminino"))

# Criar o boxplot
plot1 <- ggplot(df_combined, aes(x = sexo, y = notas, fill = sexo)) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.3, size = 1, color = "black") +
  scale_fill_manual(values = c("Feminino" = "lightcoral", "Masculino" = "lightblue")) +
  coord_flip() +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none")  # Remover a legenda

# Criar o histograma/densidade
plot2 <- ggplot(df_combined, aes(x = notas, fill = sexo)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Feminino" = "lightcoral", "Masculino" = "lightblue")) +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none")  # Remover a legenda

# Combinar os gráficos
grid.arrange(plot1, plot2, ncol = 2)
#############OFICIAL###############
#############OFICIAL###############
#############OFICIAL###############
#############OFICIAL###############
#############OFICIAL###############
#------------------------Boxplot e Histograma/Densidade arrumando-----------------------------#
library(ggplot2)
library(dplyr)
# Carregar pacotes necessários
library(gridExtra)
library(grid)
library(cowplot)

# Preparar os dados
df_female_low_ses <- data.frame(sexo = "Feminino", notas = female_low_ses)
df_male_low_ses <- data.frame(sexo = "Masculino", notas = male_low_ses)
df_combined <- rbind(df_female_low_ses, df_male_low_ses)

# Ajustar a ordem dos fatores
df_combined$sexo <- factor(df_combined$sexo, levels = c("Masculino", "Feminino"))

# Criar o boxplot
plot1 <- ggplot(df_combined, aes(x = sexo, y = notas, fill = sexo)) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.3, size = 1, color = "black") +
  scale_fill_manual(values = c("Feminino" = "lightcoral", "Masculino" = "lightblue")) +
  coord_flip() +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none")  # Remover a legenda

# Criar o histograma/densidade
plot2 <- ggplot(df_combined, aes(x = notas, fill = sexo)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Feminino" = "lightcoral", "Masculino" = "lightblue")) +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none") +  # Remover a legenda
  labs(y = "Densidade")  # Alterar rótulo do eixo Y para 'Densidade'

# Combinar os gráficos
grid.arrange(plot1, plot2, ncol = 2)

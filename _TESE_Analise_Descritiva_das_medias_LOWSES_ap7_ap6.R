library(dplyr)
library(psych)

#todos low ses 23 (ap7)
todos_low_ses_ap7 <- c(5.8, 6.9, 8.1, 7.8, 7.5, 7.8, 8.1, 2.2, 8.3, 7.5, 7.5, 7.5, 7.5, 6.9, 5.3, 7.8, 6.7, 8.3, 7.2, 7.2, 7.2, 7.5, 8.3, 6.7, 8.1, 6.1, 6.1, 7.2, 9.4, 8.1, 6.7, 3.3, 7.5, 8.9, 3.9, 8.6, 2.2, 7.5, 7.8, 8.3, 6.1, 5.3, 6.9, 7.8, 8.9, 5.8, 6.1, 6.9, 7.2, 8.3, 5, 9.7, 7.2, 3.9, 1.9, 1.9)
#

#todos low ses 22 (ap6)
todos_low_ses_ap6 <- c(5, 2.8, 7.2, 7.5, 6.7, 5.8, 8.3, 8.6, 8.3, 8.9, 7.2, 8.3, 4.2, 7.5, 8.1, 6.7, 7.5, 5.8, 8.1, 6.9, 6.7, 8.1, 7.5, 5.3, 6.4, 7.8, 7.8, 7.2, 9.4, 8.3, 7.8, 8.1, 8.1, 8.1, 7.8, 5.3, 8.9, 8.9, 9.2, 7.5, 8.6, 7.8, 6.4, 6.4, 5.3, 5.3, 7.8, 8.3, 7.8, 7.8, 7.5, 8.1, 8.9, 8.3, 8.1, 7.8, 8.6, 9.2, 6.7, 7.2, 8.6, 2.5, 7.5, 7.8, 8.1, 8.6, 6.1, 9.4, 8.9, 8.3, 8.3, 8.3, 8.1, 7.5, 8.1, 7.5, 7.5, 7.5, 8.3)
#

##########SO Vulnerabilidade_social_ap7#####################
# Criar data frames separados para Vulnerabilidade_social_AP7
df_ap7 <- data.frame(grupo = "Vulnerabilidade_social_AP7", valores = todos_low_ses_ap7)

# Criar um data frame somente para Vulnerabilidade_social_AP7
df_ap7 <- data.frame(
  grupo = "Vulnerabilidade_social_AP7",
  valores = todos_low_ses_ap7
)

# Calcule estatísticas descritivas para Vulnerabilidade_social_AP7 
df_ap7 %>% 
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

##########SO Vulnerabilidade_social_AP6#####################
# Criar data frames separados para todos_low_ses_ap6
df_ap6 <- data.frame(grupo = "Vulnerabilidade_social_AP6", valores = todos_low_ses_ap6)

# Criar um data frame somente para male
df_ap6 <- data.frame(
  grupo = "Vulnerabilidade_social_AP6",
  valores = todos_low_ses_ap6
)

# Calcule estatísticas descritivas para todos_low_ses_ap6
df_ap6 %>% 
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
# Primeiro, combine os data frames df_ap7 e df_ap6
df_combined <- rbind(df_ap7, df_ap6)

# Carregar as bibliotecas necessárias
library(ggplot2)
library(gridExtra)

# Código para os boxplots
boxplot_graph <- ggplot(df_combined, aes(x=grupo, y=valores, fill=grupo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Vulnerabilidade_social_AP7" = "lightcoral", "Vulnerabilidade_social_AP6" = "lightblue")) +
  labs(x="Grupo", y="Notas") +
  theme_bw() +
  coord_flip()

# Código para o gráfico de densidade sobreposto
density_plot <- ggplot(df_combined, aes(x=valores, fill=grupo)) +
  geom_density(alpha=0.7) +
  scale_fill_manual(values = c("Vulnerabilidade_social_AP7" = "lightcoral", "Vulnerabilidade_social_AP6" = "lightblue")) +
  labs(x = "Notas", y = "Densidade") +
  theme_light() +
  theme(legend.position="top")

#############OFICIAL###############
#############OFICIAL###############
#############OFICIAL###############
#############OFICIAL###############
#############OFICIAL###############
#------------------------Boxplot e Histograma/Densidade combinado-----------------------------#
library(ggplot2)
library(dplyr)
# Carregar pacotes necessários
library(gridExtra)
library(grid)
library(cowplot)

# Preparar os dados
df_todos_low_ses_ap7 <- data.frame(aplicação = "Vulnerabilidade_social_AP7", notas = todos_low_ses_ap7)
df_todos_low_ses_ap6 <- data.frame(aplicação = "Vulnerabilidade_social_AP6", notas = todos_low_ses_ap6)
df_combined <- rbind(df_todos_low_ses_ap7, df_todos_low_ses_ap6)

# Ajustar a ordem dos fatores
df_combined$aplicação <- factor(df_combined$aplicação, levels = c("Vulnerabilidade_social_AP6", "Vulnerabilidade_social_AP7"))

# Criar o boxplot
plot1 <- ggplot(df_combined, aes(x = aplicação, y = notas, fill = aplicação)) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.3, size = 1, color = "black") +
  scale_fill_manual(values = c("Vulnerabilidade_social_AP7" = "lightcoral", "Vulnerabilidade_social_AP6" = "lightblue")) +
  coord_flip() +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none")  # Remover a legenda

# Criar o histograma/densidade
plot2 <- ggplot(df_combined, aes(x = notas, fill = aplicação)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Vulnerabilidade_social_AP7" = "lightcoral", "Vulnerabilidade_social_AP6" = "lightblue")) +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none") +  # Remover a legenda
  labs(y = "Densidade")  # Alterar rótulo do eixo Y para 'Densidade'

# Combinar os gráficos
grid.arrange(plot1, plot2, ncol = 2)

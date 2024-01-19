library(dplyr)
library(psych)

ms_low_ses <- c(
  7.8, 8.1, 2.2, 8.3, 7.5, 7.5, 7.5, 5.3, 8.3, 7.2, 7.5, 8.3, 6.7, 6.1, 7.2, 
  9.4, 6.7, 3.3, 7.5, 8.9, 3.9, 8.6, 2.2, 6.1, 5.3, 6.9, 7.8, 8.9, 5.8, 6.1, 
  6.9, 7.2, 8.3, 5, 9.7, 7.2, 5, 2.8, 7.5, 6.7, 5.8, 8.3, 8.6, 7.2, 8.3, 6.7, 
  8.1, 5.3, 8.9, 8.9, 9.2, 8.6, 6.4, 6.4, 5.3, 7.8, 8.3, 7.8, 7.8, 8.3, 8.1, 
  9.2, 6.7, 7.2, 7.5, 8.1, 7.5, 7.5
)
hs_low_ses <- c(
  5.8, 6.9, 8.1, 7.5, 7.8, 7.5, 6.9, 7.8, 6.7, 7.2, 7.2, 8.1, 6.1, 8.1, 7.5,
  7.8, 8.3, 3.9, 1.9, 1.9, 7.2, 8.3, 8.9, 4.2, 7.5, 8.1, 7.5, 5.8, 6.9, 6.7,
  8.1, 7.5, 6.4, 7.8, 7.8, 7.2, 9.4, 8.3, 7.8, 8.1, 8.1, 8.1, 7.8, 5.3, 7.5,
  7.8, 5.3, 7.5, 8.1, 8.9, 7.8, 8.6, 8.6, 2.5, 7.8, 8.6, 6.1, 9.4, 8.9, 8.3,
  8.3, 8.3, 8.1, 8.1, 7.5, 7.5, 8.3
)

##########SO Ensino fundamental#####################
# Criar data frames separados para Ensino fundamental
df_ms <- data.frame(grupo = "Ensino_Fundamental", valores = ms_low_ses)

# Criar um data frame somente para todos_geral
df_ms <- data.frame(
  grupo = "Ensino_Fundamental",
  valores = ms_low_ses
)

# Calcule estatísticas descritivas para ensino fundamental 
df_ms %>% 
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

##########SO Ensino Medio#####################
# Criar data frames separados para hs_low_ses
df_hs <- data.frame(grupo = "Ensino_Medio", valores = hs_low_ses)

# Criar um data frame somente para male
df_hs <- data.frame(
  grupo = "Ensino_Medio",
  valores = hs_low_ses
)

# Calcule estatísticas descritivas para hs_low_ses
df_hs %>% 
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
# Primeiro, combine os data frames df_ms e df_hs
df_combined <- rbind(df_ms, df_hs)

# Carregar as bibliotecas necessárias
library(ggplot2)
library(gridExtra)

# Código para os boxplots
boxplot_graph <- ggplot(df_combined, aes(x=grupo, y=valores, fill=grupo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Ensino_Fundamental" = "lightcoral", "Ensino_Medio" = "lightblue")) +
  labs(x="Grupo", y="Notas") +
  theme_bw() +
  coord_flip()

# Código para o gráfico de densidade sobreposto
density_plot <- ggplot(df_combined, aes(x=valores, fill=grupo)) +
  geom_density(alpha=0.7) +
  scale_fill_manual(values = c("Ensino_Fundamental" = "lightcoral", "Ensino_Medio" = "lightblue")) +
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
df_ms_low_ses <- data.frame(etapa_educacional = "Ensino_Fundamental", notas = ms_low_ses)
df_hs_low_ses <- data.frame(etapa_educacional = "Ensino_Medio", notas = hs_low_ses)
df_combined <- rbind(df_ms_low_ses, df_hs_low_ses)

# Ajustar a ordem dos fatores
df_combined$etapa_educacional <- factor(df_combined$etapa_educacional, levels = c("Ensino_Medio", "Ensino_Fundamental"))

# Criar o boxplot
plot1 <- ggplot(df_combined, aes(x = etapa_educacional, y = notas, fill = etapa_educacional)) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.3, size = 1, color = "black") +
  scale_fill_manual(values = c("Ensino_Fundamental" = "lightcoral", "Ensino_Medio" = "lightblue")) +
  coord_flip() +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none")  # Remover a legenda

# Criar o histograma/densidade
plot2 <- ggplot(df_combined, aes(x = notas, fill = etapa_educacional)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Ensino_Fundamental" = "lightcoral", "Ensino_Medio" = "lightblue")) +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none") +  # Remover a legenda
  labs(y = "Densidade")  # Alterar rótulo do eixo Y para 'Densidade'

# Combinar os gráficos
grid.arrange(plot1, plot2, ncol = 2)

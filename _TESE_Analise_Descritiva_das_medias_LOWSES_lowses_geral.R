library(dplyr)
library(psych)

todos_low_ses <- c(5.8, 6.9, 8.1, 7.8, 7.5, 7.8, 8.1, 2.2, 8.3, 7.5, 
                   7.5, 7.5, 7.5, 6.9, 5.3, 7.8, 6.7, 8.3, 7.2, 7.2, 7.2, 7.5, 8.3, 
                   6.7, 8.1, 6.1, 6.1, 7.2, 9.4, 8.1, 6.7, 3.3, 7.5, 8.9, 3.9, 8.6, 2.2, 
                   7.5, 7.8, 8.3, 6.1, 5.3, 6.9, 7.8, 8.9, 5.8, 6.1, 6.9, 7.2, 8.3, 5, 9.7, 7.2, 
                   3.9, 1.9, 1.9, 5, 2.8, 7.2, 7.5, 6.7, 5.8, 8.3, 8.6, 8.3, 8.9, 7.2, 8.3, 4.2, 7.5, 
                   8.1, 6.7, 7.5, 5.8, 8.1, 6.9, 6.7, 8.1, 7.5, 5.3, 6.4, 7.8, 7.8, 7.2, 9.4, 8.3, 7.8, 
                   8.1, 8.1, 8.1, 7.8, 5.3, 8.9, 8.9, 9.2, 7.5, 8.6, 7.8, 6.4, 6.4, 5.3, 5.3, 7.8, 8.3, 7.8, 
                   7.8, 7.5, 8.1, 8.9, 8.3, 8.1, 7.8, 8.6, 9.2, 6.7, 7.2, 8.6, 2.5, 7.5, 7.8, 8.1, 8.6, 6.1, 9.4, 
                   8.9, 8.3, 8.3, 8.3, 8.1, 7.5, 8.1, 7.5, 7.5, 7.5, 8.3)
todos_geral <- c(7.8, 3.1, 8.6, 7.5, 3.1, 7.5, 2.2, 3.1, 6.4, 3.9, 3.6, 7.5, 8.6, 8.3, 6.4, 8.6, 8.3, 5.3, 4.4, 8.1, 5.6, 6.7, 8.9, 8.3, 4.2, 8.9, 2.5, 7.8, 9.4, 8.3, 7.2, 9.7, 8.6, 2.5, 8.3, 9.4, 5.8, 8.1, 8.3, 6.7, 8.1, 5, 5, 6.7, 4.7, 4.4, 6.9, 5, 8.6, 8.3, 9.2, 6.4, 5.8, 9.4, 8.3, 8.1, 9.2, 6.7, 8.3, 8.9, 9.7, 7.5, 8.6, 8.3, 8.3, 5.6, 9.2, 8.9, 7.8, 8.6, 6.1, 9.2, 9.7, 9.2, 8.6, 8.9, 9.4)


##########SO Vulnerabilidade social#####################
# Criar data frames separados para Vulnerabilidade social
df_todos_low_ses <- data.frame(grupo = "Vulnerabilidade_social", valores = todos_low_ses)

# Criar um data frame somente para Vulnerabilidade social
df_todos_low_ses <- data.frame(
  grupo = "Vulnerabilidade_social",
  valores = todos_low_ses
)

# Calcule estatísticas descritivas para ensino fundamental 
df_todos_low_ses %>% 
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

##########SO Geral#####################
# Criar data frames separados para todos_geral
df_todos_geral <- data.frame(grupo = "Geral", valores = todos_geral)

# Criar um data frame somente para male
df_todos_geral <- data.frame(
  grupo = "Geral",
  valores = todos_geral
)

# Calcule estatísticas descritivas para todos_geral
df_todos_geral %>% 
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
df_combined <- rbind(df_todos_low_ses, df_todos_geral)

# Carregar as bibliotecas necessárias
library(ggplot2)
library(gridExtra)

# Código para os boxplots
boxplot_graph <- ggplot(df_combined, aes(x=grupo, y=valores, fill=grupo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Vulnerabilidade_social" = "lightcoral", "Geral" = "lightblue")) +
  labs(x="Grupo", y="Notas") +
  theme_bw() +
  coord_flip()

# Código para o gráfico de densidade sobreposto
density_plot <- ggplot(df_combined, aes(x=valores, fill=grupo)) +
  geom_density(alpha=0.7) +
  scale_fill_manual(values = c("Vulnerabilidade_social" = "lightcoral", "Geral" = "lightblue")) +
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
df_todos_low_ses <- data.frame(origem_socioeconomica = "Vulnerabilidade_social", notas = todos_low_ses)
df_todos_geral <- data.frame(origem_socioeconomica = "Geral", notas = todos_geral)
df_combined <- rbind(df_todos_low_ses, df_todos_geral)

# Ajustar a ordem dos fatores
df_combined$origem_socioeconomica <- factor(df_combined$origem_socioeconomica, levels = c("Geral", "Vulnerabilidade_social"))

# Criar o boxplot
plot1 <- ggplot(df_combined, aes(x = origem_socioeconomica, y = notas, fill = origem_socioeconomica)) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.3, size = 1, color = "black") +
  scale_fill_manual(values = c("Vulnerabilidade_social" = "lightcoral", "Geral" = "lightblue")) +
  coord_flip() +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none")  # Remover a legenda

# Criar o histograma/densidade
plot2 <- ggplot(df_combined, aes(x = notas, fill = origem_socioeconomica)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Vulnerabilidade_social" = "lightcoral", "Geral" = "lightblue")) +
  theme_minimal() +  # Aplicar um tema com fundo branco
  theme(legend.position = "none") +  # Remover a legenda
  labs(y = "Densidade")  # Alterar rótulo do eixo Y para 'Densidade'

# Combinar os gráficos
grid.arrange(plot1, plot2, ncol = 2)

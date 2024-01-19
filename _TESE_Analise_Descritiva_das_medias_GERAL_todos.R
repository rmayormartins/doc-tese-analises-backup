library(dplyr)
library(psych)

todos_geral <- c(7.8, 3.1, 8.6, 7.5, 3.1, 7.5, 2.2, 3.1, 6.4, 3.9, 3.6, 7.5, 8.6, 8.3, 6.4, 8.6, 8.3, 5.3, 4.4, 8.1, 5.6, 6.7, 8.9, 8.3, 4.2, 8.9, 2.5, 7.8, 9.4, 8.3, 7.2, 9.7, 8.6, 2.5, 8.3, 9.4, 5.8, 8.1, 8.3, 6.7, 8.1, 5, 5, 6.7, 4.7, 4.4, 6.9, 5, 8.6, 8.3, 9.2, 6.4, 5.8, 9.4, 8.3, 8.1, 9.2, 6.7, 8.3, 8.9, 9.7, 7.5, 8.6, 8.3, 8.3, 5.6, 9.2, 8.9, 7.8, 8.6, 6.1, 9.2, 9.7, 9.2, 8.6, 8.9, 9.4)
#female_geral <- c(7.8, 3.1, 3.9, 3.6, 7.5, 6.4, 5.3, 7.2, 8.1, 8.3, 6.7, 5, 5, 8.6, 6.4, 8.1, 9.2, 7.5, 8.3, 9.2, 8.6, 8.6)
#male_geral <- c(3.1, 8.6, 7.5, 3.1, 7.5, 2.2, 6.4, 8.6, 8.3, 8.6, 8.3, 4.4, 8.1, 5.6, 6.7, 8.9, 8.3, 4.2, 8.9, 2.5, 7.8, 9.4, 8.3, 9.7, 8.6, 2.5, 8.3, 9.4, 5.8, 8.1, 6.7, 4.7, 4.4, 6.9, 5, 8.3, 9.2, 5.8, 9.4, 8.3, 6.7, 8.3, 8.9, 9.7, 8.6, 8.3, 5.6, 8.9, 7.8, 6.1, 9.2, 9.7, 9.2, 8.9, 9.4)
#ms_geral <- c(7.8, 3.1, 8.6, 7.5, 7.5, 2.2, 3.1, 6.4, 8.6, 4.4, 8.1, 5.6, 8.3, 8.1, 8.3, 6.7, 8.1, 9.2, 8.3, 8.9, 8.6)
#hs_geral <- c(3.1, 3.9, 3.6, 7.5, 8.3, 6.4, 8.6, 8.3, 5.3, 6.7, 8.9, 8.3, 4.2, 8.9, 2.5, 7.8, 9.4, 7.2, 9.7, 8.6, 2.5, 8.3, 9.4, 5.8, 5, 5, 6.7, 4.7, 4.4, 6.9, 5, 8.6, 8.3, 6.4, 5.8, 9.4, 8.3, 8.1, 9.2, 6.7, 8.9, 9.7, 7.5, 8.6, 8.3, 8.3, 5.6, 9.2, 7.8, 8.6, 6.1, 9.2, 9.7, 9.2, 8.9, 9.4)

##########SO TODOS#####################
# Criar um data frame somente para todos_geral
df_todos <- data.frame(
  grupo = "Todos Geral",
  valores = todos_geral
)

# Calcule estatísticas descritivas para todos_geral
df_todos %>% 
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
#------------------------------------------------------------------------------------#
# Carregar bibliotecas necessárias
library(ggplot2)
library(gridExtra)

# Código para o boxplot
boxplot_graph <- ggplot(df_todos, aes(x=grupo, y=valores)) +
  geom_boxplot(fill="lightcoral") +
  geom_jitter(width=0.3, size=1, color="black") +
  labs(x="Grupo", y="Notas") +
  theme_bw() +
  coord_flip()

# Código para o histograma (supondo que df_todos e todos_geral estejam definidos)
plot_histogram <- ggplot(df_todos, aes(x = valores, fill = "lightcoral")) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, bins = 30, color = "black") +
  geom_density(alpha = 0.7, color = "lightcoral") +
  labs(x = "Notas", y = "Densidade") +
  theme_light() +
  theme(legend.position="none") +
  scale_fill_identity()

# Combinar os gráficos em uma única figura
grid.arrange(boxplot_graph, plot_histogram, ncol = 2)

#------------------------------------------------------------------------------------#



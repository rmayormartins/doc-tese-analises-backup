library(dplyr)
library(psych)

todos_vulnerabilidade_social <- c(5.8, 6.9, 8.1, 7.8, 7.5, 7.8, 8.1, 2.2, 8.3, 7.5, 
                   7.5, 7.5, 7.5, 6.9, 5.3, 7.8, 6.7, 8.3, 7.2, 7.2, 7.2, 7.5, 8.3, 
                   6.7, 8.1, 6.1, 6.1, 7.2, 9.4, 8.1, 6.7, 3.3, 7.5, 8.9, 3.9, 8.6, 2.2, 
                   7.5, 7.8, 8.3, 6.1, 5.3, 6.9, 7.8, 8.9, 5.8, 6.1, 6.9, 7.2, 8.3, 5, 9.7, 7.2, 
                   3.9, 1.9, 1.9, 5, 2.8, 7.2, 7.5, 6.7, 5.8, 8.3, 8.6, 8.3, 8.9, 7.2, 8.3, 4.2, 7.5, 
                   8.1, 6.7, 7.5, 5.8, 8.1, 6.9, 6.7, 8.1, 7.5, 5.3, 6.4, 7.8, 7.8, 7.2, 9.4, 8.3, 7.8, 
                   8.1, 8.1, 8.1, 7.8, 5.3, 8.9, 8.9, 9.2, 7.5, 8.6, 7.8, 6.4, 6.4, 5.3, 5.3, 7.8, 8.3, 7.8, 
                   7.8, 7.5, 8.1, 8.9, 8.3, 8.1, 7.8, 8.6, 9.2, 6.7, 7.2, 8.6, 2.5, 7.5, 7.8, 8.1, 8.6, 6.1, 9.4, 
                   8.9, 8.3, 8.3, 8.3, 8.1, 7.5, 8.1, 7.5, 7.5, 7.5, 8.3)


##########SO TODOS#####################
# Criar um data frame somente para todos_geral
df_todos <- data.frame(
  grupo = "Todos vulnerabilidade social",
  valores = todos_vulnerabilidade_social
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
  geom_boxplot(fill="lightblue") +
  geom_jitter(width=0.3, size=1, color="black") +
  labs(x="Grupo", y="Notas") +
  theme_bw() +
  coord_flip()

# Código para o histograma (supondo que df_todos e todos_geral estejam definidos)
plot_histogram <- ggplot(df_todos, aes(x = valores, fill = "lightblue")) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, bins = 30, color = "black") +
  geom_density(alpha = 0.7, color = "lightblue") +
  labs(x = "Notas", y = "Densidade") +
  theme_light() +
  theme(legend.position="none") +
  scale_fill_identity()

# Combinar os gráficos em uma única figura
grid.arrange(boxplot_graph, plot_histogram, ncol = 2)

#------------------------------------------------------------------------------------#



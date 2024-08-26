
# Librerias ---------------------------------------------------------------

library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)

db_subte <- read_csv("subte_con_precio_inflacion.csv")
head(db_subte)

# Define custom colors
custom_colors <- c(
  "D" = "#02836A",
  "B" = "#EF4236",
  "H" = "#FFD104",
  "A" = "#09B2DE",
  "E" = "#7B2E90",
  "C" = "#0A6FB4"
)


# Análisis descriptivo por variable ---------------------------------------

# Boxplots
ggplotly(db_subte %>% ggplot() + geom_boxplot(aes(y=Viajes), fill = "green") + labs(title="Boxplot - Cantidad total de viajes"))
ggplotly(db_subte %>% ggplot() + geom_boxplot(aes(y=precio), fill = "green") + labs(title="Boxplot - Tarifa del subte"))
ggplotly(db_subte %>% ggplot() + geom_boxplot(aes(y=inflacion), fill = "green") + labs(title="Boxplot - Inflacion mensual ARG"))

# Histogramas
db_subte %>% ggplot() + geom_histogram(aes(x=Viajes), bins = 20, fill = "green") + labs(title="Histograma - Cantidad total de viajes")  +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA))
db_subte %>% ggplot() + geom_histogram(aes(x=precio), bins = 20, fill = "green") + labs(title="Histograma - Tarifa del subte")  +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA))
db_subte %>% ggplot() + geom_histogram(aes(x=inflacion), bins = 20, fill = "green") + labs(title="Histograma - Inflacion mensual ARG")

# Densidad
db_subte %>% ggplot() + geom_density(aes(x=Viajes), fill = "green") + labs(title="Densidad - Cantidad total de viajes")
db_subte %>% ggplot() + geom_density(aes(x=precio), fill = "green") + labs(title="Densidad - Tarifa del subte")
db_subte %>% ggplot() + geom_density(aes(x=inflacion), fill = "green") + labs(title="Densidad - Inflacion mensual ARG")

# Bivariado
pairs(db_subte[,c(4,5,6)])

ggcorr(
  db_subte[,c(4,5,6)], method=c("pairwise","pearson"),
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)

# Grafico Torta - Distribución --------------------------------------------

# Summarize the data by LINEA and calculate percentages
data_summary <- db_subte %>%
  group_by(LINEA) %>%
  summarise(Total_Viajes = sum(Viajes)) %>%
  mutate(Percentage = Total_Viajes / sum(Total_Viajes) * 100)

# Create the pie chart with custom colors and percentages
ggplot(data_summary, aes(x = "", y = Total_Viajes, fill = LINEA)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Distribución de viajes por línea")

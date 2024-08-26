library(dplyr)
library(lubridate)

df = read.csv('subte_23_24.csv')
tarifas_subte = read.csv('tarifas_subte.csv')
tarifas_colectivo = read.csv('precio_colectivo.csv')
inflacion = read.csv('inflacion_mensual.csv')

# Convert date columns to POSIXct
df$FECHA_DESDE = as.POSIXct(df$FECHA_DESDE, format="%Y-%m-%d %H:%M:%S")
df$FECHA_HASTA = as.POSIXct(df$FECHA_HASTA, format="%Y-%m-%d %H:%M:%S")
tarifas_colectivo$Fecha.desde = as.POSIXct(tarifas_colectivo$Fecha.desde, format="%d/%m/%y %H:%M:%S")
tarifas_subte$vigente_desde <- as.POSIXct(tarifas_subte$vigente_desde, format="%Y-%m-%d %H:%M:%S")
inflacion$Fecha_inflacion <- as.POSIXct(inflacion$Fecha_inflacion, format="%Y-%m-%d %H:%M:%S")

#   UNION DE LA BASE CON TARIFAS SUBTE

# Add a column for the next price validity start date
tarifas_subte <- tarifas_subte %>%
  arrange(vigente_desde) %>%
  mutate(vigente_hasta = lead(vigente_desde, order_by = vigente_desde))

# Fill NA for the last price validity range using the current date
tarifas_subte <- tarifas_subte %>%
  mutate(vigente_hasta = ifelse(is.na(vigente_hasta), as.Date(Sys.Date()), as.Date(vigente_hasta)))

# Ensure `vigente_hasta` is of Date type
tarifas_subte$vigente_hasta <- as.Date(tarifas_subte$vigente_hasta)

# Perform the cross join and filter
df_unified <- df %>%
  cross_join(tarifas_subte) %>%
  filter(FECHA_DESDE >= vigente_desde & FECHA_HASTA < vigente_hasta) %>%
  select(FECHA_DESDE, FECHA_HASTA, LINEA, Viajes, precio)

#   UNION DE LA BASE CON TARIFAS COLECTIVO

# Add a column for the next price validity start date
tarifas_colectivo <- tarifas_colectivo %>%
  arrange(Fecha.desde) %>%
  mutate(vigente_hasta = lead(Fecha.desde, order_by = Fecha.desde))

# Fill NA for the last price validity range using the current date
tarifas_colectivo <- tarifas_colectivo %>%
  mutate(vigente_hasta = ifelse(is.na(vigente_hasta), as.Date(Sys.Date()), as.Date(vigente_hasta)))

# Ensure `vigente_hasta` is of Date type
tarifas_colectivo$vigente_hasta <- as.Date(tarifas_colectivo$vigente_hasta)

# Perform the cross join and filter
df_unified <- df_unified %>%
  cross_join(tarifas_colectivo) %>%
  filter(FECHA_DESDE >= Fecha.desde & FECHA_HASTA < vigente_hasta) %>%
  select(FECHA_DESDE, FECHA_HASTA, LINEA, Viajes, precio, Precio.colectivo)

#   UNION DE LA BASE CON INFLACION

# Extract month and year from FECHA_DESDE to match with inflacion
df_unified <- df_unified %>%
  mutate(Month = format(FECHA_DESDE, "%Y-%m"))

# Add a column for the previous month's inflation
inflacion <- inflacion %>%
  arrange(Fecha_inflacion) %>%
  mutate(Month = format(Fecha_inflacion, "%Y-%m"),
         Prev_Month = lag(Month),
         Inflacion_prev = lag(Inflacion))

# Merge df_unified with inflacion based on the month
df_unified <- df_unified %>%
  left_join(inflacion, by = c("Month" = "Month"))

# Ensure inflation is correctly applied to the rows
df_unified <- df_unified %>%
  mutate(Inflacion_mes_pasado = coalesce(Inflacion_prev, Inflacion)) %>%
  select(-c(Month, Prev_Month, Inflacion_prev, Fecha_inflacion))

# Renombrar columnas
df_unified <- df_unified %>%
  rename(
    Precio_subte = precio,
    Precio_colectivo = Precio.colectivo,
    Pasajeros = Viajes
  )

write.csv(df_unified, file = "subte_con_todo.csv", row.names = FALSE)

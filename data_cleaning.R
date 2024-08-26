library(tidyverse)

# Define the path where your CSV files are located
file_path <- "molinetes 2023-2024"

# List of CSV files
files <- list.files(file_path, pattern = "*.csv", full.names = TRUE)

# Function to check and remove empty columns
clean_columns <- function(df) {
  # Identify columns with all NA values or empty strings
  empty_cols <- sapply(df, function(x) all(is.na(x) | x == ""))

  # Remove empty columns
  df <- df[ , !empty_cols]
  return(df)
}

# Read all CSV files into a list of data frames and clean columns
data_list <- lapply(files, function(file) {
  df <- read.csv(file)
  clean_columns(df)
})

# Find the common columns across all data frames
common_cols <- Reduce(intersect, lapply(data_list, names))

# Ensure all data frames have the same columns
data_list <- lapply(data_list, function(df) {
  df <- df[ , common_cols, drop = FALSE]
  return(df)
})

# Combine all data frames into one
db <- bind_rows(data_list)

# List all objects in the environment
all_objects <- ls()

# Specify the names of objects to keep
objects_to_keep <- c("db")

# Determine objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)

# Remove objects not in the keep list
rm(list = objects_to_remove)

# Combine the 'data' and 'from' columns, and convert to POSIXct
db$FECHA_DESDE <- as.POSIXct(paste(db$FECHA, db$DESDE), format = "%d/%m/%Y %H:%M:%S", tz = "UTC")

# Combine the 'data' and 'to' columns, and convert to POSIXct
db$FECHA_HASTA <- as.POSIXct(paste(db$FECHA, db$HASTA), format = "%d/%m/%Y %H:%M:%S", tz = "UTC")

db = subset(db, select = -c(FECHA,DESDE,HASTA,MOLINETE,ESTACION,pax_pagos,pax_pases_pagos,pax_franq))

db <- db %>%
  rename(
    Viajes = pax_TOTAL
  )

# Identify the last two columns
last_two_cols <- tail(names(db), 2)

# Identify the remaining columns
remaining_cols <- setdiff(names(db), last_two_cols)

# Reorder the columns: last two columns come first
db <- db[, c(last_two_cols, remaining_cols)]

# Remove the prefix "Linea" from the column
db$LINEA <- gsub("^Linea", "", db$LINEA)

colnames(db)

# Agregacion de filas
aggr_db <- db %>%
  group_by(FECHA_DESDE, FECHA_HASTA, LINEA) %>%
  summarise(
    Viajes = sum(Viajes),
    .groups = 'drop'
  )

# Save data frame to a CSV file
write.csv(aggr_db, file = "subte_23_24.csv", row.names = FALSE)



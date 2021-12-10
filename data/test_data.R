library(data.table)

new_data <- data.table(
  Vorname = c('Matthias', 'Christoph', 'Fritz', 'Fritz', 'Friz'),
  Name = rep('Mueller',5),
  PLZ = c(3713, 3800, 3800, 3912, 8700),
  Strasse = rep('Länggasse 12', 5)
)

hist_data <- data.table(
  Vorname = c('Albin', 'Ragnar', 'Fritz', 'Iatrino', 'Bubi'),
  Name = rep('Mueller',5),
  PLZ = c(3713, 3800, 3800, 3912, 8700),
  Strasse = rep('Länggasse 12', 5)
)

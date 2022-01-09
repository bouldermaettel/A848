library(RPostgreSQL)
# data <- tibble::tibble(readRDS(file = "data/performance_test.rds"))
pgdrv <- dbDriver(drvName = "PostgreSQL")
  # accessing database from host
# use docker inspect sp-net and copy the IPV4Addreas of the postres-container
con <-DBI::dbConnect(pgdrv,
                    dbname="postgres",
                    host="172.18.0.4", port=5432,
                    user = 'root',
                    password = 'root')

dbListTables(con)

# DBI::dbRemoveTable(con, 'historic_data')
# DBI::dbWriteTable(con, 'historic_data', data)
# DBI::dbWriteTable(con, 'historic_data', data, row.names = FALSE)


# dbListTables(con)

# res <- dbSendQuery(con, "SELECT * FROM historic_data WHERE ...")
res <- dbSendQuery(con, "SELECT * FROM historic_data")
new_data <- tibble::tibble(dbFetch(res))
colnames(new_data)
new_data$row.names[100000:100200]

# Clear the result
# dbClearResult(res)
# # Disconnect from the database
# dbDisconnect(con)

data2 <- data[1:100,]
data2[['row.names']] <- NULL

# DBI::dbWriteTable(con, 'historic_data', data2,append=TRUE)
# dbDisconnect(data$con)

# library(magrittr)
# library(randomNames)
# df <- readRDS('./data/data_test.rds')
df <- df  %>% mutate_at(vars('ID_SMC', 'Nr', 'PLZ', 'n'),  as.integer)
df <- df %>% mutate_at(vars('Datum_Eingang', 'Datum_Brief', 'Frist', 'Datum_Vernichtung', 'Stellungnahme'),  as.Date, format = "%d/%m/%Y")  %>% mutate_at(vars('Nr', 'PLZ'),  as.integer)
head(df)
saveRDS(df, './data/data_test.rds')

# for (i in 1:10000) {
#   if (i == 1){
#     df_new <- df
#   } else {
#     df_new <-  dplyr::bind_rows(df, df_new)
#   }
# }
# head(df_new)
# df_new %>% dim()
df$ID_SMC %>% unique() %>% length()

dates <- (rep(as.Date("2020-01-07"),100000) - sample(1:100010, 100000, replace=FALSE))

df$Datum_Brief <- dates
df$Frist <- dates + 90
df$Datum_Vernichtung <- dates + 100
df$Zollfall_Nr <- rep(c(rep(NA,99), paste0('VK_', sample(10000:99000, 1, replace = FALSE))),1000)

df$Stellungnahme <- dates + 200


# df_new[['Vorname']] <- randomNames::randomNames(100000, ethnicity=5, which.names='first')
# df_new[['Name']] <- randomNames::randomNames(100000, which.names='last')
# df_new[['Strasse']] <- paste(randomNames::randomNames(100000, which.names='last'), sample(1:50, replace = T))
# df[['ID_SMC']] <- c(rep(18168001,100000) - sample(1:100010, 100000, replace=FALSE))
#
# saveRDS(df_new, './data/data_test.rds')
#
# mock <- readxl::read_excel('./data/mock_data.xlsx')
# mock[['Vorname']] <- randomNames::randomNames(20, which.names='first')
# mock[['Name']] <- randomNames::randomNames(20, which.names='last')
# mock[['Strasse']] <- paste(randomNames::randomNames(20, which.names='last'), sample.int(1:50, replace = T))
#
# wb <- openxlsx::createWorkbook()
# openxlsx::addWorksheet(wb, sheetName = 'Sendungen')
# openxlsx::writeData(wb, sheet = 'Sendungen', x = mock, startCol = 1, startRow = 1)
# openxlsx::saveWorkbook(wb,'./data/mock_data_2.xlsx')
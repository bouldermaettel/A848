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

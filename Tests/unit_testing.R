##################################################################################
# Testing of Data Wrangling Functions
# Matthias Mueller: matthias.mueller@swissmedic.ch
# Project: A848
##################################################################################
rm(list = ls())
packagesToLoad <- c('testthat', 'data.table', 'xlsx')
# do the loading and print wether the package is installed
sapply(packagesToLoad, function(x) {require(x,character.only=TRUE)} )


# source data wrangling functions
source('./src/data/data_wrangler.R')
source('./data/test_data.R')

path_xlsx <- './data/Vereinfachtes_Verfahren_ab_2019.xlsx'
path_csv <- './data/Vereinfachtes_Verfahren_ab_2019.csv'
sheet_name <- 'Sendungen'

test_that('xlsx_loader', {
           expect_equal(nrow(get_data(path = path_xlsx, sheet = sheet_name)),
                        12389)
})

test_that('csv_loader', {
           expect_equal(nrow(get_data(path = path_csv)),
                        12389)
})

test_that('get_dups', {
           expect_equal(nrow(get_dups(first_df = new_data, list('Name', 'Vorname'))),
                        2)
})

test_that('get_uniques_one_df', {
           expect_equal(nrow(get_uniques(first_df = new_data, second_df = NULL, list('Name', 'Vorname'))),
                        4)
})

test_that('get_uniques_two_dfs', {
           expect_equal(nrow(get_uniques(first_df = new_data, second_df = hist_data, list('Name', 'Vorname'))),
                        3)
})


test_that('get_fuzzy_duplicated_records_one_df', {
           expect_equal(nrow(get_fuzzy_duplicated_records(first_df = new_data ,
                                                          group_vars = c('Name', 'Vorname'),
                                                          max_distance = 0.1)), 3)
})

test_that('get_fuzzy_duplicated_records_two_dfs', {
           expect_equal(nrow(get_fuzzy_duplicated_records(first_df = new_data ,
                                                          second_df = hist_data,
                                                          group_vars = c('Name', 'Vorname'),
                                                          max_distance = 0.1)), 4)
})

test_that('get_duplicate_records', {
           expect_equal(nrow(get_duplicate_records(first_df = new_data ,
                                                   second_df = NULL,
                                                   group_vars = c('Name', 'Vorname'))), 2)
})

df <- get_data(path = path_xlsx, sheet = sheet_name)
write.xlsx2(df, './data/Kopie_Vereinfachtes_Verfahren_ab_2019.csv', sheetName = "Sendungen",
  col.names = F, row.names = F, append = T)



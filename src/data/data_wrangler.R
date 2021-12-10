##################################################################################
# Data Wrangling Functions
# Matthias Mueller: matthias.mueller@swissmedic.ch
# Project: A848
##################################################################################
# empty the workspace
rm(list = ls())
packagesToLoad <- c('tidyverse', 'data.table')
# do the loading and print wether the package is installed
sapply(packagesToLoad, function(x) {require(x,character.only=TRUE)} )

# def dataloader
get_data <- function(path, sheet=NULL) {
  if (!is.null(sheet)) {
  return(readxl::read_excel(path=path,sheet=sheet) %>% data.table() %>% suppressWarnings())
  } else {
    return(data.table::fread(path))
  }
}

get_dups <- function(first_df, ...){
  group_vars<- rlang::syms(...)
  first_df %>% group_by(!!!group_vars) %>% mutate(n = n() ) %>% filter(n > 1)
}

get_uniques <- function(first_df, second_df,...) {
  group_vars <- rlang::syms(...)
  if (is.null(second_df)) {
    return(first_df %>% distinct(!!!group_vars, .keep_all = TRUE))
  } else {
    unique_first_df <- first_df %>% distinct(!!!group_vars, .keep_all = TRUE)
    return(anti_join(unique_first_df, second_df, by= c('Name', 'Vorname')))
  }
}


# speed optimized
get_fuzzy_duplicate_records <- function(first_df, second_df=NULL, group_vars, max_distance=0.0, ignore_case=TRUE) {
  # start_time <- Sys.time()
  if (is.null(second_df)) {
    i <- 0
    for (name in group_vars) {
      i <- i +1
      if (i == 1) {
      conc_strings_first <- first_df[[name]]
    } else {
        conc_strings_first <- paste(conc_strings_first, first_df[[name]])
      }
    }
    matching_matrix <- sapply(conc_strings_first, agrepl, conc_strings_first, max.distance = max_distance, ignore.case = ignore_case)
    matching_vector <- apply(matching_matrix,1, sum )
    return(first_df[matching_vector > 1,])
  } else {
    i <- 0
    for (name in group_vars) {
      i <- i +1
      if (i == 1) {
      conc_strings_first <- first_df[[name]]
      conc_strings_second <- second_df[[name]]
    } else {
        conc_strings_first <- paste(conc_strings_first, first_df[[name]])
        conc_strings_second <- paste(conc_strings_second, second_df[[name]])
      }
    }
    # get dupls from frist dataset
    matching_matrix1 <- sapply(conc_strings_first, agrepl, conc_strings_first, max.distance = max_distance, ignore.case = ignore_case)
    matching_vector1 <- apply(matching_matrix1,1, sum )

    matching_matrix2 <- sapply(conc_strings_second, agrepl, conc_strings_first, max.distance = max_distance, ignore.case = ignore_case)
    matching_vector2 <- apply(matching_matrix2,1, sum )
    dupl_first_df_matched <- first_df[(matching_vector1 + matching_vector2) > 1,]  # both, from first and first+second df
    conc_strings_first_matched <- as.data.frame(conc_strings_first)[matching_vector2 > 0,]

    matching_matrix3 <- sapply(conc_strings_first_matched, agrepl, conc_strings_second, max.distance = max_distance, ignore.case = ignore_case)
    matching_vector3 <- apply(matching_matrix3,1, sum )
    dupl_second_df_matched <- second_df[matching_vector3 > 0,]
    # end_time <- Sys.time()
    # print(end_time - start_time)
    return(bind_rows(dupl_first_df_matched, dupl_second_df_matched))
  }
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# group_vars = c('Name', 'Vorname')
# second_df <- get_data(path = path_xlsx <- './data/Vereinfachtes_Verfahren_ab_2019.xlsx', sheet = 'Sendungen')
# first_df <- get_data(path = path_xlsx <- './data/mock_data.xlsx', sheet = 'Sendungen')
#
# test1 <- get_fuzzy_duplicate_records2(first_df = first_df, second_df = second_df, group_vars = c('Name', 'Vorname'), max_distance = 0.05)
# test2 <- get_fuzzy_duplicate_records(first_df = first_df, second_df = second_df, group_vars = c('Name', 'Vorname'), max_distance = 0.05)
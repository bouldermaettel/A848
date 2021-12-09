names_list = c("mathias Mueller", "Matthias Müller", "Minder Ulrich")

df = data.frame(names = c('Matthias Mueller', 'Fritz Minder'))

agrepl("Zumbach Fritz", 'Fritz Zumbach', max.distance = 0.1, costs = NULL,
      ignore.case = FALSE, fixed = TRUE, useBytes = FALSE)

sapply(df$names, agrepl, names_list)



agrep("lasy", "1 lazy 2")
agrep("lasy", c(" 1 lazy 2", "1 lasy 2"), max.distance = list(sub = 0))
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max.distance = 2)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max.distance = 2, value = TRUE)


get_duplicated_records <- function(first_df, second_df=NULL, column_names=NA) {
  if (is.null(second_df)) {
    return(group_by(data,Name,Vorname) %>% mutate(n = n()) %>% filter(n > 1))

    return(first_df[first_df %>% duplicated(by=c('Name', 'Vorname')) | duplicated(by=c('Name', 'Vorname'), fromLast = TRUE),])
  } else {
    duplicates_first_df <- first_df[first_df %>% duplicated(by=c('Name', 'Vorname')),]
    duplicates_first_to_second <- inner_join(first_df, second_df[,c('Name', 'Vorname')],
                                             by= c('Name', 'Vorname'), keep = FALSE)
    return(rbind(duplicates_first_df,duplicates_first_to_second))
  }
}


df <- first_df %>% group_by(Name,Vorname, n= n()) %>% mutate(n = n()) %>%filter(n > 1)

first_df[, fD := .N > 1, by = key(myDT)]
df <- new_data

setkeyv(df, c('Name', 'Vorname'))
df[, dup := .N > 1, by = key(df)]

df[99:100,1:5]

setkeyv(myDT, c('fB', 'fC'))
myDT[, fD:=duplicated(myDT)]

    setkeyv(df, c('Name', 'Vorname'))
    df[, dup := .N > 1, by = key(df)]

df[df$dup == TRUE]

keep duplicates from first and search

new_data[,c('Name', 'Vorname')] == df[,c('Name', 'Vorname')]
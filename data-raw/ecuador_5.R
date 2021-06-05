## code to prepare `DATASET` dataset goes here

file. <- "ecuador_5.csv"
loc. <- here::here("data-raw")
file.full <- here::here(loc., file.)
#exists(file.)
ecuador_5 <- read.csv(file = file.full)


ecuador_5$Location <- gsub("LLAV","SHRUB",ecuador_5$Location)
ecuador_5$Location <- gsub("MAIN","MIXED",ecuador_5$Location)
ecuador_5$Location <- gsub("MASE","NATIVE",ecuador_5$Location)



usethis::use_data(ecuador_5, overwrite = TRUE)



# make_dateset_helpfile(dataset = ecuador_5,
#                       dataset_name = "ecuador_5")
#

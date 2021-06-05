## code to prepare `DATASET` dataset goes here

file. <- "ecuador_working.csv"
loc. <- here::here("data-raw")
file.full <- here::here(loc., file.)
#exists(file.)
ecuador_working <- read.csv(file = file.full)


usethis::use_data(ecuador_working, overwrite = TRUE)

#usethis::use_r("martin1995")

#
# make_dateset_helpfile(dataset = ecuador_working,
#                       dataset_name = "ecuador_working")


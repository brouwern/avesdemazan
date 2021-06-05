## code to prepare `DATASET` dataset goes here

file. <- "Ecuador_Species_List.csv"
loc. <- here::here("data-raw")
file.full <- here::here(loc., file.)
#exists(file.)
Ecuador_Species_List <- read.csv(file = file.full)


usethis::use_data(Ecuador_Species_List, overwrite = TRUE)

#usethis::use_r("martin1995")


# make_dateset_helpfile(dataset = Ecuador_Species_List,
#                       dataset_name = "Ecuador_Species_List")


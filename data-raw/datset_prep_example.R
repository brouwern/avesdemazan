## code to prepare `DATASET` dataset goes here

file. <- "random_scores_191.csv"
loc. <- here::here("data-raw")
file.full <- here::here(loc., file.)
#exists(file.)
random_scores_191 <- read.csv(file = file.full)


usethis::use_data(random_scores_191, overwrite = TRUE)

#usethis::use_r("martin1995")


# make_dateset_helpfile(dataset = random_scores_191,
#                       dataset_name = "random_scores_191")


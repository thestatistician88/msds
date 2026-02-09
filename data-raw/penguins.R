## code to prepare `penguins` dataset goes here
library(palmerpenguins)
penguins<-data.frame(penguins)

usethis::use_data(penguins, overwrite = TRUE)

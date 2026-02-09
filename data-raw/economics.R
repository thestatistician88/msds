## code to prepare `economics` dataset goes here

library(datasetsICR)
data(Economics)
economics<-Economics

usethis::use_data(economics, overwrite = TRUE)


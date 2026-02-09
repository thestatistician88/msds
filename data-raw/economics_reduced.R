## code to prepare `economics_reduced` dataset goes here
library(datasetsICR)
data(Economics)
economics_reduced<-Economics[which(Economics$University_Type=="Public"),1:6]


usethis::use_data(economics_reduced, overwrite = TRUE)

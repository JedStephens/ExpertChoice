## The code to prepare `fractional_factorial_3261_36` dataset is here
library(DoE.MIParray)
fractional_factorial_3261_36 <- gurobi_MIParray(36, nlevels, resolution = 3)
usethis::use_data(fractional_factorial_3261_36, overwrite = TRUE)

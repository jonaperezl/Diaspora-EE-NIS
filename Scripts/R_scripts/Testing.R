print("This file was created within RStudio")
print("And now it lives on GitHub")

library(readr)
varlist_info <- read_csv("varlist_info.csv")
View(varlist_info)

library(haven)
gem_gii_migr_15 <- read_dta("gem_gii_migr_15.dta")
gem_gii_remit_13_16 <- read_dta("gem_gii_remit_13_16.dta")
View(gem_gii_migr_15)
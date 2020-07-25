# Clear plots
if(!is.null(dev.list())) dev.off()
# Clean environment
rm(list=ls())
# Spanish Characters
Sys.setlocale("LC_ALL", "en_US.UTF-8")
# Erase all except var x in th environment
rm(list=setdiff(ls(), c('change')))
# Erase specific vars in environment acording to pattern
rm(list=ls(pattern = 'logit_+'))
rm(list=setdiff(ls(), c(ls(c('datset3', con)))))

#file.choose()  # Abrir Finder para tener path de un archivo


# To use Stata from R

# library(RStata)
# options("RStata.StataPath" = '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')
# options("RStata.StataVersion" = 14)
# stata('set more off, permanently')

 # Clear console
 cat("\014") 


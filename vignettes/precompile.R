# Precompiled vignettes that take a long time to run

library(knitr)
## Need to set working directory to get the figures's paths right
setwd("vignettes")
knit("BLM.Rmd.orig", "BLM.Rmd")
knit("BLM.Rmd.orig", "BLM.md")
knit("COVID19.Rmd.orig", "COVID19.Rmd")
knit("COVID19.Rmd.orig", "COVID19.md")
knit("More_Republicans_Less_Crime.Rmd.orig", "More_Republicans_Less_Crime.Rmd")
knit("More_Republicans_Less_Crime.Rmd.orig", "More_Republicans_Less_Crime.md")
knit("Indian_Reservations_and_Homicides.Rmd.orig",
     "Indian_Reservations_and_Homicides.Rmd")
knit("Indian_Reservations_and_Homicides.Rmd.orig",
     "Indian_Reservations_and_Homicides.md")
# Creating the map takes a long time
knit("Homicide_Map.Rmd.orig", "Homicide_Map.Rmd")
knit("Homicide_Map.Rmd.orig", "Homicide_Map.md")
setwd("..")



# Install the packages
pkg <- c("tidyverse", "ggimage", "shiny", "extrafont")
setRepositories(ind = c(1, 2)) # Bioconductor for ggimage
lapply(pkg,
       function (x) {
         if (!require(x, character.only = TRUE)) {
           install.packages(x)
         }
       })

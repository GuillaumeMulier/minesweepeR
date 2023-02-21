

# Install the packages
pkg <- c("tidyverse", "ggimage", "shiny", "sysfonts", "shinyalert", "devtools", "ggtextures")
setRepositories(ind = c(1, 2)) # Bioconductor for ggimage
lapply(pkg,
       function (x) {
         if (!require(x, character.only = TRUE)) {
           if (x != "ggtextures") {
             install.packages(x)
           } else {
             devtools::install_github("clauswilke/ggtextures")
           }
         }
       })

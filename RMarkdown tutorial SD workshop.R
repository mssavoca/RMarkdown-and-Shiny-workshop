######################################
# R Markdown and Shiny workshop, day 1
######################################

from_cran <- c("shiny", "rmarkdown", 
               "DT", "devtools", "flexdashboard", "gapminder",
               "rticles", "shinydashboard", "shinythemes", 
               "tidyverse", "tufte", "xaringan")

install.packages(from_cran, repos = "http://cran.rstudio.com")

# Load

library(shiny)
library(rmarkdown)
library(DT)
library(devtools)
library(flexdashboard)
library(gapminder)
library(rticles)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(tufte)
library(xaringan)
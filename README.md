#Load Required packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotrix)
# Install git2r package
install.packages("git2r")

# Load git2r package
library(git2r)

# Clone repository
repo <- clone("https://github.com/Val-Atta/PPROJECT_GOOGLE.git", "local/path/to/repo")


### 
## Import data
##


#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

install.packages("dplyr")
library(dplyr)

# set working directory
setwd("~/Desktop/ASDS/StatsI_Fall2022/tutorials/tutorial06")

# Read in your dataset 

read.csv("movies.csv")


# target variable = best_pic_win



dat %>%
  filter(grepl("Warner|WARNER", studio)) %>%
  summarise(c_score = mean(critics_score))

# We can see that none of the Warner films have won best picture
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_win == "yes") %>%
  select(title)

# But they did get two nominations...
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_nom == "yes") %>%
  select(title, thtr_rel_year)

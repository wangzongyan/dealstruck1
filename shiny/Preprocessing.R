# Preprocessing
# Upload packages for data clean and analysis
require(plyr)  #data clean
require(dplyr) #data clean
require(tidyr) #data clean
require(reshape2) # data clean
require(ggplot2)  # visualzation
require(XML)   # web scriping
require(testthat) #test model
require(kernlab) # kernel and SVM
require(datasets) # State data
require(caret) # Cross - Validation k fold
require(RCurl) # load the website link
require(maps) # heatmap: map_data
require(ggmap) # for heatmap
require(gridExtra)
require(shiny)

# Read Data and function files
LC <- read.csv("LC_biz_all.csv", header = T) #Please read LC_biz_all.csv
source("clean_f2.R") # functions for cleaning the data
source("plot_f2.R") # functions for plot(heatmap)
source("analysis_f2.R") # functions for plot and data analysis

# Data clean
lc <- perToN(LC) # transfer percentage to numeric
lc <- replaceBlank_all(lc) # replace the blank
# Transform earliest_cr_line, issue_d, last_pymnt_d from str to numeric
lc <- lc %>% dateToNum(., "earliest_cr_line") %>%
  dateToNum(., "issue_d") %>% dateToNum(., "last_pymnt_d")
lc <- lc %>% select(-id, -emp_title,-zip_code)

# ANOVA
lc1 <- lc[, !(colnames(lc) %in% c(names(n.factor_all(lc))))]
lc1$loan_status <- lc$loan_status
lc1 <- lc1 %>% subset(!loan_status == "Current")
anova.p <- data.frame(var = names(anova(lc1)), p.value = anova(lc1))


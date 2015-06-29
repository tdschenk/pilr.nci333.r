library(foreign)
library(sas7bdat)

test <- read.spss("C:/Users/Tyler/Documents/NCI333/SPSS/SPSS/Story County_online survey.sav",
                  to.data.frame = TRUE)
test2 <- read.spss("C:/Users/Tyler/Documents/NCI333/SPSS/SPSS/Story_County_health_needs_assessment2010_weighted.sav",
                  to.data.frame = TRUE)

test3 <- read.sas7bdat("C:/Users/Tyler/Documents/NCI333/Data/nationaldataset2014.sas7bdat")
test4 <- read.csv("C:/Users/Tyler/Documents/NCI333/Data/2014 CHR analytic data.csv")

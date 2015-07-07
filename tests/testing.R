library(foreign)
library(sas7bdat)

test <- read.spss("C:/Users/Tyler/Documents/NCI333/SPSS/SPSS/Story County_online survey.sav",
                  to.data.frame = TRUE)
test2 <- read.spss("C:/Users/Tyler/Documents/NCI333/Data/Story_County_health_needs_assessment2010_weighted.sav",
                   to.data.frame = TRUE)

test3 <- read.sas7bdat("C:/Users/Tyler/Documents/NCI333/Data/nationaldataset2014.sas7bdat")
test4 <- read.csv("C:/Users/Tyler/Documents/NCI333/Data/2014 CHR analytic data.csv")
b64 <- base64encode("C:/Users/Tyler/Documents/NCI333/Data/2014 CHR analytic data.csv")
b64_decoded_raw <- rawToChar(base64decode(b64))
df <- read.csv(textConnection(b64_decoded_raw))

## Convert SPSS to CSV
data <- read.spss("C:/Users/Tyler/Documents/NCI333/Data/Story_County_health_needs_assessment2010_weighted.sav",
                  to.data.frame = TRUE)
write.csv(df, "test.csv")

data2 <- read.csv("C:/Users/Tyler/Documents/NCI333/Data/Story_County_health_needs_assessment2010_weighted.csv")

## testing VGI cleanup
library(pilr.api.r)
options(pilr_default_access_code = "bd7de7fb-a1a8-4c91-860d-8cca575a7434")
options(pilr_server_default = "http://nci333.pilrhealth.com")
options(pilr_project_default = "mycommunity-333")
data <- list(vgi = read_pilr(data_set = "pilrhealth:mobile:survey_data", schema = "1", 
                             query_params = list(participant = "135")))

options(pilr_default_access_code = "5b3a37a2-069c-4bf7-b33c-3ccdee97b056")
options(pilr_server_default = "http://beta.pilrhealth.com")
options(pilr_project_default = "geneactiv_testing")
data <- list(vgi = read_pilr(data_set = "pilrhealth:mobile:survey_data", schema = "1", 
                             query_params = list(participant = "135")))
params <- ""
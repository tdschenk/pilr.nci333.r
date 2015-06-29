library(foreign)

test <- read.spss("C:/Users/Tyler/Documents/NCI333/SPSS/SPSS/Story County_online survey.sav",
                  to.data.frame = TRUE)
test2 <- read.spss("C:/Users/Tyler/Documents/NCI333/SPSS/SPSS/Story_County_health_needs_assessment2010_weighted.sav",
                  to.data.frame = TRUE)


sas_to_pilr <- function(data, params, ...) {
  b64_decoded_raw <- rawToChar(base64decode(params$files$FILE_MAPPING))
  #df <- 
}

csv_to_pilr <- function(data, params, ...) {
  b64_decoded_raw <- rawToChar(base64decode(params$files$FILE_MAPPING))
  df <- read.csv(textConnection(b64_decoded_raw))
}
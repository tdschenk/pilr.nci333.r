#' @export
spss_to_pilr_chna <- function(data, params, ...) {
  # Read in SPSS file
  b64_decoded_raw <- rawToChar(base64decode(params$files$spss_chna_file))
  df <- read.spss(textConnection(b64_decoded_raw), to.data.frame = TRUE)
  
  # Rename and select vars to return
  df <- select(df, c(caseid_1 = CaseID_1, city_rec = city_recode,
                     sc_6 = SC_6, sc_127 = SC_127, sc_129 = SC_129,
                     sc_130 = SC_130, sc_156 = SC_156, sc_176 = SC_176,
                     sc_177 = SC_177, sc_178 = SC_178, sc_184 = SC_184,
                     sc_185 = SC_185, sc_186 = SC_186, bmi_stat = bmi_status,
                     weight = Weight, age_reco = age_recode, sc15reco = SC15recode,
                     sc16reco = SC16recode, mpamin = MPAmin, vpamin = VPAmin,
                     mvpamin = MVPAmin, mpa = MPA, vpa = VPA, mvpa = MVPA,
                     `filter_$` = filter_.))
  
  # Construct return list
  datasets <- list(spss_chna = df)
  files <- list()
  ret <- list(datasets = datasets, files = files)
  ret
}

#' @export
csv_to_pilr_chna <- function(data, params, ...) {
  # Read in SPSS file
  b64_decoded_raw <- rawToChar(base64decode(params$files$csv_chna_file))
  df <- read.csv(textConnection(b64_decoded_raw))
  
  # Rename and select vars to return
  df <- select(df, c(caseid_1 = CaseID_1, city_rec = city_recode,
                     sc_6 = SC_6, sc_127 = SC_127, sc_129 = SC_129,
                     sc_130 = SC_130, sc_156 = SC_156, sc_176 = SC_176,
                     sc_177 = SC_177, sc_178 = SC_178, sc_184 = SC_184,
                     sc_185 = SC_185, sc_186 = SC_186, bmi_stat = bmi_status,
                     weight = Weight, age_reco = age_recode, sc15reco = SC15recode,
                     sc16reco = SC16recode, mpamin = MPAmin, vpamin = VPAmin,
                     mvpamin = MVPAmin, mpa = MPA, vpa = VPA, mvpa = MVPA,
                     `filter_$` = filter_.))
  
  # Add metadata
  id <- character(nrow(df))
  for (i in 1:nrow(df)) {
    id[i] <- UUIDgenerate()
    i <- i + 1
  }
  df$timestamp <- toString(Sys.Date())
  df$id <- id
  df$pt <- params$params$participant
  
  # Construct return list
  datasets <- list(community_health_needs_assessment_dataset = df)
  files <- list()
  ret <- list(datasets = datasets, files = files)
  ret
}

#' @export
sas_to_pilr_chr <- function(data, params, ...) {
  # Read in SPSS file
  b64_decoded_raw <- rawToChar(base64decode(params$files$sas_chr_file))
  df <- read.sas7bdat(textConnection(b64_decoded_raw), to.data.frame = TRUE)
  
  # Rename and select vars to return
  df <- select(df, c(statecode = statecode, countrycode = countycode, state = state,
                     county = county, measnure_70_value = measure_70_value,
                     measure_132_value = measure_132_value, measure_51_value = measure_51_value,
                     measure_52_value = measure_52_value, measure_53_value = measure_53_value,
                     measure_54_value = measure_54_value, measure_55_value = measure_55_value,
                     measure_81_value = measure_81_value))
  
  # Construct return list
  datasets <- list(sas_chr = df)
  files <- list()
  ret <- list(datasets = datasets, files = files)
  ret
}


#' @export
csv_to_pilr_chr <- function(data, params, ...) {
  # Read in CSV file
  b64_decoded_raw <- rawToChar(base64decode(params$files$csv_chr_file))
  df <- read.csv(textConnection(b64_decoded_raw))
  
  # Rename and select vars to return
  df <- df[2:nrow(df),]
  df <- select(df, c(statecode = statecode, countycode = countycode, state = state,
                     county = county, measure_70_value = measure_70_value,
                     measure_132_value = measure_132_value, measure_51_value = measure_51_value,
                     measure_52_value = measure_52_value, measure_53_value = measure_53_value,
                     measure_54_value = measure_54_value, measure_55_value = measure_55_value,
                     measure_81_value = measure_81_value))
  
  # Add metadata
  id <- character(nrow(df))
  for (i in 1:nrow(df)) {
    id[i] <- UUIDgenerate()
    i <- i + 1
  }
  df$timestamp <- toString(Sys.Date())
  df$id <- id
  df$pt <- params$params$participant
  
  # Construct return list
  datasets <- list(county_health_rankings_dataset = df)
  files <- list()
  ret <- list(datasets = datasets, files = files)
  ret
}


#' @export
csv_to_pilr_sccc <- function(data, params, ...) {
  # Read in CSV file
  b64_decoded_raw <- rawToChar(base64decode(params$files$csv_sccc_file))
  df <- read.csv(textConnection(b64_decoded_raw))
  
  # Rename and select vars to return
  df <- df[2:nrow(df),]
  df <- select(df, c(Q10_bike = Q10_bike, Q10_recr = Q10_recr, 
                     Q10_parks = Q10_parks, zip = zip, zip_recode = zip_recode))
  
  # Add metadata
  id <- character(nrow(df))
  for (i in 1:nrow(df)) {
    id[i] <- UUIDgenerate()
    i <- i + 1
  }
  df$timestamp <- toString(Sys.Date())
  df$id <- id
  df$pt <- params$params$participant
  
  # Construct return list
  datasets <- list(sccc_dataset = df)
  files <- list()
  ret <- list(datasets = datasets, files = files)
  ret
}
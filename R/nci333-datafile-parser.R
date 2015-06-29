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
#' @export
vgi_convert <- function(data, params, ...) {
  data <- data$vgi
  
  # Remove old and test surveys
  question_codes <- c("00100", "00101", "00110", "00120", "00130", "00140",
                      "00400", "00401", "00410", "00420", "00430", "00432",
                      "00433", "00434", "00435", "00440", "00450", "00500",
                      "00501", "00510", "00520", "00530", "00541", "00542",
                      "00543", "00544", "00550", "00560", "00300", "0301",
                      "00310", "00320", "00330", "00340", "00350", "00351",
                      "00352", "00353", "00354", "00355", "00360", "00370",
                      "00380", "00390", "00399", "00200", "00201", "00210",
                      "00220", "00230", "00240", "00250", "00251", "00252",
                      "00253", "00254", "00255", "00260", "00270", "")
  data <- subset(data, question_code %in% question_codes)
  evaluations <- unique(data$session)
  
  # Initialize large data frame to return
  ret <- data.frame(survey_code = character(length(evaluations)),
                    location_response_value = character(length(evaluations)), location_response = character(length(evaluations)),
                    type_response_value = character(length(evaluations)), type_response = character(length(evaluations)),
                    condition_response_value = character(length(evaluations)), condition_response = character(length(evaluations)),
                    along_road_response_value = character(length(evaluations)), along_road_response = character(length(evaluations)),
                    from_road_response_value = character(length(evaluations)), from_road_response = character(length(evaluations)),
                    buffer_response_value = character(length(evaluations)), buffer_response = character(length(evaluations)),
                    fence_response_value = character(length(evaluations)), fence_response = character(length(evaluations)),
                    tree_response_value = character(length(evaluations)), tree_response = character(length(evaluations)),
                    hedge_response_value = character(length(evaluations)), hedge_response = character(length(evaluations)),
                    landscaping_response_value = character(length(evaluations)), landscaping_response = character(length(evaluations)),
                    grass_response_value = character(length(evaluations)), grass_response = character(length(evaluations)),
                    width_response_value = character(length(evaluations)), width_response = character(length(evaluations)),
                    building_near_response_value = character(length(evaluations)), building_near_response = character(length(evaluations)),
                    from_buildings_response_value = character(length(evaluations)), from_buildings_response = character(length(evaluations)),
                    markings_response_value = character(length(evaluations)), markings_response = character(length(evaluations)),
                    signs_response_value = character(length(evaluations)), signs_response = character(length(evaluations)),
                    lane_response_value = character(length(evaluations)), lane_response = character(length(evaluations)),
                    parking_response_value = character(length(evaluations)), parking_response = character(length(evaluations)),
                    crossing_response_value = character(length(evaluations)), crossing_response = character(length(evaluations)),
                    attractive_response_value = character(length(evaluations)), attractive_response = character(length(evaluations)),
                    safe_response_value = character(length(evaluations)), safe_response = character(length(evaluations)), 
                    id = character(length(evaluations)), stringsAsFactors=FALSE)
  if (length(evaluations) == 0) stop("No surveys found for this participant")
  # Iterate through each evaluation
  for (i in 1:length(evaluations)) {
    
    # Subset evaluation and grab survey code
    eval_sub <- subset(data, session == evaluations[i])
    ret$survey_code[i] <- eval_sub$survey_code[1]
    if (eval_sub$response_value[1] == "A Ross Rd & Scott Ave") {
      ret$location_response[i] <- "Lon: 42.032570 Lat: -93.660615"
    }
    else if (eval_sub$response_value[1] == "B McCarthy Lee Park Access") {
      ret$location_response[i] <- "Lon: 42.028174 Lat: -93.658286"
    }
    else if (eval_sub$response_value[1] == "C Pammel Woods Access") {
      ret$location_response[i] <- "Lon: 42.032599 Lat: -93.655532"
    }
    else if (eval_sub$response_value[1] == "D West St & Hyland Ave") {
      ret$location_response[i] <- "Lon: 42.025364 Lat: -93.655628"
    }
    else if (eval_sub$response_value[1] == "E Lincoln Way & Hyland Ave") {
      ret$location_response[i] <- "Lon: 42.022699 Lat: -93.655634"
    }
    else if (eval_sub$response_value[1] == "F Lake Lavern Path") {
      ret$location_response[i] <- "Lon: 42.023266 Lat: -93.649473"
    }
    else if (eval_sub$response_value[1] == "G Moore Park Access") {
      ret$location_response[i] <- "Lon: 42.012279 Lat: -93.642943"
    }
    else if (eval_sub$response_value[1] == "H University Blvd & Mortensen Pkwy") {
      ret$location_response[i] <- "Lon: 42.008474 Lat: -93.639382"
    }
    else if (eval_sub$response_value[1] == "I Hans Peter Christofferson Park") {
      ret$location_response[i] <- "Lon: 41.996946 Lat: -93.644170"
    }
    else if (eval_sub$response_value[1] == "J North Loop Drive") {
      ret$location_response[i] <- "Lon: 42.001154 Lat: -93.633760"
    }
    
    # Footpath survey
    if (eval_sub$survey_code[1] == "footpath") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location_response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00100" || eval_sub$question_code[j] == "00101"){
          ret$location_response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00110"){
          ret$type_response_value[i] <- eval_sub$response_value[j]
          ret$type_response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00120"){
          ret$condition_response_value[i] <- eval_sub$response_value[j]
          ret$condition_response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00130"){
          ret$attractive_response_value[i] <- eval_sub$response_value[j]
          ret$attractive_response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00140"){
          ret$safe_response_value[i] <- eval_sub$response_value[j]
          ret$safe_response[i] <- eval_sub$response[j]
        }
      }
    }
    
    # Street survey
    if (eval_sub$survey_code[1] == "street") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location_response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00400" || eval_sub$question_code[j] == "00401"){
          ret$location_response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00410"){
          ret$type_response_value[i] <- eval_sub$response_value[j]
          ret$type_response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00420"){
          ret$condition_response_value[i] <- eval_sub$response_value[j]
          ret$condition_response[i] <- eval_sub$response[j]
        }
        # Markings
        else if (eval_sub$question_code[j] == "00430"){
          ret$markings_response_value[i] <- eval_sub$response_value[j]
          ret$markings_response[i] <- eval_sub$response[j]
        }
        # Signs
        else if (eval_sub$question_code[j] == "00432"){
          ret$signs_response_value[i] <- eval_sub$response_value[j]
          ret$signs_response[i] <- eval_sub$response[j]
        }
        # Lane
        else if (eval_sub$question_code[j] == "00433"){
          ret$lane_response_value[i] <- eval_sub$response_value[j]
          ret$lane_response[i] <- eval_sub$response[j]
        }
        # Parking
        else if (eval_sub$question_code[j] == "00434"){
          ret$parking_response_value[i] <- eval_sub$response_value[j]
          ret$parking_response[i] <- eval_sub$response[j]
        }
        # Crossing
        else if (eval_sub$question_code[j] == "00435"){
          ret$crossing_response_value[i] <- eval_sub$response_value[j]
          ret$crossing_response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00440"){
          ret$attractive_response_value[i] <- eval_sub$response_value[j]
          ret$attractive_response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00450"){
          ret$safe_response_value[i] <- eval_sub$response_value[j]
          ret$safe_response[i] <- eval_sub$response[j]
        }
      }
    }
    
    # Highway survey
    if (eval_sub$survey_code[1] == "highway") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location_response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00500" || eval_sub$question_code[j] == "00501"){
          ret$location_response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00510"){
          ret$type_response_value[i] <- eval_sub$response_value[j]
          ret$type_response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00520"){
          ret$condition_response_value[i] <- eval_sub$response_value[j]
          ret$condition_response[i] <- eval_sub$response[j]
        }
        # Markings
        else if (eval_sub$question_code[j] == "00530"){
          ret$markings_response_value[i] <- eval_sub$response_value[j]
          ret$markings_response[i] <- eval_sub$response[j]
        }
        # Signs
        else if (eval_sub$question_code[j] == "00541"){
          ret$signs_response_value[i] <- eval_sub$response_value[j]
          ret$signs_response[i] <- eval_sub$response[j]
        }
        # Lane
        else if (eval_sub$question_code[j] == "00542"){
          ret$lane_response_value[i] <- eval_sub$response_value[j]
          ret$lane_response[i] <- eval_sub$response[j]
        }
        # Parking
        else if (eval_sub$question_code[j] == "00543"){
          ret$parking_response_value[i] <- eval_sub$response_value[j]
          ret$parking_response[i] <- eval_sub$response[j]
        }
        # Crossing
        else if (eval_sub$question_code[j] == "00544"){
          ret$crossing_response_value[i] <- eval_sub$response_value[j]
          ret$crossing_response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00550"){
          ret$attractive_response_value[i] <- eval_sub$response_value[j]
          ret$attractive_response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00560"){
          ret$safe_response_value[i] <- eval_sub$response_value[j]
          ret$safe_response[i] <- eval_sub$response[j]
        }
      }
    }
    
    # Sidewalk survey
    if (eval_sub$survey_code[1] == "sidewalk") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location_response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00300" || eval_sub$question_code[j] == "0301"){
          ret$location_response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00310"){
          ret$type_response_value[i] <- eval_sub$response_value[j]
          ret$type_response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00320"){
          ret$condition_response_value[i] <- eval_sub$response_value[j]
          ret$condition_response[i] <- eval_sub$response[j]
        }
        # Along Road
        else if (eval_sub$question_code[j] == "00330"){
          ret$along_road_response_value[i] <- eval_sub$response_value[j]
          ret$along_road_response[i] <- eval_sub$response[j]
        }
        # From Road
        else if (eval_sub$question_code[j] == "00340"){
          ret$from_road_response_value[i] <- eval_sub$response_value[j]
          ret$from_road_response[i] <- eval_sub$response[j]
        }
        # Buffers
        else if (eval_sub$question_code[j] == "00350"){
          ret$buffer_response_value[i] <- eval_sub$response_value[j]
          ret$buffer_response[i] <- eval_sub$response[j]
        }
        # Fence Buffer
        else if (eval_sub$question_code[j] == "00351"){
          ret$fence_response_value[i] <- eval_sub$response_value[j]
          ret$fence_response[i] <- eval_sub$response[j]
        }
        # Tree Buffer
        else if (eval_sub$question_code[j] == "00352"){
          ret$tree_response_value[i] <- eval_sub$response_value[j]
          ret$tree_response[i] <- eval_sub$response[j]
        }
        # Hedge Buffer
        else if (eval_sub$question_code[j] == "00353"){
          ret$hedge_response_value[i] <- eval_sub$response_value[j]
          ret$hedge_response[i] <- eval_sub$response[j]
        }
        # Landscaping Buffer
        else if (eval_sub$question_code[j] == "00354"){
          ret$landscaping_response_value[i] <- eval_sub$response_value[j]
          ret$landscaping_response[i] <- eval_sub$response[j]
        }
        # Grass Buffer
        else if (eval_sub$question_code[j] == "00355"){
          ret$grass_response_value[i] <- eval_sub$response_value[j]
          ret$grass_response[i] <- eval_sub$response[j]
        }
        # Width
        else if (eval_sub$question_code[j] == "00360"){
          ret$width_response_value[i] <- eval_sub$response_value[j]
          ret$width_response[i] <- eval_sub$response[j]
        }
        # Buildings Near
        else if (eval_sub$question_code[j] == "00370"){
          ret$building_near_response_value[i] <- eval_sub$response_value[j]
          ret$building_near_response[i] <- eval_sub$response[j]
        }
        # From Buildings
        else if (eval_sub$question_code[j] == "00380"){
          ret$from_buildings_response_value[i] <- eval_sub$response_value[j]
          ret$from_buildings_response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00390"){
          ret$attractive_response_value[i] <- eval_sub$response_value[j]
          ret$attractive_response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00399"){
          ret$safe_response_value[i] <- eval_sub$response_value[j]
          ret$safe_response[i] <- eval_sub$response[j]
        }
      }
    }
    
    # Paved Trail survey
    if (eval_sub$survey_code[1] == "paved_trail") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location_response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00200" || eval_sub$question_code[j] == "00201"){
          ret$location_response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00210"){
          ret$type_response_value[i] <- eval_sub$response_value[j]
          ret$type_response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00220"){
          ret$condition_response_value[i] <- eval_sub$response_value[j]
          ret$condition_response[i] <- eval_sub$response[j]
        }
        # Along Road
        else if (eval_sub$question_code[j] == "00230"){
          ret$along_road_response_value[i] <- eval_sub$response_value[j]
          ret$along_road_response[i] <- eval_sub$response[j]
        }
        # From Road
        else if (eval_sub$question_code[j] == "00240"){
          ret$from_road_response_value[i] <- eval_sub$response_value[j]
          ret$from_road_response[i] <- eval_sub$response[j]
        }
        # Buffers
        else if (eval_sub$question_code[j] == "00250"){
          ret$buffer_response_value[i] <- eval_sub$response_value[j]
          ret$buffer_response[i] <- eval_sub$response[j]
        }
        # Fence Buffer
        else if (eval_sub$question_code[j] == "00251"){
          ret$fence_response_value[i] <- eval_sub$response_value[j]
          ret$fence_response[i] <- eval_sub$response[j]
        }
        # Tree Buffer
        else if (eval_sub$question_code[j] == "00252"){
          ret$tree_response_value[i] <- eval_sub$response_value[j]
          ret$tree_response[i] <- eval_sub$response[j]
        }
        # Hedge Buffer
        else if (eval_sub$question_code[j] == "00253"){
          ret$hedge_response_value[i] <- eval_sub$response_value[j]
          ret$hedge_response[i] <- eval_sub$response[j]
        }
        # Landscaping Buffer
        else if (eval_sub$question_code[j] == "00254"){
          ret$landscaping_response_value[i] <- eval_sub$response_value[j]
          ret$landscaping_response[i] <- eval_sub$response[j]
        }
        # Grass Buffer
        else if (eval_sub$question_code[j] == "00255"){
          ret$grass_response_value[i] <- eval_sub$response_value[j]
          ret$grass_response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00260"){
          ret$attractive_response_value[i] <- eval_sub$response_value[j]
          ret$attractive_response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00270"){
          ret$safe_response_value[i] <- eval_sub$response_value[j]
          ret$safe_response[i] <- eval_sub$response[j]
        }
      }
    }
    ret$id[i] <- UUIDgenerate()
  }
  ret$pt <- data$pt[1]
  ret$timestamp <- toString(Sys.Date())
  list(datasets = list(vgi_formatted = ret), files = list())
}
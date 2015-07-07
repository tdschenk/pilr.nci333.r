#' @export
vgi_convert <- function(data, params, ...) {
  data <- data$vgi
  
  # Initialize large data frame to return
  data <- data[data$survey_code != "test_survey",]
  evaluations <- unique(data$session)
  ret <- data.frame(survey_code = character(length(evaluations)),
                    location.response_value = character(length(evaluations)), location.response = character(length(evaluations)),
                    type.response_value = character(length(evaluations)), type.response = character(length(evaluations)),
                    condition.response_value = character(length(evaluations)), condition.response = character(length(evaluations)),
                    along_road.response_value = character(length(evaluations)), along_road.response = character(length(evaluations)),
                    from_road.response_value = character(length(evaluations)), from_road.response = character(length(evaluations)),
                    buffer.response_value = character(length(evaluations)), buffer.response = character(length(evaluations)),
                    fence.response_value = character(length(evaluations)), fence.response = character(length(evaluations)),
                    tree.response_value = character(length(evaluations)), tree.response = character(length(evaluations)),
                    hedge.response_value = character(length(evaluations)), hedge.response = character(length(evaluations)),
                    landscaping.response_value = character(length(evaluations)), landscaping.response = character(length(evaluations)),
                    grass.response_value = character(length(evaluations)), grass.response = character(length(evaluations)),
                    width.response_value = character(length(evaluations)), width.response = character(length(evaluations)),
                    building_near.response_value = character(length(evaluations)), building_near.response = character(length(evaluations)),
                    from_buildings.response_value = character(length(evaluations)), from_buildings.response = character(length(evaluations)),
                    markings.response_value = character(length(evaluations)), markings.response = character(length(evaluations)),
                    signs.response_value = character(length(evaluations)), signs.response = character(length(evaluations)),
                    lane.response_value = character(length(evaluations)), lane.response = character(length(evaluations)),
                    parking.response_value = character(length(evaluations)), parking.response = character(length(evaluations)),
                    crossing.response_value = character(length(evaluations)), crossing.response = character(length(evaluations)),
                    attractive.response_value = character(length(evaluations)), attractive.response = character(length(evaluations)),
                    safe.response_value = character(length(evaluations)), safe.response = character(length(evaluations)), stringsAsFactors=FALSE)
  
  # Iterate through each evaluation
  for (i in 1:length(evaluations)) {
    
    # Subset evaluation and grab survey code
    eval_sub <- subset(data, session == evaluations[i])
    ret$survey_code[i] <- eval_sub$survey_code[1]
    if (eval_sub$response_value[1] == "A Ross Rd & Scott Ave") {
      ret$location.response[i] <- "Lon: 42.032570 Lat: -93.660615"
    }
    else if (eval_sub$response_value[1] == "B McCarthy Lee Park Access") {
      ret$location.response[i] <- "Lon: 42.028174 Lat: -93.658286"
    }
    else if (eval_sub$response_value[1] == "C Pammel Woods Access") {
      ret$location.response[i] <- "Lon: 42.032599 Lat: -93.655532"
    }
    else if (eval_sub$response_value[1] == "D West St & Hyland Ave") {
      ret$location.response[i] <- "Lon: 42.025364 Lat: -93.655628"
    }
    else if (eval_sub$response_value[1] == "E Lincoln Way & Hyland Ave") {
      ret$location.response[i] <- "Lon: 42.022699 Lat: -93.655634"
    }
    else if (eval_sub$response_value[1] == "F Lake Lavern Path") {
      ret$location.response[i] <- "Lon: 42.023266 Lat: -93.649473"
    }
    else if (eval_sub$response_value[1] == "G Moore Park Access") {
      ret$location.response[i] <- "Lon: 42.012279 Lat: -93.642943"
    }
    else if (eval_sub$response_value[1] == "H University Blvd & Mortensen Pkwy") {
      ret$location.response[i] <- "Lon: 42.008474 Lat: -93.639382"
    }
    else if (eval_sub$response_value[1] == "I Hans Peter Christofferson Park") {
      ret$location.response[i] <- "Lon: 41.996946 Lat: -93.644170"
    }
    else if (eval_sub$response_value[1] == "J North Loop Drive") {
      ret$location.response[i] <- "Lon: 42.001154 Lat: -93.633760"
    }
    
    # Footpath survey
    if (eval_sub$survey_code[1] == "footpath") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location.response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00100" || eval_sub$question_code[j] == "00101"){
          ret$location.response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00110"){
          ret$type.response_value[i] <- eval_sub$response_value[j]
          ret$type.response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00120"){
          ret$condition.response_value[i] <- eval_sub$response_value[j]
          ret$condition.response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00130"){
          ret$attractive.response_value[i] <- eval_sub$response_value[j]
          ret$attractive.response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00140"){
          ret$safe.response_value[i] <- eval_sub$response_value[j]
          ret$safe.response[i] <- eval_sub$response[j]
        }
      }
    }
    
    # Street survey
    if (eval_sub$survey_code[1] == "street") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location.response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00400" || eval_sub$question_code[j] == "00401"){
          ret$location.response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00410"){
          ret$type.response_value[i] <- eval_sub$response_value[j]
          ret$type.response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00420"){
          ret$condition.response_value[i] <- eval_sub$response_value[j]
          ret$condition.response[i] <- eval_sub$response[j]
        }
        # Markings
        else if (eval_sub$question_code[j] == "00430"){
          ret$markings.response_value[i] <- eval_sub$response_value[j]
          ret$markings.response[i] <- eval_sub$response[j]
        }
        # Signs
        else if (eval_sub$question_code[j] == "00432"){
          ret$signs.response_value[i] <- eval_sub$response_value[j]
          ret$signs.response[i] <- eval_sub$response[j]
        }
        # Lane
        else if (eval_sub$question_code[j] == "00433"){
          ret$lane.response_value[i] <- eval_sub$response_value[j]
          ret$lane.response[i] <- eval_sub$response[j]
        }
        # Parking
        else if (eval_sub$question_code[j] == "00434"){
          ret$parking.response_value[i] <- eval_sub$response_value[j]
          ret$parking.response[i] <- eval_sub$response[j]
        }
        # Crossing
        else if (eval_sub$question_code[j] == "00435"){
          ret$crossing.response_value[i] <- eval_sub$response_value[j]
          ret$crossing.response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00440"){
          ret$attractive.response_value[i] <- eval_sub$response_value[j]
          ret$attractive.response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00450"){
          ret$safe.response_value[i] <- eval_sub$response_value[j]
          ret$safe.response[i] <- eval_sub$response[j]
        }
      }
    }
    
    # Highway survey
    if (eval_sub$survey_code[1] == "highway") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location.response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00500" || eval_sub$question_code[j] == "00501"){
          ret$location.response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00510"){
          ret$type.response_value[i] <- eval_sub$response_value[j]
          ret$type.response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00520"){
          ret$condition.response_value[i] <- eval_sub$response_value[j]
          ret$condition.response[i] <- eval_sub$response[j]
        }
        # Markings
        else if (eval_sub$question_code[j] == "00530"){
          ret$markings.response_value[i] <- eval_sub$response_value[j]
          ret$markings.response[i] <- eval_sub$response[j]
        }
        # Signs
        else if (eval_sub$question_code[j] == "00541"){
          ret$signs.response_value[i] <- eval_sub$response_value[j]
          ret$signs.response[i] <- eval_sub$response[j]
        }
        # Lane
        else if (eval_sub$question_code[j] == "00542"){
          ret$lane.response_value[i] <- eval_sub$response_value[j]
          ret$lane.response[i] <- eval_sub$response[j]
        }
        # Parking
        else if (eval_sub$question_code[j] == "00543"){
          ret$parking.response_value[i] <- eval_sub$response_value[j]
          ret$parking.response[i] <- eval_sub$response[j]
        }
        # Crossing
        else if (eval_sub$question_code[j] == "00544"){
          ret$crossing.response_value[i] <- eval_sub$response_value[j]
          ret$crossing.response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00550"){
          ret$attractive.response_value[i] <- eval_sub$response_value[j]
          ret$attractive.response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00560"){
          ret$safe.response_value[i] <- eval_sub$response_value[j]
          ret$safe.response[i] <- eval_sub$response[j]
        }
      }
    }
    
    # Sidewalk survey
    if (eval_sub$survey_code[1] == "sidewalk") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location.response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00300" || eval_sub$question_code[j] == "0301"){
          ret$location.response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00310"){
          ret$type.response_value[i] <- eval_sub$response_value[j]
          ret$type.response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00320"){
          ret$condition.response_value[i] <- eval_sub$response_value[j]
          ret$condition.response[i] <- eval_sub$response[j]
        }
        # Along Road
        else if (eval_sub$question_code[j] == "00330"){
          ret$along_road.response_value[i] <- eval_sub$response_value[j]
          ret$along_road.response[i] <- eval_sub$response[j]
        }
        # From Road
        else if (eval_sub$question_code[j] == "00340"){
          ret$from_road.response_value[i] <- eval_sub$response_value[j]
          ret$from_road.response[i] <- eval_sub$response[j]
        }
        # Buffers
        else if (eval_sub$question_code[j] == "00350"){
          ret$buffer.response_value[i] <- eval_sub$response_value[j]
          ret$buffer.response[i] <- eval_sub$response[j]
        }
        # Fence Buffer
        else if (eval_sub$question_code[j] == "00351"){
          ret$fence.response_value[i] <- eval_sub$response_value[j]
          ret$fence.response[i] <- eval_sub$response[j]
        }
        # Tree Buffer
        else if (eval_sub$question_code[j] == "00352"){
          ret$tree.response_value[i] <- eval_sub$response_value[j]
          ret$tree.response[i] <- eval_sub$response[j]
        }
        # Hedge Buffer
        else if (eval_sub$question_code[j] == "00353"){
          ret$hedge.response_value[i] <- eval_sub$response_value[j]
          ret$hedge.response[i] <- eval_sub$response[j]
        }
        # Landscaping Buffer
        else if (eval_sub$question_code[j] == "00354"){
          ret$landscaping.response_value[i] <- eval_sub$response_value[j]
          ret$landscaping.response[i] <- eval_sub$response[j]
        }
        # Grass Buffer
        else if (eval_sub$question_code[j] == "00355"){
          ret$grass.response_value[i] <- eval_sub$response_value[j]
          ret$grass.response[i] <- eval_sub$response[j]
        }
        # Width
        else if (eval_sub$question_code[j] == "00360"){
          ret$width.response_value[i] <- eval_sub$response_value[j]
          ret$width.response[i] <- eval_sub$response[j]
        }
        # Buildings Near
        else if (eval_sub$question_code[j] == "00370"){
          ret$buildings_near.response_value[i] <- eval_sub$response_value[j]
          ret$buildings_near.response[i] <- eval_sub$response[j]
        }
        # From Buildings
        else if (eval_sub$question_code[j] == "00380"){
          ret$from_buildings.response_value[i] <- eval_sub$response_value[j]
          ret$from_buildings.response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00390"){
          ret$attractive.response_value[i] <- eval_sub$response_value[j]
          ret$attractive.response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00399"){
          ret$safe.response_value[i] <- eval_sub$response_value[j]
          ret$safe.response[i] <- eval_sub$response[j]
        }
      }
    }
    
    # Paved Trail survey
    if (eval_sub$survey_code[1] == "paved_trail") {
      for (j in 1:nrow(eval_sub)) {
        # Location    
        if (eval_sub$event_type[j] == "survey_submitted") {
          #ret$location.response[i] <- paste0("Lat: ", eval_sub$more_lat[j], ", Lon: ", eval_sub$more_lon[j])
        }
        else if (eval_sub$question_code[j] == "00200" || eval_sub$question_code[j] == "00201"){
          ret$location.response_value[i] <- eval_sub$response_value[j]
        }
        # Type
        else if (eval_sub$question_code[j] == "00210"){
          ret$type.response_value[i] <- eval_sub$response_value[j]
          ret$type.response[i] <- eval_sub$response[j]
        }
        # Condition
        else if (eval_sub$question_code[j] == "00220"){
          ret$condition.response_value[i] <- eval_sub$response_value[j]
          ret$condition.response[i] <- eval_sub$response[j]
        }
        # Along Road
        else if (eval_sub$question_code[j] == "00230"){
          ret$along_road.response_value[i] <- eval_sub$response_value[j]
          ret$along_road.response[i] <- eval_sub$response[j]
        }
        # From Road
        else if (eval_sub$question_code[j] == "00240"){
          ret$from_road.response_value[i] <- eval_sub$response_value[j]
          ret$from_road.response[i] <- eval_sub$response[j]
        }
        # Buffers
        else if (eval_sub$question_code[j] == "00250"){
          ret$buffer.response_value[i] <- eval_sub$response_value[j]
          ret$buffer.response[i] <- eval_sub$response[j]
        }
        # Fence Buffer
        else if (eval_sub$question_code[j] == "00251"){
          ret$fence.response_value[i] <- eval_sub$response_value[j]
          ret$fence.response[i] <- eval_sub$response[j]
        }
        # Tree Buffer
        else if (eval_sub$question_code[j] == "00252"){
          ret$tree.response_value[i] <- eval_sub$response_value[j]
          ret$tree.response[i] <- eval_sub$response[j]
        }
        # Hedge Buffer
        else if (eval_sub$question_code[j] == "00253"){
          ret$hedge.response_value[i] <- eval_sub$response_value[j]
          ret$hedge.response[i] <- eval_sub$response[j]
        }
        # Landscaping Buffer
        else if (eval_sub$question_code[j] == "00254"){
          ret$landscaping.response_value[i] <- eval_sub$response_value[j]
          ret$landscaping.response[i] <- eval_sub$response[j]
        }
        # Grass Buffer
        else if (eval_sub$question_code[j] == "00255"){
          ret$grass.response_value[i] <- eval_sub$response_value[j]
          ret$grass.response[i] <- eval_sub$response[j]
        }
        # Attractive (bicycling)
        else if (eval_sub$question_code[j] == "00260"){
          ret$attractive.response_value[i] <- eval_sub$response_value[j]
          ret$attractive.response[i] <- eval_sub$response[j]
        }
        # Safe (bicycling)
        else if (eval_sub$question_code[j] == "00270"){
          ret$safe.response_value[i] <- eval_sub$response_value[j]
          ret$safe.response[i] <- eval_sub$response[j]
        }
      }
    }
  }
  list(datasets = list(vgi_formatted = ret), files = list())
}
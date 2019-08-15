#' @title Industrial AppStoreClient Client
#'
#' @description Package allows to connect and query Intelligent Plant Industrial AppStore.
#'
#' @param access_token Your access token for the Intelligent Plant AppStore
#'
#' @param dsn The name of the datasource being queried
#'
#' @param tags List of desired tag names
#'
#' @param startDate Absolute or relative query start time
#'
#' @param endDate Absolute or relative query end time
#'
#' @param funct Data aggregation function to use
#'
#' @param step The timestep to take the data points
#'
#' @param verbose Default value = FALSE, if true will print out extra information such as response code
#'
#' @return A dataframe with each tag and their corresponding values with the timestamps
#'
#' @examples GetHistoricalData('dummyToken', 'IP Datasource', list('head', 'flow'), '*-10d', '*', 'PLOT', '1d', TRUE)
#'
#' @export GetHistoricalData
GetHistoricalData = function(access_token, dsn, tags, startDate, endDate, funct, step, verbose = FALSE) {
  #The base URL of all appstore api queries
  baseUrl <- "https://appstore.intelligentplant.com/Gestalt"
  #The endpoint of the API
  valuesApiEndpoint <- "/api/data/v2/history/"

  #ensures none of the necessary parameters are null
  if (is.null(tags)) {
    print("Tags parameter can't be empty!")
    return();
  }
  if(is.null(dsn)) {
    print("Data source must be specified")
    return()
  }
  if (is.null(startDate)) {
    print("Start date must be specified")
    return()
  }
  if(is.null(endDate)) {
    print("End date must be specified")
    return()
  }
  if(is.null(funct)) {
    print("Data function must be specified")
    return()
  }
  if(is.null(step)) {
    print("Time-step must be specified")
    return()
  }

  #The required packages for the
  requiredPackages <- c('httr', 'jsonlite')
  # check the packages are actaually installed
  for (p in requiredPackages) {
    if (!require(p, character.only = TRUE)) install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }


  #Names each item in the tag list as "tag" this is necessary for the API query below
  naming <- c("tag")
  for (tag in tags) {
    if (tag != tags[1]) {
      naming <- append(naming, "tag", 2)
    }
  }
  names(tags) <- naming

  #Queires the API and gets the raw response
  raw.result <- GET(concat(baseUrl, valuesApiEndpoint, URLencode(dsn)),
                    query = c(tags, list(
                      "function" = funct,
                      "start" = startDate,
                      "end" = endDate,
                      "step" = step)),
                    add_headers(Authorization = paste("Bearer ", access_token))
  )

  #If @param verbose = TRUE will print lots of extra information
  if (isTRUE(verbose)) {
    print(cat("Calling URL: ", raw.result$url))
    print(cat("Response Status Code: ", raw.result$status_code))
    print(cat("Content: ", rawToChar(raw.result$content)))
  }

  #Check if the status code 200
  if (raw.result$status_code == 200) {

    # Translate response into text
    this.raw.content <- rawToChar(raw.result$content)

    # Parse DataCore results and return
    return(ParseResults(this.raw.content, tags, verbose))
  }
  #Response code was invalid
  else {
    print(cat("Response code doesn't indicate success, code: ", raw.result$status_code))
  }
}

# Parse the repsonse from DataCore
ParseResults = function(rawContent, tags, verbose) {

  #Extracts the Json from the raw response
  jsonContent <- fromJSON(rawContent, flatten = TRUE)

  #gets the timestamps
  first <- unlist(tags)
  ts <- jsonContent[[first[1]]]$Values$UtcSampleTime

  #create dataframe to be added to with the timestamps
  df <- data.frame(ts, stringsAsFactors = FALSE)

  #loops over each tag and adds a column with that tags data to the dataframe
  for (tag in tags) {
    df[tag] <- jsonContent[[tag]]$Values$NumericValue
  }
  #prints dataframe if verbose option was set to true
  if (verbose) {
    print(df)
  }
  return(df)
}

#joins as many strings together as desired with an optinonally specified seperator
concat <- function(..., sep = '') {
  paste(..., sep = sep, collapse = sep)
}

#' Import vosonSML data previously saved to disk using the `Collect()` function.
#'
#' This function imports social media data previously collected and saved
#' to disk using the `Collect()` function in vosonSML. Using this
#' function to import data ensures that the correct classes are applied
#' to the dataframe, in order for vosonSML to know how to handle it
#' (e.g. creating different types of networks using the `Create` function).
#'
#' @param file character, specifying the file path to the data to be imported
#' @param dataSource character, the type of social media. Currently supports
#' "facebook", "twitter", "youtube", and "instagram" (not case sensitive).
#' @return A dataframe with specific class attributes for vosonSML functionality
#' @author Timothy Graham <timothy.graham@anu.edu.au>
#' @seealso \code{\link{Collect}}
#' @examples
#'
#' \dontrun{
#' require(magrittr)
#' ## Facebook example
#'
#' # Specify your API keys
#' appID <- "xxxx"
#' appSecret <- "xxxx"
#'
#' # Authenticate and Collect some data (and save data to disk using `writeToFile=TRUE`)
#' myFacebookData <- Authenticate("Facebook", appID = appID, appSecret = appSecret) %>%
#'    Collect(pageName="StarWars", rangeFrom="2015-03-01",
#'      rangeTo="2015-03-02", writeToFile=TRUE)
#'
#' # Import the data (that was saved to disk in the previous step)
#' myStarWarsData <- importData("2015-03-01_to_2015-03-02_StarWars_FacebookData.csv","facebook")
#'
#' # Create a network using the imported dataframe object
#' myNetwork <- myStarWarsData %>% Create("Bimodal")
#' }
#' @export
importData <- function(file,dataSource) {
  df <- read.csv(file)
  #if(all(colnames(df)==c("X","from","to","edgeType","postType","postLink","postTimestamp","commentText","commentTimestamp"))) {
  #  class(df) <- c("data.table","data.frame","dataSource","facebook")
  #} # TODO: implement a 'smart' function that automatically detects the type of data, i.e. which dataSource (e.g. facebook)
  if (missing(file))
    stop("Need to specify a file to import data.")

  if (missing(dataSource))
    stop("Need to specify a valid data source, e.g. facebook or twitter.")

  dataSource <- tolower(dataSource)
  switch(dataSource,
         facebook={
           class(df) <- c("data.table","data.frame","dataSource","facebook")
         },
         twitter={
           class(df) <- c("data.table","data.frame","dataSource","twitter")
         },
         youtube={
           class(df) <- c("data.table","data.frame","dataSource","youtube")
         },
         instagram={
           class(df) <- c("data.table","data.frame","dataSource","instagram")
         },
         {
           print('Please provide a valid data source for the `dataSource` argument, e.g. facebook or twitter')
         }
  )
  return(df)
}

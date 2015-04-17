#' Search series name from Quandl.
#' 
#' @param keyword String
#' @param page Integer, number of pages
#' @param source String, the source filtering

search_series <- function(keyword, page = 1, source = NULL){
    Quandl.search(keyword, page = 1, source=NULL);
}
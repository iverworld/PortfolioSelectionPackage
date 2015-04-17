#' Get required field names for a data set. Possible fields are "Date" and "Close"
#' @return List of field names.
get_required_field_names <- function(){
    #return (c("Date", "Close", "Adjusted Close"));
    return (c("Date", "Close"));
}

#' Get roll back date.
#' @return A day back from today
get_roll_date <- function(days){
    return (Sys.Date() - days);
}
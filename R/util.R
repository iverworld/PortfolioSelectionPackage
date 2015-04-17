#' Get required field names for a data set. Possible fields are "Date" and "Close"
#' @return List of field names.
get_required_field_names <- function(){
    #return (c("Date", "Close", "Adjusted Close"));
    return (c("Date", "Close"));
}

#' Get roll back date.
#' @return A day back from today.
get_roll_date <- function(days){
    return (Sys.Date() - days);
}

#' Get instrument list.
#' @return Instrument list.
get_instrument_list <- function(group="all"){
    t=read.csv("./config/instrument.cfg", sep=",", header=TRUE);
    if(group=="all")
    {
        return (t[,2]);    
    }else{
        return (t[which(t[,1]==group),2]);
    }
    
}
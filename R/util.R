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
    t=read.table("./config/instrument.cfg", sep=",", header=TRUE, stringsAsFactors=FALSE);
    if(group=="all")
    {
        return (t[,2]);    
    }else{
        return (t[which(t[,1]==group),2]);
    }
    
}

#' Fill NA values in the series.
#' @param series The series.
#' @param is_rdiff Boolean, apply Quandl percentage difference transformation.
na_series_fill <- function(series, is_rdiff){
    if (is_rdiff)
    {
        return (na.fill(series, 0));
    }else{
        return (na.locf(series, 0));   
    }
}
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

#' Get instrument dictionary.
#' @return A table, first column the group and the second column the series codes.
get_instrument_dictionary <- function(group="all"){
    t=read.table("./config/instrument.cfg", 
                 quote="\"",
                 sep=",", 
                 header=TRUE, 
                 stringsAsFactors=FALSE);
    t = t[substr(t[,1],1,1)!="#",];
    if (group=="all"){
        return (t);
    } else {
        return (t[which(t[,1]==group),]);
    }
}

#' Get instrument list.
#' @return Instrument list.
get_instrument_list <- function(group="all"){
    t=read.table("./config/instrument.cfg", 
                 quote="\"",
                 sep=",", 
                 header=TRUE, 
                 stringsAsFactors=FALSE);
    t = t[substr(t[,1],1,1)!="#",];
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

#' Get the series in a specific group.
#' @param series The series.
#' @param group_name The group name.
#' @return The series filtered by the group name.
group <- function(series, group_name){
    id = get_instrument_dictionary();
    return(series[,id[id[,1]==group_name,2]]);
}

#' Get the description from the list of codes.
#' @param codes The list of codes.
#' @return The description of the codes.
description <- function(codes){
    id = get_instrument_dictionary();
    id.found = id[,2] %in% codes;
    id.final = id[id.found,c(2,4)];
    return (id.final[!duplicated(id.final),]);
}
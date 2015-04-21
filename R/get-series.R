#' Get unmanaged series from Quanl.
#'
#'@param series_name String.
#'@param from_date Start day.
#'@param is_rdiff Boolean, apply Quandl percentage difference transformation.
#'@return A time series.
get_unmanaged_series <- function(series_name, from_date, is_rdiff=FALSE){
    if (is_rdiff)
    {
        return (Quandl(series_name, 
                       type="zoo", 
                       start_date = from_date, 
                       transformation = "rdiff"));
    }
    else {
        return (Quandl(series_name, 
                       type="zoo", 
                       start_date = from_date));
    }
}

#' Get series
#' 
#'@param series_name String.
#'@param from_date Start day.
#'@param is_rdiff Boolean, apply Quandl percentage difference transformation.
#'@return A time series.
get_series <- function(series_name, from_date, is_rdiff=FALSE){
    quandl_data = get_unmanaged_series(series_name, from_date, is_rdiff);
    header_quandl_data = colnames(quandl_data) %in% get_required_field_names();
    return (quandl_data[, header_quandl_data]);
}

#' Get series list
#' 
#' @param group The name of group.
#' @param rolling_days Number of days rolled back from today.
#' @param is_rdiff Boolean, apply Quandl percentage difference transformation.
#' @return List of time series.

get_series_list <- function(group = "all", rolling_days, is_rdiff=FALSE){
    instruments = get_instrument_list(group);
    if (length(instruments) == 0)
    {
        return (zoo());    
    }
    else if (length(instruments) == 1)
    {
        return (get_series(instruments[1], 
                           get_roll_date(rolling_days), 
                           is_rdiff));
    }
    else
    {
        ret = get_series(instruments[1], 
                       get_roll_date(rolling_days), 
                       is_rdiff);
        column_names = instruments;
        instruments = instruments[-1];
        for (instrument in instruments){
            t = get_series(instrument, 
                           get_roll_date(rolling_days),
                           is_rdiff);
            ret = merge(ret, t, all = TRUE);
        }
        colnames(ret) = column_names;
        return (ret);
    }
}
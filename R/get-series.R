#' Get unmanaged series from Quanl.
#'
#'@param series_name String.
#'@param from_date Start day.
#'@param is_rdiff Boolean, apply Quandl percentage difference transformation.
#'@return A time series
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
#'@return A time series
get_series <- function(series_name, from_date, is_rdiff=FALSE){
    quandl_data = get_unmanaged_series(series_name, from_date, is_rdiff);
    header_quandl_data = colnames(quandl_data) %in% get_required_field_names();
    return (quandl_data[, header_quandl_data]);
}
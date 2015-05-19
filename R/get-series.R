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
get_series <- function(series_name, from_date, field_name = "", is_rdiff=FALSE){
    print(sprintf("[%s][get_series]: Getting series %s...", Sys.time(), series_name));
    quandl_data = get_unmanaged_series(series_name, from_date, is_rdiff);
    required_field_names = get_required_field_names();
    if (field_name != ""){
        required_field_names = c("Date", field_name);
    }
    
    header_quandl_data = colnames(quandl_data) %in% required_field_names;
    return (quandl_data[, header_quandl_data]);
}

#' Get series list
#' 
#' @param group The name of group.
#' @param rolling_days Number of days rolled back from today.
#' @param is_rdiff Boolean, apply Quandl percentage difference transformation.
#' @return List of time series.

get_series_list <- function(group = "all", rolling_days, is_rdiff=FALSE){
    instruments_dict = get_instrument_dictionary(group);
    if (nrow(instruments_dict) == 0)
    {
        return (zoo());    
    }
    else if (nrow(instruments_dict) == 1)
    {
        ret = get_series(series_name = instruments_dict[1,2], 
                         from_date = get_roll_date(rolling_days), 
                         field_name = instruments_dict[1,3],
                         is_rdiff = is_rdiff);
        return (na_series_fill(ret, is_rdiff));
    }
    else
    {
        ret = get_series(series_name = instruments_dict[1,2], 
                         from_date = get_roll_date(rolling_days), 
                         field_name = instruments_dict[1,3],
                         is_rdiff = is_rdiff);
        column_names = instruments_dict[,2];
        instruments_dict = instruments_dict[-1,];
#         for (instrument in instruments_dict){
        for (k in 1:nrow(instruments_dict)){
            t = get_series(series_name = instruments_dict[k,2], 
                           from_date = get_roll_date(rolling_days), 
                           field_name = instruments_dict[k,3],
                           is_rdiff = is_rdiff);
            ret = merge(ret, t, all = TRUE);
        }
        colnames(ret) = column_names;
        return (na_series_fill(ret, is_rdiff));
    }
}
#' Find the instrument name where it contains the largest cumulative sum.
#' @param series The series list.
#' @return The instrument name.

argmax <- function(series){
    end_cum = tail(cumsum(series), 1);
    max_cum = max(end_cum);
    return (colnames(end_cum)[which(end_cum>=max_cum)[1]]);;
}

#' Find the date of the roll-back days.
#' @param series            The series.
#' @param num_trading_days  Number of trading days.
dateof <- function(series, num_trading_days){
    if (nrow(series)>num_trading_days)
    {
        return (start(series[nrow(series)-num_trading_days]));
    } else {
        return (start(series));
    }
}


#' Find the stocks most returned.
#' @param series            The series list.
#' @param rank              The rank.
#' @param num_trading_days  Number of trading days to extract from the series.
top_series <- function(series, rank = 1, num_trading_days){
    num_row = nrow(series);
    if (num_trading_days < num_row){
        series = series[(num_row-num_trading_days):num_row];
    }
    
    start_data = coredata(series[start(series)]);
    end_data = coredata(series[end(series)]);
    
    core_end_cum = (end_data - start_data)/start_data;
    ordered_end_cum = core_end_cum[, order(core_end_cum[1,], decreasing=TRUE)];
    if (rank > 0){
        return (ordered_end_cum[1:rank]);
    } else {
        lengthOEC = length(ordered_end_cum);
        return (ordered_end_cum[(lengthOEC+rank):lengthOEC]);
    }
}
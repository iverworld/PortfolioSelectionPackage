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

#' Find the cumulative return of the series.
#' @parma series The series.
#' @param index The index series. When index is input, the return is relative to the index return.
cum_return <- function(series, index=NULL){
    if (is.null(index)){
        return (exp(cumsum(diff(log(series))))-1);
    } else {
        return (exp(cumsum(diff(log(series))))-exp(cumsum(diff(log(index)))));
    }
}

#' Find the ordered cumulative return of the series.
#' @param series The series.
#' @parma num_trading_days Number of trading days.
#' @param index The index series. When index is input, the return is relative to the index return.
ordered_cum_series <- function(series, num_trading_days, index=NULL){
    num_row = length(index(series));
    if (num_trading_days < num_row){
        series = series[(num_row-num_trading_days):num_row];
    }
    core_end_cum = coredata(cum_return(series,index)[end(series)]);
    ordered_end_cum = core_end_cum;
    if(FALSE == is.null(nrow(ordered_end_cum))) {
        ordered_end_cum = core_end_cum[, order(core_end_cum[1,], decreasing=TRUE)];
    }
    return (ordered_end_cum);
}

#' Find the stocks most returned.
#' @param series            The series list.
#' @param rank              The rank.
#' @param num_trading_days  Number of trading days to extract from the series.
#' @param index             The index series. When index is input, the return is relative to the index return.
top_series <- function(series, rank = 1, num_trading_days, index=NULL){
    ordered_end_cum = ordered_cum_series(series, num_trading_days, index);
    if (rank > 0){
        return (ordered_end_cum[1:rank]);
    } else {
        lengthOEC = length(ordered_end_cum);
        return (ordered_end_cum[(lengthOEC+rank):lengthOEC]);
    }
}
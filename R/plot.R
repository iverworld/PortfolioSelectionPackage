#' Plot zoo series in standard
#' @param series The series list.
#' @param is_legend Boolean, determine whether legend is needed.

plot_standard <- function(series, is_legend=FALSE){
    size = length(series);
    cum = cumsum(series);
    plot(cum,
         plot.type="single",
         col=1:size,
         lwd=1,
         lty=1:size,
         xlab="date",
         ylab="return");
    
    if(is_legend){
        legend(start(series), max(cum), legend=colnames(series));
    }
}
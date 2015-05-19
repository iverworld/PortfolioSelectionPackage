#' Set margin
set_margin <- function(){
    par(mai=c(1.02,0.82,0.82,0.42));
}

#' Plot zoo series in standard
#' @param series The series list.
#' @param is_legend Boolean, determine whether legend is needed.

plot_standard <- function(series, is_legend=FALSE){
    set_margin();
    size = length(series);
    plot(series,
         plot.type="single",
         col=1:size,
         lwd=1,
         lty=1:size,
         xlab="date",
         ylab="return");
    
    if(is_legend){
        #legend(start(series), max(cum), legend=colnames(series));
        legend("topleft", legend=colnames(series), fill=1:size);
    }
}
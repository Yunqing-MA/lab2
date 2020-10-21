#' Statistics for DRG average Medicare data
#'
#' @param statistics to choose which statistic is to be returned
#'
#' @return statistics chosen
#' @export
#'
#' @examples statistics_DRG_Med(statistics = 'median')
#'
statistics_DRG_Med <- function(statistics = 'mean') {
    if (statistics == 'mean') {
        mean <- mean(DRG_data$`Average Medicare Payments`)
        return(mean)
    }
    if (statistics == 'median') {
        median <- median(DRG_data$`Average Medicare Payments`)
        return(median)
    }
    if (statistics == 'sd') {
        sd <- sd(DRG_data$`Average Medicare Payments`)
        return(sd)
    }
}

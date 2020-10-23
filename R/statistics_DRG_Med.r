#' Statistics for DRG average Medicare data
#'
#' @param statistics to choose which statistic is to be returned
#'
#' @return statistics chosen
#' @export
#'
#' @examples statistics_DRG_Med(statistics = 'median')
#'
# Define the function
statistics_DRG_Med <- function(statistics = 'mean') {
    # Read data
    DRG_data <- read.csv('DRG_data.csv')

    # Select what statistics to calculate
    if (statistics == 'mean') {
        mean <- mean(DRG_data$Average.Medicare.Payments)
        return(mean)
    }
    # Select what statistics to calculate
    else if (statistics == 'median') {
        median <- median(DRG_data$Average.Medicare.Payments)
        return(median)
    }
    # Select what statistics to calculate
    else if (statistics == 'sd') {
        sd <- sd(DRG_data$Average.Medicare.Payments)
        return(sd)
    }
}

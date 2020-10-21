#' Boxplot for DRG data
#'
#' @param data to choose which average payment is to be plotted
#'
#' @return a plot with average payment
#' @export
#'
#' @examples boxplot_DRG(data = 'total')
#'
boxplot_DRG <- function(data = 'medicare') {
    if (data == 'medicare') {
        boxplot(
            DRG_data$`Average Medicare Payments`,
            xlab = 'Medicare',
            ylab = 'Average Payments',
            main = 'Average Medicare Payments'
        )
    }
    else if (data == 'total') {
        boxplot(
            DRG_data$`Average Total Payments`,
            xlab = 'Total',
            ylab = 'Average Payments',
            main = 'Average Total Payments'
        )
    }
    else if (data == 'covered') {
        boxplot(
            DRG_data$`Average Covered Charges`,
            xlab = 'Covered',
            ylab = 'Average Payments',
            main = 'Average Covered Payments'
        )
    }
}

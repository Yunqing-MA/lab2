#' Boxplot for DRG data
#'
#' @param data to choose which average payment is to be plotted
#'
#' @return a plot with average payment
#' @export
#'
#' @examples boxplot_DRG(data = 'total')
#'
# Define the function
boxplot_DRG <- function(data = 'medicare') {
    # Read data
    DRG_data <- read.csv('DRG_data.csv')

    # Set x axis text
    distinct_def <- dplyr::distinct(DRG_data, DRG.Definition)
    def_split <- strsplit(distinct_def[[1]], ' ')
    def_id <- c()
    # Use only the number to indicate the definition
    for (i in 1:100) {
        def_id <- c(def_id, def_split[[i]][1])
    }

    # Select data for plot
    if (data == 'medicare') {
        ggplot2::ggplot(DRG_data,
                        ggplot2::aes(x = DRG.Definition,
                                     y = Average.Medicare.Payments)) +
            ggplot2::geom_boxplot() +
            ggplot2::labs(title = 'Average Medicare Payments',
                          x = 'Medicare',
                          y = 'Average Payments') +
            ggplot2::scale_x_discrete(labels = def_id) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                size = 8
            ))
    }
    # Select data for plot
    else if (data == 'total') {
        ggplot2::ggplot(DRG_data,
                        ggplot2::aes(x = DRG.Definition,
                                     y = Average.Total.Payments)) +
            ggplot2::geom_boxplot() +
            ggplot2::labs(title = 'Average Total Payments',
                          x = 'Total',
                          y = 'Average Payments') +
            ggplot2::scale_x_discrete(labels = def_id) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                size = 8
            ))
    }
    # Select data for plot
    else if (data == 'covered') {
        ggplot2::ggplot(DRG_data,
                        ggplot2::aes(x = DRG.Definition,
                                     y = Average.Covered.Charges)) +
            ggplot2::geom_boxplot() +
            ggplot2::labs(title = 'Average Covered Charges',
                          x = 'Covered',
                          y = 'Average Charges') +
            ggplot2::scale_x_discrete(labels = def_id) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                size = 8
            ))
    }
}

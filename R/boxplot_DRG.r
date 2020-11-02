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
boxplot_DRG <- function(data = 'Average.Medicare.Payments') {

    # Set x axis text
    distinct_def <- dplyr::distinct(DRG_data, DRG.Definition)
    def_split <- strsplit(distinct_def[[1]], ' ')
    def_id <- c()

    # Use only the number to indicate the definition
    for (i in 1:100) {
        def_id <- c(def_id, def_split[[i]][1])
    }

    # Plot different data using get()
    ggplot2::ggplot(DRG_data,
                    ggplot2::aes(x = DRG.Definition,
                                 y = get(data))) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = gsub('.',' ',data,fixed = TRUE),
                      x = 'DRG code',
                      y = 'Average Payments') +
        ggplot2::scale_x_discrete(labels = def_id) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                size = 6))

}

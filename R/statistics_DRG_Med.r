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
statistics_DRG_Med <- function (func = mean) {

    # Group_by Definition
    grouped_DRG <- dplyr::group_by(DRG_data, DRG.Definition)

    # Calculate the grouped statistic
    return_DRG <- dplyr::summarize(grouped_DRG, method = func)

    return(return_DRG)

}

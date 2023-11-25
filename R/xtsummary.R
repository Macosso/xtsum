#' Calculate summary statistics for panel data
#'
#' This function computes summary statistics for panel data, including overall
#' statistics, between-group statistics, and within-group statistics.
#'
#' @param data A data.frame or pdata.frame object representing panel data.
#' @param variables (Optional) Vector of variable names for which to calculate statistics.
#'                  If not provided, all numeric variables in the data will be used.
#' @param id (Optional) Name of the individual identifier variable.
#' @param t (Optional) Name of the time identifier variable.
#' @param na.rm Logical indicating whether to remove NAs when calculating statistics.
#'
#' @return A table summarizing statistics for each variable, including Mean, SD, Min, and Max,
#'         broken down into Overall, Between, and Within dimensions.
#'
#' @examples
#' # Using pdata.frame object
#' data("Gasoline", package = "plm")
#' Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop.index = TRUE)
#' xtsum(Gas)
#'
#' # Using regular data.frame with id and t specified
#' data("Wages", package = "plm")
#' xtsum(Wages, id = "id", t = "year")
#'
#' # Specifying variables to include in the summary
#' xtsum(Gas, variables = c("income", "expenditure"))
#'
#' @importFrom dplyr all_of select mutate group_by summarise ungroup pull sym bind_rows
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom plm pdata.frame index
#' @importFrom purrr map
#' @importFrom tibble data_frame
xtsum <- function(data,
                  variables = NULL,
                  id = NULL,
                  t = NULL,
                  na.rm = FALSE) {
  # Check if data is a data.frame
  if (!is.data.frame(data)) {
    stop("data must be a data.frame object...")
  }

  # Check if id and t are provided if data is not a pdata.frame object
  if (any(is.null(id), is.null(t)) &
      !"pdata.frame" %in% class(data)) {
    stop("if data is not a pdata.frame object, id and t must be provided")
  }

  # If data is a pdata.frame object and id or t is missing, use pdata.frame index
  if ("pdata.frame" %in% class(data) &
      any(is.null(id), is.null(t))) {
    data$id <- plm::index(data)[1] %>% pull()
    data$t <- plm::index(data)[2] %>% pull()
    id <- "id"
    t <- "t"
  }

  # If variables are not specified, use all numeric variables in the data
  if (is.null(variables)) {
    variables <- colnames(data)[!colnames(data) %in% c(id, t)]
  }
  idt <- c(id,t)

  # Initialize an empty data.frame for the table results
  TableRes = data.frame(matrix(nrow = 0, ncol = 6))
  colnames(TableRes) <- c("Variable", "Dim", "Mean", "SD", "Min", "MAX")

  # Extract relevant data for specified variables
  data_ <- as.data.frame(data) %>%
    dplyr::select(dplyr::all_of(idt), dplyr::all_of(variables))

  # Use sapply to calculate summary statistics for each variable
  sum_ <- sapply(variables, function(x, data=data_){

    # Calculate overall mean, between-group mean, and within-group mean
    g_mean = mean(data[,x], na.rm = na.rm)

    data %>%
      dplyr::select(all_of(idt),all_of(x)) %>%
      dplyr::mutate(g_mean = mean(!!sym(x), na.rm = na.rm)) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(Xi_mean = mean(!!sym(x), na.rm = na.rm),
                    within_x = !!sym(x) - Xi_mean + g_mean) %>%
      dplyr::ungroup() %>%
      dplyr::pull(within_x) -> within_x

    data %>%
      dplyr::select(all_of(idt),all_of(x)) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(Xi_mean = mean(!!sym(x), na.rm = na.rm)) %>%
      dplyr::ungroup() %>%
      dplyr::pull(Xi_mean) -> between_x

    # Create data.frames for overall, between, and within statistics
    overal <-  data.frame(Variable = x, Dim = "Overal", Mean = mean(data[,x], na.rm = na.rm),
                          SD = sd(data[,x], na.rm = na.rm), Min = min(data[,x], na.rm = na.rm),
                          Max = max(data[,x], na.rm = na.rm))
    between <- data.frame(Variable = NA, Dim = "Between", Mean = NA, SD = sd(between_x, na.rm = na.rm),
                          Min = min(between_x, na.rm = na.rm), Max = max(between_x, na.rm = na.rm))
    within <- data.frame(Variable = NA, Dim = "Within",Mean = NA, SD = sd(within_x, na.rm = na.rm),
                         Min = min(within_x, na.rm = na.rm), Max = max(within_x, na.rm = na.rm))

    # Combine statistics into a single data.frame
    xtstats <- bind_rows(overal, between, within)

    return(xtstats)
  }, data_, simplify = FALSE, USE.NAMES = TRUE)

  # Set options for rendering NA values in the table
  opts <- options(knitr.kable.NA = "")

  # Return the summary table using knitr::kable
  return(knitr::kable(sum_ <- do.call(bind_rows, sum_)))

}

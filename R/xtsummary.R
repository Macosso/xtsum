


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
#' @param return.data.frame If the return object should be a dataframe
#' @param dec Number of significant digits to report
#'
#' @return A table summarizing statistics for each variable, including Mean, SD, Min, and Max,
#'         broken down into Overall, Between, and Within dimensions.
#'
#' @examples
#'
#' # Using a data.frame and specifying variables, id, it, na.rm, dec
#' data("nlswork", package = "sampleSelection")
#' xtsum(nlswork, "hours", id = "idcode", t = "year", na.rm = TRUE, dec = 6)
#'
#' # Using pdata.frame object without specifying a variable
#' data("Gasoline", package = "plm")
#' Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop.index = TRUE)
#' xtsum(Gas)
#'
#'
#' # Using regular data.frame with id and t specified
#' data("Crime", package = "plm")
#' xtsum(Crime, variables = c("crmrte", "prbarr"), id = "county", t = "year")
#'
#' # Specifying variables to include in the summary
#' xtsum(Gas, variables = c("lincomep", "lgaspcar"))
#'
#' @importFrom dplyr all_of select mutate group_by summarise ungroup pull sym bind_rows
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom plm pdata.frame index
#' @importFrom kableExtra kable_classic kbl
#' @importFrom sampleSelection treatReg
#' @export
xtsum <- function(data,
                  variables = NULL,
                  id = NULL,
                  t = NULL,
                  na.rm = FALSE,
                  return.data.frame = FALSE,
                  dec = 3) {
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
    numeric_cols <- colnames(data[,sapply(data, is.numeric)])
    variables <- numeric_cols[!numeric_cols %in% c(id, t)]
  }
  idt <- c(id,t)

  # Initialize an empty data.frame for the table results
  TableRes = data.frame(matrix(nrow = 0, ncol = 6))
  colnames(TableRes) <- c("Variable", "Dim", "Mean", "SD", "Min", "MAX")

  # Extract relevant data for specified variables
  data_ <- as.data.frame(data) %>%
    dplyr::select(dplyr::all_of(idt), dplyr::all_of(variables))

  # Use sapply to calculate summary statistics for each variable
  sum_ <- sapply(variables, xtsum_, data=data_,
                 id = id, t = t, na.rm = na.rm, dec = dec, simplify = FALSE, USE.NAMES = TRUE)

  # Set options for rendering NA values in the table
  opts <- options(knitr.kable.NA = "")

  if(return.data.frame){
    return(do.call(bind_rows, sum_))
  }
  else{
    # Return the summary table using kableExtra
    return(kableExtra::kbl(do.call(bind_rows, sum_)) %>% kableExtra::kable_classic())
  }

}


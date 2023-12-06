#' @importFrom dplyr all_of select mutate group_by summarise ungroup pull sym bind_rows bind_cols
#' @importFrom magrittr %>%
#' @importFrom plm pdata.frame index
#'
xtbetween <- function(data,
                      variable = NULL,
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
  idt <- c(id, t)

  data_ <- as.data.frame(data) %>%
    dplyr::select(dplyr::all_of(idt), dplyr::all_of(variable))

  data %>%
    dplyr::select(all_of(idt), all_of(variable)) %>%
    dplyr::group_by(!!sym(id)) %>%
    dplyr::summarise(Xi_mean = mean(!!sym(variable), na.rm = na.rm)) %>%
    dplyr::ungroup() %>%
    dplyr::pull("Xi_mean") -> between_

  return(between_)
}

#' Compute the minimum between-group
#'
#' This function calculates the minimum between-group of a panel data.
#'
#' @param data A data.frame or pdata.frame object containing the panel data.
#' @param variable The variable for which the minimum between-group effect is calculated.
#' @param na.rm Logical. Should missing values be removed? Default is FALSE.
#' @param id (Optional) Name of the individual identifier variable.
#' @param t (Optional) Name of the time identifier variable.
#'
#' @return The minimum between-group effect.
#'
#' @examples
#' # Example using pdata.frame
#' data("Gasoline", package = "plm")
#' Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop.index = TRUE)
#' between_min(Gas, variable = "lgaspcar")
#'
#' # Using regular data.frame with id and t specified
#' data("Crime", package = "plm")
#' between_min(Crime, variable = "crmrte", id = "county", t = "year")
#'
#' @export
between_min <- function(data, variable, id=NULL, t=NULL, na.rm = FALSE) {
  return(min(xtbetween(data, variable, id, t, na.rm = na.rm), na.rm = na.rm))
}

#' Compute the maximum between-group
#'
#' This function calculates the maximum between-group in a panel data.
#'
#' @param data A data.frame or pdata.frame object containing the panel data.
#' @param variable The variable for which the maximum between-group effect is calculated.
#' @param na.rm Logical. Should missing values be removed? Default is FALSE.
#' @param id (Optional) Name of the individual identifier variable.
#' @param t (Optional) Name of the time identifier variable.
#'
#' @return The maximum between-group effect.
#'
#' @examples
#' # Example using pdata.frame
#' data("Gasoline", package = "plm")
#' Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop.index = TRUE)
#' between_max(Gas, variable = "lgaspcar")
#'
#' # Using regular data.frame with id and t specified
#' data("Crime", package = "plm")
#' between_max(Crime, variable = "crmrte", id = "county", t = "year")
#'
#' @export
between_max <- function(data, variable, id=NULL, t=NULL, na.rm = FALSE) {
  return(max(xtbetween(data, variable, id, t, na.rm = na.rm), na.rm = na.rm))
}

#' Compute the standard deviation of between-group
#'
#' This function calculates the standard deviation of between-group in a panel data.
#'
#' @param data A data.frame or pdata.frame object containing the panel data.
#' @param variable The variable for which the standard deviation of between-group effects is calculated.
#' @param na.rm Logical. Should missing values be removed? Default is FALSE.
#' @param id (Optional) Name of the individual identifier variable.
#' @param t (Optional) Name of the time identifier variable.
#'
#' @return The standard deviation of between-group effects.
#'
#' @examples
#' # Example using pdata.frame
#' data("Gasoline", package = "plm")
#' Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop.index = TRUE)
#' between_sd(Gas, variable = "lgaspcar")
#'
#' # Using regular data.frame with id and t specified
#' data("Crime", package = "plm")
#' between_sd(Crime, variable = "crmrte", id = "county", t = "year")
#'
#' @export
between_sd <- function(data, variable, id=NULL, t=NULL, na.rm = FALSE) {
  return(stats::sd(xtbetween(data, variable, id=id, t=t, na.rm = na.rm), na.rm = na.rm))
}

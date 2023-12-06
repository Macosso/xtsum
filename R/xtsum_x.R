xtsum_ <- function(data,
                  variable = NULL,
                  id = NULL,
                  t = NULL,
                  na.rm = FALSE,
                  dec = 3){

  if(!"pdata.frame" %in% class(data)){
    data <- plm::pdata.frame(data, index = c(id,t), drop.index = TRUE)
  }

  N <- sum(!is.na(xtwithin(data, variable, na.rm = na.rm)))
  n <- sum(!is.na(xtbetween(data, variable, na.rm = na.rm)))
  `T` <- round(N/n, dec)

  x_list <- list(
    "________" = c("Mean" = NA, "SD" = NA, "Min" = NA, "Max" = NA, "Observations" = NA),
    "Overal" = c("Mean" = round(mean(data[,variable], na.rm = na.rm),dec),
                    "SD" = round(stats::sd(data[,variable], na.rm = na.rm),dec),
                    "Min" = round(min(data[,variable], na.rm = na.rm),dec),
                    "Max" = round(max(data[,variable], na.rm = na.rm),dec),
                    "Observations" = paste("N =",N)),
    "between" = c("SD" = round(between_sd(data, variable, na.rm = na.rm),dec),
                  "Min" = round(between_min(data, variable, na.rm = na.rm),dec),
                  "Max" = round(between_max(data, variable, na.rm = na.rm),dec),
                  "Observations" = paste("n =",n)),
    "within" = c("SD" = round(within_sd(data, variable, na.rm = na.rm),dec),
                  "Min" = round(within_min(data, variable, na.rm = na.rm),dec),
                  "Max" = round(within_max(data, variable, na.rm = na.rm),dec),
                  "Observations" = paste("T =",`T`)))
  return(bind_cols("Variable" = c("___________",variable, NA, NA),
                   "Dim" = c("_________", "overall", "between", "within"),
                   bind_rows(x_list)))
}

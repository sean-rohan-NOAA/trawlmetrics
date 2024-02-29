#' Build a mixed model 
#'
#' A function.
#' 
#' @param BCS_data Bottom contact dataframe output of get_bottom_data()
#' @export

get_model <- function() {

numeric_data <- BCS_data[, sapply(BCS_data, is.numeric)]
scaled_numeric_data <- scale(numeric_data)
scaled_data <- cbind(scaled_numeric_data, BCS_data[, !sapply(BCS_data, is.numeric)])

head(scaled_data)

bcs_lmer <- lmer(median.z ~ NET_NUMBER*YEAR + mean.x + (1|STATION), data = scaled_data)

output <- bcs_lmer


return(output)

}


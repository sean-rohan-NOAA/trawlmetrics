#' Build a mixed model 
#'
#' A function.
#' 
#' @param BCS_data Bottom contact dataframe output of get_bottom_data()
#' @export

get_model <- function(BCS_data = BCS_data) {

numeric_data <- BCS_data[, sapply(BCS_data, is.numeric)]
scaled_numeric_data <- scale(numeric_data)
scaled_data <- cbind(scaled_numeric_data, BCS_data[, !sapply(BCS_data, is.numeric)])

head(scaled_data)

bcs_z <- lmer(median.z ~ NET_NUMBER:YEAR + (1|STATION), data = scaled_data)
bcs_y <- lmer(median.y ~ NET_NUMBER:YEAR + (1|STATION), data = scaled_data)
bcs_x <- lmer(median.x ~ NET_NUMBER:YEAR + (1|STATION), data = scaled_data)

output <- list(bcs_z = bcs_z,
               bcs_y = bcs_y,
               bcs_x = bcs_x)


return(output)

}


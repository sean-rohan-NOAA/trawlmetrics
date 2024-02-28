#' Build a mixed model 
#'
#' A function.
#' 
#' @param channel Optional. An RODBC class ODBC connection
#' @param BCS_data Bottom contact dataframe output by get_bottom_data()
#' @export

numeric_data <- BCS_data[, sapply(BCS_data, is.numeric)]
scaled_numeric_data <- scale(numeric_data)
scaled_data <- cbind(scaled_numeric_data, BCS_data[, !sapply(BCS_data, is.numeric)])

head(scaled_data)

bcs_lmer <- lmer(median.z ~ NET_NUMBER*YEAR + mean.x + (1|STATION), data = scaled_data)

summary(bcs_lmer)

plot(residuals(bcs_lmer))
qqnorm(residuals(bcs_lmer))
qqline(residuals(bcs_lmer))

plot(bcs_lmer)
vif(bcs_lmer)



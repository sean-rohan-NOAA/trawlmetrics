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

#bcs_x <- gamm(median.x ~ NET_NUMBER:YEAR + s(STATION, bs = 're', by = dummy), data = scaled_data)
#bcs_y <- gamm(median.y ~ NET_NUMBER:YEAR + s(STATION, bs = 're', by = dummy), data = scaled_data)
#bcs_z <- gamm(median.z ~ NET_NUMBER:YEAR + s(STATION, bs = 're', by = dummy), data = scaled_data)

bcs_x <- lmer(median.x ~ NET_NUMBER:YEAR + (1|STATION), data = scaled_data)
bcs_y <- lmer(median.y ~ NET_NUMBER:YEAR + (1|STATION), data = scaled_data)
bcs_z <- lmer(median.z ~ NET_NUMBER:YEAR + (1|STATION), data = scaled_data) 

fit <- dplyr::select(BCS_data, YEAR, NET_NUMBER, STATION) |> unique()
fit$NET_YEAR <- paste(fit$YEAR, fit$NET_NUMBER, sep = "-")

fit$fit_x <- predict(bcs_x, newdata = fit)
fit$fit_z <- predict(bcs_z, newdata = fit)

# Summarize bottom contact by net_number and year
bottomcontact_netyear <- BCS_data |>
  dplyr::group_by(YEAR, NET_NUMBER, NET_YEAR) |>
  dplyr::summarise(MEAN_median.x = mean(median.x, na.rm = TRUE),
                   MIN_median.x = min(median.x, na.rm = TRUE),
                   MAX_median.x = max(median.x, na.rm = TRUE),
                   Q025_median.x = quantile(median.x, probs = 0.025, na.rm = TRUE),
                   Q975_median.x = quantile(median.x, probs = 0.975, na.rm = TRUE),
                   Q25_median.x = quantile(median.x, probs = 0.25, na.rm = TRUE),
                   Q75_median.x = quantile(median.x, probs = 0.75, na.rm = TRUE),
                   SD_median.x = sd(median.x, na.rm = TRUE),
                   MEAN_median.z = mean(median.z, na.rm = TRUE),
                   MIN_median.z = min(median.z, na.rm = TRUE),
                   MAX_median.z = max(median.z, na.rm = TRUE),
                   Q025_median.z = quantile(median.z, probs = 0.025, na.rm = TRUE),
                   Q975_median.z = quantile(median.z, probs = 0.975, na.rm = TRUE),
                   Q25_median.z = quantile(median.z, probs = 0.25, na.rm = TRUE),
                   Q75_median.z = quantile(median.z, probs = 0.75, na.rm = TRUE),
                   SD_median.z = sd(median.z, na.rm = TRUE))

#summarize model fit for bottom contact by net_number and year
bottomcontact_fit <- fit |>
  dplyr::group_by(YEAR, NET_NUMBER, NET_YEAR) |>
  dplyr::summarise(MEAN_FITX = mean(fit_x, na.rm = TRUE),
                   MIN_FITX = min(fit_x, na.rm = TRUE),
                   MAX_FITX = max(fit_x, na.rm = TRUE),
                   Q025_FITX = quantile(fit_x, probs = 0.025, na.rm = TRUE),
                   Q975_FITX = quantile(fit_x, probs = 0.975, na.rm = TRUE),
                   Q25_FITX = quantile(fit_x, probs = 0.25, na.rm = TRUE),
                   Q75_FITX = quantile(fit_x, probs = 0.75, na.rm = TRUE),
                   SD_FITX = sd(fit_x, na.rm = TRUE),
                   MEAN_FITZ = mean(fit_z, na.rm = TRUE),
                   MIN_FITZ = min(fit_z, na.rm = TRUE),
                   MAX_FITZ = max(fit_z, na.rm = TRUE),
                   Q025_FITZ = quantile(fit_z, probs = 0.025, na.rm = TRUE),
                   Q975_FITZ = quantile(fit_z, probs = 0.975, na.rm = TRUE),
                   Q25_FITZ = quantile(fit_z, probs = 0.25, na.rm = TRUE),
                   Q75_FITZ = quantile(fit_z, probs = 0.75, na.rm = TRUE),
                   SD_FITZ = sd(fit_z, na.rm = TRUE))

#summarize all data combined, all net/years
all_average <- BCS_data |>
  dplyr::summarise(MEAN_median.x = mean(median.x, na.rm = TRUE),
                   MIN_median.x = min(median.x, na.rm = TRUE),
                   MAX_median.x = max(median.x, na.rm = TRUE),
                   Q025_median.x = quantile(median.x, probs = 0.025, na.rm = TRUE),
                   Q975_median.x = quantile(median.x, probs = 0.975, na.rm = TRUE),
                   Q25_median.x = quantile(median.x, probs = 0.25, na.rm = TRUE),
                   Q75_median.x = quantile(median.x, probs = 0.75, na.rm = TRUE),
                   SD_median.x = sd(median.x, na.rm = TRUE),
                   MEAN_median.z = mean(median.z, na.rm = TRUE),
                   MIN_median.z = min(median.z, na.rm = TRUE),
                   MAX_median.z = max(median.z, na.rm = TRUE),
                   Q025_median.z = quantile(median.z, probs = 0.025, na.rm = TRUE),
                   Q975_median.z = quantile(median.z, probs = 0.975, na.rm = TRUE),
                   Q25_median.z = quantile(median.z, probs = 0.25, na.rm = TRUE),
                   Q75_median.z = quantile(median.z, probs = 0.75, na.rm = TRUE),
                   SD_median.z = sd(median.z, na.rm = TRUE))

#summarize model fit, all net/years
all_fit <- fit |>
  dplyr::summarise(MEAN_FITX = mean(fit_x, na.rm = TRUE),
                   MIN_FITX = min(fit_x, na.rm = TRUE),
                   MAX_FITX = max(fit_x, na.rm = TRUE),
                   Q025_FITX = quantile(fit_x, probs = 0.025, na.rm = TRUE),
                   Q975_FITX = quantile(fit_x, probs = 0.975, na.rm = TRUE),
                   Q25_FITX = quantile(fit_x, probs = 0.25, na.rm = TRUE),
                   Q75_FITX = quantile(fit_x, probs = 0.75, na.rm = TRUE),
                   SD_FITX = sd(fit_x, na.rm = TRUE),
                   MEAN_FITZ = mean(fit_z, na.rm = TRUE),
                   MIN_FITZ = min(fit_z, na.rm = TRUE),
                   MAX_FITZ = max(fit_z, na.rm = TRUE),
                   Q025_FITZ = quantile(fit_z, probs = 0.025, na.rm = TRUE),
                   Q975_FITZ = quantile(fit_z, probs = 0.975, na.rm = TRUE),
                   Q25_FITZ = quantile(fit_z, probs = 0.25, na.rm = TRUE),
                   Q75_FITZ = quantile(fit_z, probs = 0.75, na.rm = TRUE),
                   SD_FITZ = sd(fit_z, na.rm = TRUE))

plot_all <- merge(all_average, all_fit)
plot_all$YEAR <- "ALL"
plot_all$NET_NUMBER <- "ALL"
plot_all$NET_YEAR <- "ALL"

# Data frame for the year

plot_data <- merge(bottomcontact_netyear, bottomcontact_fit, by = 'NET_YEAR', all = TRUE)
plot_data = subset(plot_data, select = -c(NET_NUMBER.y, YEAR.y) )
plot_data <- plot_data |> dplyr::rename(NET_NUMBER = NET_NUMBER.x, YEAR = YEAR.x)
plotdata_all <- rbind(plot_data, plot_all)

output <- list(bcs_x = bcs_x,
               bcs_z = bcs_z,
               plotdata_all = plotdata_all)


return(output)

}


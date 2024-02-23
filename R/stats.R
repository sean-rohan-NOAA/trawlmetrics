library(dplyr)
install.packages(lme4)
library(lme4)
library(Matrix)

xstat <- bottom_contact_data %>% group_by(HAUL_ID) %>% summarize(min = min(X_AXIS),
                                              q1 = quantile(X_AXIS, 0.25),
                                              median = median(X_AXIS),
                                              mean = mean(X_AXIS),
                                              q3 = quantile(X_AXIS, 0.75),
                                              max = max(X_AXIS))

ystat <- bottom_contact_data %>% group_by(HAUL_ID) %>% summarize(min = min(Y_AXIS),
                                                        q1 = quantile(Y_AXIS, 0.25),
                                                        median = median(Y_AXIS),
                                                        mean = mean(Y_AXIS),
                                                        q3 = quantile(Y_AXIS, 0.75),
                                                        max = max(Y_AXIS))

zstat <- bottom_contact_data %>% group_by(HAUL_ID) %>% summarize(min = min(Z_AXIS),
                                                        q1 = quantile(Z_AXIS, 0.25),
                                                        median = median(Z_AXIS),
                                                        mean = mean(Z_AXIS),
                                                        q3 = quantile(Z_AXIS, 0.75),
                                                        max = max(Z_AXIS))
zstat <- zstat %>% rename(min.z = min,
                          q1.z = q1,
                      median.z = median,
                        mean.z = mean,
                          q3.z = q3,
                          max.z = max)

stats <- merge(xstat, ystat, by="HAUL_ID")
stats <- merge (stats, zstat, by="HAUL_ID")


bcs_lmer = lmer(mean.z + (1|STATION) + NET_NUMBER, data = full)

summary(bcs_lmer)


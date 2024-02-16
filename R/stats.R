xstat <- bottom_contact_full %>% group_by(HAUL_ID) %>% summarize(min = min(X_AXIS),
                                              q1 = quantile(X_AXIS, 0.25),
                                              median = median(X_AXIS),
                                              mean = mean(X_AXIS),
                                              q3 = quantile(X_AXIS, 0.75),
                                              max = max(X_AXIS))

ystat <- bottom_contact_full %>% group_by(HAUL_ID) %>% summarize(min = min(Y_AXIS),
                                                        q1 = quantile(Y_AXIS, 0.25),
                                                        median = median(Y_AXIS),
                                                        mean = mean(Y_AXIS),
                                                        q3 = quantile(Y_AXIS, 0.75),
                                                        max = max(Y_AXIS))

zstat <- bottom_contact_full %>% group_by(HAUL_ID) %>% summarize(min = min(Z_AXIS),
                                                        q1 = quantile(Z_AXIS, 0.25),
                                                        median = median(Z_AXIS),
                                                        mean = mean(Z_AXIS),
                                                        q3 = quantile(Z_AXIS, 0.75),
                                                        max = max(Z_AXIS))


library(plotly)

testplot <- subset(bottom_contact_full, bottom_contact_full$HAUL_ID == '6120',
                    select=c(DATE_TIME, X_AXIS, Y_AXIS, Z_AXIS))

onehaul <-

plot_ly(testplot,
        x = ~X_AXIS, 
        y = ~Y_AXIS, 
        z = ~Z_AXIS, 
        type = 'scatter3d', 
        mode = 'markers'
        
)

remove(badhaul)
remove(snags)
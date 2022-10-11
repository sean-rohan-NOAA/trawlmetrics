library(vegan)
library(ggplot2)
library(plotly)
library(lme4)

library(trawlmetrics)

channel <- trawlmetrics:::get_connected(schema = "AFSC")

survey = c("AI")
year = 2022

survey_id <- c(98, 143, 47, 52, 78, 6)[match(survey, c("EBS", "NBS", "GOA", "AI", "SLOPE", "CHUKCHI"))]


# survey_standard_gear

trawl_dat <- RODBC::sqlQuery(channel = channel,
                             query =
                               paste("select a.hauljoin, a.vessel, a.cruise, a.haul, a.net_measured, a.net_height, a.net_width, a.wire_length, a.bottom_depth, a.performance, a.gear, a.accessories,
a.stationid, a.start_time, d.net_number, d.footrope_number, d.autotrawl_method, d.starboard_door_number, d.port_door_number, d.haul_type, e.description gear_description, f.description performance_description
from
racebase.haul a, 
race_data.cruises b, 
race_data.surveys c, 
race_data.hauls d, 
race_data.gear_codes e, 
racebase.performance f
where c.survey_definition_id in (", paste(survey_id, collapse = ","), ")",
                                     "and b.survey_id = c.survey_id
and a.cruisejoin = b.racebase_cruisejoin
and d.cruise_id = b.cruise_id
and a.haul = d.haul
and e.gear_code = a.gear
and a.performance = f.performance"))

trawl_sel <- trawl_dat |>
  dplyr::mutate(YEAR = floor(CRUISE/100),
                SCOPE_RATIO = WIRE_LENGTH/BOTTOM_DEPTH) |>
  dplyr::filter(HAUL_TYPE %in% c(3, 13, 20), 
                           NET_MEASURED == "Y", 
                           !is.na(NET_HEIGHT), 
                           !is.na(NET_WIDTH))

  
trawl_sel$TRAWL_ID <- paste(trawl_sel$YEAR, trawl_sel$VESSEL, trawl_sel$NET_NUMBER, sep = "_")


# Scaled height and width
net_height_scaled <- scale(trawl_sel$NET_HEIGHT)
net_width_scaled <- scale(trawl_sel$NET_WIDTH)
net_scope_ratio <- scale(trawl_sel$SCOPE_RATIO)

trawl_sel$SCALED_NET_HEIGHT <- net_height_scaled
trawl_sel$SCALED_NET_WIDTH <- net_width_scaled
trawl_sel$SCALED_SCOPE_RATIO <- net_scope_ratio

trawl_mat <- cbind(net_height_scaled[,1],
                   net_width_scaled[,1],
                   net_scope_ratio[,1])


# Summarize height and width by trawl
trawl_dimensions_summary <- trawl_sel |>
  dplyr::group_by(YEAR, VESSEL, NET_NUMBER) |>
  dplyr::summarise(MEAN_NET_HEIGHT = mean(NET_HEIGHT),
                   MIN_NET_HEIGHT = min(NET_HEIGHT),
                   MAX_NET_HEIGHT = max(NET_HEIGHT),
                   Q025_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.025),
                   Q975_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.975),
                   Q25_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.25),
                   Q75_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.75),
                   SD_NET_HEIGHT = sd(NET_HEIGHT),
                   MEAN_NET_WIDTH = mean(NET_WIDTH),
                   MIN_NET_WIDTH = min(NET_WIDTH),
                   MAX_NET_WIDTH = max(NET_WIDTH),
                   Q025_NET_WIDTH = quantile(NET_WIDTH, probs = 0.025),
                   Q975_NET_WIDTH = quantile(NET_WIDTH, probs = 0.975),
                   Q25_NET_WIDTH = quantile(NET_WIDTH, probs = 0.25),
                   Q75_NET_WIDTH = quantile(NET_WIDTH, probs = 0.75),
                   SD_NET_WIDTH = sd(NET_WIDTH),
                   N_HAULS = dplyr::n(),
                   N_GOOD = sum(PERFORMANCE >= 0),
                   N_BAD = sum(PERFORMANCE < 0))

# Summarize height and width for all nets and all years
trawl_dimensions_all_years <- trawl_sel |>
  dplyr::summarise(MEAN_NET_HEIGHT = mean(NET_HEIGHT),
                   MIN_NET_HEIGHT = min(NET_HEIGHT),
                   MAX_NET_HEIGHT = max(NET_HEIGHT),
                   Q025_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.025),
                   Q975_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.975),
                   Q25_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.25),
                   Q75_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.75),
                   SD_NET_HEIGHT = sd(NET_HEIGHT),
                   MEAN_NET_WIDTH = mean(NET_WIDTH),
                   MIN_NET_WIDTH = min(NET_WIDTH),
                   MAX_NET_WIDTH = max(NET_WIDTH),
                   Q025_NET_WIDTH = quantile(NET_WIDTH, probs = 0.025),
                   Q975_NET_WIDTH = quantile(NET_WIDTH, probs = 0.975),
                   Q25_NET_WIDTH = quantile(NET_WIDTH, probs = 0.25),
                   Q75_NET_WIDTH = quantile(NET_WIDTH, probs = 0.75),
                   SD_NET_WIDTH = sd(NET_WIDTH),
                   N_HAULS = dplyr::n()) |>
  dplyr::mutate(NET_NUMBER = "All nets, all years")

# Data frame for the year
trawl_dimensions_year <- trawl_dimensions_summary |>
  dplyr::filter(YEAR == year)

net_number_year <- as.character(sort(unique(trawl_dimensions_year$NET_NUMBER)))

plot_dat <- trawl_dimensions_year |>
  dplyr::mutate(NET_NUMBER = as.character(NET_NUMBER)) |>
  dplyr::bind_rows(trawl_dimensions_all_years) |>
  dplyr::mutate(NET_NUMBER = factor(NET_NUMBER, 
                                    levels = c(net_number_year, "All nets, all years")))

plot_dat$W25 <- trawl_dimensions_all_years$Q25_NET_WIDTH
plot_dat$W75 <- trawl_dimensions_all_years$Q75_NET_WIDTH
plot_dat$H25 <- trawl_dimensions_all_years$Q25_NET_HEIGHT
plot_dat$H75 <- trawl_dimensions_all_years$Q75_NET_HEIGHT

cowplot::plot_grid(
ggplot() +
  geom_segment(data = plot_dat,
               mapping = aes(y = NET_NUMBER, 
                             yend = NET_NUMBER, 
                             x = MIN_NET_HEIGHT, 
                             xend = MAX_NET_HEIGHT,
                             color = MEAN_NET_HEIGHT > H25 & MEAN_NET_HEIGHT < H75)) +
  geom_segment(data = plot_dat,
               mapping = aes(y = NET_NUMBER, 
                             yend = NET_NUMBER, 
                             x = Q25_NET_HEIGHT, 
                             xend = Q75_NET_HEIGHT,
                             color = MEAN_NET_HEIGHT > H25 & MEAN_NET_HEIGHT < H75),
               size = 3) +
  geom_point(data = plot_dat,
               mapping = aes(y = NET_NUMBER, 
                             x = MEAN_NET_HEIGHT,
                             color = MEAN_NET_HEIGHT > H25 & MEAN_NET_HEIGHT < H75),
               size = 4,
             shape = 21,
             fill = "white") +
  geom_label(data = plot_dat,
            aes(y = NET_NUMBER,
                x = 0.8,
                color = MEAN_NET_HEIGHT > H25 & MEAN_NET_HEIGHT < H75,
                label = N_HAULS)) +
  scale_y_discrete(name = "Net Number") +
  scale_x_continuous(name = "Net Height (m)") +
  scale_color_manual(name = "Mean within 25-75% range?", 
                     values = c("red", "darkgreen"), 
                     labels = c("No", "Yes")) +
  theme_bw() +
  theme(legend.position = "bottom"),
ggplot() +
  geom_segment(data = plot_dat,
               mapping = aes(y = NET_NUMBER, 
                             yend = NET_NUMBER, 
                             x = MIN_NET_WIDTH, 
                             xend = MAX_NET_WIDTH,
                             color = MEAN_NET_WIDTH > W25 & MEAN_NET_WIDTH < W75)) +
  geom_segment(data = plot_dat,
               mapping = aes(y = NET_NUMBER, 
                             yend = NET_NUMBER, 
                             x = Q25_NET_WIDTH, 
                             xend = Q75_NET_WIDTH,
                             color = MEAN_NET_WIDTH > W25 & MEAN_NET_WIDTH < W75),
               size = 3) +
  geom_point(data = plot_dat,
             mapping = aes(y = NET_NUMBER, 
                           x = MEAN_NET_WIDTH,
                           color = MEAN_NET_WIDTH > W25 & MEAN_NET_WIDTH < W75),
             size = 4,
             shape = 21,
             fill = "white") +
  geom_label(data = plot_dat,
             aes(y = NET_NUMBER,
                 x = 10,
                 color = MEAN_NET_WIDTH > W25 & MEAN_NET_WIDTH < W75,
                 label = N_HAULS)) +
  scale_y_discrete(name = "Net Number") +
  scale_x_continuous(name = "Net Width (m)") +
  scale_color_manual(name = "Mean within 25-75% range?", 
                     values = c("red", "darkgreen"), 
                     labels = c("No", "Yes")) +
  theme_bw() +
  theme(legend.position = "bottom"))


# Make reports for each trawl

unique_trawls <- trawl_dimensions_year |>
  dplyr::select(YEAR, VESSEL, NET_NUMBER)

ii <- 1
# for(ii in 1:nrow(unique_trawls)) {
  
  sel_net <- dplyr::filter(trawl_sel, 
                YEAR == unique_trawls$YEAR[ii],
                VESSEL == unique_trawls$VESSEL[ii],
                NET_NUMBER == unique_trawls$NET_NUMBER[ii])
  
  
  sel_net_summary <- sel_net[ii, ] |>
    dplyr::inner_join(trawl_dimensions_year)
  
  
# }








sample_index <- sample(1:nrow(trawl_mat), size = 500, replace = FALSE)

trawl_dist <- dist(trawl_mat[sample_index,], method = "euclidean")


trawl_pca <- prcomp(trawl_dist)

trawl_sample <- trawl_sel[sample_index,]
trawl_sample$PC1 <- trawl_pca$x[,1]
trawl_sample$PC2 <- trawl_pca$x[,2]
trawl_sample$PC3 <- trawl_pca$x[,3]


ggplotly(
ggplot() +
  geom_point(data = trawl_sample, 
             mapping = aes(x = PC1, 
                           y = PC2, 
                           color = factor(YEAR), 
                           shape = factor(VESSEL),
                           text = paste(YEAR, VESSEL, HAUL, sep = "_")))
)


ggplotly(
ggplot() +
  geom_point(data = trawl_sel, 
             mapping = aes(x = NET_HEIGHT, 
                           y = NET_WIDTH, 
                           color = factor(YEAR), 
                           shape = factor(VESSEL),
                           text = paste(YEAR, VESSEL, HAUL, sep = "_")))
)


ggplotly(
  ggplot() +
    geom_point(data = trawl_sel, 
               mapping = aes(x = NET_HEIGHT, 
                             y = SCOPE_RATIO, 
                             color = factor(YEAR), 
                             shape = factor(VESSEL),
                             text = paste(YEAR, VESSEL, HAUL, sep = "_"))) +
    scale_y_continuous(limits = c(0,20))
)

dplyr::filter(trawl_sel, SCOPE_RATIO > 100)


cor(trawl_sample$NET_HEIGHT, trawl_sample$PC1)
cor(trawl_sample$NET_WIDTH, trawl_sample$PC2)
cor(trawl_sample$NET_WIDTH, trawl_sample$PC3)
cor(trawl_sample$SCOPE_RATIO, trawl_sample$PC3)
cor(trawl_sample$SCOPE_RATIO, trawl_sample$PC2)


ggplot() +
  geom_density2d_filled(data = trawl_sel, 
             mapping = aes(x = NET_HEIGHT, 
                           y = NET_WIDTH))

ggplot() +
  geom_density(data = trawl_sel,
             mapping = aes(x = SCALED_NET_HEIGHT)) +
  geom_density(data = trawl_sel |>
                 dplyr::filter(VESSEL == 162, CRUISE == 202201),
               mapping = aes(x = SCALED_NET_HEIGHT,
                             fill = factor(NET_NUMBER)),
               alpha = 0.5)


ggplot() +
  geom_boxplot(data = trawl_sel,
               mapping = aes(x = YEAR, y = NET_HEIGHT, group = YEAR))

ggplot() +
  geom_boxplot(data = trawl_sel,
               mapping = aes(x = YEAR, 
                             y = NET_WIDTH, 
                             group = YEAR))


ggplot() +
  geom_density(data = trawl_sel,
               mapping = aes(x = NET_HEIGHT)) |>
  geom_vline(data = )

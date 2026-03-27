library(tidyverse)

url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfSea=2024%7C&player_type=batter&batters_lookup%5B%5D=664728&type=details&game_date_gt=2024-03-01&game_date_lt=2024-09-30"

salvy <- read_csv(url)

glimpse(salvy)

salvy_2025 <- read_csv("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfSea=2025%7C&player_type=batter&batters_lookup%5B%5D=664728&type=details&game_date_gt=2025-03-01&game_date_lt=2025-09-30")

nrow(salvy_2025)

salvy_2025 %>%
  filter(!is.na(launch_speed), !is.na(launch_angle)) %>%
  mutate(outcome = case_when(
    events == "home_run"       ~ "Home run",
    events == "single"         ~ "Single",
    events == "double"         ~ "Double",
    events == "triple"         ~ "Triple",
    events %in% c("field_out", "force_out", "grounded_into_double_play",
                  "double_play", "fielders_choice_out") ~ "Out",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(outcome)) %>%
  ggplot(aes(x = launch_angle, y = launch_speed, color = outcome)) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c(
    "Home run" = "#174B8B",
    "Double"   = "#4A7FC1",
    "Triple"   = "#8AADD4",
    "Single"   = "#C09A5B",
    "Out"      = "#CCCCCC"
  )) +
  labs(
    title   = "Salvador Pérez — quality of contact, 2025",
    x       = "Launch angle (degrees)",
    y       = "Exit velocity (mph)",
    color   = NULL,
    caption = "Source: Baseball Savant · ProPlotFits"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title    = element_text(size = 16, face = "bold", color = "#174B8B"),
    plot.caption  = element_text(size = 8, color = "#888888"),
    legend.position = "top"
  )

avg_ev <- salvy_2025 %>%
  filter(!is.na(launch_speed)) %>%
  summarise(mean_ev = mean(launch_speed)) %>%
  pull(mean_ev)

salvy_2025 %>%
  filter(!is.na(launch_speed), !is.na(launch_angle)) %>%
  mutate(outcome = case_when(
    events == "home_run"       ~ "Home run",
    events == "single"         ~ "Single",
    events == "double"         ~ "Double",
    events == "triple"         ~ "Triple",
    events %in% c("field_out", "force_out", "grounded_into_double_play",
                  "double_play", "fielders_choice_out") ~ "Out",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(outcome)) %>%
  ggplot(aes(x = launch_angle, y = launch_speed, color = outcome)) +
  # Sweet spot zone
  annotate("rect",
           xmin = 8, xmax = 32, ymin = 95, ymax = Inf,
           fill = "#174B8B", alpha = 0.08
  ) +
  annotate("text",
           x = 20, y = 117, label = "sweet spot",
           size = 3, color = "#174B8B", alpha = 0.6, fontface = "italic"
  ) +
  geom_point(alpha = 0.7, size = 2.5) +
  # Average EV line
  geom_hline(
    yintercept = avg_ev,
    linetype   = "dashed",
    color      = "#888888",
    linewidth  = 0.5
  ) +
  annotate("text",
           x = -75, y = avg_ev + 1.5,
           label = paste0("avg EV: ", round(avg_ev, 1), " mph"),
           size = 3, color = "#888888", hjust = 0
  ) +
  # Home run cluster label
  annotate("text",
           x = 35, y = 108,
           label = "home runs cluster\n10–30° / 95+ mph",
           size = 3, color = "#174B8B", hjust = 0, fontface = "italic"
  ) +
  scale_color_manual(values = c(
    "Home run" = "#174B8B",
    "Double"   = "#4A7FC1",
    "Triple"   = "#8AADD4",
    "Single"   = "#C09A5B",
    "Out"      = "#CCCCCC"
  )) +
  scale_x_continuous(limits = c(-80, 80)) +
  scale_y_continuous(limits = c(20, 120)) +
  labs(
    title   = "Salvador Pérez — quality of contact, 2025",
    x       = "Launch angle (degrees)",
    y       = "Exit velocity (mph)",
    color   = NULL,
    caption = "Source: Baseball Savant · ProPlotFits"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title      = element_text(size = 16, face = "bold", color = "#174B8B"),
    plot.caption    = element_text(size = 8, color = "#888888"),
    legend.position = "top"
  )

# ggsave("salvy_quality_of_contact_2025.png",
#        height = 4,
#        width = 6,
#        unit = "in")


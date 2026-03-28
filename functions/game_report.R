library(tidyverse)
library(glue)

dir.create("report_cards", showWarnings = FALSE)

royals_game_report <- function(game_date) {
  season <- substr(game_date, 1, 4)
  url <- glue(
    "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
    "&hfSea={season}%7C&player_type=batter&hfTeam=KC%7C",
    "&type=details",
    "&game_date_gt={game_date}&game_date_lt={game_date}"
  )
  
  Sys.setenv("VROOM_TEMP_PATH" = "C:/Users/peter/Documents/proplotfits")
  tmp <- "C:/Users/peter/Documents/proplotfits/temp_game.csv"
  download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
  data <- read_csv(tmp, show_col_types = FALSE)
  file.remove(tmp)
  data
}


plot_game_report <- function(game_date, opponent) {
  
  # pull data
  game_data <- royals_game_report(game_date)
  
  # determine season from date
  season <- substr(game_date, 1, 4)
  
  # build contact summary
  plot_data <- game_data %>%
    filter(!is.na(launch_speed), !is.na(launch_angle)) %>%
    mutate(
      outcome = case_when(
        events == "home_run"       ~ "Jonrón",
        events == "single"         ~ "Sencillo",
        events == "double"         ~ "Doble",
        events == "triple"         ~ "Triple",
        events %in% c("field_out", "force_out", "grounded_into_double_play",
                      "double_play", "fielders_choice_out") ~ "Out",
        TRUE ~ NA_character_
      ),
      last_name = str_extract(player_name, "^[^,]+"),
      last_name = case_when(
        player_name == "Witt Jr., Bobby"     ~ "Witt Jr.",
        player_name == "Perez, Salvador"     ~ "Salvy",
        player_name == "Pasquantino, Vinnie" ~ "Pasquantino",
        TRUE ~ last_name
      )
    ) %>%
    filter(!is.na(outcome))
  
  # avg exit velo for reference line
  avg_ev <- game_data %>%
    filter(!is.na(launch_speed)) %>%
    summarise(mean_ev = mean(launch_speed)) %>%
    pull(mean_ev)
  
  # game result
  final <- game_data %>%
    filter(!is.na(home_score), !is.na(away_score)) %>%
    slice_max(at_bat_number, n = 1) %>%
    slice(1)
  
  home <- final$home_team
  away <- final$away_team
  home_score <- final$post_home_score
  away_score <- final$post_away_score
  
  result <- if_else(
    away_score > home_score,
    glue("KC {away_score}, {opponent} {home_score}"),
    glue("{opponent} {home_score}, KC {away_score}")
  )
  
  # palette
  royal_blue_dark  <- "#174B8B"
  gold             <- "#C09A5B"
  
  plot <- 
    ggplot(plot_data, aes(x = launch_angle, y = launch_speed, color = outcome)) +
    annotate("rect",
             xmin = 8, xmax = 32, ymin = 95, ymax = Inf,
             fill = royal_blue_dark, alpha = 0.08
    ) +
    annotate("text",
             x = 20, y = 120, label = "sweet spot",
             size = 3, color = royal_blue_dark, alpha = 0.6, fontface = "italic"
    ) +
    geom_point(alpha = 0.7, size = 3) +
    ggrepel::geom_text_repel(
      data = plot_data %>%
        filter(
          (launch_angle >= 8 & launch_angle <= 32 & launch_speed >= 95) |
            launch_speed < avg_ev
        ),
      aes(label = last_name),
      size               = 2.5,
      color              = "#444444",
      max.overlaps       = 20,
      box.padding        = 1.0,
      point.padding      = 0.5,
      force              = 3,
      force_pull         = 0.5,
      segment.color      = "#AAAAAA",
      segment.size       = 0.3,
      min.segment.length = 0
    ) +
    geom_hline(
      yintercept = avg_ev,
      linetype   = "dashed",
      color      = "gray40",
      linewidth  = 0.2
    ) +
    annotate("text",
             x = -80, y = avg_ev + 2,
             hjust = 0,
             label = paste0("velosalida media: ", round(avg_ev, 1), " mph"),
             size = 3, color = "#888888"
    ) +
    scale_color_manual(values = c(
      "Jonrón"   = royal_blue_dark,
      "Doble"    = "#4A7FC1",
      "Triple"   = "#8AADD4",
      "Sencillo" = gold,
      "Out"      = "#CCCCCC"
    )) +
    scale_x_continuous(limits = c(-80, 80)) +
    scale_y_continuous(limits = c(20, 120)) +
    labs(
      title   = glue("Royals vs. {opponent} · {game_date}"),
      subtitle = glue("Velosalida vs. lanzángulo · cada batazo · {result}"),
      x       = "Lanzángulo (grados)",
      y       = "Velosalida (mph)",
      color   = NULL,
      caption = "Fuente: Baseball Savant · ProPlotFits"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title       = element_text(size = 16, face = "bold", color = royal_blue_dark),
      plot.subtitle    = element_text(size = 9, color = "#555555"),
      plot.caption     = element_text(size = 8, color = "#888888"),
      legend.position  = "top",
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    filename = glue("report_cards/royals_{game_date}_{tolower(opponent)}.png"),
    width    = 6,
    height   = 5,
    dpi      = 300,
    bg       = "white"
  )
  
  return(plot)
}


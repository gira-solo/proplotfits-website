library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# Pull state boundaries for MO and KS
state_borders <- states(cb = TRUE, year = 2020) %>%
  filter(STUSPS %in% c("MO", "KS")) %>%
  st_transform(crs = 4326)


# -------------------------------------------------------------
# Census variable: B16001_003E
# "Speak Spanish" — from table B16001 (Language Spoken at Home)
# ACS 5-year estimates
# -------------------------------------------------------------

# Set your Census API key if not already done:
census_api_key("187f5aac4952d3a712229e1f84b52a31d52ee628", install = TRUE)

kc_counties <- list(
  MO = c("Jackson", "Clay", "Platte", "Cass"),
  KS = c("Johnson", "Wyandotte", "Leavenworth")
)

pull_hispanic_pop <- function(state, counties) {
  get_decennial(
    geography = "tract",
    variables = c(hispanic_pop = "P2_002N"),
    state     = state,
    county    = counties,
    year      = 2020,
    sumfile   = "dhc",
    geometry  = TRUE
  )
}

kc_mo <- pull_hispanic_pop("MO", kc_counties$MO)
kc_ks <- pull_hispanic_pop("KS", kc_counties$KS)

# Dissolve county tracts to state-level outlines (only your 7 counties)
mo_outline <- kc_mo %>% st_union()
ks_outline <- kc_ks %>% st_union()

kc_metro <- bind_rows(kc_mo, kc_ks) %>%
  st_transform(crs = 4326)

# -------------------------------------------------------------
# Royal Blue palette
# Official KC Royals Royal Blue: #174B8B
# Build a sequential ramp from near-white to full Royal Blue
# -------------------------------------------------------------

royal_blue_dark  <- "#174B8B"
royal_blue_light <- "#D6E4F5"

ggplot(kc_metro) +
  geom_sf(
    aes(fill = value),
    color     = "white",
    linewidth = 0.15
  ) +
  geom_sf(
    data      = mo_outline,
    fill      = NA,
    color     = royal_blue_dark,
    linewidth = 0.6
  ) +
  geom_sf(
    data      = ks_outline,
    fill      = NA,
    color     = royal_blue_dark,
    linewidth = 0.6
  ) +
  scale_fill_gradient(
    low      = royal_blue_light,
    high     = royal_blue_dark,
    na.value = "#EEEEEE",
    name     = "Hispanic Population",
    labels   = scales::comma
  ) +
  labs(
    title    = "Population of Hispanic Descent across\nthe KC metro by Census Tract",
    subtitle = "",
    caption  = "Source: 2020 Census · Table P2_002N · ProPlotFits"
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title      = element_text(size = 16, face = "bold",
                                   color = royal_blue_dark, margin = margin(b = 6)),
    plot.subtitle   = element_text(size = 10, color = "#555555", margin = margin(b = 12)),
    plot.caption    = element_text(size = 8,  color = "#888888", margin = margin(t = 10)),
    legend.position = "right",
    legend.title    = element_text(size = 9,  color = "#333333"),
    legend.text     = element_text(size = 8,  color = "#555555"),
    plot.margin     = margin(16, 16, 16, 16)
  )

library("tidyverse")
library("dplyr")
library("stringr")
library("ggplot2")
library("plotly")
library("leaflet")
library("ggmap")
library("maps")
library("mapproj")
library("patchwork")
library("openintro")
library("lintr")

# load the data about incarceration trends
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# total black population aged between 15 - 64 from 1970 to 2018
prison_admission <- incarceration_trends %>%
  group_by(year) %>%
  select(black_pop_15to64, white_pop_15to64, black_prison_adm,
         white_prison_adm) %>%
  summarize(total_black = sum(black_pop_15to64, na.rm = TRUE),
            total_white = sum(white_pop_15to64, na.rm = TRUE),
            black_prison = sum(black_prison_adm, na.rm = TRUE),
            white_prison = sum(white_prison_adm, na.rm = TRUE)
            ) %>%
  mutate(black_prison_admission_rate = black_prison / total_black,
         white_prison_admission_rate = white_prison / total_white) %>%
  select(year, black_prison_admission_rate, white_prison_admission_rate) %>%
  filter_all(all_vars(is.finite(.)))
  

# Black race Prison Admission Population according to the county
black_pri_rate_state <- incarceration_trends %>%
  group_by(state) %>%
  summarize(total_black_prison_pop = sum(black_prison_pop, na.rm = TRUE),
            total_state_black_pop = sum(black_pop_15to64, na.rm = TRUE)) %>%
  mutate(state_black_prison_rate = total_black_prison_pop /
            total_state_black_pop) %>%
  select(state, state_black_prison_rate)

# A chart that shows trends over time for a variable
trends <- ggplot() +
  geom_line(data = prison_admission, aes(x = year,
                                        y = black_prison_admission_rate,
                      colour = "black race prison admission rate"), size = 1) +
  geom_line(data = prison_admission, aes(x = year,
                                      y = white_prison_admission_rate,
                      colour = "white race prison admission rate"), size = 1) +
  scale_colour_manual("", values =
                        c("black race prison admission rate" = "#330066",
                          "white race prison admission rate" = "#336699")) +
  xlab("Year") +
  ylab("rate") +
  theme(text = element_text(size = 13, family = "Comic Sans MS")) +
  ggtitle("black race versus white race incarceration")
trends

# A chart that compares two variables to one another
chart <- ggplot(data = black_pri_rate_state) +
  geom_col(mapping = aes(x = state, y = state_black_prison_rate),
           fill = "#CC99FF") +
  xlab("State") +
  ylab("black race prison admission rate") +
  labs(
    title = "The Rate of Prison Admission for Black Race with States",
    subtitle = "From 1990-2018",
    x = "State",
    y = "black race prison admission rate",
  ) +
  coord_flip()
chart


# A map that shows black people incarceration rate varies geographically
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

state_shape <- map_data("state") %>%
  mutate(state = state2abbr(region)) %>%
  left_join(black_pri_rate_state, by = "state")

rate_by_state <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group,
                  fill = state_black_prison_rate),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#120B46", high = "Pink") +
  labs(titles = "black race incarceration rate by state",
       fill = "black race incarceration rate") +
  blank_theme
rate_by_state
# 5 values using dplyr

max_black_pri_adm_year <- incarceration_trends %>%
  filter(black_prison_adm == max(black_prison_adm, na.rm = TRUE)) %>%
  pull(year)
max_black_pri_adm_year

max_black_pri_adm <- incarceration_trends %>%
  filter(black_prison_adm == max(black_prison_adm, na.rm = TRUE)) %>%
  pull(black_prison_adm)
max_black_pri_adm

recent_black_pri_adm_pop <- prison_admission %>%
  filter(year == "2016") %>%
  pull(black_prison_admission_rate)
recent_black_pri_adm_pop

recent_white_pri_adm_pop <- prison_admission %>%
  filter(year == "2016") %>%
  pull(white_prison_admission_rate)
recent_white_pri_adm_pop

max_black_pri_rate_state <- black_pri_rate_state %>%
  filter(state_black_prison_rate == max(state_black_prison_rate)) %>%
  pull(state)
max_black_pri_rate_state

library(sf)
library(mapSpain)
library(readr)
library(dplyr)
library(glue)
library(ggplot2)
library(ggtext)
library(ragg)


# load Spain vaccination data
esp_data <- read_csv("day01/day01.csv")

# load map of Spain
sf_spain <- esp_get_ccaa() %>%
  st_transform(crs = 25830) %>%
  # Exclude Canary Islands for plotting - remove this to include
  filter(ine.ccaa.name != "Canarias")

# Create grid
# need to set seed for reproducibility
set.seed(1234)
r <-
  st_sample(sf_spain,
            size = 80000,
            # Number of cells - increase for more definition
            type = "regular",
            exact = TRUE)
plot(r)


# Turn grid object to data.frame
esp_tiles <- r %>%
  st_coordinates() %>%
  as.data.frame()

colnames(esp_tiles) <- c("x", "y")

esp_tiles <- esp_tiles %>%
  arrange(y) %>%
  # Here we split the x-y coordinates according to % of each (non/)vaccinated group
  mutate(id = 1:nrow(.),
         group_name = as.factor(
           case_when(
             id <= nrow(.) * esp_data$percent_second ~ "second",
             id >= nrow(.) * esp_data$percent_second &
               id <= nrow(.) * esp_data$percent_first ~ "first",
             TRUE ~ "nonvac"
           )
         ))


# Identify percent labels as y-axis
id_to_filter <- data.frame(
  id = 1:nrow(esp_tiles),
  point_to_label = as.character(
    cut(
      esp_tiles$id,
      breaks = quantile(esp_tiles$id, probs = 0:5 / 5),
      labels = paste0(seq(0, 80, 20), "%"),
      include.lowest = TRUE
    )
  ) %>%
    ifelse(duplicated(.), NA, .)
)


pct_labels <- inner_join(esp_tiles, id_to_filter) %>%
  filter(!is.na(point_to_label)) %>%
  mutate(
    start_line = min(esp_tiles$x) - 200000,
    # We want the line to end a little before the shape of the map, which means where X is at minimum.
    # The 0 is a place holder for the first value
    end_line = with(esp_tiles[esp_tiles$y %in% .$y, ], tapply(x, y, min)) + 5000
  ) %>%
  filter(id != 1)

# Vertical bars
ver_bars <- esp_tiles %>%
  group_by(group_name) %>%
  summarise(yend = max(y))

ver_bars$y <- min(esp_tiles$y)
# Add some white space on the side of the map
ver_bars$x <- max(esp_tiles$x) + 70000
ver_bars$xend <- ifelse(ver_bars$group_name == "first",
                        ver_bars$x,
                        ver_bars$x + 20e4)


# Same idea but for easier reading in the plot itself
hor_bars <- ver_bars

# Colors info
aes_details <- data.frame(
  group_name = c("nonvac", "first", "second"),
  group_color = c("gray70", "#1ab0d4", "#1a698a")
)


# Position of annotations
text_pos <- data.frame(
  # Find the middle location between where the segments starts and ends
  transmute(ver_bars, y = (yend - y) / 2 + y),
  group_name = ver_bars$group_name,
  x = ver_bars$xend + 4e3,
  group_color = aes_details$group_color,
  # Create label using {glue} and the downloaded df so it's easy to update when necessary
  label = c(
    glue(
      "{round(esp_data$first/1e6, 1)}M <br> <span style='color:{aes_details$group_color[2]}'><b>target<br>({round({esp_data$percent_first}*100, 1)}%)</b></span>"
    ),
    glue(
      "{round(esp_data$nonvac/1e6, 1)}M vaccines <br><span style='color:gray50'><b>not yet received ({round({1 - esp_data$percent_second}*100, 1)}%)</b></span>"
    ),
    glue(
      "{round(esp_data$second/1e6, 1)}M<br>vaccines<br><span style='color:{aes_details$group_color[3]}'><b>administered<br>({round({esp_data$percent_second}*100, 1)}%)</b></span>"
    )
  )
)

# Plot----

p <- ggplot(esp_tiles) +
  # The tiles that fill the map
  geom_tile(aes(x = x, y = y, fill = group_name),
            size = .3,
            show.legend = FALSE) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(
      size = 32,
      face = "bold",
      family = "Arial",
      hjust = 0
    ),
    # adjust subtitle space between lines
    plot.subtitle = element_markdown(
      size = 18,
      color = "gray25",
      lineheight = 1.2
    ),
    plot.caption = element_text(
      color = "gray50",
      hjust = 0,
      size = 11
    ),
    plot.margin = margin(8, 6, 6, 8, "mm"),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_x_continuous(limits = c(min(pct_labels$start_line) - 1e4,
                                max(text_pos$x) + 50e4)) +
  scale_fill_manual(values = c(
    "nonvac" = "gray65",
    "first" = "#1ab0d4",
    "second" = "#1a698a"
  )) +
  geom_segment(
    data = pct_labels,
    aes(
      x = start_line,
      xend = end_line,
      y = y,
      yend = y
    ),
    color = "gray80",
    linetype = "dashed"
  ) +
  # Percent labels
  geom_text(
    data = pct_labels,
    aes(x = start_line - 10000, y = y, label = point_to_label),
    size = 4.5,
    color = "gray60",
    family = "Arial"
  ) +
  # vertical bars on the side for each group
  geom_segment(data = ver_bars,
               aes(
                 x = xend,
                 xend = xend,
                 y = y,
                 yend = yend
               ),
               color = "gray70") +
  # horizontal bars with the *minimum* y value (y at minimum for each group)
  geom_segment(data = hor_bars[hor_bars$group_name != "nonvac", ],
               aes(
                 x = xend - 10000,
                 xend = xend,
                 y = y,
                 yend = y
               ),
               color = "gray70") +
  # horizontal bars with the *maximum* y values (y at max for each group)
  geom_segment(data = hor_bars,
               aes(
                 x = xend - 10000,
                 xend = xend,
                 y = yend,
                 yend = yend
               ),
               color = "gray70") +
  # Annotation text
  geom_richtext(
    data = text_pos,
    aes(x = x, y = y, label = label),
    fill = NA,
    label.color = NA,
    hjust = 0,
    size = 6,
    color = "gray55",
    family = "Arial"
  ) +
  labs(
    title = "Australia's Vaccine Admission",
    subtitle = glue(
      "With a population of 25.7M, up to {round(esp_data$population/1e6, 1)}M vaccines could be administered in Australia.<br><b><span style='color:{aes_details$group_color[2]}'>{round(esp_data$first/1e6, 1)}M administered vaccines was the target by 31 March 2021 ({round(esp_data$percent_first*100, 1)}%)</span></b>.<br>But only <b><span style='color:{aes_details$group_color[3]}'>{round(esp_data$second/1e6, 1)}M vaccines were administered by 31 March 2021 ({round(esp_data$percent_second*100, 1)}%)</span></b>.<br>Color represents portion of group out of the whole shape, and not of where vaccinated individuals reside."
    ),
    # Take the most up to date from the esp_data df we created above
    caption = glue(
      "Data: Covid19data.com.au | {format(esp_data$date, '%B %d, %Y')} | Viz: @AndyBridger | Inspiration: @Amit_Levinson"
    )
  )




# save chart
ggsave("day01/day01_esp.png", p, height = 10, width = 15, device = ragg::agg_png())


library(tidyverse)
library(tidytext)
library(ggfx)
library(ggimage)
library(ggtext)
library(showtext)
library(colorspace)

dial <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

sent <- dial %>% 
  select(season, episode, dialogue) %>% 
  tidytext::unnest_tokens('word', dialogue) %>% 
  na.omit() %>% 
  filter(!word %in% tidytext::get_stopwords()$word) %>% 
  left_join(get_sentiments('afinn'), by = 'word') %>% 
  na.omit() %>%
  group_by(season, episode) %>%
  summarise(sentiment = sum(value), .groups = 'drop_last') %>% 
  mutate(
    sentiment = cumsum(sentiment),
    lead_episode = lead(episode),
    lead_sentiment = lead(sentiment)
  )

paint_candle <- function(.season, .color = "red"){
  with_outer_glow(
    expand = 10, colour = .color, sigma = 21,
    with_inner_glow(
      expand = 10, colour = .color, sigma = 21,
      geom_image(
        data = sent %>% filter(season == .season), 
        image='2022/candle.png',
        nudge_y = -14
      )
    )
  )
}

font_add_google("Heebo", "heebo")
font_add_google("Libre Baskerville", "lb")
showtext_auto()
ft <- "heebo"
ft1 <- "lb"
tc <- darken("tomato", .3)

sent %>%
  # filter(season == 1) %>%
  ggplot(aes(y = sentiment, x = episode)) +
  paint_candle(1, 'darkgreen') +
  paint_candle(2, 'skyblue') +
  paint_candle(3, 'gold') +
  paint_candle(4, 'tomato') +
  # geom_point(aes(color = as.factor(season))) +
  geom_curve(
    aes(x = episode, y = sentiment, xend = lead_episode, yend = lead_sentiment, color = as.factor(season))
  ) +
  scale_colour_manual( values = c("1" = "darkgreen", "2" = "skyblue", "3" = "gold", "4" = "tomato") ) +
  scale_x_continuous(breaks = 1:9, limits = c(NA, 9)) +
  annotate("text", x = 0.7, y = 300, label = "Positive", angle = 0, size = 3, color = tc) +
  annotate("text", x = 0.7, y = -300, label = "Negative", angle = 0, size = 3, color = tc) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  ylim(-530, NA) +
  labs(
    title = "<img src='2022/stranger_things_logo.png' height = 180>",
    x = 'Episodes',
    color = 'Seasons',
    caption = '@rodserrr | Source: 8flix.com | #tidytuesday',
    subtitle = "Cumulative sentiment score throughout the seasons."
  ) +
  theme(
    plot.margin = margin(t = 0, b = 15, l = 50, r = 50),
    plot.title = element_markdown(hjust = 0.5, margin = margin(b=-10, t = -5)),
    plot.subtitle = element_text(hjust = .5, margin = margin(b = 15)),
    plot.caption = element_text(margin = margin(t = 5)),
    plot.background = element_rect(fill = "grey9", color = NA),
    legend.position = 'top', 
    legend.justification = 'center',
    text = element_text(color = tc, family = ft1, size = 12),
    axis.text.x = element_text(),
    axis.title.x = element_text(margin = margin(t = 10), lineheight = 0.3),
  )

ggsave('2022/w42-strangerthings/w42.png', dpi = 320, width = 12, height = 14)



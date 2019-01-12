
# plot results ----------------------------------------------------------

library(ggplot2)

tbl_bm <- readRDS(file = "benchmark/tbl_bm2.Rds")

tbl_bm %>%
  mutate(num_obs = num_obs) %>%
  ggplot() +
  geom_line(aes(y = mean, x = num_obs, col = expr)) +
  theme_bw() +
  theme(
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15) 
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = 1:10*10^6) +
  labs(
    title    = "Linear Model Implementations on in-memory Data",
    y        = "Mean Seconds",
    x        = "Number of Observations",
    caption  = "Mean seconds "
  )
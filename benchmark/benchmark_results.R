
# plot results ----------------------------------------------------------

library(ggplot2)

result_glm_psql %>%
  mutate(num_obs = num_obs) %>%
  ggplot() +
  geom_line(aes(y = mean, x = num_obs, col = expression)) +
  theme_bw() +
  theme(
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15) 
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  # scale_x_continuous(breaks = 1:10*10^6) +
  labs(
    title    = "Linear Model Implementations on PostgreSQL Feed",
    y        = "Mean Seconds",
    x        = "Number of Observations",
    caption  = "Mean seconds "
  )
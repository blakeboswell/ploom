
# plot results ----------------------------------------------------------

library(rlang)
library(ggplot2)
library(RColorBrewer)


files <- list.files("benchmark/results", full.names = TRUE)


model_color <- rep(brewer.pal(4, "Set1"), each = 2) %>%
  set_names(
    "lm"
    , "glm"
    , "oomlm"
    , "oomglm"
    , "biglm"
    , "bigglm"
    , "speedlm"
    , "speedglm"
  )

title_subtitle <- function(file_name) {

  y <- tibble(
    backend  = c("tbl", "psql"),
    model    = c("glm", "lm"),
    oom      = c(FALSE, TRUE),
    title    = c("Generalized Linear Model", "Linear Model"),
    prefix   = c("In-Memory", "Out-of-Memory"),
    subtitle = str_c(c("tibble", "PostgreSQL"), " Backend")
  )
  
  x   <- str_split(str_remove(file_name, ".Rds"), "_", simplify = TRUE)
  
  title <- y %>%
    filter(model == x[1]) %>%
    pull(title) %>%
    head(1)
  
  subtitle <- y %>%
    filter(backend == x[2]) %>%
    pull(subtitle) %>%
    head(1)
  
  prefix <- y %>%
    filter(backend == x[2]) %>%
    pull(prefix) %>%
    head(1)
  
  list(
    title    = str_c(prefix, " ", title),
    subtitle = subtitle
  )
    
}

plot_benchmark <- function(df, nm, yval, ylab) {
  
  lbls <- title_subtitle(nm)
  
  df %>%
    mutate(num_obs = num_obs) %>%
    ggplot(aes_string(y = yval, x = "num_obs", col = "expression")) +
    geom_line()  +
    geom_point() +
    theme_bw()   +
    theme(
      panel.grid =  element_blank(),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 15) 
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_colour_manual(name = "expression", values = model_color) +
    labs(
      title    = lbls$title,
      subtitle = lbls$subtitle,
      x        = "Number of Observations",
      y        = ylab
    )
  
}


results <- map(files, readRDS) %>%
  map(~.x %>% mutate(mem_alloc = mem_alloc / 10^9)) %>%
  set_names(str_remove(basename(files), ".Rds"))


time_plots <- map2(results, names(results), plot_benchmark,
                   yval = "mean", ylab = "Mean Seconds")

memr_plots <- map2(results, names(results), plot_benchmark,
                   yval = "mem_alloc", ylab = "Memory Allocated")



memr_plots[[1]]
memr_plots[[2]]
memr_plots[[3]]
memr_plots[[4]]


map_chr(files, title_subtitle)



#Create a custom color scale


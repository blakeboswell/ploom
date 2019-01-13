
# plot results ----------------------------------------------------------


library(ggplot2)
library(RColorBrewer)
library(grid)

files <- list.files("benchmark/results", full.names = TRUE)


model_color <- rep(brewer.pal(4, "Set2"), each = 2) %>%
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
    subtitle = str_c("Data stored in ", c("tibble", "PostgreSQL table"), " ")
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
  
  df <- df %>%
    mutate(
      label = if_else(
        num_obs == max(num_obs),
        as.character(expression), NA_character_)
    ) 
  
  gt <- df %>%
    ggplot(aes_string(y = yval, x = "num_obs", col = "expression")) +
    geom_line(size = 1)  +
    geom_point(size = 2) +
    theme_bw()   +
    theme(
      plot.title    = element_text(size = 18),
      plot.subtitle = element_text(size = 14),
      plot.margin   = margin(t = 10, r = 45, b = 20, l = 25),
      panel.border  = element_blank(),
      axis.title    = element_text(size = 12),
      axis.title.y  = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x  = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
      axis.text     = element_text(size = 10),
      axis.ticks.y  = element_blank(),
      legend.position = "none"
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 5),
      labels = function(x) str_c(x, ifelse(yval == "mean", "", " gb"))
    ) +
    scale_colour_manual(
      name   = "expression",
      values = model_color
    ) +
    geom_text(
      data = df %>%
        filter(!is.na(label)) %>%
        mutate(num_obs = num_obs*1.05),
      aes_string(
        label = "label",
        x     = Inf,
        y     = yval,
        color = "expression"
      ),
      size = 5
    ) +
    labs(
      title    = lbls$title,
      subtitle = lbls$subtitle,
      x        = "Number of Observations",
      y        = ylab
    ) 
  
  gt <- ggplot_build(gt)
  gt <- ggplot_gtable(gt)
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  
  gt
  
}

save_plot <- function(gt_name, gt) {
  svg(
    filename = glue("benchmark/results/{gt_name}.svg"),
    width = 11, height = 8.5
  )
  grid.draw(gt)
  dev.off()
}


results <- map(files, readRDS) %>%
  map(~mutate(.x, mem_alloc = mem_alloc / 10^9)) %>%
  set_names(str_remove(basename(files), ".Rds"))

time_plots <- map2(results, names(results), plot_benchmark,
                   yval = "mean", ylab = "Mean Seconds")

walk2(.x = names(time_plots), .y = time_plots, save_plot)


memr_plots <- map2(results, names(results), plot_benchmark,
                   yval = "mem_alloc", ylab = "Memory Allocated")



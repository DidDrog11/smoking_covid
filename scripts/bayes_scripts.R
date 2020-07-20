extract_TE <- function(dataset) {
  tibble(dataset$studlab,
         dataset$TE,
         dataset$seTE
         ) %>%
    rename(Author = 1, TE = 2, seTE = 3)
}

bayes_test <- function(dataset, defined_prior, iterations = 4000) {
  brm(
    TE | se(seTE) ~ 1 + (1 | Author),
    data = dataset,
    prior = defined_prior,
    iter = 4000,
    control = list(adapt_delta = 0.99)
  )
}

post_samples <- function(dataset) {
  p_samples <- posterior_samples(dataset, c("^b", "^sd")) %>%
    rename("TE" = 1,
           "tau" = 2)
  print(ggplot(aes(x = TE), data = p_samples)+
          geom_density(fill = "lightblue",
                       color = "lightblue",
                       alpha = 0.7) +
          geom_point(y = 0, x = mean(p_samples$TE)) +
          labs(x = expression(italic(TE)),
               y = element_blank()) +
          theme_minimal())
  return(p_samples)
}

study_draw <- function(dataset) {
  spread_draws(dataset, r_Author[Author,], b_Intercept) %>%
    mutate(b_Intercept = r_Author + b_Intercept) %>%
    left_join(., study_review_version)
}

pooled_effect_draw <- function(dataset) {
  spread_draws(dataset, b_Intercept) %>%
    mutate(Author = "Pooled Effect",
           review_version = factor("combined_pooled"))
}

forest_plot <- function(model, data_study, data_pooled, title, type, filename) {
  forest_data <- bind_rows(data_study, data_pooled) %>%
    ungroup() %>%
    mutate(Author = str_replace_all(Author, "[.]", " ")) %>%
    mutate(Author = reorder(Author, b_Intercept))
  summary_forest <- group_by(forest_data, Author, review_version) %>%
    mean_qi(b_Intercept)
  graph <- ggplot(aes(b_Intercept, relevel(Author, "Pooled Effect", after = Inf), fill = review_version),
                  data = forest_data) +
    geom_vline(xintercept = fixef(model)[1, 1],
               color = "grey",
               size = 1) +
    geom_vline(xintercept = fixef(model)[1, 3:4],
               color = "grey",
               linetype = 2) +
    geom_vline(xintercept = 0,
               color = "black",
               size = 1) +
    geom_density_ridges(
      aes(fill = review_version),
      rel_min_height = 0.01,
      col = NA,
      scale = 1,
      alpha = 0.8
    ) +
    geom_pointinterval(data = summary_forest, size = 1) +
    geom_text(
      data = mutate_if(summary_forest, is.numeric, round, 2),
      aes(
        label = glue("{b_Intercept} [{.lower}, {.upper}]"),
        x = Inf
      ),
      hjust = "inward"
    ) +
    facet_grid(review_version~  ., scales = "free_y", space = "free") +
    labs(x = "log Relative Risk",
         y = element_blank(),
         title = title,
         caption = type
         ) +
    scale_fill_discrete(name = "Review Version", labels = c("Previous versions", "Current version", "All versions")) +
    theme_minimal() +
    theme(panel.spacing = unit(0.1, "lines"),
          strip.text = element_blank())
  
  outputs <- list("plot" = graph, "data" = forest_data, "summary" = summary_forest)
  ggsave(filename = filename, plot = graph, device = "png", path = here("reports", "figure"))
  return(outputs)
}

save_plots <- function(plot_name) {
  png(here::here('reports', 'figure', paste(plot_name, ".png", sep = "")), width=1024, height=546, res=120)
}
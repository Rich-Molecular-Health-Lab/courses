list_scores <- function(x, points) {
  assign(x, points) %>%
    set_names(x)
}



holc_tbl <- function(data, title) {
  card(
    card_header(title),
    reactable(
      data             = data,
      theme            = nytimes(),
      compact          = TRUE,
      pagination       = FALSE,
      defaultColDef    = colDef(
        align    = 'center',
        cell     = data_bars(
          data          = data,
          text_position = 'outside-end',
          box_shadow    = TRUE,
          fill_color    = rev(MetBrewer::met.brewer("Nizami")),
          number_fmt    = scales::label_percent()
        )
      ),
      columns          = list(
        Class = colDef(
          name   = "Class",
          sticky = "left"
        ),
        "density (1,000 persons/km2)" = colDef(
          cell = bubble_grid(
            data       = data,
            shape      = 'squares',
            colors     = rev(MetBrewer::met.brewer("Nizami")),
            box_shadow = TRUE,
            number_fmt = scales::label_number(accuracy = 0.1)
          )
        )
      )
    ),
    card_footer(
      class = "text-muted",
      "Values shown as difference from the City's Overall Average"
    )
  )
}
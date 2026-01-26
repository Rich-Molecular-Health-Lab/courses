case_convos <- list(
  "Case Conversation 1" = list(
    topic   = "Extinction",
    day     = "W4D2",
    leaders = c("Francesca Kerkhofs", "Zipporah Piper", "Sam Dillon")
  ),
  "Case Conversation 2" = list(
    topic = "Habitat Loss, Fragmentation, and Degradation",
    day   = "W6D1",
    leaders = c("Amanda Fagan", "Litzy Jaime", "Chloe Miller", "Joel Miller")
  ),
  "Case Conversation 3" = list(
    topic = "Overexploitation",
    day   = "W7D1",
    leaders = c("Hannah Pierson", "Lindy Slatten", "Grecia Torres")
  ),
  "Case Conversation 4" = list(
    topic = "Invasive Alien Species",
    day   = "W8D2",
    leaders = c("E Bradley", "Cian Hassovic", "Samuel Jacobsen", "Jameson Young")
  ),
  "Case Conversation 5" = list(
    topic = "Climate Change",
    day   = "W9D2",
    leaders = c("E Bradley", "Cian Hassovic", "Samuel Jacobsen", "Jameson Young")
  ),
  "Case Conversation 6" = list(
    topic = "Species-Level Approaches",
    day   = "W12D2",
    leaders = c("Amanda Fagan", "Litzy Jaime", "Chloe Miller", "Joel Miller")
  ),
  "Case Conversation 7" = list(
    topic = "Community & Ecosystem Approaches",
    day   = "W14D2",
    leaders = c("Hannah Pierson", "Lindy Slatten", "Grecia Torres")
  ),
  "Case Conversation 8" = list(
    topic = "Landscape-Scale Approaches",
    day   = "W15D2",
    leaders = c("Francesca Kerkhofs", "Zipporah Piper", "Sam Dillon")
  )
)

case_convos_tbl <- enframe(case_convos, name = "case") %>%
  unnest_wider("value") %>%
  group_by(case, topic, day) %>%
  summarize(leaders = str_flatten_comma(unlist(leaders), na.rm = TRUE), .groups = "drop") %>%
  mutate(
    case_display = as.character(str_glue("{row_number()}. {topic}")),
    week = str_remove_all(day, "D\\d+"),
    day  = str_remove_all(day, "W\\d+")
    ) %>%
  select(
    week,
    day,
    case = case_display,
    leaders
  )
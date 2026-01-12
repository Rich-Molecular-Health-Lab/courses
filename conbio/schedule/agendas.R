agendas <- list(
  W1 = list(
    D1 = list(
      sections = c(NA),
      topic    = "Overview & Syllabus",
      agenda   = NA
    ),
    D2 = list(
      sections = c(2.2),
      topic    = "The Rise of Conservation Biology",
      agenda   = NA
      )
  ),
  W2 = list(
    D1 = list(
      sections = c(NA),
      topic    = "Holiday",
      agenda   = NA
    ),
    D2 = list(
      sections = c(3.2, 3.3, 3.4, 3.5, 3.6),
      topic    = "Biodiversity Concepts & Measurement",
      agenda   = NA
    )
  ),
  W3 = list(
    D1 = list(
      sections = c(3.2, 3.3, 3.4, 3.5, 3.6),
      topic    = "Biodiversity Concepts & Measurement",
      agenda   = NA
    ),
    D2 = list(
      sections = c(3.2, 3.3, 3.4, 3.5, 3.6),
      topic    = "Biodiversity Concepts & Measurement",
      agenda   = NA
    )
  ),
  W4 = list(
    D1 = list(
      sections = c(8.1, 8.3),
      topic    = "Extinction",
      agenda   = NA
      ),
    D2 = list(
      sections = c(8.1, 8.3),
      topic    = "Extinction",
      agenda   = "Case Conversation 1"
    )
  ),
  W5 = list(
    D1 = list(
      sections = c(9.1, 9.2, 9.3, 9.4),
      topic    = "Habitat Loss, Fragmentation, and Degradation",
      agenda   = NA
    ),
    D2 = list(
      sections = c(9.1, 9.2, 9.3, 9.4),
      topic    = "Habitat Loss, Fragmentation, and Degradation",
      agenda   = NA
    )
  ),
  W6 = list(
    D1 = list(
      sections = c(9.1, 9.2, 9.3, 9.4),
      topic    = "Habitat Loss, Fragmentation, and Degradation",
      agenda   = "Case Conversation 2"
    ),
    D2 = list(
      sections = c(10.1, 10.2, 10.3, 10.4),
      topic    = "Overexploitation",
      agenda   = "Fishing Game"
    )
  ),
  W7 = list(
    D1 = list(
      sections = c(10.1, 10.2, 10.3, 10.4),
      topic    = "Overexploitation",
      agenda   = "Case Conversation 3"
    ),
    D2 = list(
      sections = c(11.1, 11.2, 11.4),
      topic    = "Invasive Alien Species",
      agenda   = NA
    )
  ),
  W8 = list(
    D1 = list(
      sections = c(NA),
      topic    = NA,
      agenda   = "Exam 1"
    ),
    D2 = list(
      sections = c(11.1, 11.2, 11.4),
      topic    = "Invasive Alien Species",
      agenda   = "Case Conversation 4"
    )
  ),
  W9 = list(
    D1 = list(
      sections = c(12.1, 12.2, 12.3, 12.4),
      topic    = "Climate Change",
      agenda   = NA
    ),
    D2 = list(
      sections = c(12.1, 12.2, 12.3, 12.4),
      topic    = "Climate Change",
      agenda   = "Case Conversation 5"
    )
  ),
  W10 = list(
    D1 = list(
      sections = c(NA),
      topic    = "Spring Break",
      agenda   = NA
    ),
    D2 = list(
      sections = c(NA),
      topic    = "Spring Break",
      agenda   = NA
    )
  ),
  W11 = list(
    D1 = list(
      sections = c(16.1, 16.2),
      topic    = "Ex Situ Conservation",
      agenda   = NA
    ),
    D2 = list(
      sections = c(16.1, 16.2),
      topic    = "Ex Situ Conservation",
      agenda   = NA
    )
  ),
  W12 = list(
    D1 = list(
      sections = c(13.1, 13.2, 13.3, 13.4, 13.5, 13.6),
      topic    = "Species-Level Approaches",
      agenda   = NA
    ),
    D2 = list(
      sections = c(13.1, 13.2, 13.3, 13.4, 13.5, 13.6),
      topic    = "Species-Level Approaches",
      agenda   = "Case Conversation 6"
    )
  ),
  W13 = list(
    D1 = list(
      sections = c(13.1, 13.2, 13.3, 13.4, 13.5, 13.6),
      topic    = "Species-Level Approaches",
      agenda   = "Population Management Excercise"
    ),
    D2 = list(
      sections = c(14.1, 14.2, 14.4),
      topic    = "Community & Ecosystem Approaches",
      agenda   = NA
    )
  ),
  W14 = list(
    D1 = list(
      sections = c(14.1, 14.2, 14.4),
      topic    = "Community & Ecosystem Approaches",
      agenda   = NA
    ),
    D2 = list(
      sections = c(14.1, 14.2, 14.4),
      topic    = "Community & Ecosystem Approaches",
      agenda   = "Case Conversation 7"
    )
  ),
  W15 = list(
    D1 = list(
      sections = c(15.3, 15.4),
      topic    = "Landscape-Scale Approaches",
      agenda   = NA
    ),
    D2 = list(
      sections = c(15.3, 15.4),
      topic    = "Landscape-Scale Approaches",
      agenda   = "Case Conversation 8"
    )
  ),
  W16 = list(
    D1 = list(
      sections = c(NA),
      topic    = "Review",
      agenda   = NA
    ),
    D2 = list(
      sections = c(NA),
      topic    = "Review",
      agenda   = NA
    )
  ),
  Finals = list(
    D1 = list(
      sections = c(NA),
      topic    = NA,
      agenda   = "Final Exam"
    )
  )
)  %>%
  enframe(name = "Week") %>%
  unnest_longer("value", indices_to = "Day", values_to = "Agenda") %>%
  unnest_wider("Agenda") %>%
  rowwise() %>%
  mutate(reading = unlist(str_flatten_comma(sections))) %>%
  ungroup() %>%
  mutate(reading = if_else(!is.na(lag(reading)) & reading == lag(reading), NA, reading)) %>%
  select(
    Week,
    Day,
    reading,
    topic,
    agenda
  )
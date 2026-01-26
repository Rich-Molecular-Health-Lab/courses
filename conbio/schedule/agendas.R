topics <- list(
  "Overview & Syllabus"                          = list("W1D1"),
  "The Rise of Conservation Biology"             = list("W1D2", "W2D2"),
  "Holiday"                                      = list("W2D1"),
  "Biodiversity Concepts & Measurement"          = list("W3D1", "W3D2"),
  "Extinction"                                   = list("W4D1", "W4D2"),
  "Habitat Loss, Fragmentation, and Degradation" = list("W5D1", "W5D2", "W6D1"),
  "Overexploitation"                             = list("W6D2", "W7D1"),
  "Invasive Alien Species"                       = list("W7D2", "W8D2"),
  "Exam 1"                                       = list("W8D1"),
  "Climate Change"                               = list("W9D1", "W9D2"),
  "Spring Break"                                 = list("W10D1", "W10D2"),
  "Ex Situ Conservation"                         = list("W11D1", "W11D2"),
  "Species-Level Approaches"                     = list("W12D1", "W12D2", "W13D1"),
  "Community & Ecosystem Approaches"             = list("W13D2", "W14D1", "W14D2"),
  "Landscape-Scale Approaches"                   = list("W15D1", "W15D2", "W16D1"),
  "Review"                                       = list("W16D2"),
  "Final Exam"                                   = list("W17Finals")
)

readings <- list(
  "Overview & Syllabus"                          = c(NA),
  "The Rise of Conservation Biology"             = c(2.2),
  "Holiday"                                      = c(NA),
  "Biodiversity Concepts & Measurement"          = c(3.2, 3.3, 3.4, 3.5, 3.6),
  "Extinction"                                   = c(8.1, 8.3),
  "Habitat Loss, Fragmentation, and Degradation" = c(9.1, 9.2, 9.3, 9.4),
  "Overexploitation"                             = c(10.1, 10.2, 10.3, 10.4),
  "Invasive Alien Species"                       = c(11.1, 11.2, 11.4),
  "Exam 1"                                       = c(NA),
  "Climate Change"                               = c(12.1, 12.2, 12.3, 12.4),
  "Spring Break"                                 = c(NA),
  "Ex Situ Conservation"                         = c(16.1, 16.2),
  "Species-Level Approaches"                     = c(13.1, 13.2, 13.3, 13.4, 13.5, 13.6),
  "Community & Ecosystem Approaches"             = c(14.1, 14.2, 14.4),
  "Landscape-Scale Approaches"                   = c(15.3, 15.4),
  "Review"                                       = c(NA),
  "Final Exam"                                   = c(NA)
)

films <- list(
  "Extinction"                 = list(title = "Human Footprint: Vanishing Act (S2E6)", url = "https://www.pbs.org/show/human-footprint/"),
  "Habitat Loss, Fragmentation, and Degradation" = list(title = "The Territory", url = "https://films.nationalgeographic.com/the-territory"),
  "Invasive Alien Species"     = list(title = "Human Footprint: Strangers in Paradise (S1E1)", url = "https://www.pbs.org/video/strangers-in-paradise-sd5mkz/"),
  "Climate Change"             = list(title = "Dynamic Planet: Earth (S1E4)", url = "https://www.pbs.org/show/dynamic-planet/"),
  "Landscape-Scale Approaches" = list(title = "Nature: Running with the Beest", url = "https://www.pbs.org/wnet/nature/about-running-with-the-beest/27769/")
)

slides <- list(
  "Overview & Syllabus"                          = "slides/conbio/0_conbio_syllabus_overview.qmd",
  "The Rise of Conservation Biology"             = "slides/conbio/2_contemporary_conservation_biology.qmd",
  "Biodiversity Concepts & Measurement"          = "slides/conbio/3_biodiversity.qmd",
  "Extinction"                                   = "https://www.canva.com/design/DAG_FWkPS2k/FcT30U7YAvgdl97gjVabRg/view?utm_content=DAG_FWkPS2k&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=h60e6bdbe76",
  "Habitat Loss, Fragmentation, and Degradation" = "https://www.canva.com/design/DAG_FXHWfdo/rrILF8y6y48b2T-rkeeP4A/view?utm_content=DAG_FXHWfdo&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=h28aa7445d4",
  "Overexploitation"                             = "https://www.canva.com/design/DAG_FaCg4UM/vYT5p7zoQK7gsVblo2_b1Q/view?utm_content=DAG_FaCg4UM&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=h33332edd57",
  "Invasive Alien Species"                       = "https://www.canva.com/design/DAG_FYkCMOQ/DnwXNvBTvmuJg1AKG4kvNA/view?utm_content=DAG_FYkCMOQ&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=hc6e0ba1510",
  "Climate Change"                               = "https://www.canva.com/design/DAG_FZYnAxA/vLeMXXSc2IyRejjHiHmSgw/view?utm_content=DAG_FZYnAxA&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=h3381ae972e",
  "Ex Situ Conservation"                         = "https://www.canva.com/design/DAG_FazNkaM/0BKQycwch8o4O7QIk6WnVA/view?utm_content=DAG_FazNkaM&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=h1e2d368ff4",
  "Species-Level Approaches"                     = "https://www.canva.com/design/DAG_FVnmq38/SiEbro-JPcm4SAQzIMAreg/view?utm_content=DAG_FVnmq38&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=he8ec3e6d54",
  "Community & Ecosystem Approaches"             = "https://www.canva.com/design/DAG_FcA1BKA/BggO589srHeQ-IOlvDe-Gw/view?utm_content=DAG_FcA1BKA&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=h60040cedac",
  "Landscape-Scale Approaches"                   = "https://www.canva.com/design/DAG_FeFF5jE/PmA4uO66UzdWHIPM0hurAQ/view?utm_content=DAG_FeFF5jE&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=hc225bf3f9a"
)

agendas <-


agendas <- list(
  W1 = list(
    D1 = list(sections = c(NA), topic = "Overview & Syllabus", slides = "slides/conbio/0_conbio_syllabus_overview.qmd"),
    D2 = list(sections = c(2.2), topic = "The Rise of Conservation Biology", slides = "slides/conbio/2_contemporary_conservation_biology.qmd")
  ),
  W2 = list(
    D1 = list(sections = c(NA), topic = "Holiday"),
    D2 = list(sections = c(2.2), topic = "The Rise of Conservation Biology" )
  ),
  W3 = list(
    D1 = list(sections = c(3.2, 3.3, 3.4, 3.5, 3.6), topic = "Biodiversity Concepts & Measurement", slides = "slides/conbio/3_biodiversity.qmd"),
    D2 = list(
      sections = c(3.2, 3.3, 3.4, 3.5, 3.6),
      topic    = "Biodiversity Concepts & Measurement"
    )
  ),
  W4 = list(
    D1 = list(
      sections = c(8.1, 8.3),
      topic    = "Extinction",
      film      = "<a href='https://www.pbs.org/show/human-footprint/'>Human Footprint: Vanishing Act (S2E6)</a>"
      ),
    D2 = list(
      sections = c(8.1, 8.3),
      topic    = "Extinction"
    )
  ),
  W5 = list(
    D1 = list(
      sections = c(9.1, 9.2, 9.3, 9.4),
      topic    = "Habitat Loss, Fragmentation, and Degradation"
    ),
    D2 = list(
      sections = c(9.1, 9.2, 9.3, 9.4),
      topic    = "Habitat Loss, Fragmentation, and Degradation"
    )
  ),
  W6 = list(
    D1 = list(
      sections = c(9.1, 9.2, 9.3, 9.4),
      topic    = "Habitat Loss, Fragmentation, and Degradation"
    ),
    D2 = list(
      sections = c(10.1, 10.2, 10.3, 10.4),
      topic    = "Overexploitation",
      activity = "Fishing Game"
    )
  ),
  W7 = list(
    D1 = list(
      sections = c(10.1, 10.2, 10.3, 10.4),
      topic    = "Overexploitation"
    ),
    D2 = list(
      sections = c(11.1, 11.2, 11.4),
      topic    = "Invasive Alien Species"
    )
  ),
  W8 = list(
    D1 = list(
      sections = c(NA),
      topic    = "Exam 1"
    ),
    D2 = list(
      sections = c(11.1, 11.2, 11.4),
      topic    = "Invasive Alien Species"
    )
  ),
  W9 = list(
    D1 = list(
      sections = c(12.1, 12.2, 12.3, 12.4),
      topic     = "Climate Change",
      film      = "<a href='https://www.pbs.org/show/dynamic-planet/'>Dynamic Planet: Earth (S1E4)</a>"
    ),
    D2 = list(
      sections = c(12.1, 12.2, 12.3, 12.4),
      topic    = "Climate Change"
    )
  ),
  W10 = list(
    D1 = list(
      sections = c(NA),
      topic    = "Spring Break"
    ),
    D2 = list(
      sections = c(NA),
      topic    = "Spring Break"
    )
  ),
  W11 = list(
    D1 = list(
      sections = c(16.1, 16.2),
      topic    = "Ex Situ Conservation"
    ),
    D2 = list(
      sections = c(16.1, 16.2),
      topic    = "Ex Situ Conservation"
    )
  ),
  W12 = list(
    D1 = list(
      sections = c(13.1, 13.2, 13.3, 13.4, 13.5, 13.6),
      topic    = "Species-Level Approaches"
    ),
    D2 = list(
      sections = c(13.1, 13.2, 13.3, 13.4, 13.5, 13.6),
      topic    = "Species-Level Approaches"
    )
  ),
  W13 = list(
    D1 = list(
      sections = c(13.1, 13.2, 13.3, 13.4, 13.5, 13.6),
      topic    = "Species-Level Approaches"
    ),
    D2 = list(
      sections = c(14.1, 14.2, 14.4),
      topic    = "Community & Ecosystem Approaches"
    )
  ),
  W14 = list(
    D1 = list(sections = c(14.1, 14.2, 14.4), topic = "Community & Ecosystem Approaches"),
    D2 = list(sections = c(14.1, 14.2, 14.4), topic = "Community & Ecosystem Approaches")
  ),
  W15 = list(
    D1 = list(sections = c(15.3, 15.4), topic = "Landscape-Scale Approaches"),
    D2 = list(sections = c(15.3, 15.4), topic = "Landscape-Scale Approaches")
  ),
  W16 = list(
    D1 = list(sections = c(NA), topic = "Landscape-Scale Approaches", film = "<a href='https://www.pbs.org/wnet/nature/about-running-with-the-beest/27769/'>Nature: Running with the Beest</a>"),
    D2 = list(
      sections = c(NA),
      topic = "Review"
    )
  ),
  Finals = list(
    D1 = list(sections = c(NA),topic = "Final Exam")
  )
)  %>%
  enframe(name = "Week") %>%
  unnest_longer("value", indices_to = "Day", values_to = "Agenda") %>%
  unnest_wider("Agenda") %>%
  rowwise() %>%
  mutate(reading   = unlist(str_flatten_comma(sections, na.rm = TRUE))) %>%
  ungroup() %>%
  select(
    Week,
    Day,
    reading,
    topic,
    case,
    slides,
    film
  )
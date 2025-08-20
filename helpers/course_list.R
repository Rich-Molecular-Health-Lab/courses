
course_info <- list(
  hhe_25f = list(
      course_no          = "ENVN-4320, BIOL-4030",
      file_prefix        = "hhe_25f",
      semester           = "Fall 2025",
      course_description = tagList(
        withTags(
          card_body(
            p(
              "Explore how human health intersects with ",
              strong("environmental change, urbanization, and social justice "),
              "in this engaging interdisciplinary course designed to appeal broadly to students across the humanities, social sciences, pre-health fields, and environmental studies. Delve into critical contemporary issues such as",
              strong(" climate justice, urban health disparities, impacts of food production, emerging infectious diseases, and environmental toxicology."),
              " Real-world case studies—including the Flint water crisis, zoonotic pandemics, and climate-induced health challenges—will ground your exploration in tangible scenarios."
              ),
            p(
              "Course materials uniquely incorporate curated ",
              strong("podcast playlists"),
              " and ",
              strong("documentary films, enriching discussions"),
              " and broadening perspectives on urgent global health and environmental challenges. Through lively discussions and practical activities, you'll develop thoughtful insights and innovative ideas for creating healthier communities and sustainable futures.")
          )
        )
      ),
      day_time           = "T/R, 4:00 - 5:15",
      wdays              = c(3, 5),
      start_time         = "16:00",
      location           = "AH 314",
      location_assigned  = "AH 314",
      semesters          = "Fall",
      credits            = 3,
      prereqs            = NA,
      canvas             = "92534",
      project            = "Poster",
      final_exam         = ymd_hm("2025-12-16 17:00", tz = "America/Chicago"),
      grade_deadline     = ymd("2025-12-23")
    ),
  conbio_25f = list(
    course_no          = "BIOL-4120, BIOL-8126",
    file_prefix        = "conbio_25f",
    semester           = "Fall 2025",
    course_description = tagList(
      withTags(
        card_body(
          p(
            "Engage with the urgent task of protecting biodiversity in Conservation Biology—a dynamic, interdisciplinary course addressing the global ecological crisis. Often described as a ",
            em("crisis discipline"),
            ", conservation biology emphasizes swift, informed actions in response to rapidly evolving environmental challenges. You'll explore topics like ",
            strong("species extinction, habitat loss, ecosystem management, invasive species, climate impacts, and conservation genetics"),
            " through engaging discussions and real-world case studies. Skills-based assignments and activities will introduce you to ",
            strong("essential tools for management, practical application, and quantitative assessment.")
            ),
          p("Open to students across biology, environmental sciences, social sciences, and humanities, this course empowers you with the knowledge and practical skills needed to contribute meaningfully to conservation efforts.")
        )
      )
    ),
    day_time           = "M/W, 4:00 - 5:15",
    wdays              = c(2, 4),
    start_time         = "16:00",
    location           = "AH 422D",
    location_assigned  = "AH 304",
    semesters          = "Fall & Spring",
    credits            = 3,
    prereqs            = "BIOL 1450, 1750, 2140 and Junior-Senior in biology. Not open to non-degree graduate students.",
    canvas             = "91321",
    project            = "Grant Proposal",
    final_exam         = ymd_hm("2025-12-15 17:00", tz = "America/Chicago"),
    grade_deadline     = ymd("2025-12-22")
  ),
  zoobio_26s = list(
    course_no          = "BIOL-4030/4034",
    file_prefix        = "zoobio_26s",
    semester           = "Spring 2026",
    course_description = tagList(
      withTags(
        card_body(
          p(
            "Dive into the fascinating world of zoo biology, where conservation, animal welfare, and management intersect. This dynamic course explores the roles of modern zoos in ",
            strong("biodiversity conservation, assurance populations, and community engagement"),
            ". Engage deeply with key topics such as ",
            strong("population genetics and demography"),
            ", nutrition, reproductive technologies, behavioral training, environmental enrichment, and welfare assessment."),
          p("Through interactive labs and workshops, you'll gain hands-on experience in creating Institutional Collection Plans, designing enrichment programs, analyzing studbook data, and assessing animal welfare. Critically examine ethical considerations and emerging practices shaping the future of zoos, and develop practical, evidence-based strategies to enhance animal care and conservation outcomes.")
        )
      )
    ),
    day_time           = "M/W, 1:00 - 2:15 | T (labs), 1:00 - 3:50",
    wdays              = c(2, 3, 4),
    semesters          = "Spring",
    credits            = 4,
    prereqs            = "BIOL 1450, 1750, and Junior-Senior in biology. Not open to non-degree graduate students.",
    project            = "Fictional Zoo Portfolio"
  )
)

textbooks <- list(
  conbio = list(
    title   = "Conservation Biology",
    href    = "https://bookshelf.vitalsource.com/reader/books/9780197667033",
    image   = "images/conbio_text.jpg",
    authors = "Bradley Cardinale; James D. Murdoch",
    date    = ymd("2025-1-17"),
    edition = "2nd",
    isbn    = "9780197667033",
    pages   = 654,
    embed   = FALSE,
    type    = "etext"
  ),
  hhe = list(
    title   = "Podcast Playlist for Human Health & the Environment",
    href    = "https://open.spotify.com/playlist/4NTGkvvAL2OxXaDIzV3BS0?si=b21104990e8d4971",
    image   = "images/logo_spotify.png",
    authors = "Misc.",
    date    = ymd("2025-8-1"),
    edition = "3rd",
    isbn    = NA,
    pages   = NA,
    embed   = TRUE,
    type    = "playlist"
  )
)

slos <- list(
  zoobio = tribble(
    ~Level      ,      ~Outcome           ,
    "Remember"  , "Identify and recall key concepts and terminology related to zoo biology, such as taxonomic classifications, animal behavior terms, and basic biological principles." ,
    "Understand", "Explain the fundamental theories and principles of zoo biology, including the importance of biodiversity conservation and the role of zoos in wildlife conservation efforts." ,
    "Apply"     , "Apply their knowledge of zoo biology to analyze and solve real-world problems related to animal care, exhibit design, and wildlife conservation strategies within a controlled zoo environment.",
    "Analyze"   , "Evaluate the ethical and practical considerations involved in managing zoo populations, critically assess the impact of human activities on animal habitats, and propose evidence-based solutions for enhancing animal welfare and conservation efforts."
  ),
  conbio = tribble(
    ~Level       , ~Outcome           ,
    "Remember"   , "Identify and recall key concepts and terminology related to zoo biology, such as taxonomic classifications, animal behavior terms, and basic biological principles." ,
    "Understand" , "Explain the fundamental theories and principles of zoo biology, including the importance of biodiversity conservation and the role of zoos in wildlife conservation efforts." ,
    "Apply"      , "Apply their knowledge of zoo biology to analyze and solve real-world problems related to animal care, exhibit design, and wildlife conservation strategies within a controlled zoo environment.",
    "Analyze"    , "Evaluate the ethical and practical considerations involved in managing zoo populations, critically assess the impact of human activities on animal habitats, and propose evidence-based solutions for enhancing animal welfare and conservation efforts."
  )
)

assessment <- list(
  conbio_25f = list(
    exams       = map(paste0("exam", 1:3), \(x) list_scores(x, 50)),
    quizzes     = map(paste0("quiz", 1:10), \(x) list_scores(x, 3)),
    assignments = list(grant_proposal = 50),
    inclass     = map(paste0("exercise", 1:5), \(x) list_scores(x, 3))
  ),
  hhe_25f = list(
    exams       = map(paste0("exam", 1:3), \(x) list_scores(x, 50)),
    quizzes     = map(paste0("quiz", 1:10), \(x) list_scores(x, 3)),
    assignments = list(poster = 50),
    inclass     = map(paste0("exercise", 1:5), \(x) list_scores(x, 3))
  ),
  zoobio_26s = list(
    exams       = map(paste0("exam", 1:2), \(x) list_scores(x, 50)),
    quizzes     = map(paste0("quiz", 1:10), \(x) list_scores(x, 3)),
    assignments = list(portfolio = 60, enrichment = 20),
    inclass     = map(paste0("lab", 1:10), \(x) list_scores(x, 10))
  )
)


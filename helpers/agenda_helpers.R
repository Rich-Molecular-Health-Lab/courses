revise_topics <- function(topics, discarded) {
 flat <- list_flatten(topics, name_spec = "{inner}") %>%
    list_flatten(name_spec = "{inner}") %>% discard_at(discarded)

 nested <- map_depth(topics, 2, \(x) compact(keep_at(x, names(flat)))) %>%
   lmap(compact)

 return(nested)
}

lesson_days <- function(topics) {
  list_flatten(topics, name_spec = "{inner}") %>%
   list_flatten(name_spec = "{inner}") %>%
   map(\(x) keep_at(x, "n_days")) %>%
    list_flatten(name_spec = "{outer}")
}

est_weeks <- function(topics) {
  days  <- lesson_days(topics)
  weeks <- ceiling(reduce(days, `+`)/2)
  return(weeks)
}

flatten_topics <- function(topics) {
  map_depth(topics, 2, \(x) imap(x, \(y, idy) list_assign(y, topic = idy))) %>%
    map(\(x) imap(x, \(y, idy) map(y, \(z) list_assign(
      z,
      title   = idy,
      chapter = floor(as.integer(z[["section"]]))
    )))) %>%
    imap(\(x, idx) map_depth(x, 2, \(y) list_assign(y, theme = idx))) %>%
    list_flatten(name_spec = "{inner}") %>%
    list_flatten(name_spec = "{inner}") %>%
    set_names(map(., \(x) pluck(x, "section")))
}

assign_topics <- function(topics_list, names_topics) {
  keep_at(topics_list, names_topics)
}

flatten_films <- function(videos_list) {
  full_length <- imap(videos_list$films, \(x, idx) assign_in(x, "title", idx)) %>%
    imap(\(x, idx) assign_in(x, "series", idx))

  map(
    videos_list$series, \(x) map_at(
      x, "episodes", \(y) map(
        y, \(z) assign_in(
          z, "streaming", x$streaming
        )
      )
    )
  ) %>%
    map(\(x) keep_at(x, "episodes")) %>%
    imap(\(x, idx) map_depth(x, 2, \(y) assign_in(y, "series", idx))) %>%
    list_flatten(name_spec = "{outer}") %>%
    list_flatten(name_spec = "{inner}") %>%
    set_names(map(., \(x) pluck(x, "title"))) %>%
    list_assign(!!!full_length)
}

films_by_topic <- function(videos_list) {
  films_flat <- flatten_films(videos_list)
    films_flat %>%
    map(\(x) keep_at(x, "topics")) %>%
    list_flatten(name_spec = "{outer}") %>%
    list_flatten(name_spec = "{outer}") %>%
    imap(\(x, idx) list(film = idx, topic = x)) %>%
    set_names(map(., \(x) pluck(x, "topic"))) %>%
    map(\(x) discard_at(x, "topic"))  %>%
    list_flatten(name_spec = "{outer}") %>%
    map(\(x) list(discard_at(pluck(films_flat, x), "topics"))) %>%
    map(\(x) set_names(x, map(x, \(y) pluck(y, "title")))) %>%
    split(names(.)) %>%
    map(\(x) list_flatten(x, name_spec = "{inner}"))

}

flatten_podcasts <- function(podcasts_list) {
  list_flatten(podcasts_list) %>%
    imap(\(x, idx) list(
      episode  = idx,
      date     = x[["date"]],
      minutes  = x[["minutes"]],
      topics   = x[["topics"]],
      href     = x[["href"]]
    ))
}

podcasts_by_episode <- function(podcasts_list) {
  imap(
    podcasts_list,
    \(x, idx) imap(
      x,
      \(y, idy) list_assign(y, series = idx, episode = idy)
    )
  ) %>%
    list_flatten(name_spec = "{inner}") %>%
    map(\(x) keep_at(x, c("date", "minutes", "series", "episode", "href")))
}

podcasts_by_topic <- function(podcasts_list) {
  podcasts_flat <- flatten_podcasts(podcasts_list)
  map(podcasts_flat, \(x) keep_at(x, "topics")) %>%
    list_flatten(name_spec = "{outer}") %>%
    list_flatten(name_spec = "{outer}") %>%
    imap(\(x, idx) list(podcast = idx, topic = x)) %>%
    set_names(map(., \(x) pluck(x, "topic"))) %>%
    map(\(x) discard_at(x, "topic")) %>%
    list_flatten(name_spec = "{outer}") %>%
    map(\(x) list(discard_at(pluck(podcasts_flat, x), "topics"))) %>%
    map(\(x) set_names(x, map(x, \(y) pluck(y, "episode")))) %>%
    split(names(.)) %>%
    map(\(x) list_flatten(x, name_spec = "{inner}"))
}

media_by_topic <- function(topics_list, podcasts_list, videos_list) {
  podcasts_topics <- podcasts_by_topic(podcasts_list)
  films_topics    <- films_by_topic(videos_list)

  topics_flat <- imap(topics_list, \(x, idx) imap(x, \(y, idy) list_assign(
    y, podcasts = list(
      pluck(podcasts_topics, idy),
      pluck(podcasts_topics, idx))
  ))) %>%
    imap(\(x, idx) imap(
      x, \(y, idy) list_assign(
        y, films = list(
          pluck(films_topics, idy),
          pluck(films_topics, idx)
        )
      )
    )) %>%
    map_depth(2, \(x) map_at(x, c("podcasts", "films"), \(y) list_flatten(y, name_spec = "{inner}")))%>%
    map_depth(3, \(x) discard_at(x, duplicated(x))) %>%
    list_flatten(name_spec = "{inner}") %>%
    imap(\(x, idx) assign_in(x, "topic", idx)) %>%
    map(\(x) map_at(x, c("podcasts", "films"), \(y) compact(y)))

  return(topics_flat)
}

map_media_options <- function(blank_schedule, topics_list, podcasts_list, videos_list) {
  topics_flat <- media_by_topic(topics_list, podcasts_list, videos_list)
  themes_flat <- imap(topics_list, \(x, idx) map(x, \(y) list(idx))) %>%
    list_flatten(name_spec = "{inner}") %>%
    list_flatten(name_spec = "{outer}")

  schedule <- imap(blank_schedule, \(x, idx) list_assign(
    x,
    agenda = compact(keep(topics_flat, \(y) idx %in% y[["days"]]))
  )) %>%
    map(\(x) list_assign(
      x,
      podcast_options = map(x[["agenda"]], \(y) pluck(y, "podcasts"))
    )) %>%
    map(\(x) list_assign(
      x,
      film_options = map(x[["agenda"]], \(y) pluck(y, "films"))
    )) %>%
    map(\(x) map_at(
      x, c("podcast_options", "film_options"),
      \(y) list_flatten(y, name_spec = "{inner}"))) %>%
    map(\(x) map_at(x, c("podcast_options", "film_options"), \(y) discard_at(y, duplicated(y)))) %>%
    map(\(x) list_assign(x, topic = names(x[["agenda"]]))) %>%
    map(\(x) discard_at(x, "agenda")) %>%
    map(\(x) map_at(x, "podcast_options", \(y) set_names(y, str_extract(names(y), ".+(?=_)")))) %>%
    map(\(x) map_at(x, "film_options", \(y) set_names(y, map(y, \(z) pluck(z, "series"))))) %>%
    map(\(x) map_at(x, c("podcast_options", "film_options"), \(y) split(y, names(y)))) %>%
    map(\(x) assign_in(x, "theme", unlist(unique(compact(keep_at(themes_flat, \(y) y %in% pluck(x, "topic")))))))  %>%
    map(\(x) map_at(x, "film_options", \(y) map(y, \(z) set_names(z, map(z, \(a) pluck(a, "title")))))) %>%
    map(\(x) map_at(x, "podcast_options", \(y) map(y, \(z) set_names(z, map(
      z, \(a) str_remove(pluck(a, "episode"), ".+_")
    )))))

  return(schedule)

}

enframe_schedule_options <- function(blank_schedule, topics_list, podcasts_list, videos_list) {
  map_media_options(blank_schedule, topics_list, podcasts_list, videos_list) %>%
    enframe(name = "Day") %>%
    unnest_wider(value) %>%
    unnest_longer(topic, indices_include = FALSE)  %>%
    unnest(!c("podcast_options", "film_options")) %>%
    mutate(date = as_datetime(date)) %>%
    select(
      week,
      daywk,
      date,
      theme,
      topic,
      podcast_options,
      film_options
    ) %>%
    group_by(
      week,
      daywk,
      date,
      podcast_options,
      film_options
    ) %>%
    summarize(
      theme      = str_flatten_comma(unique(theme  )),
      topic      = str_flatten_comma(unique(topic  ))
    ) %>%
    ungroup() %>%
    arrange(date) %>%
    select(
      week,
      daywk,
      date,
      theme,
      topic,
      podcast_options,
      film_options
    )
}

filter_podcasts <- function(podcasts_list, podcast_selections) {
  podcast_details <- podcasts_by_episode(podcasts_list)
    map(podcast_selections, \(x) keep(podcast_details, \(y) y[["episode"]] %in% x)) %>%
    set_names(imap(., \(x, idx) paste0(idx, "D1")))
}

filter_films <- function(videos_list, film_selections) {
  film_details <- flatten_films(videos_list) %>%
    map(\(x) discard_at(x, "topics"))

  films_filtered <- map(
    film_selections,
    \(x) keep(film_details, \(y) y[["title"]] %in% x)
  )

  return(films_filtered)
}

assign_podcasts <- function(blank_schedule, podcasts_list, podcast_selections) {
  podcasts_weekly <- filter_podcasts(podcasts_list, podcast_selections)

  imap(blank_schedule, \(x, idx) list_assign(x, day = idx)) %>%
    set_names(map(., \(x) paste0("W", x[["week"]]))) %>%
    split(names(.)) %>%
    imap(\(x, idx) set_names(x, paste0(idx, "D", seq_along(x)))) %>%
    list_flatten(name_spec = "{inner}") %>%
    imap(\(x, idx) list_assign(x, podcasts = pluck(podcasts_weekly, idx)))
}

populate_schedule <- function(course, blank_schedule, topics_list, podcasts_list, podcast_selections, videos_list, film_selections, assignments) {
  films_filtered    <- filter_films(videos_list, film_selections)
  podcasts_filtered <- filter_podcasts(podcasts_list, podcast_selections)
  themes_flat <- imap(topics_list, \(x, idx) map(x, \(y) list(idx))) %>%
    list_flatten(name_spec = "{inner}") %>%
    list_flatten(name_spec = "{outer}")


  schedule_list <- imap(blank_schedule, \(x, idx) list_assign(
    x,
    agenda = names(compact(
      keep(list_flatten(
        topics_list, name_spec = "{inner}"
      ), \(y) idx %in% y[["days"]])
    ))
  )) %>%
    imap(\(x, idx) list_assign(x, day = idx)) %>%
    set_names(map(., \(x) paste0("W", x[["week"]]))) %>%
    split(names(.)) %>%
    imap(\(x, idx) set_names(x, paste0(idx, "D", seq_along(x)))) %>%
    list_flatten(name_spec = "{inner}") %>%
    imap(\(x, idx) list_assign(x, podcasts  = pluck(podcasts_filtered, idx))) %>%
    imap(\(x, idx) list_assign(x, films     = pluck(films_filtered, idx))) %>%
    imap(\(x, idx) list_assign(x, deadlines = pluck(assignments, "homework", idx))) %>%
    imap(\(x, idx) list_assign(x, location  = pluck(course_info, course, "location"))) %>%
    map(\(x) assign_in(x, "theme", unlist(unique(compact(keep_at(themes_flat, \(y) y %in% pluck(x, "agenda")))))))

  return(schedule_list)
}

map_topics <- function(topics_list, topics_nested, blank_schedule) {
  map(topics_nested, \(x) keep_at(topics_list, x)) %>%
    set_names(~paste0("D", seq_along(.))) %>%
    imap(\(x, idx) list(
      lesson = x,
      date   = pluck(blank_schedule, idx, "date"),
      daywk  = pluck(blank_schedule, idx, "daywk"),
      week   = pluck(blank_schedule, idx, "week")
    )) %>%
    list_assign(FINAL = pluck(blank_schedule, "FINAL")) %>%
    assign_in(list("FINAL", "lesson"), list(list(
      topic = "Final Exam",
      title = "Exam"
    )))
}

enframe_hhe_schedule <- function(schedule_list) {
  location_default <- pluck(course_info, "hhe_25f", "location")
  location_exams   <- pluck(course_info, "hhe_25f", "location_assigned")

 schedule <- enframe(schedule_list, name = "Day") %>%
   unnest_wider(value) %>%
   select(
     week,
     Day,
     daywk,
     date,
     location,
     theme,
     agenda,
     deadlines,
     podcasts,
     films
   ) %>%
   rowwise() %>%
   mutate(agenda = str_flatten_comma(unique(agenda))) %>%
   ungroup() %>%
   mutate(podcasts = case_when(
     Day == "W1D2" ~ lag(podcasts),
     Day == "W1D1" ~ lead(podcasts),
     .default = podcasts
   )) %>%
   unnest(!c("podcasts", "films")) %>%
   arrange(date) %>%
   unnest_longer(podcasts, indices_include = FALSE, keep_empty = TRUE) %>%
   unnest_wider(podcasts, names_sep = "_") %>%
   unnest(films, keep_empty = TRUE) %>%
   unnest_wider(films, names_sep = "_") %>%
   rowwise() %>%
   mutate(
     podcast = if_else(is.na(podcasts_episode), NA,
                       as.character(str_glue(
                         "{podcasts_series}: {podcasts_episode} ({year(podcasts_date)}, {podcasts_minutes} min)"
                       ))),
     film = case_when(
       is.na(films_title)         ~ NA,
       films_series == films_title ~  as.character(str_glue(
         "{films_title} ({year(films_date)}, {films_minutes} min)"
       )),
       films_series != films_title ~  as.character(str_glue(
         "{films_series}: {films_title} ({year(films_date)}, {films_minutes} min)"
       ))
     )
   ) %>%
   group_by(
     week,
     Day,
     daywk,
     date,
     location,
     theme,
     agenda,
     deadlines
   ) %>%
   summarize(
     podcasts     = str_flatten_comma(unique(podcast)),
     podcasts_min = sum(podcasts_minutes),
     film         = str_flatten_comma(unique(film))
   ) %>%
   ungroup() %>%
   mutate(
     podcasts = if_else(!is.na(podcasts), as.character(str_glue("{podcasts} - Total: {podcasts_min} min")), NA),
     agenda   = if_else(!is.na(film), as.character(str_glue("{agenda} (Film: {film})")), agenda)
     ) %>%
   select(
     week,
     Day,
     daywk,
     date,
     location,
     theme,
     agenda,
     assignment = deadlines,
     podcasts
   )

 return(schedule)

}

enframe_assignments <- function(assignments_list) {
  enframe(assignments_list, name = "type") %>%
    unnest_wider(value) %>%
    unnest(everything()) %>%
    pivot_longer(
      !type,
      names_to       = "assignment",
      values_to      = "section",
      values_drop_na = TRUE
    ) %>%
    mutate(
      assignment = if_else(
        type == "inclass",
        as.character(str_glue("{assignment} (In-Class Excercise)")),
        as.character(str_glue("{assignment} (Due on Canvas by 4 PM)"))
        ),
      section = as.character(section),
      .keep = "unused"
    ) %>%
    arrange(section)
}

enframe_locations <- function(location_list) {
  enframe(location_list, name = "section", value = "location") %>%
    unnest(everything())
}

enframe_schedule <- function(schedule_list, assignments_list, location_list, course) {
  location_default <- pluck(course_info, course, "location")
  location_exams   <- pluck(course_info, course, "location_assigned")
  enframe(schedule_list, name = "Day") %>%
    unnest_wider(value) %>%
    unnest_longer(lesson, indices_include = FALSE) %>%
    unnest_wider(lesson) %>%
    mutate(section = as.character(section)) %>%
    unnest(everything()) %>%
    mutate(date = as_datetime(date)) %>%
    select(
      week,
      daywk,
      date,
      theme,
      chapter,
      title,
      section,
      topic
    ) %>%
    left_join(enframe_assignments(assignments_list), by = "section") %>%
    left_join(enframe_locations(location_list), by = "section") %>%
    mutate(reading = if_else(
      date == first(date) & !(section %in% c("Midterm", "Film", "Exam")),
      section,
      NA
    ),
    .by = "section") %>%
    group_by(date) %>%
    mutate(location = case_when(
      !is.na(location) ~ location,
      is.na(location) & str_detect(topic, "Exam")  ~ location_exams,
      is.na(location) & !str_detect(topic, "Exam") ~ location_default,
      .default = location_default
    )) %>%
    fill(reading, .direction = "updown") %>%
    group_by(
      week,
      daywk,
      date,
      location
    ) %>%
    fill(assignment, .direction = "downup") %>%
    summarize(
      theme      = str_flatten_comma(unique(theme  )),
      chapter    = str_flatten_comma(unique(chapter)),
      title      = str_flatten_comma(unique(title  )),
      section    = str_flatten_comma(unique(section)),
      topic      = str_flatten_comma(unique(topic  )),
      reading    = str_flatten_comma(unique(reading  )),
      assignment = str_flatten_comma(unique(assignment  ))
    ) %>%
    ungroup() %>%
    fill(theme, .direction = "updown") %>%
    fill(chapter, .direction = "updown") %>%
    mutate(
      theme    = if_else(str_detect(title, "Exam|Film"), "Other", theme),
      chapter  = if_else(str_detect(title, "Exam") , NA      , chapter),
      section  = if_else(str_detect(topic, "Final"), "Final" , section),
      reading  = if_else(str_detect(topic, "Final"), NA      , reading),
      title    = case_when(
        str_detect(title, "Exam")   ~ "None - EXAM",
        str_detect(section, "Film") ~ "None - Film Day",
        .default = title
        )
    ) %>%
    arrange(date) %>%
    select(
      week,
      daywk,
      date,
      location,
      theme,
      chapter,
      title,
      section,
      topic,
      reading,
      assignment
    )
}





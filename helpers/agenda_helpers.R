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






row_class <- function(x) {
  row <- pluck(x, "row")
  if (row == "d1") {
    "border-primary"
  } else if (row == "d2") {
    "border-secondary"
  } else if (row == "dnoclass") {
    "text-white bg-secondary"
  } else if (row == "dexam") {
    "text-white bg-warning"
  }
}

import_schedule <- function(course = str_remove(params$course, "_.+$")) {
  yaml::read_yaml(here::here(course, "schedule.yaml")) %>%
    imap(\(x, idx) imap(x, \(y, idy) list_assign(
      y,
      class_day = paste0(idx, idy),
      week  = idx,
      wday  = format(as.POSIXct(pluck(y, "date")), format = "%a"),
      date  = format(as.POSIXct(pluck(y, "date")), format = "%A, %B %e")
    ))) %>%
    map_depth(2, \(x) list_assign(
      x,
      row  = row_vals(x),
      unit = format_unit(x)
    )) %>%
    map_depth(2, \(x) list_assign(
      x,
      class     = row_class(x)
    ))
}

get_icon <- function(x, size = NULL) {
  name <-  switch(
    x,
    "podcast"    = "fa-brands fa-spotify",
    "textbook"   = "fa-solid fa-book",
    "literature" = "fa-solid fa-newspaper",
    "slides"     = "fa-brands fa-chromecast",
    "transcript" = "fa-solid fa-closed-captioning",
    "film"       = "fa-solid fa-film",
    "video"      = "fa-brands fa-youtube",
    "case_convo" = "fa-solid fa-comments",
    "link"       = "fa-solid fa-link",
    "pdf"        = "fa-solid fa-file-pdf",
    "topic"      = "fa-solid fa-arrow-up-right-from-square",
    "page"       = "fa-solid fa-arrow-up-right-from-square"
  )
  icon <- if (is.null(size)) name else paste(name, size)
  return(str_glue("<i class='{icon}'></i>"))
}


icon_link <- function(href, icon = NULL, class = "nav-item px-1 text-primary") {
  icon <- if (is.null(icon)) get_icon("link") else icon
  str_glue("<a href='{href}' class='{class}'>{icon}</a>")
}


flatten_content <- function(x, type = "background") {
  content <- if (!(type %in% names(x)) || length(pluck(x, type)) < 1) "" else str_flatten(
    unlist(pluck(x, type)),
    na.rm = TRUE
  )

  return(content)

}

flatten_background <- function(x) {
  background <- if (!("background" %in% names(x)) || length(pluck(x, "background")) < 1) "" else str_flatten(
    unlist(pluck(x, "background")),
    na.rm = TRUE
    )

  return(background)

}

blank_rows <- function(x) {
  cases <- if ("cases" %in% names(x) && length(pluck(x, "cases")) > 0)    pluck(x, "cases" ) else ""
  slides <- if ("slides" %in% names(x) && length(pluck(x, "slides")) > 0) pluck(x, "slides") else ""
  topics <- if ("topics" %in% names(x) && length(pluck(x, "topics")) > 0) pluck(x, "topics") else ""
  unit <- if ("unit" %in% names(x) && length(pluck(x, "unit")) > 0)       pluck(x, "unit"  ) else ""
  background <- if ("background" %in% names(x) && length(pluck(x, "background")) > 0)       pluck(x, "background"  ) else ""

  return(list_assign(x, cases = cases, slides = slides, topics = topics, unit = unit, background = background))
}


populate_schedule <- function(course = str_remove(params$course, "_.+$")) {
  schedule   <- import_schedule(course = course) %>%
    map_depth(
      2,
      \(x) modify_at(x, "topics", \(y) map(y, topic_pages))
    ) %>%
    map_depth(
      2,
      \(x) modify_at(x, "background", \(y) map(y, content_background))
    ) %>%
    map_depth(
      2,
      \(x) modify_at(x, "cases", \(y) map(y, content_cases))
    ) %>%
    map_depth(
      2,
      \(x) modify_at(x, "slides", \(y) map(y, content_slides))
    ) %>%
    map_depth(
      2,
      \(x) list_assign(
        x,
        background = flatten_background(x),
        topics     = flatten_content(x, "topics"),
        slides     = flatten_content(x, "slides")
      )
    ) %>%
    map_depth(2, blank_rows)
  return(schedule)
}

content_cases <- function(x) {
  if (is.null(x) || length(x) < 1) return(NA_character_)
  number <- pluck(x, "number")
  topic  <- pluck(x, "topic")
  leaders <- if ("leaders" %in% names(x)) pluck(x, "leaders") else NULL
  leaders_flat <- if (is.null(leaders) || length(leaders) < 1) "" else str_flatten_comma(leaders, na.rm = TRUE)
  return(str_glue("<li class='list-group-item list-group-item-success d-flex justify-content-between align-items-center'><span class='badge bg-success me-1 float-start'>Case</span><div class='d-flex flex-column justify-content-around'><div class='text-success-emphasis'>Conversation {number}</div><em class='text-muted'>{topic}</em></div><small class='text-muted'>{leaders_flat}</small><span class='float-end'><i class='fa-solid fa-comments'></i></span></li>"))
}

content_slides <- function(slides) {
  if (is.null(slides) || length(slides) < 1) return(NA_character_)
  links <- map(slides, \(x) icon_link(x, icon = get_icon("slides", size = "fa-lg"))) %>%
    unlist() %>%
    unique() %>%
    str_flatten(na.rm = TRUE)
  return(str_glue("<li class='list-group-item list-group-item-info d-flex justify-content-between align-items-center'><span class='badge bg-info me-1 float-start'>Slides</span><span class='float-end'>{links}</span></li>"))
}



content_background <- function(x) {
  if (is.null(x) || length(x) < 1) return(NA_character_)
  type <- pluck(x, "type")
  icon <- get_icon(type, size="fa-xl")

  if (type %in% c("podcast")) {
    action <- "Listen"
    url_direct <- pluck(x, "path_spotify")
    url_second <- pluck(x, "path_page")
    my <- format(as.POSIXct(pluck(x, "released")), format = "%b %Y")
    title_main <- pluck(x, "series")
    title_second <- sprintf("%s (%s, released %s)", pluck(x, "title"), pluck(x, "duration_string"), my)
    link_direct <- icon_link(pluck(x, "path_spotify"), icon = icon)
    link_second <- icon_link(pluck(x, "path_page"), icon = get_icon("page"))
  } else {
    action <- "Reading"
    if (type == "literature") {
      link_second <- icon_link(pluck(x, "path_pdf"), icon = get_icon("pdf", size = "fa-xl"))
      title_main <- sprintf("%s, %s", pluck(x, "author_display"), pluck(x, "year"))
      title_second <- pluck(x, "title")
      link_direct <- icon_link(pluck(x, "path_external"), icon = icon)
    } else {
      link_second <- icon_link(pluck(x, "path_external"), icon = icon)
      link_direct <- icon_link(pluck(x, "path_pdf"), icon = get_icon("pdf", size = "fa-xl"))
      title_main   <- sprintf("Ch %.0f", pluck(x, "chapter"))
      length <- pluck(x, "duration_string")
      assigned <- if (is.null(length) || length(length) < 1) "" else sprintf(" (%s)", str_flatten_comma(length, na.rm = TRUE))
      title_second <- paste0(pluck(x, "title"), assigned)
    }
  }
  badge <- str_glue("<span class='badge bg-primary me-1 float-start'>{action}</span>")
  content <- str_glue("<li class='list-group-item list-group-item-primary d-flex justify-content-between align-items-center'>{badge}<div class='d-flex flex-column justify-content-around'><div class='text-primary-emphasis'>{title_main}</div><small class='text-muted'>{title_second}</small></div>{link_direct}{link_second}</li>")
  return(content)
}

topic_pages <- function(topics) {
  str_glue_data(topics, "<a href='{path}' class='card-link'>{title}</a>")
}

day_card <- function(x) {
  str_glue_data(x, "<div class='list-group-item card {class} m-1 p-1 w-50'><div class='card-header'>{date}</div><div class='card-body'><h5 class='card-title'>{topics}</h5><h6 class='card-subtitle text-muted mb-3'>{unit}</h6><ul class='list-group my-3'>{background}{cases}{slides}</ul></div></div>")
}

accord_week <- function(x, idx) {
  dates <- sprintf("%s - %s", str_extract(pluck(x, 1, "date"), "(?<=, ).+$"), str_extract(pluck(x, -1, "date"), "(?<=, ).+$"))
  cards <- str_flatten(unlist(map(x, day_card)), na.rm = TRUE)
  str_glue("<div class='accordion-item'><h2 class='accordion-header' id=heading{idx}><button class='accordion-button collapsed' type='button' data-bs-toggle='collapse' data-bs-target='#collapse{idx}' aria-expanded='false' aria-controls='collapse{idx}'><div class='d-flex justify-content-between'><strong class='pe-3 me-3'>{idx}</strong><small class='text-muted text-end px-3 mx-3'>{dates}</small></div></button></h2><div id='collapse{idx}' class='accordion-collapse collapse' aria-labelledby='heading{idx}' data-bs-parent='#main'><div class='accordion-body list-group list-group-horizontal d-flex justify-content-between align-content-center w-100'>{cards}</div></div></div>")
}


accord_top <- function(schedule_list) {
  weeks <- str_flatten(unlist(imap(schedule_list, accord_week)), na.rm = TRUE)
  str_glue("<div class='accordion' id='main'>{weeks}</div>")
}


remove_course_prefix <- function(x) {
  str_trim(str_remove(x, "\\w{4}\\d{4}$"))
}

generate_id <- function(title) {
  str_to_lower(str_sub(str_remove_all(title, "[^\\w\\d]"), 1L, 6L))
}


format_unit <- function(x) {
  unit <- pluck(x, "themes")
  if (str_detect(unit, "(Exam)|(Break)")) {
    return("")
  } else {
    return(remove_course_prefix(unit))
  }
}

row_vals <- function(x) {
  themes <- pluck(x, "themes")
  topics <- pluck(x, "topics") %>%
    map(\(y) keep_at(y, "title")) %>%
    list_flatten(name_spec = "{inner}") %>%
    unlist() %>%
    unique() %>%
    str_flatten_comma(na.rm = TRUE)

  if (str_detect(themes, "Exam") || str_detect(topics, "Exam")) {
    return("dexam")
  } else if (str_detect(themes, "(Holiday)|(Break)|(No Class)") || str_detect(topics, "(Holiday)|(Break)|(No Class)")) {
    return("dnoclass")
  } else {
    return(str_to_lower(str_extract(pluck(x, "class_day"), "D\\d+")))
  }
}



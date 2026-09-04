podcast_card_html <- function(podcast, course_name = str_remove(params$course, "_.+$")) {
  if (length(podcast) < 1 || is.null(podcast)) return(NULL)

  classes <- str_extract(pluck(podcast, "classes_assigned", 1, "class_day"), "(?<=_).+$")
  dates   <- str_extract(pluck(podcast, "classes_assigned", 1, "date"),"(?<=2026-)\\d+-\\d+")
  deadline <- sprintf("%s (%s)", dates, classes)
  card_data <- list_assign(
    podcast,
    clean_title     = str_remove_all(pluck(podcast, "title"), "_"),
    image_link      = str_sub(str_extract(pluck(podcast, "image"), "https.+"), 1, -2),
    course_name     = course_name,
    themes          = str_flatten_comma(unlist(pluck(podcast, "themes"))),
    topics          = str_flatten_comma(unlist(pluck(podcast, "topics"))),
    deadline        = deadline
  )

  return(as.character(str_glue_data(card_data, "<div class='card border-primary my-2 w-auto'><h5 class='card-header d-flex justify-content-between align-items-center'><small>{clean_title}</small><a href='{url_spotify}' class='card-link'><i class='fa-brands fa-spotify fa-xl'></i></a></h6><div class='card-body'><h5 class='d-inline-flex'><em class='card-title'>{series}</em><a href='{series_spotify}' class='card-link'><i class='fa-solid fa-rss fa-lg'></i></a></h6><div class='d-flex justify-content-between align-items-center'><button type='button' class='btn btn-outline-dark'>Released {released}</button><button type='button' class='btn btn-outline-dark'>Duration {duration_string}</button><button type='button' class='btn btn-outline-dark'>Listen before {deadline}</button><button type='button' class='btn btn-outline-dark'><a href='https://rich-molecular-health-lab.github.io/courses/{course_name}/podcast/{title}.html' class='card-link'><i class='fa-solid fa-link fa-lg px-2'></i>Transcript</a></button></div></div><div id='accordion'><div class='card-footer text-muted'><small><a class='btn' data-bs-toggle='collapse' href='#collapseOne'>Description</a></small><div id='collapseOne' class='collapse' data-bs-parent='#accordion'><p>{description}</p></div></div></div>")))
}

case_card_html <- function(case_convo) {
  if (length(case_convo) < 1 || is.null(case_convo)) return(NULL)

  leaders <- if (length(pluck(case_convo, "leaders")) > 0) str_flatten_comma(unlist(pluck(case_convo, "leaders"))) else "Not assigned"
  card_data <- list_assign(
    case_convo,
    leaders = leaders
  )

  as.character(str_glue_data(card_data, "<div class='card border-info mb-1 w-auto'><div class='card-header'>{title}</div><div class='card-body'><div class='d-flex w-100 justify-content-between'><strong class='card-text mb-1'>Leaders:</strong><p class='card-text mb-1'>{leaders}</p></div>"))
}

podcast_card <- function(podcast) {
  if (length(podcast) > 1) {
    map(podcast, podcast_card_html) %>%
      merge_cards()
  } else {
    podcast_card_html(list_flatten(podcast))
  }
}

case_card <- function(case_convo, course_name = str_remove(params$course, "_.+$")) {
  if (length(case_convo) > 1) {
    map(case_convo, case_card_html) %>%
      merge_cards()
  } else {
    case_card_html(list_flatten(case_convo))
  }
}

merge_cards <- function(cards) {
  if (length(cards) < 2) return(cards)
  as.character(
    paste0(
      "<div class='d-flex justify-content-around align-items-start w-auto my-1 mx-0 p-0'>",
      str_c(cards),
      "</div>"
    )
  )
}

merge_days <- function(list, days) {
  new_list <- map(days, \(x) compact(list(keep_at(list, \(y) str_starts(y, x))))) %>%
    compact() %>%
    list_flatten()
  new_names <- names(list_flatten(map(new_list, \(x) keep_at(x, 1)))) %>%
    map(\(x) str_extract(x, ".+(?=_)"))

  result <-  set_names(new_list, new_names) %>%
    map(merge_cards)

  return(result)
}


page_link <- function(title, path) {
  if (str_detect(title, "(Exam)|(Break)|(Holiday)|(No Class)")) return(str_remove(title, " ENVN4320$"))
  as.character(str_glue(
    "<div class='d-inline-flex'><span>{title}</span><a class='nav-link active' href={path} target='_blank'><i class='fa-solid fa-link'></i></a></div>"
  ))
}

format_html <- function(li, course_name = str_remove(params$course, "_.+$")) {
  if (length(li) < 1) return(li)
  titles <- map(li, \(x) str_remove(str_remove_all(pluck(x, "title"), "_"), " ENVN4320$"))
  links  <- map(li, \(x) sprintf("https://rich-molecular-health-lab.github.io/courses/%s/%s", course_name, str_replace(pluck(x, "path"), "qmd", "html")))

  result <- map(li, \(x) page_link(
    title = str_remove(str_remove_all(pluck(x, "title"), "_"), " ENVN4320$"),
    path  = sprintf("https://rich-molecular-health-lab.github.io/courses/%s/%s", course_name, str_replace(pluck(x, "path"), "qmd", "html"))
  )) %>%
    unlist() %>%
    str_flatten_comma(na.rm = TRUE)

  return(result)
}

format_unit <- function(x) {
  unit <- pluck(x, "unit", 1, "title")
  if (str_detect(unit, "(Exam)|(Break)")) return("")
  return(str_remove(unit, " ENVN4320$"))
}

topics_linked <- function(x) {
  topics <- pluck(x, "topics")
  if (str_detect(pluck(topics, 1, "title"), "Case Conversation")) return("Case Conversations")

  return(format_html(topics))
}

row_vals <- function(x) {
  themes <- pluck(x, "unit") %>%
    list_flatten(name_spec = "{inner}") %>%
    keep_at("title") %>%
    unlist() %>%
    str_flatten_comma(na.rm = TRUE)
  topics <- pluck(x, "topics") %>%
    list_flatten(name_spec = "{inner}") %>%
    keep_at("title") %>%
    unlist() %>%
    str_flatten_comma(na.rm = TRUE)

  if (str_detect(themes, "Exam") || str_detect(topics, "Exam")) {
    return("dexam")
  } else if (str_detect(themes, "(Holiday)|(Break)|(No Class)") || str_detect(topics, "(Holiday)|(Break)|(No Class)")) {
    return("dnoclass")
  } else {
    return(str_to_lower(str_extract(pluck(x, "class_day"), "D\\d+")))
  }
}


podcast_titles <- function(x) {
  background <- pluck(x, "background")
  if (length(background) < 1 || is.null(background)) return(NA_character_)
  paste(
    "<i class='fa-solid fa-podcast'></i>",
    str_flatten_comma(unique(unlist(compact(map(background, \(y) pluck(y, "series"))))), na.rm = TRUE),
    sep = " "
  )
}

case_titles <- function(x) {
  case_convo <- pluck(x, "case_convo")
  if (length(case_convo) < 1 || is.null(case_convo)) return(NA_character_)
  paste(
    "<i class='fa-solid fa-comments'></i>",
    str_flatten_comma(unique(unlist(compact(map(case_convo, \(y) paste0("CC", pluck(y, "number")))))), na.rm = TRUE),
    sep = " "
  )
}

special_vals <- function(x) {
  podcast <- podcast_titles(x)
  case    <- case_titles(x)

  if (!is.na(podcast) || !is.na(case)) {
    return(str_flatten_comma(c(podcast, case), na.rm = TRUE))
  } else {
    return(NA_character_)
  }
}

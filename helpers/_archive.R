
format_slides <- function(slides, course) {
  if (length(slides) < 1 || is.null(slides)) {
    NULL
  } else if (str_starts(slides, "http")) {
    slides_link(slides)
  } else {
    slides_link(sprintf("https://rich-molecular-health-lab.github.io/courses/%s/slides/%s/index.html", course, slides))
  }
}

special_vals <- function(x) {
  podcast <- podcast_titles(x)
  case    <- case_titles(x)
  text    <- text_titles(x)

  if (!is.na(podcast) || !is.na(case) || !is.na(text)) {
    return(str_flatten_comma(c(podcast, case, text), na.rm = TRUE))
  } else {
    return(NA_character_)
  }
}


case_titles <- function(x) {
  if ("case_convo" %in% names(x)) {
    case_convo <- pluck(x, "case_convo")
    if (length(case_convo) < 1 || is.null(case_convo)) {
      return(NA_character_)
    } else {
      return(
        paste(
          "<i class='fa-solid fa-comments'></i>",
          str_flatten_comma(unique(unlist(compact(map(case_convo, \(y) paste0("CC", pluck(y, "number")))))), na.rm = TRUE),
          sep = " "
        )
      )
    }
  } else {
    return(NA_character_)
  }
}


lit_titles <- function(x) {
  if ("background" %in% names(x) && "journal" %in% names(pluck(x, "background", 1))) {
    background <- pluck(x, "background")
    if (length(background) < 1 || is.null(background)) {
      return(NA_character_)
    } else {
      return(paste(
        "<i class='fa-solid fa-scroll'></i>",
        str_flatten_comma(unique(unlist(compact(map(background, \(y) pluck(y, "sections_assigned"))))), na.rm = TRUE),
        sep = " "
      ))
    }
  } else {
    return(NA_character_)
  }
}


text_titles <- function(x) {
  if ("background" %in% names(x) && "text_title" %in% names(pluck(x, "background", 1))) {
    background <- pluck(x, "background")
    if (length(background) < 1 || is.null(background)) {
      return(NA_character_)
    } else {
      return(paste(
        "<i class='fa-solid fa-book-open-reader'></i>",
        str_flatten_comma(unique(unlist(compact(map(background, \(y) pluck(y, "sections_assigned"))))), na.rm = TRUE),
        sep = " "
      ))
    }
  } else {
    return(NA_character_)
  }
}



podcast_titles <- function(x) {
  if ("background" %in% names(x) && "url_spotify" %in% names(pluck(x, "background", 1))) {
    background <- pluck(x, "background")
    if (length(background) < 1 || is.null(background)) {
      return(NA_character_)
    } else {
      return(
        paste(
          "<i class='fa-solid fa-podcast'></i>",
          str_flatten_comma(unique(unlist(compact(map(background, \(y) pluck(y, "series"))))), na.rm = TRUE),
          sep = " "
        )
      )
    }
  }
}


topics_linked <- function(x) {
  topics <- pluck(x, "topics")
  if (str_detect(pluck(topics, 1, "title"), "Case Conversation")) return("Case Conversations")

  return(format_html(topics))
}


slides_link <- function(path) {
  as.character(str_glue("<a href={path} class='card-link'><i class='fa-brands fa-slideshare'></i></a>"))
}


format_html <- function(li, course_name = str_remove(params$course, "_.+$")) {
  if (length(li) < 1) return(li)
  titles <- map(li, \(x) remove_course_prefix(str_remove_all(pluck(x, "title"), "_")))
  links  <- map(li, \(x) sprintf("https://rich-molecular-health-lab.github.io/courses/%s/%s", course_name, str_replace(pluck(x, "path"), "qmd", "html")))

  result <- map(li, \(x) page_link(
    title = remove_course_prefix(str_remove_all(pluck(x, "title"), "_")),
    path  = sprintf("https://rich-molecular-health-lab.github.io/courses/%s/%s", course_name, str_replace(pluck(x, "path"), "qmd", "html"))
  )) %>%
    unlist() %>%
    str_flatten_comma(na.rm = TRUE)

  return(result)
}


page_link <- function(title, path) {
  if (str_detect(title, "(Exam)|(Break)|(Holiday)|(No Class)")) return(remove_course_prefix(title))
  as.character(str_glue(
    "<div class='d-inline-flex'><span>{title}</span><a class='nav-link active' href={path} target='_blank'><i class='fa-solid fa-link'></i></a></div>"
  ))
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


merge_cards <- function(cards) {
  if (length(cards) < 2) return(cards)
  as.character(
    paste0(
      "<div class='d-flex justify-content-around align-items-start w-auto my-1 mx-0 p-0'>",
      str_flatten(cards, na.rm = TRUE),
      "</div>"
    )
  )
}


podcast_card <- function(podcast) {
  if (length(podcast) > 1) {
    map(podcast, podcast_card_html) %>%
      merge_cards()
  } else {
    podcast_card_html(list_flatten(podcast))
  }
}

textbook_card <- function(chapters) {
  if (length(unique(chapters)) > 1) {
    map(chapters, text_card_html) %>%
      merge_cards()
  } else {
    text_card_html(pluck(chapters, 1))
  }
}


lit_card <- function(literature) {
  if (length(unique(literature)) > 1) {
    map(literature, lit_card_html) %>%
      merge_cards()
  } else {
    lit_card_html(pluck(literature, 1))
  }
}

case_card <- function(case_convo) {
  if (length(unique(case_convo)) > 1) {
    map(case_convo, case_card_html) %>%
      merge_cards()
  } else {
    case_card_html(pluck(case_convo, 1))
  }
}



flatten_table_strings <- function(x) {
  if (is.null(x)) return(x) else if (length(x) < 2) return(compact(keep_at(x, "table_string")))
  strings <- unique(unlist(map(x, \(y) list_flatten(compact(keep_at(y, "table_string"))))))

  return(as.character(str_flatten_comma(strings, na.rm = TRUE)))
}


flatten_cards <- function(x) {
  if (is.null(x)) return(x)
  cards <-str_flatten(unique(unlist(map(x, \(y) list_flatten(compact(keep_at(y, "card")))))), na.rm = TRUE)
  if (length(cards) < 2) return(cards) else return(paste0("<div class='d-flex justify-content-around align-items-start w-auto my-1 mx-0 p-0'>", cards, "</div>"))
}

attachment_link <- function(course = str_remove(params$course, "_.+$")) {
  if (str_detect(course, "conbio")) return("https://github.com/Rich-Molecular-Health-Lab/courses/tree/dd050e72ed98ad06b7b22d968acc7bd49b290587/conbio/attachments") else return("https://github.com/Rich-Molecular-Health-Lab/courses/tree/dd050e72ed98ad06b7b22d968acc7bd49b290587/hhe/attachments")
}


format_literature <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "literature.yaml"))) return(NULL)
  yaml::read_yaml(here::here(course, "literature.yaml")) %>%
    map(\(x) list_assign(
      x,
      table_string = paste0(icon_string(string = sprintf("%s et al. %s", pluck(x, "author_first"), pluck(x, "year")), fa_icon = "fa-solid fa-scroll", class = "info", id = pluck(x, "citekey")), lit_card_html(x, course_name = course)),
      card         = lit_card_html(x, course_name = course),
      class_days   = pluck(x, "classes_assigned", 1, "class_day")
    )) %>%
    map(\(x) keep_at(x, c("table_string", "card", "class_days")))
}
format_background <- function(course = str_remove(params$course, "_.+$")) {
  if (str_detect(course, "conbio")) {

    background <- as.list(format_chapters(course = course), format_literature(course = course))

  } else if (str_detect(course, "hhe")) {
    if (!file_exists(here::here(course, "podcast.yaml"))) return(NULL)
    background <- yaml::read_yaml(here::here(course, "podcast.yaml")) %>%
      map(\(x) list_assign(
        x,
        table_string = paste0(icon_string(string = pluck(x, "series"), fa_icon = "fa-brands fa-spotify", class = "info", id = generate_id(pluck(x, "title"))), podcast_card_html(x, course_name = course)),
        card         = podcast_card_html(x, course_name = course),
        class_days   = pluck(x, "classes_assigned", 1, "class_day")
      )) %>%
      map(\(x) keep_at(x, c("table_string", "card", "class_days")))
  }

  return(background)
}


course_url_prefix <- function(course = str_remove(params$course, "_.+$")) {
  sprintf("https://rich-molecular-health-lab.github.io/courses/%s", course)
}



podcast_card_html <- function(podcast, course_name = str_remove(params$course, "_.+$")) {
  if (length(podcast) < 1 || is.null(podcast)) return(NULL)

  classes <- str_extract(pluck(podcast, "classes_assigned", 1, "class_day"), "(?<=_).+$")
  dates   <- str_extract(pluck(podcast, "classes_assigned", 1, "date"),"(?<=2026-)\\d+-\\d+")
  deadline <- sprintf("%s (%s)", dates, classes)
  transcript_link <- icon_string(string = "Transcript", href = get_local_href(type = "podcast", title = pluck(podcast, "title")))

  card_out <- render_card(
    header_string = str_remove_all(pluck(podcast, "title"), "_"),
    id            = generate_id(pluck(podcast, "title")),
    data_strings  = list(
      paste("Listen before", deadline),
      paste("Duration:", pluck(podcast, "duration_string")),
      paste("Released:", year(ymd(pluck(podcast, "released")))),
      transcript_link
    ),
    title_string = pluck(podcast, "series"),
    footer_title = "Description",
    footer_content = sprintf("<p>%s</p>", str_squish(pluck(podcast, "description"))),
    header_href = pluck(podcast, "url_spotify"),
    title_href  = pluck(podcast, "series_spotify"),
    header_icon = "fa-brands fa-spotify",
    title_icon  = "fa-solid fa-rss",
    card_class  = "primary"
  )

  return(card_out)
}



lit_card_html <- function(literature, course_name = str_remove(params$course, "_.+$")) {
  if (length(literature) < 1 || is.null(literature)) return(NULL)

  classes <- str_extract(pluck(literature, "classes_assigned", 1, "class_day"), "(?<=_).+$")
  dates   <- str_extract(pluck(literature, "classes_assigned", 1, "date"),"(?<=2026-)\\d+-\\d+")
  deadline <- sprintf("%s (%s)", dates, classes)
  pdf_link <- icon_string(string = "Local PDF", href = get_local_href(type = "literature", citekey = pluck(literature, "citekey")))

  card_out <- render_card(
    header_string = sprintf("%s et al. (%s)", str_to_title(pluck(literature, "author_first")), pluck(literature, "year")),
    id = pluck(literature, "citekey"),
    data_strings  = list(
      paste("Read before", deadline),
      pdf_link,
      paste("DOI:", pluck(literature, "doi"))
    ),
    title_string    = str_remove_all(pluck(literature, "title"), "_"),
    subtitle_string = pluck(literature, "journal"),
    footer_title = "Abstract",
    footer_content = sprintf("<p>%s</p>", str_squish(pluck(literature, "description"))),
    header_href = pluck(literature, "url"),
    card_class  = "info"
  )

  return(card_out)
}

text_card_html <- function(chapters, course_name = str_remove(params$course, "_.+$"), text_url = "https://conbio.org/publications/free-textbook/") {
  if (length(chapters) < 1 || is.null(chapters)) return(NULL)

  classes <- str_extract(pluck(chapters, "classes_assigned", 1, "class_day"), "(?<=_).+$")
  dates   <- str_extract(pluck(chapters, "classes_assigned", 1, "date"),"(?<=2026-)\\d+-\\d+")
  deadline <- sprintf("%s (%s)", dates, classes)
  pdf_link <- icon_string(string = "Local PDF", href = get_local_href(type = "literature", citekey = pluck(chapters, "citekey")))

  card_out <- render_card(
    header_string = sprintf("Chapter %.0f", pluck(chapters, "chapter")),
    id = pluck(chapters, "citekey"),
    data_strings  = list(
      paste("Read before", deadline),
      pdf_link,
      paste("Pages:", pluck(chapters, "pages"))
    ),
    data_list       = pluck(chapters, "sections_assigned"),
    data_list_title = "Chapter Sections",
    title_string    = str_squish(pluck(chapters, "title")),
    subtitle_string = pluck(chapters, "text_title"),
    subtitle_href   = text_url,
    card_class  = "info"
  )

  return(card_out)
}

case_card_html <- function(case_convo) {
  if (length(case_convo) < 1 || is.null(case_convo)) return(NULL)

  classes <- str_extract(pluck(case_convo, "classes_assigned", 1, "class_day"), "(?<=_).+$")
  dates   <- str_extract(pluck(case_convo, "classes_assigned", 1, "date"),"(?<=2026-)\\d+-\\d+")
  deadline <- sprintf("%s (%s)", dates, classes)

  card_out <- render_card(
    header_string = sprintf("Case Conversation %.0f", pluck(case_convo, "number")),
    id = sprintf("case%.0f", pluck(case_convo, "number")),
    data_list       = pluck(case_convo, "leaders"),
    data_list_title = "Leaders",
    title_string    = str_squish(pluck(case_convo, "topic")),
    subtitle_string = deadline,
    card_class      = "success"
  )

  return(card_out)
}


slides_href <- function(x, course = str_remove(params$course, "_.+$")) {
  if (is.null(x)) return(NULL) else if (str_starts(x, "http")) return(x) else return(get_local_href(type = "slides", title = x))
}


format_case_convos <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "case_convos.yaml"))) return(NULL)

  case_convos <- yaml::read_yaml(here::here(course, "case_convos.yaml")) %>%
    map(\(x) list_assign(
      x,
      table_string = paste0(icon_string(string = pluck(x, "title"), fa_icon = "fa-brands fa-discourse", class = "secondary", id = sprintf("case%.0f", pluck(x, "number"))), case_card_html(x)),
      card         = case_card_html(x)
    )) %>%
    map(\(x) keep_at(x, c("table_string", "card", "class_days")))

  return(case_convos)
}



format_topics <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "topics.yaml"))) return(NULL)
  topics <- yaml::read_yaml(here::here(course, "topics.yaml")) %>%
    map(\(x) list_assign(
      x,
      slides = map(pluck(x, "slides"), slides_href)
    )) %>%
    map(\(x) list_assign(
      x,
      slides = str_flatten(map(pluck(x, "slides"), \(y) icon_string(href = y, fa_icon = "fa-brands fa-chromecast")), na.rm = TRUE)
    )) %>%
    map(\(x) list_assign(
      x,
      table_string = str_glue_data(x, "<div class='d-flex justify-content-start align-content-stretch'><div class='text-primary-emphasis'>{remove_course_prefix(title)}</div>{slides}</div>"),
      card         = NULL
    )) %>%
    map(\(x) keep_at(x, c("table_string", "card", "class_days")))

  return(topics)
}

format_chapters <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "textbook.yaml"))) return(NULL)
  yaml::read_yaml(here::here(course, "textbook.yaml")) %>%
    map(\(x) list_assign(
      x,
      table_string = paste0(icon_string(string = pluck(x, "title"), fa_icon = "fa-solid fa-book", class = "info", id = pluck(x, "citekey")), text_card_html(x, course_name = course)),
      card         = text_card_html(x, course_name = course),
      class_days   = pluck(x, "classes_assigned", 1, "class_day")
    )) %>%
    map(\(x) keep_at(x, c("table_string", "card", "class_days")))
}


format_literature <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "literature.yaml"))) return(NULL)
  yaml::read_yaml(here::here(course, "literature.yaml")) %>%
    map(\(x) list_assign(
      x,
      table_string = paste0(icon_string(string = sprintf("%s et al. %s", pluck(x, "author_first"), pluck(x, "year")), fa_icon = "fa-solid fa-scroll", class = "info", id = pluck(x, "citekey")), lit_card_html(x, course_name = course)),
      card         = lit_card_html(x, course_name = course),
      class_days   = pluck(x, "classes_assigned", 1, "class_day")
    )) %>%
    map(\(x) keep_at(x, c("table_string", "card", "class_days")))
}

format_background <- function(course = str_remove(params$course, "_.+$")) {
  if (str_detect(course, "conbio")) {

    background <- as.list(format_chapters(course = course), format_literature(course = course))

  } else if (str_detect(course, "hhe")) {
    if (!file_exists(here::here(course, "podcast.yaml"))) return(NULL)
    background <- yaml::read_yaml(here::here(course, "podcast.yaml")) %>%
      map(\(x) list_assign(
        x,
        table_string = paste0(icon_string(string = pluck(x, "series"), fa_icon = "fa-brands fa-spotify", class = "info", id = generate_id(pluck(x, "title"))), podcast_card_html(x, course_name = course)),
        card         = podcast_card_html(x, course_name = course),
        class_days   = pluck(x, "classes_assigned", 1, "class_day")
      )) %>%
      map(\(x) keep_at(x, c("table_string", "card", "class_days")))
  }

  return(background)
}
render_card <- function(header_string, id, data_strings = list(NULL), data_list = list(NULL), data_list_title = NULL, title_string = NULL, subtitle_string = NULL, footer_title = NULL, footer_content = NULL, header_href = NULL, title_href = NULL, subtitle_href = NULL, header_icon = "fa-solid fa-link", title_icon = "fa-solid fa-link", subtitle_icon = "fa-solid fa-link", card_class = "primary") {
  footer <- if (is.null(footer_content) || is.null(footer_title)) "" else card_footer_collapse(footer_title = footer_title, footer_content = footer_content)
  title  <- if (is.null(title_string)) "" else card_title_block(title_string = title_string, subtitle_string = subtitle_string, title_href = title_href, subtitle_href = subtitle_href, title_icon = title_icon, subtitle_icon = subtitle_icon)
  body_list <- if (length(data_list) < 1) "" else card_data_list(data_list = data_list, list_header = data_list_title)
  body_row  <- if (length(data_strings) < 1) "" else card_data_row(unlist(map(data_strings, card_data_button)))
  card_data <- list(
    id     = id,
    class  = card_class,
    header = card_header_row(string = header_string, href = header_href, fa_icon = header_icon),
    title  = title,
    body   = paste0(body_row, body_list),
    footer = footer
  )
  return(as.character(str_glue_data(card_data, "<div class='card border-{class} my-2 w-auto id='{id}' class='collapse'>{header}<div class='card-body'>{title}{body}{footer}</div></div>")))
}


format_schedule <- function(schedule_list, course = str_remove(params$course, "_.+$")) {
  background  <- format_background(course = course)
  topics      <- format_topics(course = course)
  case_convos <- format_case_convos(course = course)
  map(schedule_list, \(x) list_assign(
    x,
    background = compact(keep(background, \(y) all(pluck(x, "class_day") %in% pluck(y, "class_days")))),
    topics     = compact(keep(topics, \(y) all(pluck(x, "class_day") %in% pluck(y, "class_days")))),
    case_convos = compact(keep(case_convos, \(y) all(pluck(x, "class_day") %in% pluck(y, "class_days"))))
  )) %>%
    map(\(x) list_assign(
      x,
      background = flatten_content(pluck(x, "background")),
      topics     = flatten_content(pluck(x, "topics")),
      case_convos = flatten_content(pluck(x, "case_convos"))
    )) %>%
    map(\(x) list_assign(
      x,
      background = pluck(x, "background", "table_string"),
      background_detail = pluck(x, "background", "card"),
      topics     = merge_topics(topics = pluck(x, "topics", "table_string"), case_convos = pluck(x, "case_convos", "table_string")),
      topics_detail = pluck(x, "case_convos", "card")
    )) %>%
    map(\(x) keep_at(x, c(
      "row",
      "class_day",
      "date",
      "week",
      "day",
      "unit",
      "topics",
      "background",
      "topics_detail",
      "background_detail"
    )))
}


get_local_href <- function(type = "slides", citekey = NULL, title = NULL) {
  if ((is.null(citekey) && type %in% c("pdf", "literature", "textbook", "attachments")) || (is.null(title) && type %in% c("slides", "topic", "unit", "podcast", "page"))) return("")

  if (type == "slides") return (str_glue("slides/{title}/index.html")) else if (type == "podcast") return(str_glue("podcast/{title}.qmd")) else return(str_glue("attachments/{citekey}.pdf"))
}

icon_string <- function(string = NULL, href = NULL, fa_icon = "fa-solid fa-link", class = "primary", id = NULL) {
  if (is.null(string) & is.null(href)) return("")

  if (!is.null(href) && !is.null(string)) {
    as.character(str_glue("<div class='d-flex justify-content-start align-content-stretch'>{string}<button type='button' class='btn btn-{class} btn-sm'><a class='nav-link active' href={href}><i class='{fa_icon}'></i></a></button></div>"))
  } else if (is.null(href) && !is.null(string)) {
    as.character(str_glue("<div class='d-flex justify-content-start align-content-stretch'>{string}<button data-bs-toggle='collapse' data-bs-target='#{id}' type='button' class='btn btn-{class} btn-sm><i class='{fa_icon}'></i></button></div>"))
  } else if (is.null(href) && is.null(string)) {
    as.character(str_glue("<button data-bs-toggle='collapse' data-bs-target='#{id}' type='button' class='btn btn-{class} btn-sm><i class='{fa_icon}'></i></button>"))
  } else {
    as.character(str_glue("<button type='button' class='btn btn-{class} btn-sm'><a class='nav-link active' href={href}><i class='{fa_icon}'></i></a></button>"))
  }
}


card_header_row <- function(string, href = NULL, fa_icon = "fa-solid fa-link", class = "primary") {
  if (is.null(href)) {
    str_glue("<h5 class='card-header'><small>{string}</small></h5>")
  } else {
    icon <- header_icon(href = href, fa_icon = fa_icon, class = class)
    return(str_glue("<h5 class='card-header d-flex justify-content-between align-items-center'><small>{string}</small>{icon}</h5>"))
  }
}

card_title_block <- function(title_string, subtitle_string = NULL, title_href = NULL, subtitle_href = NULL, title_icon = "fa-solid fa-link", subtitle_icon = "fa-solid fa-link", class = "primary") {

  if (!is.null(title_href)) {
    icon_title <- header_icon(href = title_href, fa_icon = title_icon, class = class)
    title <- str_glue("<h5 class='card-title d-inline-flex'><small>{title_string}</small>{icon_title}</h5>")
  } else {
    title <- str_glue("<h5 class='card-title'><small>{title_string}</small></h5>")
  }

  if (is.null(subtitle_string)) {
    subtitle <-  ""
  } else if (is.null(subtitle_href)) {
    subtitle <- str_glue("<h6 class='card-subtitle text-muted'><small>{subtitle_string}</small></h6>")
  } else {
    icon_subtitle <- header_icon(href = subtitle_href, fa_icon = subtitle_icon, class = class)
    subtitle <- str_glue("<h6 class='card-subtitle text-muted d-inline-flex'><small>{subtitle_string}</small>{icon_subtitle}</h6>")
  }

  return(paste0(title, subtitle))
}

card_data_button <- function(string) {
  str_glue("<button type='button' class='btn btn-outline-dark'>{string}</button>")
}
card_data_li <- function(string) {
  str_glue("<button type='button' class='list-group-item btn btn-outline-dark'>{string}</button>")
}

card_data_list <- function(data_list, list_header = NULL) {
  title <- if (is.null(list_header)) "" else sprintf("<h5 class='card-title'><small>%s: </small></h5>", list_header)
  str_glue("<ul class='list-group list-group-horizontal'>{str_flatten(unlist(map(data_list, card_data_li)), na.rm = TRUE)}</div>")
}
card_data_row <- function(buttons) {
  str_glue("<div class='d-flex justify-content-start align-items-center'>{str_flatten(buttons, na.rm = TRUE)}</div>")
}

accord_cell <- function(class_day, col, cards) {
  str_glue("<div id='{class_day}_{col}'>{cards}</div>")
}

header_icon <- function(href, fa_icon = "fa-solid fa-link", class = "primary") {

  as.character(str_glue("<button type='button' class='btn btn-{class}'><a href={href} class='card-link'><i class='{fa_icon} fa-lg px-2'></i></a></button>"))

}


merge_topics <- function(topics, case_convos = NULL) {
  if (length(case_convos) < 1 || is.null(case_convos)) return(str_glue("<div class='d-flex flex-column align-content-start'>{str_flatten(topics, na.rm = TRUE)}</div>")) else return(str_glue("<div class='d-flex flex-column align-content-start'>{str_flatten(topics, na.rm = TRUE)}{str_flatten(case_convos, na.rm = TRUE)}</div>"))
}

flatten_content <- function(content) {
  if (length(content) < 1 || is.null(content)) return(content)
  strings <- map(content, \(x) keep_at(x, "table_string")) %>%
    list_flatten()
  cards <- map(content, \(x) keep_at(x, "card")) %>%
    list_flatten() %>%
    unlist() %>%
    unique() %>%
    str_flatten(na.rm = TRUE)
  cards_html <- if (length(cards) < 1) NA_character_ else if (length(cards) > 1) str_glue("<div class='d-flex justify-content-between align-items-start w-auto my-1 mx-0 p-0'>{cards}</div>") else cards
  return(list(
    table_string = merge_topics(topics = unique(unlist(strings))),
    card         = cards_html
  ))
}


populate_schedule <- function(schedule, course = str_remove(params$course, "_.+$")) {
  background <- list(
    chapters   = format_chapters(course = course),
    literature = format_literature(course = course),
    podcasts   = format_podcast(course = course)
  ) %>%
    compact() %>%
    map_depth(2, \(x) keep_at(x, "card"))

  topics <- format_topics(course = course)

  updated <- schedule  %>%
    imap(\(x, idx) list_assign(
      x,
      cards_background = str_flatten(unlist(list_flatten(map_depth(background, 1, \(y) pluck(y, idx)))), na.rm = TRUE),
      background_id    = paste0(idx, "_background")
    )) %>%
    imap(\(x, idx) list_assign(
      x,
      background = background_accord(x),
      topics     = pluck(topics, idx),
      wday       = format(as.POSIXct(pluck(x, "date")), format = "%a"),
      date       = format(as.POSIXct(pluck(x, "date")), format = "%b %e")
    )) %>%
    map(\(x) keep_at(
      x,
      c(
        "week",
        "wday",
        "date",
        "unit",
        "topics",
        "background",
        "row"
      )
    ))

  return(updated)
}
generate_schedule <- function(course = str_remove(params$course, "_.+$")) {
  schedule <- import_schedule(course = course) %>%
    populate_schedule(course = course) %>%
    enframe(name = "week_day") %>%
    unnest_wider(value)

  return(schedule)
}


format_chapters <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "textbook.yaml"))) return(NULL)
  yaml::read_yaml(here::here(course, "textbook.yaml")) %>%
    extract_deadline() %>%
    map(\(x) list_assign(
      x,
      id      = pluck(x, "citekey"),
      fa_icon = "fa-solid fa-book",
      title_block   = str_glue_data(x, "<h5 class='card-title'>{title}</h5><h6 class='card-subtitle text-muted'>by {author} (pages {pages})</h6>"),
      header_string = str_glue_data(x, "<div class='d-flex justify-content-between align-content-center'><small class='text-body-secondary'>{text_title}</small><div class='text-primary-emphasis'>Chapter {chapter}</div></div>"),
      body_block    = list_grp_body(
        c(
          list_item_body(name = "Read by"     , value = pluck(x, "deadline")),
          list_item_body(name = "Sections"    , value = sections_assigned(sections = pluck(x, "sections_assigned"))),
          list_item_body(name = "PDF download", value = download_pdf(citekey = pluck(x, "citekey"))),
          list_item_body(name = "eText"       , value = etext_url(isbn = pluck(x, "isbn")))
        )
      )
    )) %>%
    map(\(x) list_assign(
      x,
      card = accord_card(
        id            = pluck(x, "id"),
        header_string = pluck(x, "header_string"),
        accord_id     = paste0(pluck(x, "first_class"), "_background"),
        header_icon   = pluck(x, "fa_icon"),
        title_block   = pluck(x, "title_block"),
        body_block    = pluck(x, "body_block")
      )
    ))
}

format_topics <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "topics.yaml"))) return(NULL)
  topics <- yaml::read_yaml(here::here(course, "topics.yaml"))  %>%
    map(\(x) map_at(x, "class_days", \(y) str_extract(y, "W\\d{1,2}D\\d{1,2}$")))  %>%
    map(\(x) list_assign(
      x,
      id            = paste(pluck(x, "class_days"), generate_id(pluck(x, "title")), sep = "_"),
      accord_id     = paste(pluck(x, "class_days"), "topics", sep = "_"),
      fa_icon       = rep("fa-brands fa-chromecast", length(pluck(x, "class_days"))),
      header_string = rep(pluck(x, "title"), length(pluck(x, "class_days"))),
      body_block    = rep(list_grp_body(unlist(map(pluck(x, "slides"), slides_button))), length(pluck(x, "class_days")))
    )) %>%
    map(\(x) keep_at(
      x,
      c(
        "id",
        "accord_id",
        "fa_icon",
        "header_string",
        "body_block",
        "class_days",
        "dates"
      )
    )) %>%
    map(\(x) list_transpose(x, simplify = FALSE)) %>%
    list_flatten(name_spec = "{inner}") %>%
    discard(\(x) is.null(pluck(x, "class_days"))) %>%
    set_names(map(., \(x) pluck(x, "id"))) %>%
    map(\(x) list_assign(
      x,
      card = accord_card(
        id            = pluck(x, "id"),
        header_string = pluck(x, "header_string"),
        accord_id     = pluck(x, "accord_id"),
        header_icon   = pluck(x, "fa_icon"),
        body_block    = pluck(x, "body_block")
      )
    ))

  accords <- unique(map(topics, \(x) pluck(x, "accord_id"))) %>%
    set_names(~paste0(.)) %>%
    imap(\(x, idx) compact(keep(topics, \(y) any(pluck(y, "accord_id") %in% idx)))) %>%
    imap(\(x, idx) list_assign(
      x,
      cards = str_flatten(unlist(map(x, \(y) pluck(y, "card"))), na.rm = TRUE),
      accord_id = idx
    )) %>%
    map(\(x) list_assign(
      x,
      accordion = str_glue_data(x, "<div id={accord_id}>{cards}</div>")
    )) %>%
    map(\(x) keep_at(x, "accordion")) %>%
    list_flatten(name_spec = "{outer}") %>%
    set_names(imap(., \(x, idx) str_remove(idx, "_.+$")))

  return(accords)
}

format_cases <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "case_convos.yaml"))) return(NULL)
  yaml::read_yaml(here::here(course, "case_convos.yaml")) %>%
    extract_scheduled() %>%
    map(\(x) list_assign(
      x,
      id      = str_glue_data(x, "case{number}"),
      fa_icon = "fa-brands fa-discourse",
      header_string = str_glue_data(x, "<div class='d-flex justify-content-between align-content-center'><small class='text-body-secondary'>Case Conversation {number}</small><div class='text-primary-emphasis'>{topic}</div></div>"),
      title_block   = str_glue_data(x, "<h5 class='card-title'>{scheduled_date}</h5><h6 class='card-subtitle text-muted'>{scheduled_class}</h6>"),
      body_block    = case_leaders(pluck(x, "leaders"))
    )) %>%
    map(\(x) list_assign(
      x,
      card = accord_card(
        id            = pluck(x, "id"),
        header_string = pluck(x, "header_string"),
        header_icon   = pluck(x, "fa_icon"),
        title_block   = pluck(x, "title_block"),
        body_block    = pluck(x, "body_block")
      )
    ))
}

format_literature <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "literature.yaml"))) return(NULL)
  yaml::read_yaml(here::here(course, "literature.yaml")) %>%
    extract_deadline() %>%
    map(\(x) list_assign(
      x,
      id      = pluck(x, "citekey"),
      fa_icon = "fa-solid fa-newspaper",
      title_block   = str_glue_data(x, "<h5 class='card-title'>{title}</h5><h6 class='card-subtitle text-muted'>by {authors}</h6>"),
      header_string = str_glue_data(x, "<div class='d-flex justify-content-between align-content-center'><div class='text-primary-emphasis'>{author_first} et al. ({year})</div><small class='text-body-secondary'>{journal}</small><</div>"),
      body_block    = list_grp_body(c(list_item_body("Read by", pluck(x, "deadline")), list_item_body("URL", other_url(pluck(x, "url"))), list_item_body("PDF download", download_pdf(pluck(x, "citekey"))), list_item_body("DOI", pluck(x, "doi")))),
      footer_block  = card_footer_collapse("Abstract", str_glue_data(x, "<p class='card-footer text-muted'>{str_squish(description)}</p>"))
    )) %>%
    map(\(x) list_assign(
      x,
      card = accord_card(
        id            = pluck(x, "id"),
        header_string = pluck(x, "header_string"),
        accord_id     = paste0(pluck(x, "first_class"), "_background"),
        header_icon   = pluck(x, "fa_icon"),
        title_block   = pluck(x, "title_block"),
        body_block    = pluck(x, "body_block"),
        footer_block  = pluck(x, "footer_block")
      )
    ))
}

format_podcast <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "podcast.yaml"))) return(NULL)
  yaml::read_yaml(here::here(course, "podcast.yaml")) %>%
    extract_deadline() %>%
    map(\(x) list_assign(
      x,
      id      = generate_id(pluck(x, "title")),
      fa_icon = "fa-brands fa-spotify",
      header_string = str_glue_data(x, "<div class='text-primary-emphasis'>{title}</div>"),
      title_block   = str_glue_data(x, "<h5 class='card-title'>{series}</h5>"),
      body_block    = list_grp_body(c(list_item_body("Listen by", pluck(x, "deadline")), list_item_body("Duration", pluck(x, "duration_string")), list_item_body("Episode", other_url(pluck(x, "url_spotify"), fa_icon = "fa-brands fa-spotify")), list_item_body("Series", other_url(pluck(x, "series_spotify"), fa_icon = "fa-solid fa-square-rss")), list_item_body("transcript", other_url(str_glue_data(x, "'podcast/{title}.qmd'"), fa_icon = "fa-solid fa-closed-captioning")), list_item_body("Released", pluck(x, "released")))),
      footer_block  = card_footer_collapse("Description", str_glue_data(x, "<p class='card-footer text-muted'>{str_squish(description)}</p>"))
    )) %>%
    map(\(x) list_assign(
      x,
      card = accord_card(
        id            = pluck(x, "id"),
        header_string = pluck(x, "header_string"),
        accord_id     = paste0(pluck(x, "first_class"), "_background"),
        header_icon   = pluck(x, "fa_icon"),
        title_block   = pluck(x, "title_block"),
        body_block    = pluck(x, "body_block"),
        footer_block  = pluck(x, "footer_block")
      )
    ))
}





extract_deadline <- function(background_list) {
  background_list %>%
    map(\(x) map_at(
      x,
      "classes_assigned",
      \(y) map(y, \(z) modify_in(z, "date", as.POSIXct))
    )) %>%
    map(\(x) list_assign(
      x,
      dates = map(
        list_flatten(keep_at(x, "classes_assigned")),
        \(y) pluck(y, "date")
      ),
      classes = map(
        list_flatten(keep_at(x, "classes_assigned")),
        \(y) str_extract(pluck(y, "class_day"), "W\\d{1,2}D\\d{1,2}$")
      )
    )) %>%
    map(\(x) modify_in(
      x,
      "dates",
      \(y) set_names(y, pluck(x, "classes"))
    )) %>%
    map(\(x) map_at(x, "dates", \(y) map(y, sort))) %>%
    map(\(x) list_assign(
      x,
      deadline    = format(pluck(x, "dates", 1), "%a, %b %d"),
      first_class = first(names(pluck(x, "dates")))
    )) %>%
    set_names(map(., \(x) pluck(x, "first_class")))
}
extract_scheduled <- function(content_list) {
  content_list %>%
    map(\(x) modify_in(x, "dates", as.POSIXct)) %>%
    map(\(x) modify_in(x, "class_days", \(y) str_extract(y, "W\\d{1,2}D\\d{1,2}$"))) %>%
    map(\(x) list_assign(
      x,
      scheduled_date  = format(pluck(x, "dates", 1), "%a, %b %d"),
      scheduled_class = pluck(x, "class_days", 1)
    )) %>%
    set_names(map(., \(x) pluck(x, "scheduled_class")))
}

background_accord <- function(x) {
  if (length(pluck(x, "background")) < 1) return(NULL) else return(str_glue_data(x, "<div id='{background_id}'>{cards_background}</div>"))
}

accord_card <- function(id, header_string, accord_id = NULL, header_icon = "fa-solid fa-chevron-down", title_block = NULL, body_block = NULL, footer_block = NULL, class = "primary") {
  title_block  <- if (is.null(title_block)) "" else title_block
  body_block   <- if (is.null(body_block)) "" else body_block
  footer_block <- if (is.null(footer_block)) "" else footer_block
  accord_id <- if (is.null(accord_id)) sprintf("accordion%s", id) else accord_id

  return(str_glue("<div class='card border-{class} my-2 w-auto'><div class='card-header d-flex justify-content-start align-content-center'><a class='collapsed btn' data-bs-toggle='collapse' href='#{id}' data-bs-parent='#{accord_id}'><i class='{header_icon} fa-lg px-2 mx-3'></i> {header_string}</a></div><div class='card-body'>{title_block}{body_block}{footer_block}</div></div>"))
}



download_pdf <- function(citekey) {
  str_glue("<a href=attachments/{citekey}.pdf><span class='mx-3'><i class='fa-solid fa-file-pdf'></i></span></a>")
}

etext_url <- function(isbn) {
  url <- if (str_detect(isbn, "95542")) "https://conbio.org/publications/free-textbook/" else "https://bookshelf.vitalsource.com/reader/books/9780197667033"
  str_glue("<a href={url}><span class='mx-3'><i class='fa-solid fa-link'></i></span></a>")
}

other_url <- function(href, fa_icon = "fa-solid fa-link") {
  str_glue("<a href={href}><span class='mx-3'><i class='{fa_icon}'></i></span></a>")
}

sections_assigned <- function(sections) {
  if (is.null(sections)) return ("All") else return(str_flatten_comma(sections, na.rm = TRUE))
}

case_leaders <- function(leaders) {
  if (length(leaders) < 1 || is.null(leaders)) return(NULL)
  sprintf("<div class='d-flex justify-content-start align-content-start'><h6>Leaders:</h6>%s</div>", list_grp_body(str_flatten(unlist(map(leaders, list_item_basic)), na.rm = TRUE)))
}
list_grp_body <- function(items) {
  if (is.null(items) || length(items) < 1) return(NULL)
  str_glue("<ul class='list-group list-group-horizontal'>{str_flatten(items, na.rm = TRUE)}</ul>")
}

list_item_body <- function(name, value, class="primary") {
  str_glue("<li class='list-group-item list-group-item-{class} d-flex justify-content-between align-items-center'><span class='badge bg-{class}'>{name}</span>{value}</li>")
}

list_item_basic <- function(value, class="light") {
  str_glue("<li class='list-group-item list-group-item-{class}'>{value}</li>")
}


slides_button <- function(slides) {
  if (is.null(slides) || length(slides) < 1) return("")
  url <- if (str_starts(slides, "http")) slides else sprintf("attachments/%s/index.html", slides)
  str_glue("<a class='btn btn-outline-primary p-3 m-2' href={url}>Slides <i class='fa-solid fa-link'></i></a>")
}



card_footer_collapse <- function(footer_title, footer_content) {
  footer_id <- generate_id(footer_title)
  footer_head <- str_glue("<small><div class='card-header'><a class='collapsed btn' data-bs-toggle='collapse' href='#{footer_id}'>{footer_title}</a></div></small>")
  footer_body <- str_glue("<div id='{footer_id}' class='collapse hide' data-bs-parent='#accordion{footer_id}'>{footer_content}</div>")
  str_glue("<div id='accordion{footer_id}'><div class='card-footer text-muted'>{footer_head}{footer_body}</div></div>")
}


populate_schedule <- function(course = str_remove(params$course, "_.+$")) {
  background <- import_background(course = course)
  agenda     <- import_agenda(course = course)
  cases      <- format_content(import_content("case_convos", course = course))
  schedule   <- import_schedule(course = course) %>%
    format_schedule() %>%
    map_depth(2, \(x) list_assign(
      x,
      agenda     = keep(agenda, \(y) any(names(y) %in% pluck(x, "class_day"))),
      background = keep_at(background, pluck(x, "class_day")),
      cases      = keep(cases, \(y) any(names(y) %in% pluck(x, "class_day")))
    ))  %>%
    map_depth(2, compact) %>%
    map_depth(2, \(x) modify_at(
      x,
      c("agenda", "cases"),
      \(y) list_flatten(
        map(y, \(z) keep_at(z, pluck(x, "class_day"))),
        name_spec = "{outer}"
      )
    )) %>%
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
      \(x) list_assign(
        x,
        slides = str_flatten(
          unlist(
            map(
              pluck(x, "agenda"),
              \(y) pluck(y, "slides")
            )
          ),
          na.rm = TRUE
        ),
        background = flatten_background(x)
      )
    ) %>%
    map_depth(2, blank_rows)
  return(schedule)
}


content_type <- function(x) {
  tags <- pluck(x, "tags")
  type <- if ("podcast" %in% tags) "podcast" else if ("literature" %in% tags) "literature" else if ("lesson" %in% tags) "topics" else if ("case_convo" %in% tags) "case_convo" else "textbook"
}

format_schedule <- function(schedule_init) {
  as.list(paste0("W", 1:16)) %>%
    set_names(map(., \(x) x)) %>%
    map(\(x) keep(
      schedule_init, \(y) any(pluck(y, "week") %in% x)
    )) %>%
    map(\(x) imap(x, \(y, idy) list_assign(
      y,
      day    = str_extract(idy, "D\\d{1,2}$"),
      topics = str_flatten_comma(
        unique(unlist(map(pluck(y, "topics"), \(z) str_remove(pluck(z, "title"), " - .+$")))),
        na.rm = TRUE
      )
    ))) %>%
    map(\(x) set_names(x, map(x, \(y) pluck(y, "wday")))) %>%
    map_depth(2, compact)
}

slides_href <- function(href) {
  if (!str_starts(href, "http")) sprintf("slides/%s/index.html", href) else href
}
pluck_first_day <- function(classes_assigned) {

  dates <- map(classes_assigned, \(x) as.POSIXct(pluck(x, "date"))) %>%
    map(sort)

  class <- keep(classes_assigned, \(x) as.POSIXct(pluck(x, "date")) %in% pluck(dates, 1))

  return(class)
}

import_content <- function(type, course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, sprintf("%s.yaml", type)))) return(NULL)

  init <- yaml::read_yaml(file = here::here(course, sprintf("%s.yaml", type)))

  if (is.null(init) || length(init) < 1) return(NULL)

  if (type %in% c("textbook", "literature", "podcast")) {
    content <- map(init, \(x) modify_at(x, "classes_assigned", \(y) pluck_first_day(y))) %>%
      map(\(x) list_assign(
        x,
        class_days = pluck(x, "classes_assigned", 1, "class_day"),
        dates      = pluck(x, "classes_assigned", 1, "date")
      ))
  } else {
    content <- init
  }

  content %>%
    map(\(x) modify_at(x, "class_days", \(y) str_extract(y, "(?<=_).+$"))) %>%
    map(\(x) list_assign(x, type = content_type(x))) %>%
    map_depth(1, compact) %>%
    map(\(x) modify_at(x, "slides", \(y) content_slides(y)))
}

import_background <- function(course = str_remove(params$course, "_.+$")) {
  types <- list("podcast", "textbook", "literature") %>%
    keep(\(x) file_exists(here::here(course, sprintf("%s.yaml", x))))

  background <- map(types, \(x) format_content(import_content(x, course = course))) %>%
    list_flatten(name_spec = "{inner}")

  return(background)
}

import_agenda <- function(course = str_remove(params$course, "_.+$")) {
  types <- list("topics", "case_convos") %>%
    keep(\(x) file_exists(here::here(course, sprintf("%s.yaml", x))))

  agenda <- map(types, \(x) format_content(import_content(x, course = course))) %>%
    list_flatten(name_spec = "{inner}")

  return(agenda)
}

format_content <- function(content) {
  if (is.null(content) || length(content) < 1) return(NULL)
  type <- pluck(content, 1, "type")
  vars <- unique(unlist(map(content, names)))
  vars_dup <- unlist(discard(vars, \(y) all(y %in% c("class_days", "tags", "dates"))))
  vars_keep <- unlist(discard(vars, \(y) any(y %in% c(
    "section",
    "days_needed",
    "themes",
    "projects",
    "background",
    "area",
    "created",
    "datetimeModified",
    "course_name",
    "course_prefix",
    "course_number",
    "semesters",
    "tags",
    "classes_assigned",
    "zotero_uri",
    "zotero_pdf_path",
    "profile",
    "categories",
    "category",
    "topics",
    "keywords"
  ))))

  revised <- discard(content, \(x) any(
    length(pluck(x, "class_days")) < 1
  ))  %>%
    map(\(x) keep_at(x, vars_keep)) %>%
    map(\(x) map_at(x, vars_dup, \(y) list(y))) %>%
    map(\(x) modify_at(
      x,
      vars_dup,
      \(y) rep(y, length(pluck(x, "class_days")))
    )) %>%
    map(\(x) map_at(x, c("class_days", "dates"), \(y) as.list(y))) %>%
    map_depth(2, compact) %>%
    map_depth(1, compact)  %>%
    map(transpose) %>%
    map(\(x) set_names(x, map(x, \(y) pluck(y, "class_days")))) %>%
    set_names(map(., \(x) pluck(x, 1, "title")))

  if (type %in% c("literature", "textbook", "podcast")) {
    return(list_flatten(revised, name_spec = "{inner}"))
  } else {
    return(revised)
  }
}














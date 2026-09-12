
course_url_prefix <- function(course = str_remove(params$course, "_.+$")) {
  sprintf("https://rich-molecular-health-lab.github.io/courses/%s", course)
}


remove_course_prefix <- function(x) {
  str_remove(x, "\\w{4}\\d{4}$")
}



card_footer_collapse <- function(footer_title, footer_content) {
  footer_head <- str_glue("<small><h6 class='card-header'><a class='btn' data-bs-toggle='collapse' href='#collapseOne'>{footer_title}</a></h6></small>")
  footer_body <- str_glue("<div id='collapseOne' class='collapse hide' data-bs-parent='#accordion'>{footer_content}</div>")
  str_glue("<div id='accordion'><div class='card-footer text-muted'>{footer_head}{footer_body}</div></div>")
}

card_header_row <- function(string, href = NULL, fa_icon = "fa-solid fa-link") {
  if (is.null(href)) {
    str_glue("<h5 class='card-header'><small>{string}</small></h5>")
  } else {
    str_glue("<h5 class='card-header d-flex justify-content-between align-items-center'><small>{string}</small><a href={href} class='card-link'><i class='{fa_icon} fa-lg px-2'></i></a></h5>")
  }
}

card_title_block <- function(title_string, subtitle_string = NULL, title_href = NULL, subtitle_href = NULL, title_icon = "fa-solid fa-link", subtitle_icon = "fa-solid fa-link") {
    title <- if (!is.null(title_href)) sprintf("<h5 class='card-title d-inline-flex'><small>%s</small><a href=%s class='card-link'><i class='%s fa-lg'></i></a></h5>", title_string, title_href, title_icon) else sprintf("<h5 class='card-title'><small>%s</small></h5>", title_string)
    subtitle <- if (is.null(subtitle_string)) "" else if (is.null(subtitle_href)) sprintf("<h6 class='card-subtitle text-muted'><small>%s</small></h6>", subtitle_string) else sprintf("<h6 class='card-subtitle text-muted d-inline-flex'><small>%s</small><a href=%s class='card-link'><i class='%s fa-lg'></i></a></h6>", subtitle_string, subtitle_href, subtitle_icon)

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
  str_glue("<div class='d-flex justify-content-between align-items-center'>{str_flatten(buttons, na.rm = TRUE)}</div>")
}


render_card <- function(header_string, data_strings = list(NULL), data_list = list(NULL), data_list_title = NULL, title_string = NULL, subtitle_string = NULL, footer_title = NULL, footer_content = NULL, header_href = NULL, title_href = NULL, subtitle_href = NULL, header_icon = "fa-solid fa-link", title_icon = "fa-solid fa-link", subtitle_icon = "fa-solid fa-link", card_class = "primary") {
  footer <- if (is.null(footer_content) || is.null(footer_title)) "" else card_footer_collapse(footer_title = footer_title, footer_content = footer_content)
  title  <- if (is.null(title_string)) "" else card_title_block(title_string = title_string, subtitle_string = subtitle_string, title_href = title_href, subtitle_href = subtitle_href, title_icon = title_icon, subtitle_icon = subtitle_icon)
  body_list <- if (length(data_list) < 1) "" else card_data_list(data_list = data_list, list_header = data_list_title)
  body_row  <- if (length(data_strings) < 1) "" else card_data_row(unlist(map(data_strings, card_data_button)))
  card_data <- list(
    class  = card_class,
    header = card_header_row(string = header_string, href = header_href, fa_icon = header_icon),
    title  = title,
    body   = paste0(body_row, body_list),
    footer = footer
  )
  return(as.character(str_glue_data(card_data, "<div class='card border-{class} my-2 w-auto'>{header}<div class='card-body'>{title}{body}{footer}</div></div>")))
}


podcast_card_html <- function(podcast, course_name = str_remove(params$course, "_.+$")) {
  if (length(podcast) < 1 || is.null(podcast)) return(NULL)

  classes <- str_extract(pluck(podcast, "classes_assigned", 1, "class_day"), "(?<=_).+$")
  dates   <- str_extract(pluck(podcast, "classes_assigned", 1, "date"),"(?<=2026-)\\d+-\\d+")
  deadline <- sprintf("%s (%s)", dates, classes)
  transcript_link <- sprintf("<a href='https://rich-molecular-health-lab.github.io/courses/%s/podcast/%s.html' class='card-link'><i class='fa-solid fa-link fa-lg px-2'></i>Transcript</a>", course_name, pluck(podcast, "title"))

  card_out <- render_card(
    header_string = str_remove_all(pluck(podcast, "title"), "_"),
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
  pdf_link <- sprintf("<a class='card-link' href=https://rich-molecular-health-lab.github.io/courses/%s/attachments/%s.pdf>Local PDF <i class='fa-solid fa-link fa-lg px-2'></i></a>", course_name, pluck(literature, "citekey"))

  card_out <- render_card(
    header_string = sprintf("%s et al. (%s)", str_to_title(pluck(literature, "author_first")), pluck(literature, "year")),
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
  pdf_link <- sprintf("<a class='card-link' href=https://rich-molecular-health-lab.github.io/courses/%s/attachments/%s.pdf>Local PDF <i class='fa-solid fa-link fa-lg px-2'></i></a>", course_name, pluck(chapters, "citekey"))

  card_out <- render_card(
    header_string = sprintf("Chapter %.0f", pluck(chapters, "chapter")),
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
    data_list       = pluck(case_convo, "leaders"),
    data_list_title = "Leaders",
    title_string    = str_squish(pluck(case_convo, "topic")),
    subtitle_string = deadline,
    card_class      = "success"
  )

  return(card_out)
}



format_unit <- function(x) {
  unit <- pluck(x, "unit", 1, "title")
   if (str_detect(unit, "(Exam)|(Break)")) {
      return("")
  } else {
    return(remove_course_prefix(unit))
    }
}


icon_string <- function(string = NULL, href = NULL, fa_icon = "fa-solid fa-link") {
  if (is.null(string) & is.null(href)) return("")

  if (!is.null(href) && !is.null(string)) {
    as.character(str_glue("<span><a class='nav-link active' href={href}><i class='{fa_icon}'></i></a> {string}</span>"))
  } else if (is.null(href) && !is.null(string)) {
    as.character(str_glue("<span><i class='{fa_icon}'></i> {string}</span>"))
  } else {
    as.character(str_glue("<a class='nav-link active' href={href}><i class='{fa_icon}'></i></a>"))
  }
}


attachment_url <- function(attachment_file, course = str_remove(params$course, "_.+$")) {
  paste(course_url_prefix(course), "attachments", attachment_file, sep = "/")
}

format_chapters <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "textbook.yaml"))) return(NULL)
  yaml::read_yaml(here::here(course, "textbook.yaml")) %>%
    map(\(x) list_assign(
      x,
      table_string = icon_string(string = pluck(x, "title"), href = attachment_url(pluck(x, "attachments", 1), course), fa_icon = "fa-solid fa-book-open-reader"),
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
      table_string = icon_string(string = sprintf("%s et al. %s", pluck(x, "author_first"), pluck(x, "year")), href = attachment_url(pluck(x, "attachments", 1), course), fa_icon = "fa-solid fa-scroll"),
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
        table_string = icon_string(string = pluck(x, "series"), href = pluck(x, "url_spotify"), fa_icon = "fa-brands fa-spotify"),
        card         = podcast_card_html(x, course_name = course),
        class_days   = pluck(x, "classes_assigned", 1, "class_day")
      )) %>%
      map(\(x) keep_at(x, c("table_string", "card", "class_days")))
  }

  return(background)
}


format_case_convos <- function(course = str_remove(params$course, "_.+$")) {
  if (!file_exists(here::here(course, "case_convos.yaml"))) return(NULL)

    case_convos <- yaml::read_yaml(here::here(course, "case_convos.yaml")) %>%
      map(\(x) list_assign(
        x,
        table_string = icon_string(string = pluck(x, "title"), fa_icon = "fa-brands fa-discourse"),
        card         = case_card_html(x)
      )) %>%
      map(\(x) keep_at(x, c("table_string", "card", "class_days")))

  return(case_convos)
}


slides_href <- function(x, course = str_remove(params$course, "_.+$")) {
  if (is.null(x)) return(NULL) else if (str_starts(x, "http")) return(x) else return(paste(course_url_prefix(course), "slides", x, "index.html", sep = "/"))
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
      slides = str_flatten(map(pluck(x, "slides"), \(y) icon_string(href = y, fa_icon = "fa-brands fa-slideshare")), collapse = " ", na.rm = TRUE)
      )) %>%
    map(\(x) list_assign(
      x,
      table_string = str_glue_data(x, "<span>{remove_course_prefix(title)} {slides}</span>"),
      card         = NULL
      )) %>%
    map(\(x) keep_at(x, c("table_string", "card", "class_days")))

  return(topics)
}

row_vals <- function(x) {
  themes <- pluck(x, "unit") %>%
    list_flatten(name_spec = "{inner}") %>%
    keep_at("title") %>%
    unlist() %>%
    unique() %>%
    str_flatten_comma(na.rm = TRUE)
  topics <- pluck(x, "topics") %>%
    list_flatten(name_spec = "{inner}") %>%
    keep_at("title") %>%
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

import_schedule <- function(course = str_remove(params$course, "_.+$")) {
  yaml::read_yaml(here::here(course, "schedule.yaml")) %>%
    set_names(map(., \(x) str_extract(pluck(x, "class_day"), "(?<=_).+$"))) %>%
    map(\(x) list_assign(
      x,
      row  = row_vals(x),
      unit = format_unit(x)
      ))
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
    cards_html <- if (length(cards) < 1) NA_character_ else if (length(cards) > 1) str_glue("<div class='d-flex justify-content-around align-items-start w-auto my-1 mx-0 p-0'>{cards}</div>") else cards
    return(list(
      table_string = str_flatten_comma(unique(unlist(strings)), na.rm = TRUE),
      card         = cards_html
    ))
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
      topics     = str_flatten_comma(pluck(x, "topics", "table_string"), pluck(x, "case_convos", "table_string"), na.rm = TRUE),
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

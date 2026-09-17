
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

import_schedule <- function(course = str_remove(params$course, "_.+$")) {
  yaml::read_yaml(here::here(course, "schedule.yaml")) %>%
    set_names(map(., \(x) str_extract(pluck(x, "class_day"), "(?<=_).+$"))) %>%
    map(\(x) list_assign(
      x,
      row  = row_vals(x),
      unit = format_unit(x)
    )) %>%
    map(\(x) list_assign(
      x,
      class     = row_class(x),
      class_day = str_extract(pluck(x, "class_day"), "(?<=_).+$"),
      week  = paste0("W", pluck(x, "week")),
      wday  = format(as.POSIXct(pluck(x, "date")), format = "%a"),
      date  = format(as.POSIXct(pluck(x, "date")), format = "%A, %B %e")
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
    "pdf"        = "fa-solid fa-file-pdf"
  )
  icon <- if (is.null(size)) name else paste(name, size)
  return(str_glue("<i class='{icon}'></i>"))
}


icon_link <- function(href, icon = NULL, class = "nav-item px-1 text-primary") {
  icon <- if (is.null(icon)) get_icon("link") else icon
  str_glue("<a href={href} class='{class}'>{icon}</a>")
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

content_cases <- function(x) {
  if (is.null(x) || length(x) < 1) return(NA_character_)
  number <- pluck(x, "number")
  topic  <- pluck(x, "topic")
  leaders <- pluck(x, "leaders")
  leaders_flat <- if (is.null(leaders) || length(leaders) < 1) "" else str_flatten_comma(leaders, na.rm = TRUE)
  return(str_glue("<li class='list-group-item list-group-item-success d-flex justify-content-between align-items-center'><span class='badge bg-success me-1 float-start'>Case</span><div class='d-flex flex-column justify-content-around'><div class='text-success-emphasis'>Conversation {number}</div><em class='text-muted'>{topic}</em></div><small class='text-muted'>{leaders_flat}</small><span class='float-end'><i class='fa-solid fa-comments'></i></span></li>"))
}

content_slides <- function(slides) {
  if (is.null(slides) || length(slides) < 1) return(NA_character_)
  links <- map(slides, slides_href) %>%
    map(\(x) icon_link(x, icon = get_icon("slides", size = "fa-lg"))) %>%
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
    action <- "Podcast"
    url_direct <- pluck(x, "url_spotify")
    url_second <- sprintf("'podcast/%s.qmd'", pluck(x, "title"))
    my <- format(as.POSIXct(pluck(x, "released")), format = "%b %Y")
    title_main <- pluck(x, "series")
    title_second <- sprintf("%s (%s, released %s)", pluck(x, "title"), pluck(x, "duration_string"), my)
    link_direct <- icon_link(url_direct, icon = icon)
    link_second <- icon_link(url_second, icon = get_icon("transcript"))
  } else {
    action <- "Reading"
    if (type == "literature") {
      url_direct <-  pluck(x, "url")
      url_second <-  sprintf("attachments/%s.pdf", pluck(x, "citekey"))
      link_second <- icon_link(url_second, icon = get_icon("pdf", size = "fa-xl"))
      title_main <- sprintf("%s et al. %s", pluck(x, "author_first"), pluck(x, "year"))
      title_second <- sprintf("%s (DOI: %s)", pluck(x, "title"), pluck(x, "doi"))
      link_direct <- icon_link(url_direct, icon = icon)
    } else {
      url_direct <-  sprintf("attachments/%s.pdf", pluck(x, "citekey"))
      url_second <-  pluck(x, "url")
      link_second <- icon_link(url_second, icon = icon)
      link_direct <- icon_link(url_direct, icon = get_icon("pdf", size = "fa-xl"))
      title_main   <- sprintf("Ch %.0f", pluck(x, "chapter"))
      sections <- pluck(x, "sections_assigned")
      assigned <- if (is.null(sections) || length(sections) < 1) "" else sprintf(" (Sections %s)", str_flatten_comma(sections, na.rm = TRUE))
      title_second <- paste0(pluck(x, "title"), assigned)
    }
  }
  badge <- str_glue("<span class='badge bg-primary me-1 float-start'>{action}</span>")
  content <- str_glue("<li class='list-group-item list-group-item-primary d-flex justify-content-between align-items-center'>{badge}<div class='d-flex flex-column justify-content-around'><div class='text-primary-emphasis'>{title_main}</div><small class='text-muted'>{title_second}</small></div>{link_direct}{link_second}</li>")
  return(content)
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
  str_remove(x, "\\w{4}\\d{4}$")
}

generate_id <- function(title) {
  str_to_lower(str_sub(str_remove_all(title, "[^\\w\\d]"), 1L, 6L))
}


format_unit <- function(x) {
  unit <- pluck(x, "unit", 1, "title")
  if (str_detect(unit, "(Exam)|(Break)")) {
    return("")
  } else {
    return(remove_course_prefix(unit))
  }
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

slides_button <- function(slides) {
  if (is.null(slides) || length(slides) < 1) return("")
  url <- if (str_starts(slides, "http")) slides else sprintf("attachments/%s/index.html", slides)
  str_glue("<a class='btn btn-outline-primary p-3 m-2' href={url}>Slides <i class='fa-solid fa-link'></i></a>")
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

card_footer_collapse <- function(footer_title, footer_content) {
  footer_id <- generate_id(footer_title)
  footer_head <- str_glue("<small><div class='card-header'><a class='collapsed btn' data-bs-toggle='collapse' href='#{footer_id}'>{footer_title}</a></div></small>")
  footer_body <- str_glue("<div id='{footer_id}' class='collapse hide' data-bs-parent='#accordion{footer_id}'>{footer_content}</div>")
  str_glue("<div id='accordion{footer_id}'><div class='card-footer text-muted'>{footer_head}{footer_body}</div></div>")
}




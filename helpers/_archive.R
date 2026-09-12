
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

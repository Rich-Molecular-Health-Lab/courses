
embed_resource <- function(course) {
  if (str_detect(course, "hhe")) {
    withTags(
      iframe(
        `data-testid`     = "embed-iframe",
        style           = "border-radius:12px",
        src             = "https://open.spotify.com/embed/playlist/4NTGkvvAL2OxXaDIzV3BS0?utm_source=generator",
        width           = "100%",
        height          = "352",
        frameBorder     = "0",
        allowfullscreen = "",
        allow           = "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
        loading         = "lazy"
      )
    )
  } else { span("") }
}

resource_card <- function(course) {

  course_short <- str_extract(course, "\\w+(?=_)")
  if (course_short == "hhe") {

    resources <- content_card(
      title_text    = textbooks[[course_short]][["title"]],
      subtitle_text = tags$a(href = paste0("schedule_", course_short, ".qmd"), "See course schedule for deadlines."),
      body          = accordion(
        accordion_panel(
          "Access Spotify Playlist",
          embed_resource(course)
        ),
        open = FALSE
      ),
      footer_text   = "Assigned listening may be subject to change at least 2 weeks ahead of the deadline.",
      card_class    = "primary",
      heading       = "Required Media",
      icon_name     = "podcast"
    )

  } else if (course_short == "conbio") {

   resources <- content_card(
      title_text    = textbooks[[course_short]][["title"]],
      subtitle_text = "eText Available via Canvas IA-Bookshelf",
      body          = ul_group(
        layout    = "grid",
        item_list = list(
          "Authors:" = textbooks[[course_short]][["authors"]],
          "Edition:" = span(textbooks[[course_short]][["edition"]], paste0(" (", textbooks[[course_short]][["date"]], ")")),
          "ISBN:"    = textbooks[[course_short]][["isbn"]]
        )
      ),
      footer_text   = "If you prefer a hardcopy version then you must opt out of automatic purchase of the eBook through Canvas by the end of Week 2.",
      card_class    = "primary",
      heading       = "Required Text",
      icon_name     = "book",
      image         = card_image(textbooks[[course_short]][["image"]], width = "20%")
    )

  } else if (course_short == "zoobio") {
    resources <-    withTags(
        card(
          class = "card text-white bg-primary mb-3",
          card_header("Required Readings"),
          card_title(
            h3(
              "Published Articles ", span(class = "text-muted", "from journals and other online materials"),
              p(class = "card-subtitle text-muted",  "See course schedule for list and deadlines.")
            )
          ),
          card_title(
            layout_columns(
              col_widths = c(5, 7),
              card_image(textbooks[[course_short]][["image"]], width = "15%"),
              h3(
                textbooks[[course_short]][["title"]],
                p(class = "card-subtitle text-muted",  "See course schedule for deadlines.")
              )
            )
          ),
          card_body(
            p("We will use our class time to work together on some of the most important or challenging concepts, but this will not be a substitute for reading and studying the material on your own."),
            p(strong("You should be prepared to answer quiz questions on anything from the assigned readings starting on the deadline listed in the schedule."))
          )
        )
      )
  }

  return(resources)

}


logistics_card <- function(course) {
    withTags(
      card(
        class = "card text-white bg-primary mb-3",
        card_header(
          span("Course Logistics"),
          span(course_info[[course]][["course_no"]])
        ),
        card_image(file = paste0("graphics/", course_info[[course]][["file_prefix"]], "_header.png"), width = "100%"),
        card_title(paste0("This course meets: ", course_info[[course]][["day_time"]], " in ", course_info[[course]][["location"]])),
        accordion(
          accordion_panel(
            "Description",
            course_info[[course]][["course_description"]]
          ),
          open = FALSE
        ),
        card_footer(
          span(if (!is.na(course_info[[course]][["prereqs"]])) paste0("Prerequisites: ", course_info[[course]][["prereqs"]]) else ""),
          span(layout_columns(p(course_info[[course]][["semester"]]), p(paste(course_info[[course]][["credits"]], "Credits"))))
        )
      )
    )
}

total_possible <- function(x, drop = FALSE) {
  if (isTRUE(drop)) {
    as.integer(reduce(discard_at(x, 1:ceiling(length(x)*0.15)), `+`))
  } else if (isFALSE(drop)) {
    as.integer(reduce(x, `+`))
  }
}

assessment_summarize <- function(x, idx) {
  if (idx %in% c("quizzes", "inclass")) {
    list(
      points_each = as.integer(reduce(x, `+`))/length(x),
      count       = length(x),
      n_dropped   = ceiling(length(x)*0.15),
      total       = total_possible(x, drop = TRUE)
    )
  } else {
    list(
      points_each = as.integer(reduce(x, `+`))/length(x),
      count       = length(x),
      n_dropped   = 0,
      total       =  total_possible(x)
    )
  }
}

assessment_course <- function(course) {
  imap(pluck(assessment, course), \(x, idx) assessment_summarize(x, idx)) %>%
    enframe(name = "Format") %>%
    unnest_wider(value)
}

project_details <- function(course) {
  format       <- pluck(course_info, course, "project")
  course_short <- str_extract(course, "\\w+(?=_)")
  totals       <- assessment_course(course)
  deadline     <- tags$a(href = paste0("schedule_", course_short, ".qmd"), "See course schedule")
  if (format == "Grant Proposal") {
    points   <- assessment[[course]]$assignments$grant_proposal
    subtitle <- "Essay Assignment"
    detail   <- span("Modified version of the ", a(href = "https://www.aza.org/cgf-tips-for-success", "AZA Conservation Grants Fund Proposal"))

  } else if (str_detect(format, "Portfolio")) {
    points <- assessment[[course]]$assignments$portfolio
    subtitle <- "Group Assignment"
    detail   <- "Online portfolio summarizing a fictional zoo designed over the semester's lab exercises"

  } else if (str_detect(format, "Poster")) {
    points <- assessment[[course]]$assignments$poster
    subtitle <- "Independent Project"
    detail   <- "Poster communicating a leading issue introduced this semester and your proposed approach to mitigation"
  }

  percent <- round((points/sum(totals$total))*100, 0)

  display <- content_card(
    title_text    = format,
    subtitle_text = subtitle,
    body          = ul_group(
      layout    = "grid",
      item_list = list(
        "Deadline:" = deadline,
        "Points:"   = p(points, span(class = "text-body-secondary", tags$i(paste0("(", percent, "%)")))),
        "Format:"   = detail
      )
    ),
    footer_text   = "These details (especially points and deadlines) may be subject to change no less than 2 weeks before the final deadline.",
    card_class    = "info",
    heading       = "Graded Project",
    icon_name     = "pen-to-square"
  )

   return(display)
}


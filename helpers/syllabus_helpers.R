
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
   resources <-   withTags(
        card(
          class = "card text-white bg-primary mb-3",
          card_header("Required Media"),
          card_title(
            h3(
              textbooks[[course_short]][["title"]],
              p(class = "card-subtitle text-muted",  "See course schedule for deadlines.")
            )
          ),
          card_body(embed_resource(course)),
          card_footer(
            class = "text-secondary",
            "Assigned listening may be subject to change at least 2 weeks ahead of the deadline."
          )
        )
      )
  } else if (course_short == "conbio") {
    resources <-    withTags(
        card(
          class = "card text-white bg-primary mb-3",
          card_header(h3("Required Text")),
          card_body(
            bscols(
              widths = c(4, 8),
              card_image(textbooks[[course_short]][["image"]]),
              card_body(
                h3(textbooks[[course_short]][["title"]]),
                ul(
                  class = "list-group",
                  li(
                    class="list-group-item list-group-item-primary d-flex justify-content-between align-items-start",
                    span(h6(em("eText Available via Canvas IA-Bookshelf"))
                  ),
                  li(
                    class="list-group-item list-group-item-primary d-flex justify-content-between align-items-start",
                    span(h6(class="text-body-secondary", "Authors: ")),
                    textbooks[[course_short]][["authors"]]
                  ),
                  li(
                    class="list-group-item list-group-item-primary d-flex justify-content-between align-items-start",
                    span(h6(class="text-body-secondary", "Edition: ")),
                    span(
                      textbooks[[course_short]][["edition"]],
                      span(class = "text-body-tertiary", paste0(" (", textbooks[[course_short]][["date"]], ")"))
                    )
                  ),
                  li(
                    class="list-group-item list-group-item-primary d-flex justify-content-between align-items-start",
                    span(h6(class="text-body-secondary", "ISBN: ")),
                    textbooks[[course_short]][["isbn"]]
                  )
                )
              )
            )
          )
          ),
          card_footer(
            class = "card-footer text-light",
            em("See course schedule for assigned chapters."),
            em("If you prefer a hardcopy version then you must opt out of automatic purchase of the eBook through Canvas by the end of Week 2.")
          )
        )
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
        course_info[[course]][["course_description"]],
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
  if (format == "Grant Proposal") {
    points   <- assessment[[course]]$assignments$grant_proposal
    subtitle <- "Essay Assignment"
    detail   <- span("Modified version of the ", a(href = "https://www.aza.org/cgf-tips-for-success", "AZA Conservation Grants Fund Proposal"))
    deadline <- "Deadline placeholder"

  } else if (str_detect(format, "Portfolio")) {
    points <- assessment[[course]]$assignments$portfolio
    subtitle <- "Group Assignment"
    detail   <- "Online portfolio summarizing a fictional zoo designed over the semester's lab exercises"
    deadline <- "Deadline placeholder"

  } else if (str_detect(format, "Poster")) {
    points <- assessment[[course]]$assignments$poster
    subtitle <- "Independent Project"
    detail   <- "Poster communicating a leading issue introduced this semester and your proposed approach to mitigation"
    deadline <- "Deadline placeholder"

  }

  percent <- round((points/sum(totals$total))*100, 0)

  li_class   <- "list-group-item"
  col_widths <- c(4, 8)

  display <- withTags(
    card(
      class = "card border-info mb-3",
      card_header(
        class = "card-header text-white bg-info",
        span(icon("circle-info", class = "fa-solid")),
        span(strong("Format Details"))
      ),
      card_title(
        class = "card-title mb-1",
        h2(format),
        h3(class = "card-subtitle text-muted", subtitle)
        ),
      card_body(
        ul(
          class = "list-group",
          li(
            class = li_class,
            bscols(
              widths = col_widths,
              h6(class="text-body-secondary", "Deadline: "),
              deadline
            )
          ),
          li(
            class = li_class,
            bscols(
              widths = col_widths,
              h6(class="text-body-secondary", "Points: "),
              p(points, span(class = "text-body-secondary", i(paste0("(", percent, "%)"))))
            )
          ),
          li(
            class = li_class,
            bscols(
              widths = col_widths,
              h6(class="text-body-secondary", "Format: "),
              detail
              )
            )
          )
        ),
      card_footer(
        class = "text-body-tertiary",
        p("You will submit your final assignment via canvas."),
        em("Note: These details (especially points and deadlines) may be subject to change no less than 2 weeks before the final deadline.")
        )
      )
    )
  return(display)
}


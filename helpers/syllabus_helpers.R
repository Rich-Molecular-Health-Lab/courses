
render_course_graphics <- function(course = NULL) {
  fs::dir_create(here("syllabi", "graphics"))
  if (!is.null(course)) {
    course_short <- shorten_course(course)
    fs::dir_create(here("syllabi", "graphics", course_short))
    logistics_card(course)    %>% save_html(file = here("syllabi", "graphics", course_short, "logistics.html"))
    resource_card(course)     %>% save_html(file = here("syllabi", "graphics", course_short, "resources.html"))
    project_details(course)   %>% save_html(file = here("syllabi", "graphics", course_short, "project_details.html"))
    missed_exams(course)      %>% save_html(file = here("syllabi", "graphics", course_short, "missed_exams.html"))
    exam_format(course)       %>% save_html(file = here("syllabi", "graphics", course_short, "exam_format.html"))
    learning_outcomes(course) %>% gtsave(here("syllabi", "graphics", course_short, "learning_outcomes.png"))
    grade_breakdown(course)   %>% gtsave(here("syllabi", "graphics", course_short, "grade_breakdown.png"))
    resources(course)         %>% gtsave(here("syllabi", "graphics", course_short, "resources.png"))
  } else {
    NULL
  }

}

render_syllabus_defaults <- function() {
  fs::dir_create(here("syllabi", "graphics"))
  prep_quizzes()                   %>% save_html(file = here("syllabi", "graphics", "prep_quizzes.html"))
  instructor_card()                %>% save_html(file = here("syllabi", "graphics", "instructor.html"))
  activities()                     %>% save_html(file = here("syllabi", "graphics", "activities.html"))
  class_culture()                  %>% save_html(file = here("syllabi", "graphics", "class_culture.html"))
  engagement()                     %>% save_html(file = here("syllabi", "graphics", "engagement.html"))
  late_work()                      %>% save_html(file = here("syllabi", "graphics", "late_work.html"))
  accommodations()                 %>% save_html(file = here("syllabi", "graphics", "accommodations.html"))
  plagiarism()                     %>% save_html(file = here("syllabi", "graphics", "plagiarism.html"))
  titleix()                        %>% save_html(file = here("syllabi", "graphics", "titleix.html"))
  inclusion_office()               %>% save_html(file = here("syllabi", "graphics", "inclusion_office.html"))
  other_notices()                  %>% save_html(file = here("syllabi", "graphics", "other_notices.html"))
}


embed_resource <- function(course) {
  if (str_detect(course, "hhe")) {
    withTags(
      iframe(
        `data-testid`     = "embed-iframe",
        style           = "border-radius:12px",
        src             = "https://open.spotify.com/playlist/0zc41BWYHIMa8yE42iu41u?si=54d60d7eb9dc473f",
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
  textbooks <- get_textbooks(course)

  if (course_short == "hhe") {

    resources <- content_card(
      title_text    = textbooks[["title"]],
      subtitle_text = tags$a(href = schedule_link(course), "See course schedule for deadlines."),
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
      title_text    = textbooks[["title"]],
      subtitle_text = "eText Available via Canvas IA-Bookshelf",
      body          = ul_group(
        layout    = "grid",
        item_list = list(
          "Authors:" = textbooks[["authors"]],
          "Edition:" = span(textbooks[["edition"]], paste0(" (", textbooks[["date"]], ")")),
          "ISBN:"    = textbooks[["isbn"]]
        )
      ),
      footer_text   = "If you prefer a hardcopy version then you must opt out of automatic purchase of the eBook through Canvas by the end of Week 2.",
      card_class    = "primary",
      heading       = "Required Text",
      icon_name     = "book",
      image         = card_image(here(textbooks[["image"]]), width = "20%")
    )

  } else if (course_short == "zoobio") {
    resources <-    tagList(
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
              card_image(textbooks[["image"]], width = "15%"),
              h3(
                textbooks[["title"]],
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
  course_short <- shorten_course(course)
  course_info <- get_course_info(course)
    withTags(
      card(
        class = "card text-white bg-primary mb-3",
        card_header(
          span("Course Logistics"),
          span(course_info[["course_no"]])
        ),
        card_image(file = here("graphics", "headers", paste0(course_short, ".png")), width = "100%"),
        card_title(paste0("This course meets: ", course_info[["day_time"]], " in ", course_info[["location"]])),
        accordion(
          accordion_panel(
            "Description",
            course_info[["course_description"]]
          ),
          open = FALSE
        ),
        card_footer(
          span(if (!is.na(course_info[["prereqs"]])) paste0("Prerequisites: ", course_info[["prereqs"]]) else ""),
          span(layout_columns(p(course_info[["semester"]]), p(paste(course_info[["credits"]], "Credits"))))
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
  imap(assessment(course), \(x, idx) assessment_summarize(x, idx)) %>%
    enframe(name = "Format") %>%
    unnest_wider(value)
}

project_details <- function(course) {
  course_info <- get_course_info(course)
  format       <- pluck(course_info, "project")
  course_short <- str_extract(course, "\\w+(?=_)")
  assessment <- assessment(course)
  totals       <- assessment_course(course)
  deadline     <- tags$a(href = schedule_link(course), "See course schedule")
  if (format == "Grant Proposal") {
    points   <- assessment$assignments$grant_proposal
    subtitle <- "Essay Assignment"
    detail   <- span("Modified version of the ", a(href = "https://www.aza.org/cgf-tips-for-success", "AZA Conservation Grants Fund Proposal"))

  } else if (str_detect(format, "Guided Case Conversations")) {
    points <- sum(assessment$assignments$case_convo1, assessment$assignments$case_convo2)
    subtitle <- "Group Assignment"
    detail   <- "Online portfolio summarizing a fictional zoo designed over the semester's lab exercises"

  } else if (str_detect(format, "Portfolio")) {
    points <- assessment$assignments$portfolio
    subtitle <- "Group Assignment"
    detail   <- "Online portfolio summarizing a fictional zoo designed over the semester's lab exercises"

  } else if (str_detect(format, "Poster")) {
    points <- assessment$assignments$poster
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

project_card <- function(course) {
 course_info <- get_course_info(course)
 course_short <- shorten_course(course)
 format       <- pluck(course_info, "project")
 totals       <- assessment_course(params$course)
 deadline     <- tags$a(href = schedule_link(course), "See course schedule")
 assessment <- assessment(course)

 if (str_detect(format, "Case Conversations")) {
   points   <- sum(as.numeric(assessment$assignments$case_convo1), as.numeric(assessment$assignments$case_convo2))
   subtitle <- "Group Project"
   detail   <- "Each student will lead a class discussion two times during the semester."

 } else if (str_detect(format, "Portfolio")) {
   points <- assessment$assignments$portfolio
   subtitle <- "Group Assignment"
   detail   <- "Online portfolio summarizing a fictional zoo designed over the semester's lab exercises"

 } else if (str_detect(format, "Poster")) {
   points <- assessment$assignments$poster
   subtitle <- "Independent Project"
   detail   <- "Poster communicating a leading issue introduced this semester and your proposed approach to mitigation"
 } else {
   points   <- 0
   subtitle <- "No project"
   detail   <- ""
 }

 percent <- round((points/sum(totals$total))*100, 0)
  result <- content_card(
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
  return(result)
}


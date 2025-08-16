list_education <- function(phd       = NULL,
                           masters   = NULL,
                           undergrad = NULL) {
  if (is.null(phd) & is.null(masters)) {
    education <- tags$dl(
      tags$dd(undergrad)
    )
  } else if (is.null(phd) & !is.null(masters)) {
    education <- tags$dl(
      tags$dd(masters),
      tags$dd(undergrad)
    )
  } else if (!is.null(phd) & !is.null(masters)) {
    education <- tags$dl(
      tags$dd(phd),
      tags$dd(masters),
      tags$dd(undergrad)
    )
  }
  return(education)
}

card_template <- function(name_role,
                          photo,
                          education,
                          pronouns,
                          callme,
                          research,
                          projects,
                          personal) {
  projects_list <- map(projects, \(x) withTags(li(x)))
  card_out <- card(
    class = "card mb-3",
    card_header(tags$h3(name_role)),
    card_body(
      tags$div(layout_columns(col_widths = c(2, 10),
                              card_image(file = paste0("images/", photo), border_radius = "all", width = "100%"),
                              tags$div(education,
                                       tags$div(tags$span(
                                         tags$small(tags$b("Pronouns: "), tags$i(pronouns), tags$br(),
                                                    tags$b("Please Call Me: "), tags$i(callme))))))),
      tags$hr(),
      tags$dl(tags$dt("Research Interests"), tags$dd(research),
              tags$dt("Current Project(s)"), tags$dd(tags$ul(projects)))
    ),
    card_footer(tags$dl(tags$dt("Personal Interests"), tags$dd(personal)))
  )
  return(card_out)
}

course_card <- function(course_info, file_prefix) {
  semester_short <- str_extract(file_prefix, "(?<=_)\\d{2}\\w")
  name_short     <- str_extract(file_prefix, "\\w+(?=_)")
  tagList(
    withTags(
      card(
        class = "card mb-3",
        card_header(
          layout_columns(
            widths = c(4, 4, 4),
            course_info[["course_no"]],
            tags$a(
              href = paste0("https://rich-molecular-health-lab.github.io/courses/docs/", semester_short, "/", name_short, "/syllabus_", file_prefix, ".html"),
              paste0("Syllabus (", course_info[["semester"]], ")")
            ),
            tags$a(
              href = paste0("https://rich-molecular-health-lab.github.io/courses/docs/", semester_short, "/", name_short, "/schedule_", file_prefix, ".html"),
              paste0("Schedule (", course_info[["semester"]], ")")
            )
          )
        ),
        card_image(file = paste0("graphics/", file_prefix, "_header.png"), width = "100%"),
        course_info[["course_description"]],
        card_footer(layout_columns(widths = c(4, 4, 4), course_info[["day_time"]], paste(course_info[["semesters"]], "Semesters"), paste(course_info[["credits"]], "credits")))
      )
    )
    )
}

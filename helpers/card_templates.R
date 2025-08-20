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
                              card_image(file = here::here(paste0("images/", photo)), border_radius = "all", width = "100%"),
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

li_cols <- function(
    item_content,
    lead_text,
    layout,
    col_widths = c(4, 8),
    justify    = "between",
    align      = "center") {
  if (layout == "flex") {
    withTags(
      li(
        class = paste0("list-group-item d-flex justify-content-", justify, " align-items-", align),
        span(h6(class="text-body-secondary", lead_text)),
        item_content
      )
    )
  } else if (layout == "bscols") {
    withTags(
      li(
        class = "list-group-item",
        bscols(
          widths = col_widths,
          h6(class="text-body-secondary", lead_text),
          item_content
        )
      )
    )
  }
}

ul_group <- function(list_lead = NULL, layout, item_list, col_widths = c(4, 8), justify = "between", align = "center") {
  if (!is.null(list_lead)) {
    withTags(
      ul(
        class = "list-group",
        li(
          class = "list-group-item d-flex",
          h6(list_lead)
        ),
        imap(item_list, \(x, idx) li_cols(x, idx, layout, col_widths, justify, align))
      )
    )
  } else {
    withTags(
      ul(
        class = "list-group",
        imap(item_list, \(x, idx) li_cols(x, idx, layout, col_widths, justify, align))
      )
    )
  }

}

content_card <- function(title_text, subtitle_text, body, footer_text, card_class, heading, icon_name) {
    card(
      class = paste0("card border-", card_class, " mb-3"),
      card_header(
        class = paste0("card-header text-white bg-", card_class),
        span(shiny::icon(name = icon_name, class = "fa-solid")),
        span(strong(heading))
      ),
      card_title(
        class = "card-title mb-1",
        h4(title_text),
        h6(
          class = "card-subtitle text-muted",
          subtitle_text
        )
      ),
      card_body(body),
      card_footer(
        class = "text-warning-emphasis",
        p(footer_text)
      )
    )

}

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
              href = paste0("syllabus_", file_prefix, ".qmd"),
              paste0("Syllabus (", course_info[["semester"]], ")")
            ),
            tags$a(
              href = paste0("schedule_", file_prefix, ".qmd"),
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
        class = "list-group-item",
        style = paste0("display:flex; flex-flow:row nowrap; justify-content:space-", justify, "; align-items:", align),
        h6(class = "text-body-secondary", lead_text),
        div(item_content)
      )
    )
  } else if (layout == "bscols") {
    withTags(
      li(
        class = "list-group-item",
        bscols(
          widths = col_widths,
          div(h6(class="text-body-secondary", lead_text)),
          div(item_content)
        )
      )
    )
  } else if (layout == "wrap") {
    div(
      layout_column_wrap(
        div(h6(class="text-body-secondary", lead_text)),
        div(item_content),
        gap = "2px"
      )
    )
  } else if (layout == "grid") {
    div(
      style = "display:grid; grid-template: auto / 25% 75%",
        div(h6(class="text-body-secondary", lead_text)),
        div(item_content)
      )
  }
}

ul_group <- function(list_lead = NULL, layout, item_list, col_widths = c(4, 8), justify = "between", align = "center") {
  if (!is.null(list_lead)) {
    withTags(
      ul(
        class = "list-group",
        style = "width:90%",
        li(
          class = "list-group-item",
          h6(list_lead)
        ),
        imap(item_list, \(x, idx) li_cols(x, idx, layout, col_widths, justify, align))
      )
    )
  } else {
    withTags(
      ul(
        class = "list-group",
        style = "width:90%",
        imap(item_list, \(x, idx) li_cols(x, idx, layout, col_widths, justify, align))
      )
    )
  }

}

content_card <- function(title_text, subtitle_text, body, footer_text, card_class, heading, icon_name, image = NULL) {

  class_card <- paste0("card border-", card_class, " mb-3")

  title <- div(
    style = "display:flex; flex-flow:column nowrap; justify-content:flex-start; align-items:flex-start; padding-bottom:10px; margin-top:0px",
    h4(title_text),
    div(
      class = "card-subtitle text-muted",
      subtitle_text
    )
  )

  card_heading <- card_header(
    class = paste0("card-header text-white bg-", card_class),
    style = "margin-bottom:0px",
    span(shiny::icon(name = icon_name, class = "fa-solid")),
    tags$strong(heading)
  )

  footer <- card_footer(
      class = "text-warning-emphasis",
      tags$small(footer_text)
    )

  if (is.null(image)) {
    card(
      class = class_card,
      card_heading,
      title,
      card_body(body),
      footer
    )
  } else {
    card(
      class = class_card,
      style = "margin-top:20px; margin-bottom:70px",
      card_heading,
      title,
      div(
        style = "display:flex; flex-flow:row nowrap; justify-content:space-between; align-items:flex-start; padding-top:0px; margin-top:0px",
        image,
        card_body(body)
      ),
      footer
    )
  }

}

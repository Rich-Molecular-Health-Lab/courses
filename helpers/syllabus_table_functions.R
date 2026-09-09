grade_breakdown <- function(course) {

  assessment_tbl <- assessment_course(course)
  course_info <- get_course_info(course)

  if (str_ends(course, "26f")) {
    possible <- as.integer(sum(assessment_tbl$total))
    assessment_tbl  %>%
      mutate(
        Percent_total = total/possible,
        Format        = if_else(Format == "inclass", "Activity", str_to_title(Format))
      ) %>%
      select(
        Format,
        Points_each  = points_each,
        Count        = count,
        N_dropped    = n_dropped,
        Points_total = total,
        Percent_total
      ) %>%
      gt(rowname_col = "Format") %>%
      tab_stubhead(label = "Format") %>%
      cols_label(Points_each   ~ "Points",
                 Count         ~ "Count",
                 N_dropped     ~ "Drops",
                 Points_total  ~ "Total",
                 Percent_total ~ "% Total") %>%
      fmt_percent(columns  = Percent_total,
                  decimals = 0) %>%
      opt_table_lines(extent = "none") %>%
      tab_style(style = list(cell_borders(color   = "#43475BFF",
                                          sides   = c("top", "bottom"),
                                          style   = "double"),
                             cell_text(stretch = "ultra-condensed",
                                       size    = "small",
                                       weight  = "bold",
                                       style   = "oblique",
                                       align   = "right")),
                locations = list(cells_stubhead(), cells_column_labels())) %>%
      tab_style(style = list(cell_fill(color   = "#E8EADFFF"),
                             cell_borders(sides = "bottom",
                                          color = "#43475BFF",
                                          style = "double"),
                             cell_text(align   = "right",
                                       weight  = "bold",
                                       size    = "large")),
                locations = list(cells_stub(), cells_body(columns = Percent_total))) %>%
      tab_style(style = list(cell_fill(color   = "#E8EADFFF"),
                             cell_borders(sides = "bottom",
                                          color = "#43475BFF",
                                          style = "double"),
                             cell_text(align   = "right",
                                       size    = "large")),
                locations = cells_body(columns = !Percent_total)) %>%
      tab_footnote(
        paste0(
          "Assignment credit mainly comes from a ",
          pluck(course_info, "project"),
          " that you will submit toward the end of the semester."
        ),
        locations = cells_stub(rows = Format == "Assignments")) %>%
      tab_footnote(
        md("These exercises will involve in-class, group activities. You will submit your work at the end of class for completion credit. I may or may not give advance notice for graded in-class activities. **You will not have any makeup opportunities for missed work**, but I will drop at least one of your lowest scores."),
        locations = cells_stub(rows = Format == "Activity")) %>%
      tab_footnote(
        md("These are short quizzes given at the start of class to assess your preparation for the week's topic. **You will not have any makeup opportunities for missed quizzes**, but I will drop at least one of your lowest scores."),
        locations = cells_stub(rows = Format == "Quizzes")) %>%
      tab_footnote(
        "Number (if any) of lowest scores that will automatically be dropped from your final grade. I will drop at least 1 and up to 15% of the total number of quizzes and activities at the end of the semester.",
        locations = cells_column_labels(columns = N_dropped)) %>%
      tab_footnote(
        "You may use hard copy notes/materials for all exams, but not electronic devices/resources.",
        locations = cells_stub(rows = Format == "Exams")) %>%
      tab_style(style     = cell_text(size = "small"),
                locations = cells_footnotes()) %>%
      tab_source_note("These values are predicted estimates, which will likely change as we progress and adjust pace. You should maintain your own assessment records and cross-reference with canvas for the most up-to-date estimates.") %>%
      cols_width(stub()        ~ px(120),
                 Points_each   ~ px(85),
                 Count         ~ px(85),
                 N_dropped     ~ px(85),
                 Points_total  ~ px(85),
                 Percent_total ~ px(100))

  } else {
    assessment_tbl_2 <- assessment_tbl %>%
      mutate(Points_total  = (Count - N_dropped) * Points_each)
    possible <- as.integer(sum(assessment_tbl_2$Points_total))
    assessment_tbl_2 %>%
      mutate(Percent_total = Points_total/possible) %>%
      arrange(Percent_total) %>%
      select(Format, Points_each, Count, N_dropped, Points_total, Percent_total) %>%
      gt(rowname_col = "Format") %>%
      tab_stubhead(label = "Format") %>%
      cols_label(Points_each   ~ "Points",
                 Count         ~ "Count",
                 N_dropped     ~ "Drops",
                 Points_total  ~ "Total",
                 Percent_total ~ "% Total") %>%
      fmt_percent(columns  = Percent_total,
                  decimals = 0) %>%
      opt_table_lines(extent = "none") %>%
      tab_style(style = list(cell_borders(color   = "#43475BFF",
                                          sides   = c("top", "bottom"),
                                          style   = "double"),
                             cell_text(stretch = "ultra-condensed",
                                       size    = "small",
                                       weight  = "bold",
                                       style   = "oblique",
                                       align   = "right")),
                locations = list(cells_stubhead(), cells_column_labels())) %>%
      tab_style(style = list(cell_fill(color   = "#E8EADFFF"),
                             cell_borders(sides = "bottom",
                                          color = "#43475BFF",
                                          style = "double"),
                             cell_text(align   = "right",
                                       weight  = "bold",
                                       size    = "large")),
                locations = list(cells_stub(), cells_body(columns = Percent_total))) %>%
      tab_style(style = list(cell_fill(color   = "#E8EADFFF"),
                             cell_borders(sides = "bottom",
                                          color = "#43475BFF",
                                          style = "double"),
                             cell_text(align   = "right",
                                       size    = "large")),
                locations = cells_body(columns = !Percent_total)) %>%
      tab_footnote(md(
        "These assignments will focus on specialized skills or involve in-class activities. They will be due at the start of class on the deadline and submitted through Canvas."
      ), locations = cells_stub(rows = Format == "Assignments")) %>%
      tab_footnote(md(
        "These are short quizzes given at the start of class to assess your preparation for the week's topic."
      ), locations = cells_stub(rows = Format == "Quizzes")) %>%
      tab_footnote(md(
        "Number (if any) of lowest scores that will automatically be dropped from your final grade"
      ), locations = cells_column_labels(columns = N_dropped)) %>%
      tab_footnote(md(
        "You may use hard copy notes/materials for all exams, but not electronic devices/resources."
      ), locations = cells_stub(rows = Format == "Exams")) %>%
      tab_style(style     = cell_text(size = "small"),
                locations = cells_footnotes()) %>%
      cols_width(stub()        ~ px(120),
                 Points_each   ~ px(85),
                 Count         ~ px(85),
                 N_dropped     ~ px(85),
                 Points_total  ~ px(85),
                 Percent_total ~ px(100))
  }


}

resources <- function(course) {
  course_short <- shorten_course(course)
  course_info <-get_course_info(course)
  textbooks <- get_textbooks(course)
  instructor <- get_instructor_info()
 list(
    Logistics = list(
      Name    = "Canvas",
      Link    = paste0("https://unomaha.instructure.com/courses/", course_info[["canvas"]]),
      Comment = "You must review Canvas regularly. For technical support, please use Canvas Student Support.",
      Image   = here("images/logo_canvas.png")
    ),
    Github = list(
      Name    = "Course Website",
      Link    = sprintf("https://rich-molecular-health-lab.github.io/courses/%s", course_short),
      Comment = "Any pertinent information I keep here will also appear on Canvas",
      Image   = pluck(instructor, "logo")
    ),
    Text = list(
      Name    = textbooks[["title"]],
      Link    = textbooks[["href"]],
      Comment = if (course_short %in% c("conbio", "zoobio")) "Required text for this course (details below)." else "Playlist embedded below",
      Image   = here(textbooks[["image"]])
    )
  ) %>%
    enframe(name = "Category") %>%
    unnest_wider("value") %>%
    select(
      Image,
      Link,
      Resource = Name,
      Comment
      ) %>%
    gt(rowname_col = "Image") %>%
    rm_header() %>%
    fmt_image(columns = stub()) %>%
    fmt_url(columns = "Link", label = from_column("Resource")) %>%
    cols_hide("Resource") %>%
    opt_table_lines(extent = "none") %>%

    tab_style(style = list(
      cell_fill(color    = "#E8EADF40"),
      cell_text(v_align = "middle",
                align   = "left")),
      locations = list(cells_body(), cells_stub())) %>%

    tab_header(title = "Summary of Required Course Materials") %>%

    tab_style(style = list(
      cell_text(size   = "x-large",
                align  = "left",
                weight = "bolder")),
      locations = cells_title()) %>%
    cols_width(stub()   ~ px(100),
               Link     ~ px(300),
               Comment  ~ px(400))
}


learning_outcomes <- function(course) {
  get_slos(course) %>%
    mutate(icon = case_match(
      Level,
      "Remember"   ~ "list-check",
      "Understand" ~ "lightbulb",
      "Apply"      ~ "wrench",
      "Analyze"    ~ "chart-line",
      "Evaluate"   ~ "bars",
      "Critique"   ~ "pen-nib",
      "Recognize"  ~ "eye",
      "Summarize"  ~ "comment"
    )) %>%
    relocate(icon) %>%
    gt() %>%
    fmt_icon(columns = "icon") %>%
    opt_table_lines(extent = "none") %>%
    cols_label(
      icon ~ "",
      Level ~ "",
      Outcome ~ ""
    ) %>%
    tab_style(style = list(
      cell_fill(color    = "#84697F40"),
      cell_text(
        size      = "larger",
        weight    = "bold",
        font      = google_font("Arsenal SC"),
        stretch   = "semi-expanded"
        )),
      locations = list(cells_body(columns = c("icon", "Level")))) %>%
    tab_style(style = list(
      cell_fill(color    = "#84697F40"),
      cell_text(align    = "right")),
      locations = cells_body(columns = "icon")) %>%
    tab_style(style = list(
      cell_fill(color    = "#84697F40"),
      cell_text(align    = "left")),
      locations = cells_body(columns = -c("icon"))) %>%
    tab_style(style = cell_text(
      font     = google_font("Arsenal"),
      style    = "italic"
    ),
    locations = cells_body(columns = "Outcome")
    ) %>%
    cols_width(icon          ~ px(50),
               Level         ~ px(150),
               Outcome       ~ px(500)) %>%
    rm_header() %>%
    tab_spanner(label = "Students will be able to...", columns = everything()) %>%
    tab_style(style = list(
      cell_fill(color    = "#84697FFF"),
      cell_text(style    = "oblique",
                font     = google_font("Arsenal SC"),
                color    = "#E8EADFFF",
                weight   = "bolder",
                align    = "left"),
      cell_borders(sides = c("top", "bottom"))),
      locations = list(cells_column_spanners()))
}




tooltips <- list(
  assignments = tippy(
    "Turn in",
    "You will receive completion credit for in-class exercises at the end of that class and do not need to complete any extra work ahead of class. Keep in mind that I will not offer makeup opportunities for in-class activities, and these dates may change without advance notice. Other assignments are due on Canvas by 4 PM on the day listed."
  ),
  reading = tippy(
    "Reading",
    "You should have read the assigned textbook section(s) before the start of that class. I may give a short preparation quiz at the start of class related to material from that day's assigned reading."
  ),
  location = tippy(
    "Location",
    "Alternative locations (e.g., field trips, computer labs) are tentative until confirmed the week before."
  )
)

location_font <- function(location, course) {
  location_default <- pluck(course_info, course, "location")
  location_exam    <- pluck(course_info, course, "location_assigned")
  style_defaults   <- "font-variant:small-caps"

  if (location == location_default) {
    style_spec <- "font-size:12px; font-weight:lighter; color:gray"
    location_tooltip <- location

  } else {
    style_spec <- "font-size:15px; font-weight:bold"
    location_tooltip <- tippy(location, "Alternative locations (e.g., field trips, computer labs) are tentative until confirmed the week before.")
  }

  display <-  div(style = paste(style_defaults, style_spec, sep = ";"), location_tooltip)

  return(display)
}

theme_tooltip <- function(data, value, index) {
  theme   <- data$theme[index]
  display <- tippy(value, paste("Theme: ", theme))
  return(display)
}


date_display <- function(date) {
 date_string <- as.character(
    paste0(
      wday(date, label = TRUE, abbr = TRUE), " ",
      month(date), "/",
      mday(date))
    )
 div(
   style = "font-variant:small-caps",
   date_string
 )
}

day_colors <- function(date) {
  if (wday(date) == 2) "#3F858CFF" else if (wday(date) == 3) "#707322FF" else if (wday(date) == 4) "#F2D43DFF" else if (wday(date) == 5) "#D9814EFF" else "#731A12FF"
}

theme_colors <- function(theme) {
  if (str_detect(theme, "Foundations")) "#660D20FF" else if (str_detect(theme, "Threats")) "#E59A52FF" else if (str_detect(theme, "Approaches")) "#EDCE79FF" else "#094568FF"
}

subtab_theme <- reactableTheme(
  headerStyle = list(
    fontVariant     = "small-caps",
    fontSize        = "12px",
    color           = "black",
    backgroundColor = "#eeeeee",
    verticalAlign   = "baseline",
    borderWidth     = "0px",
    paddingTop      = "0px",
    paddingBottom   = "0px"
  ),
  style       = list(
    fontFamily  = "Anaheim",
    fontVariant = "normal",
    borderColor = "#eeeeee"
  )
)

maintab_theme <- reactableTheme(
  headerStyle = list(
    fontVariant     = "small-caps",
    fontSize        = "13px",
    color           = "white",
    backgroundColor = "black",
    verticalAlign   = "baseline",
    borderWidth     = "0px"
  ),
  style       = list(fontFamily = "Anaheim", fontVariant = "small-caps")
)

daily_subtab <- function(daily) {
  div(
    style = "margin:0px 10px 10px 10px; padding:1px 10px 10px 10px",
    reactable(
      daily,
      highlight  = TRUE,
      fullWidth  = FALSE,
      borderless = TRUE,
      outlined   = TRUE,
      theme      = subtab_theme,
      columns    = list(
        week  = colDef(show = FALSE),
        date  = colDef(
          name        = "",
          maxWidth    = 150,
          sticky      = "left",
          align       = "left",
          vAlign      = "center",
          cell        = function(value) { date_display(value) }
        ),
        location = colDef(
          name        = "",
          maxWidth    = 100,
          align       = "left",
          vAlign      = "center",
          cell        = function(value) { location_font(value, params$course) }
        ),
        agenda     = colDef(
          vAlign      = "center",
          name        = "Agenda",
          minWidth    = 400
        ),
        reading    = colDef(
          maxWidth    = 200,
          vAlign      = "center",
          header      = tooltips$reading
        ),
        assignment = colDef(
          vAlign      = "center",
          header      = tooltips$assignment,
          minWidth    = 300
        )
      ),
      rowStyle = function(index) {
        date  <- daily$date[index]
        return(paste0("border-left: 3px solid ", day_colors(date)))
      }
    )
  )
}

schedule_reactable <- function(schedule) {
  weekly_schedule <- select(
    schedule,
    week,
    theme,
    title
  ) %>%
    summarize(
      theme   = str_flatten_comma(unique(theme)),
      title   = str_flatten_comma(unique(title)),
      .by = "week"
    ) %>%
    mutate(
      theme = case_when(
        theme == "Other" & str_detect(title, "EXAM") ~ "Exam",
        theme == "Other" & !str_detect(title, "EXAM") & str_detect(title, "Film") ~ lag(theme),
        .default = theme
      )
    )

  daily_schedule <- schedule %>%
    mutate(date = as_date(date)) %>%
    select(
      week,
      date,
      reading,
      agenda = topic,
      location,
      assignment
    )

 display <- reactable(
    weekly_schedule,
    highlight       = TRUE,
    borderless      = TRUE,
    theme           = maintab_theme,
    defaultExpanded = TRUE,
    defaultPageSize = 17,
    columns = list(
      theme = colDef(show = FALSE),
      week  = colDef(
        name        = "Week",
        maxWidth    = 75,
        align       = "left",
        style       = "font-weight:bold",
        details     = function(index) {
          daily <- filter(daily_schedule, week == weekly_schedule$week[index])
          daily_subtab(daily)
        }
      ),
      title = colDef(
        name        = "Chapter",
        minWidth    = 300,
        cell        = function(value, index) {
          theme_tooltip(weekly_schedule, value, index)
          }
        )
    ),
    rowStyle = function(index) {
      theme  <- weekly_schedule$theme[index]
      return(paste0("border-left: 6px solid ", theme_colors(theme)))
    }
  )
 return(display)

}


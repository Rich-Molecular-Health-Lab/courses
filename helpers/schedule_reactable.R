
library(reactable)
library(reactablefmtr)
library(tippy)


tooltips <- list(
  assignments = tippy(
    "Turn in",
    "You will receive completion credit for in-class exercises at the end of that class and do not need to complete any extra work ahead of class. Keep in mind that I will not offer makeup opportunities for in-class activities, and these dates may change without advance notice. Other assignments are due on Canvas by 4 PM on the day listed."
  ),
  reading = tippy(
    "Reading",
    "You should have read the assigned textbook section(s) before the start of that class. I may give a short preparation quiz at the start of class related to material from that day's assigned reading."
  ),
  podcasts = tippy(
    "Podcasts",
    "You should have listened to the assigned episodes before the start of the first class of the week. I may give a short preparation quiz at the start of class related to material from that weeks's assigned listening."
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


day_colors <- list(
  "#009392FF",
  "#39B185FF",
  "#9CCB86FF",
  "#E9E29CFF",
  "#EEB479FF",
  "#E88471FF",
  "#CF597EFF"
)

theme_colors <- list(
  conbio = list(
    "Foundations" = "#3A9AB2FF",
    "Threats"     = "#A5C2A3FF",
    "Approaches"  = "#DCCB4EFF",
    "Other"       = "#F11B00FF"
  ),
  hhe = list(
    "Introduction"            = "#3A9AB2FF",
    "Climate"                 = "#6FB2C1FF",
    "Natural Disasters"       = "#91BAB6FF",
    "Built Environments"      = "#A5C2A3FF",
    "Vector Ecology"          = "#BDC881FF",
    "Exam Week"               = "#DCCB4EFF",
    "Zoonotic Transmission"   = "#E3B710FF",
    "Toxicology"              = "#E79805FF",
    "Agriculture"             = "#EC7A05FF",
    "Projects"                = "#EF5703FF",
    "Final Exam Week"         = "#F11B00FF"
  )
)

subtab_header_visible <- list(
    fontVariant     = "small-caps",
    fontSize        = "12px",
    color           = "black",
    backgroundColor = "#eeeeee",
    verticalAlign   = "baseline",
    borderWidth     = "0px",
    paddingTop      = "0px",
    paddingBottom   = "0px"
  )

subtab_theme <- reactableTheme(
  headerStyle = list(display = "none"),
  style       = list(
    fontFamily  = "Anaheim",
    fontVariant = "normal",
    borderColor = "#eeeeee"
  )
)

week_past_theme <- reactableTheme(
  rowStyle = "text-body-tertiary"
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


daily_subtab <- function(course, daily, week_now) {

  reactable(
      daily,
      highlight  = TRUE,
      fullWidth  = FALSE,
      borderless = TRUE,
      outlined   = TRUE,
      theme      = subtab_theme,
      columns    = list(
        reading    = if (str_detect(course, "conbio")) colDef(
          maxWidth    = 200,
          vAlign      = "center",
          header      = tooltips$reading
        ) else if (str_detect(course, "hhe")) colDef(show = FALSE),
        week       = colDef(show = FALSE),
        date       = colDef(
          name        = "",
          maxWidth    = 150,
          sticky      = "left",
          align       = "left",
          vAlign      = "center",
          cell        = function(value) { date_display(value) }
        ),
        location   = colDef(
          name        = "",
          maxWidth    = 100,
          align       = "left",
          vAlign      = "center",
          cell        = function(value) { location_font(value, course) }
        ),
        agenda     = colDef(
          vAlign      = "center",
          name        = "Agenda",
          minWidth    = 400
        ),
        assignment = colDef(
            vAlign      = "center",
            header      = tooltips$assignment,
            maxWidth    = 200
          )
      ),
      rowStyle = function(index) {
        date  <- daily$date[index]
        week  <- daily$week[index]
        color <- pluck(day_colors, wday(date))
        border_th <- if (week_now == week) "6px" else "3px"
        font_weight <- if (week_now > week) "lighter" else "normal"
        return(paste0(
          "border-left: ",
          border_th,
          " solid ",
          color, "; font-weight:", font_weight
          ))
      }
    )
}

podcast_li <- function(podcast) {
  div(
    class = "list-group-item",
    style = "display:flex; flex-flow:column nowrap; justify-content:flex-start; align-items:flex-start; padding-bottom:10px",
    div(tags$strong(podcast[["series"]]), tags$small(paste0(" (", year(podcast[["date"]]), ") : "))),
    div(
      style = "display:flex; flex-flow:row nowrap; justify-content:space-between; align-items:flex-start; font-variant:normal",
      tags$a(href = podcast[["href"]], podcast[["episode"]]),
      div(class = "badge bg-light rounded-pill", style = "padding-left:5px", paste0(podcast[["minutes"]], "m"))
      )
  )
}

week_podcasts <- function(podcasts_week, week, week_now, theme) {
  theme_col <- pluck(theme_colors, "hhe", theme)
  bg <- gsub("FF", "0D", theme_col)
  div(
    style = paste0("display:flex; flex-flow:row nowrap; align-items:flex-start; justify-content:space-evenly; margin:5px; padding:10px; width:100%; border: 2px outset #bcbcbc; background:", bg),
    span(
      style = "padding-top:10px",
      shiny::icon("podcast", class = "fa-solid fa-2xl")
      ),
    span(
      class = "list-group",
      style = "display:flex; flex-flow:column nowrap; align-items:flex-start; justify-content:flex-start; width:90%",
      map(podcasts_week, \(x) podcast_li(x))
    )
  )
}

schedule_weeks <- function(course, schedule) {
  if (str_detect(course, "conbio")) {
    select(
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
  } else if (str_detect(course, "hhe")) {
    select(
      schedule,
      week,
      theme
    ) %>%
      summarize(
        theme    = str_flatten_comma(unique(theme)),
        .by = "week"
      )
  }
}

schedule_days <- function(course, schedule) {
  if (str_detect(course, "conbio")) {
    schedule %>%
      mutate(date = as_date(date)) %>%
      select(
        week,
        date,
        reading,
        agenda = topic,
        location,
        assignment
      )
  } else if (str_detect(course, "hhe")) {
    schedule %>%
      mutate(date = as_date(date)) %>%
      select(
        week,
        date,
        agenda,
        location,
        assignment
      ) %>%
      mutate(reading = NA)
  }
}

week_expanded <- function(course, daily, week, week_now, schedule_list = NULL) {
  outer <- "margin:0px 2px 10px 0px; padding:1px 10px 10px 10px"
   if (str_detect(course, "hhe") & !(week %in% c(7, 17))) {
    podcasts_week <- pluck(schedule_list, paste0("W", week, "D1"), "podcasts")
    theme <- pluck(schedule_list, paste0("W", week, "D1"), "theme")
    color <- if (week_now > week) gsub("FF", "30", pluck(theme_colors, "hhe", theme)) else pluck(theme_colors, "hhe", theme)
    display <- div(
      style = paste0(
        "display:flex; flex-flow:column nowrap; align-items:start; ",
        outer,
        "; border-left: 6px solid ",
        color
        ),
      daily_subtab(course, daily, week_now),
      week_podcasts(podcasts_week, week, week_now, theme)
    )
  } else {
    div(
      style = outer,
      daily_subtab(course, daily, week_now)
    )
  }
}

title_colDef <- function(data) {
  colDef(
    name        = "Chapter",
    minWidth    = 300,
    cell        = function(value, index) {
      theme   <- data$theme[index]
      tippy(value, paste("Theme: ", theme))
    }
  )
}

schedule_reactable <- function(course, schedule, schedule_list = NULL) {
  weekly_schedule <- schedule_weeks(course, schedule)
  daily_schedule  <- schedule_days(course, schedule)
  course_short <- str_remove(course, "_\\d{2}\\w")
  semester     <- str_remove(course, "\\w+_")

  week_now <- as.integer(str_remove(current_week(semester), "\\w"))

  if (str_detect(course, "conbio")) {
    cols <- list(
      theme = colDef(show = FALSE),
      title = title_colDef(weekly_schedule)
      )
  } else if (str_detect(course, "hhe")) {
    cols <- list(
      theme = colDef(
        name     = "Agenda",
        minWidth = 300
      )
    )
  }

 display <- div(
   tags$button(
      class = "btn btn-primary",
     "Expand/Collapse All Weeks",
     onclick = sprintf("Reactable.toggleAllRowsExpanded('schedule_%s')", course),
     ),
   reactable(
     weekly_schedule,
     borderless      = TRUE,
     theme           = maintab_theme,
     defaultExpanded = FALSE,
     defaultPageSize = 17,
     elementId       = paste0("schedule_", course),
     columns         = list_assign(
       cols,
       week  = colDef(
         name        = "Week",
         maxWidth    = 100,
         align       = "left",
         style       = "font-weight:bold",
         details     = function(index) {
           daily <- filter(daily_schedule, week == weekly_schedule$week[index])
           week_expanded(course, daily, daily$week[1], week_now, schedule_list)
         }
       )
     ),
     rowStyle        = function(index) {
       theme  <- str_remove_all(weekly_schedule$theme[index], ",.+")
       color <- pluck(theme_colors, course_short, theme)
       color_border <- if (week_now > index) gsub("FF", "30", color) else color
       color_bg <- if (week_now == index) gsub("FF", "30", color) else "#ffffff"
       font_weight <- if (week_now == index) "bolder" else if (week_now > index) "lighter" else "normal"

       style <- paste0(
         "border-left: 6px solid ",
         color_border,
         "; background-color:",
         color_bg,
         "; font-weight:", font_weight
         )

       return(style)
     }
   )

 )
 return(display)

}



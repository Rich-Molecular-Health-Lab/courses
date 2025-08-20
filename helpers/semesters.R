semesters <- list(
  "25f" = list(
    start_class  = ymd("2025-08-25"),
    end_class    = ymd("2025-12-12"),
    start_finals = ymd("2025-12-15"),
    end_finals   = ymd("2025-12-19"),
    holidays = list(
      ymd("2025-09-01"),
      ymd("2025-10-20"),
      ymd("2025-10-21"),
      ymd("2025-11-26"),
      ymd("2025-11-27"),
      ymd("2025-11-28")
    ),
    trips = list(
      ymd("2025-10-17"),
      ymd("2025-10-18")
    )
  )
)

semester_days <- function(course) {
  semester     <- str_extract(course, "(?<=_)\\d{2}\\w")
  first <- semesters[[semester]][["start_class"]]
  last  <- semesters[[semester]][["end_class"]]
  days_seq  <- as.list(seq.Date(first, last)) %>% discard(\(x) wday(x) %in% c(7, 1))
  weeks_seq <- as.list(rep(1:16, each = 5))

  holidays <- map(
    semesters[[semester]][["holidays"]], \(x) ymd_hm(
      paste(as.character(x), pluck(course_info, course, "start_time")), tz = "America/Chicago"
      )
    )

  result <- map2(days_seq, weeks_seq, \(x, y) list(
      date  = ymd_hm(paste(as.character(x), pluck(course_info, course, "start_time")), tz = "America/Chicago"),
      daywk = as.character(wday(x, label = TRUE, abbr = TRUE)),
      week  = y
      )) %>%
    discard(\(x) x$date %in% list_simplify(holidays)) %>%
    keep(\(x) wday(x$date) %in% pluck(course_info, course, "wdays")) %>%
    set_names(~paste0("D", seq_along(.))) %>%
    list_assign(FINAL = list(
      date  = pluck(course_info, course, "final_exam"),
      daywk = as.character(wday(pluck(course_info, course, "final_exam"), label = TRUE, abbr = TRUE)),
      week  = 17
      ))

  return(result)
}
list_scores <- function(x, points) {
  assign(x, points) %>%
    set_names(x)
}

shorten_course <- function(course) {
  stringr::str_extract(course, "\\w+(?=_)")
}


schedule_link <- function(course, subdir = "course", basename = "schedule") {
  subpath <- if (str_detect(subdir, "course")) shorten_course(course) else subdir
  filename <- if (str_detect(basename, "course")) shorten_course(course) else basename
  sprintf("https://rich-molecular-health-lab.github.io/courses/%s/%s.html", subpath, filename)
}

syllabus_link <- function(course, subdir = "syllabi", basename = "course") {
  subpath <- if (str_detect(subdir, "course")) shorten_course(course) else subdir
  filename <- if (str_detect(basename, "course")) shorten_course(course) else basename
  sprintf("https://rich-molecular-health-lab.github.io/courses/%s/%s.html", subpath, basename)
}

get_instructor_info <- function(
    bookings     = "https://outlook.office.com/bookwithme/user/c35af21f7b904e7d82e5cffc9144bce2@nebraska.edu/meetingtype/OBbQKPv_8U-i7oMmPayEZg2?anonymous&ismsaljsauthenabled&ep=mlink",
    office_hours = "Use this link to check/book an available time.",
    zoom         = "https://zoom.us/launch/chat?src=direct_chat_link&email=aliciarich%40unomaha.edu"
) {
  list(
    call_me           = "Dr. Rich or Professor Rich (she/her)",
    full_name         = "Alicia M. Rich, Ph.D.",
    position          = "Assistant Professor of Biology & Environmental Science",
    office            = "Allwine Hall 413",
    bookings_link     = bookings,
    office_hours_text = office_hours,
    zoom_link         = zoom,
    contact_href      = "mailto:aliciarich@unomaha.edu",
    contact_text      = "aliciarich@unomaha.edu",
    headshot          = here("syllabi/graphics/headshot_rich.png"),
    logo              = here("syllabi/graphics/logo_richlab.png")
  )
}

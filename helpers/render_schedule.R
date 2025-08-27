here::i_am("helpers/render_schedule.R")

course <- str_extract(params$course, "\\w+(?=_)")

load(here(paste0(course, "/schedule.RData")))

if (str_detect(course, "hhe")) {
  load(here("hhe/schedule_list.RData"))
  schedule_reactable(params$course, schedule, schedule_list) %>%
    save_html(file = here(paste0(course, "/schedule_tbl.html")))
} else {
  schedule_reactable(params$course, schedule) %>%
    save_html(file = here(paste0(course, "/schedule_tbl.html")))
}
here::i_am("hhe/schedule_compile.R")
source(here::here("_common.R"))

course <- "hhe_25f"

source(here("helpers/course_list.R"))
source(here("helpers/agenda_helpers.R"))
source(here("helpers/semesters.R"))

blank_schedule <- semester_days(course)

source(here("hhe/topics.R"))
source(here("hhe/schedule_detail.R"))

schedule_list <- populate_schedule(
  course,
  blank_schedule,
  topics,
  podcasts,
  podcast_selections,
  videos,
  film_selections,
  assignments
)
schedule      <- enframe_hhe_schedule(schedule_list)

save(schedule_list, file = here("hhe/schedule_list.RData"))
save(schedule, file = here("hhe/schedule.RData"))


save(topics, file = here("../assessment/hhe/topics.RData"))
save(videos, file = here("../assessment/hhe/videos.RData"))
save(podcast_selections, file = here("../assessment/hhe/podcast_selections.RData"))
save(film_selections, file = here("../assessment/hhe/film_selections.RData"))
save(schedule_list, file = here("../assessment/hhe/schedule_list.RData"))
save(schedule, file = here("../assessment/hhe/schedule.RData"))
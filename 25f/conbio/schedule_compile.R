here::i_am("25f/conbio/schedule_compile.R")
source(here::here("_common.R"))

course <- "conbio_25f"

source(here("helpers/course_list.R"))
source(here("helpers/agenda_helpers.R"))
source(here("helpers/semesters.R"))

blank_schedule <- semester_days(course)

source(here("25f/conbio/topics.R"))
source(here("25f/conbio/schedule_detail.R"))

topics_revised <- revise_topics(topics, discarded)
topics_flat    <- flatten_topics(topics_revised)

schedule_list <- map_topics(topics_flat, topics_nested, blank_schedule)
schedule      <- enframe_schedule(schedule_list, assignments, locations, course)

save(schedule, file = here("25f/conbio/schedule.RData"))
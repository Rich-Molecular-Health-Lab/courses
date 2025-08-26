here::i_am("helpers/generate_graphics.R")

learning_outcomes(params$course) %>% gtsave(here(paste0("graphics/tables/",  params$course, "_learning_outcomes.png")))

grade_breakdown(params$course) %>% gtsave(here(paste0("graphics/tables/",  params$course, "_grade_breakdown.png")))

resources(params$course) %>% gtsave(here(paste0("graphics/tables/",  params$course, "_resources.png")))
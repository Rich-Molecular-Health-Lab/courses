here::i_am("helpers/generate_graphics.R")

learning_outcomes(params$course) %>% gtsave(here(paste0("graphics/tables/",  params$course, "learning_outcomes.png")))
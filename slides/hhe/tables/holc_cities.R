
holc <- list(
  baltimore = tibble(
    "Category" = c(
      "African American population"     ,
      "Hispanic/Latino population"      ,
      "Poverty rate"                    ,
      "Non-citizen population"          ,
      "Older and very young population" ,
      "Disability status"   ,
      "Population density (1,000 persons/km2)"
    ),
    "Class_A" = c(
      .5371,
      .0387,
      .1436,
      .0519,
      .2053,
      .0919,
      3.38
    ),
    "Class_B"	= c(
      .7133,
      .0242,
      .1826,
      .0373,
      .2136,
      .1084,
      3.17
    ),
    "Class_C"	= c(
      .7103,
      .0526,
      .2857,
      .0412,
      .2026,
      .13	 ,
      3.83
    ),
    "Class_D"	= c(
      .5379,
      .0641,
      .2906,
      .0569,
      .1516,
      .1309,
      4.43
    ),
    "City"    = c(
      .628,
      .0496,
      .2243,
      .0475,
      .1937,
      .1184,
      2.96
    )
  ),


  dallas = tibble(
    "Category" = c(
      "African American population"     ,
      "Hispanic/Latino population"      ,
      "Poverty rate"                    ,
      "Non-citizen population"          ,
      "Older and very young population" ,
      "Disability status"   ,
      "Population density (1,000 persons/km2)"
    ),
    "Class_A" = c(
      .0342,
      .1083,
      .0554,
      .0374,
      .1844,
      0865,
      1.92
    ),
    "Class_B"	= c(
      .0809,
      .4943,
      .1574,
      .1768,
      .1679,
      .0732,
      2.03
    ),
    "Class_C"	= c(
      .2212	  ,
      .498	,
      .2517	,
      .1926	,
      .1453	,
      .0717	  ,
      2.36
    ),
    "Class_D"	= c(
      .3966	  ,
      .3229	,
      .3384	,
      .1096	,
      .1424	,
      .0611	  ,
      1.07
    ),
    "City"    = c(
      .1973,
      .3476,
      .1549,
      .1548,
      .1741,
      .0757,
      0.83
    )
  ),

  kansascity = tibble(
    "Category" = c(
      "African American population"     ,
      "Hispanic/Latino population"      ,
      "Poverty rate"                    ,
      "Non-citizen population"          ,
      "Older and very young population" ,
      "Disability status"   ,
      "Population density (1,000 persons/km2)"
    ),
    "Class_A" = c(
      .0339	,
      .0562	,
      .0443	,
      .012	,
      .229	,
      .0658	,
      1.63
    ),
    "Class_B"	= c(
      .1563,
      .1175,
      .1251,
      .0655,
      .1976,
      .0844,
      1.62
    ),
    "Class_C"	= c(
      .3315,
      .1638,
      .2423,
      .0751,
      .1915,
      .071,
      1.7
    ),
    "Class_D"	= c(
      .3968,
      .3043,
      .3463,
      .1462,
      .1886,
      .078,
      1.39
    ),
    "City"    = c(
      .1428,
      .0986,
      .1183,
      .0458,
      .203,
      .0778,
      0.35
    )
  )

) %>%
  map(\(x) pivot_longer(x, !Category)) %>%
  map(\(x) mutate(x, Class = if_else(name == "City", "City Average", str_sub(name, -1L, -1L)), .keep = "unused"))  %>%
  map(\(x) mutate(x, Diff  = value - value[Class == "City Average"], .by = "Category"))   %>%
  map(\(x) rename(x, Real = value))   %>%
  map(\(x) mutate(x, Class  = fct(Class, levels = c("City Average", "A", "B", "C", "D")))) %>%
  map(\(x) mutate(x, Category  = fct(
    Category,
    levels = c(
      "Population density (1,000 persons/km2)",
      "Poverty rate"                    ,
      "Disability status"   ,
      "Older and very young population" ,
      "Non-citizen population"          ,
      "Hispanic/Latino population"      ,
      "African American population"
    )
  ))) %>%
  map(\(x) arrange(x, Category, Class)) %>%
  map(\(x) pivot_wider(x, names_from = "Category", values_from = c("Real", "Diff"), names_sep = "_")) %>%
  map(\(x) select(
    x,
    Class,
    starts_with("Diff_")
  )) %>%
  map(\(x) rename_with(
    x,
    ~str_remove_all(., "Diff_")
  )) %>%
  map(\(x) rename_with(
    x,
    ~str_trim(str_remove_all(., "(Population)|(population)"))
  )) %>%
  map(\(x) filter(
    x,
    Class != "City Average"
  ))

save(holc, file = here::here("hhe/slides/tables/holc.RData"))
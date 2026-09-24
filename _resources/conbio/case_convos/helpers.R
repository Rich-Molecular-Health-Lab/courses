group_df <- function(my_canvas, canvas_id) {
  groups <- get_course_groups(my_canvas, params$canvas_id) %>%
    right_join(
      map_df(groups$id, \(x) group_memberships(my_canvas, x)),
      by = "id"
    ) %>%
    distinct() %>%
    mutate(member_order = row_number(), .by = "id") %>%
    pivot_wider(
      names_from   = "member_order",
      values_from  = "member",
      names_prefix = "member_",
      names_sort   = TRUE
    ) %>%
    select(
      group_id   = id,
      group_name = name,
      group_category_id,
      max_membership,
      members_count,
      has_submission,
      starts_with("member_")
    )

}

group_memberships <- function(my_canvas, x) {

  group_memberships <- get_group_users(my_canvas, x)

  tibble(
    id     = rep(x, length(group_memberships$name)),
    member = group_memberships$name
  )

}





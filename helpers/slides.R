
slides_value_list <- list(
  Authenticity = list(
      "Speak up if you are confused or struggling."              = "Attempt to manipulate or mislead your instructors or classmates.",
      "Be honest with yourself about your needs & capabilities." = "Hide or deny your biases or areas for growth.",
      "Use tools like AI and collaboration with integrity."      = "Present direct results from AI tools like ChatGPT as your own work."
    ),
  Curiosity = list(
      "Use the readings and assignments as a guide."                                 = "Let the syllabus limit your investigation." ,
      "Ask thoughtful questions of yourself, your instructor, and your colleagues."  = "Expect me to fill your brain with facts for you to recall.",
      "Actively seek insights from your classmates."                                 = "Dominate discussions so that only your voice occupies the space.",
      "Maintain awareness and openness to shifting your perspectives or drawing new conclusions." = "Assume your perspectives and views are permanent or fully-informed"
    ),
  Responsibility = list(
      "Take initiative when you see an opportunity to encourage thoughtful discourse." = "Wait for your instructor to lead every discussion.",
     "Take full personal responsibility for mistakes or missed opportunities."  = "Deflect blame or redirect responsibilities on your instructor or your classmates.",
     "Cultivate a professional and respectful attitude that match for all your instructors" = "Let your implicit bias cloud your expectations of or attitude toward your instructor",
     "Bring your barriers to my attention so I can help you navigate them." = "Assume only you are navigating barriers or that barriers are insurmountable with cooperation."
  )
)

roles <- list(
  "Organize topics & resources"                  = "Record, organize, and manage all necessary information*",
  "Distill & clarify major themes"               = "Study assigned content & investigate further",
  "Guide discussions & answer questions"         = "Engage with your classmates during scheduled meetings",
  "Ensure classroom equity & accessibility"      = "Manage your time, priorities, and needs",
  "Assess your mastery of the learning outcomes" = "Set/adjust your goals and monitor your progress"
)


slide_li_cols <- function(item_left, item_right, layout = NULL, font_size = NULL) {

  style_defaults <- sprintf("; padding:10px; margin:10px; font-size:%s", if (is.null(font_size)) "25px" else font_size)


  div(
    style = paste0(if (is.null(layout) || layout == "grid") "display:grid; grid-template: auto / 50% 50%" else "display:flex; flex-flow:row nowrap; justify-content:space-between; align-items:flex-start", style_defaults),
    div(style = "padding-right:5px", item_left),
    div(style = "padding-left:5px", item_right)
  )
}


slide_list_cols <- function(list, layout = NULL, font_size = "25px", head_left = NULL, head_right = NULL) {
  head_L <- if (is.null(head_left)) "" else head_left
  head_R <- if (is.null(head_right)) "" else head_right
  size <- if (is.null(font_size)) "25px" else font_size

  if (!is.null(head_left) || !is.null(head_right)) {
    div(
      slide_li_cols(
        item_left  = head_L,
        item_right = head_R,
        layout     = layout,
        font_size  = "45px; font-weight:bold"
      ),
      imap(list, \(x, idx) slide_li_cols(
        item_left  = idx,
        item_right = x,
        layout     = layout,
        font_size  = size
      ))
    )
  } else {
    div(
      imap(list, \(x, idx) slide_li_cols(
        item_left  = idx,
        item_right = x,
        layout     = layout,
        font_size  = size
      ))
    )
  }
}




value_list <- list(
  Authenticity = list(
    list(
      do   = "Speak up if you are confused or struggling.",
      dont = "Attempt to manipulate or mislead your instructors or classmates."
    ),
    list(
      do   = "Be honest with yourself about your needs & capabilities.",
      dont = "Hide or deny your biases or areas for growth."
    ),
    list(
      do   = "Use tools like AI and collaboration with integrity.",
      dont = "Present direct results from AI tools like ChatGPT as your own work."
    )
  ),
  Curiosity = list(
    list(
      do   = "Use the readings and assignments as a guide."  ,
      dont = "Let the syllabus limit your investigation."
    ),
    list(
      do   = "Ask thoughtful questions of yourself, your instructor, and your colleagues." ,
      dont = "Expect me to fill your brain with facts for you to recall."
    ),
    list(
      do   = "Actively seek insights from your classmates." ,
      dont = "Dominate discussions so that only your voice occupies the space."
    ),
    list(
      do   = "Maintain awareness and openness to shifting your perspectives or drawing new conclusions." ,
      dont = "Assume your perspectives and views are permanent or fully-informed"
    )
  ),
  Responsibility = list(
    list(
      do   = "Take initiative when you see an opportunity to encourage thoughtful discourse."     ,
      dont = "Wait for your instructor to lead every discussion."
    ),
    list(
      do   = "Take full personal responsibility for mistakes or missed opportunities." ,
      dont = "Deflect blame or redirect responsibilities on your instructor or your classmates."
    ),
    list(
      do   = "Cultivate a professional and respectful attitude that match for all your instructors" ,
      dont = "Let your implicit bias cloud your expectations of or attitude toward your instructor"
    ),
    list(
      do   = "Bring your barriers to my attention so I can help you navigate them." ,
      dont =  "Assume only you are navigating barriers or that barriers are insurmountable with cooperation."
    )
  )
)


item_do_dont <- function(text, type, text_style = "font-size:13px; padding:5px", justify = "justify-content-between") {
    div(
      class = paste0("list-group-item list-group-item-", if (type == "do") "success" else "danger"," d-flex ", justify, " align-items-center"),
      style = "padding:10px",
      span(
        style = "margin:10px",
        shiny::icon(name = if (type == "do") "circle-check" else "ban", class = "fa-solid")
        ),
      span(
        style = text_style,
        text
        )
    )
}


list_do_dont <- function(x, idx) {
  withTags(
    card(
      class = "card bg-light",
      card_header(
        h3(
          style = "font-variant:small-caps; font-weight:bold",
          idx
          )
        ),
      card_body(
        div(
          style = "display:grid; grid-template: auto / 50% 50%",
          item_do_dont(
            "Do", "do",
            text_style = "font-size:15px; font-weight:bold; font-variant:small-caps",
            justify = "justify-content-start"
            ),
          item_do_dont(
            "Don't", "dont",
            text_style = "font-size:15px; font-weight:bold; font-variant:small-caps",
            justify = "justify-content-start"
            ),
          imap(list_flatten(x, name_spec = "{inner}"), \(y, idy) item_do_dont(y, idy))
        )
      )
     )
    )
}

class_culture <- function(value_list) {
  withTags(
    card(
      class = "card bg-light mb-1",
      card_header(h6("The following three values form the foundation of my classroom culture and expectations:")),
      card_body(
        imap(value_list, \(x, idx) list_do_dont(x, idx))
      )
    )
  )
}

quiz_rubric <- function() {
  tribble(
    ~Score,   ~Standard,
    0     ,   "Nothing submitted." ,
    1     ,   "Response suggests the student is not familiar with any of the background content." ,
    2     ,   "Response suggests the student reviewed some of the material but did not fully comprehend or has not thought critically about it." ,
    3     ,   "Response suggests the student has reviewed the background material and came to class prepared to engage in a thoughtful discussion."
  ) %>%
    gt(rowname_col = "Score") %>%
    opt_table_lines("none") %>%
    tab_header(title = "Quiz Rubric", subtitle = "The assessment system will be broad, so each quiz will only recieve a 0, 1, 2, or 3 as an assessment of the student's basic preparation level for the class.") %>%
    tab_stubhead("Score") %>%
    cols_label(Standard ~ "Standard Met by Response") %>%
    tab_style(style = list(cell_fill(color  = "#CBB593FF"),
                           cell_borders(sides = c("top", "bottom")),
                           cell_text(color  = "#43475BFF",
                                     weight = "bold",
                                     size   = "large",
                                     align  = "left",
                                     stretch = "condensed")),
              locations = list(cells_column_labels(), cells_stubhead())) %>%
    tab_style(style = list(
      cell_fill(color = "#CBB593FF"),
      cell_borders(sides = c("top", "bottom"),
                   color = "#43475BFF"),
      cell_text(color  = "#43475BFF",
                size = "x-large",
                align  = "left",
                weight = "bolder")),
      locations = cells_title(groups = "title")) %>%
    tab_style(style = list(
      cell_fill(color = "#CBB59340"),
      cell_text(align = "left",
                color = "#43475BFF")),
      locations = list(cells_title(groups = "subtitle"), cells_source_notes())) %>%
    tab_style(style = list(cell_fill(color  = "#CBB59340"),
                           cell_text(color  = "#43475BFF"),
                           cell_borders(sides = "bottom",
                                        color = "#CBB593FF"),
                           cell_text(align = "left")),
              locations = cells_body()) %>%
    tab_style(style = list(cell_fill(color  = "#CBB59340"),
                           cell_borders(sides = "bottom",
                                        color = "#CBB593FF"),
                           cell_text(color  = "#43475BFF",
                                     align  = "center",
                                     size   = "large",
                                     weight = "bold")),
              locations = cells_stub())

}

instructor_card <- function() {
  withTags(
    card(
      class = "card border-secondary mb-3",
      card_header(
        h3(
          "Course Instructor - ",
          span(class = "text-body-secondary", "Alicia M. Rich, Ph.D."),
        ),
        h6(
          class = "text-body-secondary",
          span("Assistant Professor - ", span(class = "text-body-tertiary",em("Biology & Environmental Science")))
        ),
        h6(
          class = "text-body-secondary",
          span("Principal Investigator - ", span(class = "text-body-tertiary", em("Rich Lab for Molecular Health")))
        )
      ),
      card_body(
        bscols(
          widths = c(3, 9),
          card_image(file = here("images/headshot_rich.png")),
          ul(
            class = "list-group",
            li(
              class="list-group-item d-flex justify-content-between align-items-start",
              span(h6(class="text-body-secondary", "Please call me: ")),
              "Dr. Rich or Professor Rich ",
              span(em(class = "text-body-tertiary", "(she/her)"))
            ),
            li(
              class="list-group-item d-flex justify-content-between align-items-start",
              span(h6(class="text-body-secondary", "Find me in: ")),
              span("Allwine Hall 413", span(em(class = "text-body-tertiary", "(Office)")), ", 501b", span(em(class = "text-body-tertiary", "(Lab)")))
            ),
            li(
              class="list-group-item d-flex justify-content-between align-items-start",
              span(h6(class="text-body-secondary", "Office Hours: ")),
              span(em("By appointment "), "Thur 12-2, Fri 1-3")
            ),
            li(
              class="list-group-item d-flex justify-content-between align-items-start",
              span(h6(class="text-body-secondary", "Remote Meetings: ")),
              span(a(href = "https://zoom.us/launch/chat?src=direct_chat_link&email=aliciarich%40unomaha.edu", "Video or direct message via Zoom"))
            ),
            li(
              class="list-group-item d-flex justify-content-between align-items-start",
              span(h6(class="text-body-secondary", "Email me at: ")),
              span(a(href = "mailto:aliciarich@unomaha.edu", "aliciarich@unomaha.edu"))
            )
          )
        )
      ),
      card_footer(
        class = "text-body-secondary",
        p("I will be reachable via Zoom during office hours, or you may email me at least 1 business day in advance to arrange an in-person meeting."),
        p("I do not schedule student meetings outside regular workday hours (M-F, 9-5), so please do not ask me to meet with you in the evenings or on weekends."),
        p(span(b("I do not use Canvas messages")), ", so please only contact me via email. You can expect a response within 2 business days, ", span(b("but I do not monitor my email during evenings or on the weekends.")), "If you are still waiting for a response after three business days, please reach out again.")
      ),
      card_footer(card_image(file = here("graphics/comics/prof_door.png")))
    )
  )
}

accommodations <- function() {
  body <- div(
    p(
      "The University of Nebraska at Omaha is committed to providing reasonable accommodations for all persons with disabilities. This syllabus is available in alternate formats upon request. Reasonable accommodations are offered to students who are registered with the ",
      span(tags$a(href = "https://www.unomaha.edu/student-life/accessibility/student-services.php", "Accessibility Services Center (ASC)."))
    ),
    ul_group(
        layout    = "flex",
        item_list = list(
          "For more information: " = card_image(
              here("images/button_asc.png"),
              width = "300px",
              href = "https://www.unomaha.edu/student-life/accessibility/index.php"
            ),
          "Location: " = "104 H&K",
          "Phone: "    = "402.554.2872",
          "Email: "    = span(tags$a(href = "mailto:unoaccessibility@unomaha.edu", "unoaccessibility@unomaha.edu"))
        ),
        align  = "start"
    ),
    p(
      class = "text-body",
      "I do my best to integrate a reasonable and universal degree of flexibility into our schedule and assessment plan to accommodate some work/life balance while also ensuring equity and reasonable progress in your learning. I am happy to receive requests and recommendations to help me update and improve this approach for everyone, but I will only create personalized accommodations by working directly with the ASC."
      ),
    p(
      class = "text-body-tertiary",
      style = "font-size:13px",
      tags$em("Note: Please do not provide me with any personal or medical details to explain your barriers to attendance or assignment completion. While I do care about accommodating these issues when they arise,",
      span(tags$b("I also care about your privacy and respect that office's expertise in managing the conditions and complications that are outside of my purview.")))
    )
  )
  result <- content_card(
    title_text    = "Accessibility Services Center",
    subtitle_text = "The Accessibility Services Center (ASC) collaborates with students, administrators, faculty, and staff to ensure access to reasonable and appropriate student disability accommodations.",
    body          = tagList(body),
    footer_text   = "I genuinely appreciate opportunities to work with and around student barriers by collaborating with the ASC. If you know that you are coming into the semester with learning barriers (e.g., neurodivergence, visual/auditory processing differences), I strongly encourage you to register this with their office and then communicate with me about the ways I can work this into my approach from the beginning.",
    card_class    = "danger",
    heading       = "Accommodations",
    icon_name     = "users"
  )
  return(result)

}

attendance <- function() {
  body <- div(
    h6("In-class, low-stakes graded assessments may or may not be announced in advance throughout the semester."),
    p("These may include group activities or short quizzes to assess class preparation, and will generally amount to 3 points each."),
    p(
      "I do not accommodate any unregistered accommodation requests for class absences.",
      span(tags$strong("Instead, I will drop the lowest 1 or more scores on these items for every student at the end of the semester.")),
      "More specifically, I will calculate ~15% of the total frequency of in-class assignments (quizzes, activities, etc.) and then drop that number of lowest scores from each person's final grade."
      ),
    p(
      "This builds a reasonable degree of flexibility into our semester for routine barriers (e.g., minor illness, transportation issues, caregiver responsibilities) without requiring you to share any personal details or me to act as an arbitrary judge of worthy excuses.",
      span(tags$strong("Any accommodations beyond this will require documentation through ASC.")),
      "Please do not provide me with any personal or medical details to explain your barriers to attendance or assignment completion. While I do care about accommodating these issues when they arise,",
      span(tags$strong("I also care about your privacy and respect that office's expertise in managing the conditions and complications that are outside of my purview."))
    )
  )
  result <- content_card(
    title_text    = "Attendance is Required for Success",
    subtitle_text = "You will not pass this class if you are unable to attend, engage, and take your own notes.",
    body          = tagList(body),
    footer_text   = "If you do miss class, please, do not ask me to provide a comprehensive written or oral record of what you missed. I do not give repeat lectures and cannot prepare individualized lesson plans for each member of the class.",
    card_class    = "warning",
    heading       = "Attendance",
    icon_name     = "person-chalkboard"
  )
  return(result)
}



exam_format <- function(course) {
  withTags(
    card(
      class = "card border-info mb-3",
      card_header(
        class = "card-header text-white bg-info",
        span(icon("circle-info", class = "fa-solid")),
        span(strong("Format Details"))
      ),
      card_body(
        class = "card-text",
        h5(paste0("You will take ", length(pluck(assessment, course, "exams")), " synchronous, written exams this semester.")),
        bscols(
          widths = c(8, 4),
          p(
            "You may reference any materials you wish during the exam (e.g., notes, text), but ",
            span(b("electronic devices are not permitted. ")),
            "That means you should plan ahead to bring a hard-copy version of any materials you plan to use."
          ),
            ul(
              class = "list-group",
              li(
                class="list-group-item d-flex justify-content-between align-items-center",
                h6("General Exam Format:")
              ),
              li(
                class="list-group-item d-flex justify-content-between align-items-center",
                span(h6(class="text-body-secondary", "50%")),
                "Multiple Choice"
              ),
              li(
                class="list-group-item d-flex justify-content-between align-items-center",
                span(h6(class="text-body-secondary", "10%")),
                "True/False"
              ),
              li(
                class="list-group-item d-flex justify-content-between align-items-center",
                span(h6(class="text-body-secondary", "40%")),
                "Short Essay*"
              )
            ),
          p(
            "You are soley responsible for exam preparation and compilation of materials. That means you should not only attend class every day, but ",
            span(b("also take down clear and organized notes ")),
            "for your exams. I may or may not provide access to lecture slides, ",
            span(b("so you should rely entirely on my slides as your notes."))
          ),
            small(
              class = "text-tertiary",
              "*In most cases, you will answer ~2-3 total short essay questions, but I provide a choice of at least two prompts for each."
            )
        )
      )
    )
  )
}

missed_exams <- function() {
  withTags(
    card(
      class = "card border-warning mb-3",
      card_header(
        class = "card-header text-white bg-warning",
        span(icon("circle-exclamation", class = "fa-solid")),
        span(strong("Important Note"))
      ),
      card_body(
        class = "card-text",
        h5("All course exams follow a traditional, in-person, synchronous format."),
        p(
          "Most of the course schedule may change significantly as we progress. As a result, the content covered in each exam may change accordingly, but ",
          span(b("I will keep to the exam dates and times that I set at the beginning of the semester. ")),
        )
      ),
      card_footer(
        class = "text-warning-emphasis",
        "You should mark those dates and times on your calendar now, because I will only offer makeup opportunities under extenuating circumstances that the Accessibility Services Center communicates directly to me."
      )
    )
  )
}


engagement <- function() {
  body <- tagList(
    div(
        p(
          "While I am deeply committed to open science, I believe that open access to teaching materials can also deincentivize the development of note-taking skills. As a result, ",
          span(strong("you should not assume you will have open access to all slides and other teaching materials for this class in any asynchronous format ")),
          " (e.g., posting my slides to canvas or emailing them after class). You are more than welcome to take pictures of slides during class or use other forms of recording to help you later, but you must attend class and pay attention to grasp the key concepts and decide which ideas or concepts are worth noting for later, but ",
          span(strong("you should remember that your instructor's role is to organize and disseminate content during scheduled class time, while yours is to record, review, and retain the most pertinent information."))
        ),
        p(
          "This also means that if you miss class, you should rely on reciprocal collaboration with your classmates, if they are willing to share their notes with you. This is also an important skill for learning how to work on teams and collaborate with colleagues for long-term success.",
          span(strong("If you do miss class, please, do not ask me to provide a comprehensive written or oral record of what you missed.")),
          "I do not give repeat lectures and cannot prepare individualized lesson plans for each member of the class."
          )
        )
      )
 result <- content_card(
    title_text    = "Engagement & Record-Keeping",
    subtitle_text = "You will only succeed in this class if you take full responsibility for your processing and retentionof the content.",
    body          = body,
    footer_text   = "Work directly with Accessibility Services if legitimate barriers arise, and then I will arrange offical make-up options for you.",
    card_class    = "success",
    heading       = "Expectations",
    icon_name     = "book-open-reader"
  )
 return(result)
}

other_notices <- function() {
  body <- div(
    h3("Fair Use Policy"),
    p("Copying or recording synchronous classes and asynchronous course materials without the express prior approval of Dr. Rich is prohibited. All copies and recordings remain the property of UNO and Dr. Rich. UNO and Dr. Rich reserve the right to retrieve, inspect, or destroy them after their intended use. These policies are not intended to affect the rights of students with disabilities under applicable law or UNO policies.")
  )
  result <- content_card(
    title_text    = "Right of Revision",
    subtitle_text = "The instructor reserves the right to revise or adjust the course syllabus best to accommodate the pace and needs of the students.",
    body          = tagList(body),
    footer_text   = "Please contact Dr. Rich if you would like a text-file version of this syllabus.",
    card_class    = "secondary",
    heading       = "Policies",
    icon_name     = "certificate"
  )
  return(result)
}

plagiarism <- function() {
  body <- div(
    p("I will assume that for this course, you will adhere to the University of Nebraska at Omaha policies and maintain the highest academic integrity standards. In other words, don't cheat by giving answers to others or taking them from anyone else. I will also adhere to the highest standards of academic integrity, so please do not ask me to change (or expect me to change) your grade illegitimately or to bend or break the rules for one person that will not apply to everyone.")
  )
  result <- content_card(
    title_text    = "Academic Integrity",
    subtitle_text = span("Plagiarism and cheating of any kind on an examination, quiz, or assignment will result at least in an 'F' for that assignment.",
    span(
      class = "card-subtitle text-muted",
      "(and may, depending on the severity of the case, lead to an 'F' for the entire course) and may be subject to appropriate referral to the Office of Academic and Student Affairs for further action."
    )),
    body          = tagList(body),
    footer_text   = span(
      "Advances in AI continue to necessitate new boundaries and policies in our definition of authenticity and originality. If you are ever unsure where the line between plagiarism and original work is, ",
      span(tags$strong("it is always better to ask your instructor and not to guess."))),
    card_class    = "danger",
    heading       = "Policies",
    icon_name     = "certificate"
  )
  return(result)

}







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

accordion_do_dont <- function(x, idx) {
    accordion_panel(
      idx,
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
      class = "card bg-light mb-3",
      card_header(h6("The following three values form the foundation of my classroom culture and expectations:")),
      accordion(
        imap(value_list, \(x, idx) accordion_do_dont(x, idx)),
        open = FALSE
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

accommodations <- function() {
  body <- div(
    p(
      "The University of Nebraska at Omaha is committed to providing reasonable accommodations for all persons with disabilities. This syllabus is available in alternate formats upon request. Reasonable accommodations are offered to students who are registered with the ",
      span(tags$a(href = "https://www.unomaha.edu/student-life/accessibility/student-services.php", "Accessibility Services Center (ASC)."))
    ),
    div(
      style = "padding:10px",
      ul_group(
        layout    = "grid",
        item_list = list(
          "For more information: " = card_image(
            here("images/button_asc.png"),
            width = "200px",
            href = "https://www.unomaha.edu/student-life/accessibility/index.php"
          ),
          "Location: " = "104 H&K",
          "Phone: "    = "402.554.2872",
          "Email: "    = span(tags$a(href = "mailto:unoaccessibility@unomaha.edu", "unoaccessibility@unomaha.edu"))
        ),
        align   = "start",
        justify = "evenly"
      )
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

instructor_card <- function() {
  body <- ul_group(
        layout    = "grid",
        item_list = list(
          "Please call me:"   = "Dr. Rich or Professor Rich (she/her)",
          "Find me in:"       = "Allwine Hall 413",
          "Office Hours:"     = "By appointment Thur 12-2 or Fri 1-3",
          "Remote Meetings:"  = tags$a(href = "https://zoom.us/launch/chat?src=direct_chat_link&email=aliciarich%40unomaha.edu", "Video or direct message via Zoom"),
          "Contact me via:"   = tags$a(href = "mailto:aliciarich@unomaha.edu", "aliciarich@unomaha.edu")
        )
      )
  content_card(
    title_text    = "Alicia M. Rich, Ph.D.",
    subtitle_text = "Assistant Professor of Biology & Environmental Science",
    body          = tagList(body),
    footer_text   = div(
      p("I will be reachable via Zoom during office hours, or you may email me at least 1 business day in advance to arrange an in-person meeting."),
      p(
        span(tags$strong("Please use email and not canvas messages for communication.")),
        "You can expect a response within 2 business days, ",
        span(tags$strong("but I do not monitor my work email during evenings or on the weekends."))
      )
    ),
    card_class    = "info",
    heading       = "Course Instructor",
    icon_name     = "user",
    image         = card_image(file = here("images/headshot_rich.png"), width = "200px")
  )
}

exam_format <- function(course) {
  body <- div(
    ul_group(
      list_lead = "General Exam Format:",
      layout    = "grid",
      item_list = list(
        "Multiple Choice" = "50%",
        "True/False"      = "10%",
        "Short Essay*"    = "40%"
      )
    ),
    p(
      "You are soley responsible for exam preparation and compilation of materials. That means you should not only attend class every day, but ",
      span(tags$strong("also take down clear and organized notes ")),
      "for your exams. I may or may not provide access to lecture slides, ",
      span(tags$strong("so you should rely entirely on my slides as your notes."))
    )
  )

  display <- content_card(
    title_text    = paste0("You will take ", length(pluck(assessment, course, "exams")), " synchronous, written exams this semester."),
    subtitle_text = span(
      "You may reference any materials you wish during the exam (e.g., notes, text), but ",
      span(tags$strong("electronic devices are not permitted. ")),
      "That means you should plan ahead to bring a hard-copy version of any materials you plan to use."
    ),
    body          = tagList(body),
    footer_text   = "*In most cases, you will answer ~2-3 total short essay questions, but I provide a choice of at least two prompts for each.",
    card_class    = "info",
    heading       = "Exam Details",
    icon_name     = "pen-to-square"
  )
  return(display)
}

missed_exams <- function() {

  content_card(
    title_text    = "Exam Policy",
    subtitle_text = "All course exams follow a traditional, in-person, synchronous format.",
    body          = p(
        "Most of the course schedule may change significantly as we progress. As a result, the content covered in each exam may change accordingly, but ",
        span(tags$strong("I will keep to the exam dates and times that I set at the beginning of the semester.")),
      ),
    footer_text   = "You should mark those dates and times on your calendar now, because I will only offer makeup opportunities under extenuating circumstances that the Accessibility Services Center communicates directly to me.",
    card_class    = "danger",
    heading       = "Note",
    icon_name     = "circle-exclamation"
  )
}

titleix <- function() {
  content_card(
    title_text    = "Title IX Sexual Misconduct",
    subtitle_text = "As your instructor, one of my responsibilities is to help create a safe learning environment on our campus.",
    body          = div(
      p("Title IX and our Sexual Misconduct policy prohibit sexual misconduct.  If you have experienced sexual misconduct or know someone who has, the University can help."),
      p(
        tags$a(
          href = "https://www.unomaha.edu/university-compliance/civil-rights/title-ix-information/index.php",
          "I encourage you to visit the Title IX website to learn more."
          ),
        tags$a(
          href = "https://www.unomaha.edu/student-life/wellness/counseling-and-psychological-services/index.php",
          "If you seek help and want to speak to someone confidentially, you can contact the Counseling and Psychological Services (CAPS)."
          )
        )
    ),
    footer_text   = "It is also crucial that you know that federal regulations and University policy require me to promptly convey any information about potential sexual misconduct known to me to UNO’s Title IX Coordinator. In that event, they will work with a few others on campus to ensure appropriate measures are taken, and resources are available to the student who may have been harmed.  Protecting a student’s privacy is of utmost concern, and all involved will only share information with those who need to know to ensure the University can respond and assist.",
    card_class    = "warning",
    heading       = "University Policies",
    icon_name     = "landmark-flag"
  )
}

inclusion_office <- function() {
  content_card(
    title_text    = "Office of Student Leadership, Involvement, and Inclusion",
    subtitle_text = "The university's former Gender and Sexuality Resource Center has been reorganized to form a more intersectional program.",
    body          = "This office is meant as a space for students to learn from each other, build relationships, and foster an environment of understanding and respect. If you are interested in contributing to or benefiting from their work to make UNO a more inclusive environment or you find yourself in need of support and resources, I recommend you start with a visit to their office.",
    footer_text   = tags$a(
      href = "https://www.unomaha.edu/office-of-student-leadership-involvement-and-inclusion/",
      "You can find them in rooms 112 and 113 of the Milo Bail Student Center or online."
      ),
    card_class    = "info",
    heading       = "University Resources",
    icon_name     = "handshake"
  )
}

prep_quizzes <- function() {
  content_card(
    title_text    = "Preparation Quizzes",
    subtitle_text = "We will begin some of our classes with a short assessment of your preparation for that week's materials.",
    body          = div(
      p("These quizzes will only serve as an incentive to ensure widespread accountability for engaging with our discussions and keeping up with the assigned background material. I will ask 1-2 questions that require you to provide a surface-level reflection on some component of the assigned material in no more than 1-3 sentences."),
      accordion(
        accordion_panel(
          "Quiz Rubric",
          quiz_rubric()
        ),
        open = FALSE
      )
    ),
    footer_text   = "Please arrive to each class on time, having completed the assigned background material for that week and brought a writing utencil. If you arrrive after a quiz is over, I will not interrupt the class to give you the quiz, and I will not let you take the quiz after recieving additional information that the others did not access before taking theirs.",
    card_class    = "info",
    heading       = "Assessment",
    icon_name     = "pen-to-square"
  )
}

activities <- function() {
  content_card(
    title_text    = "In-Class Activities",
    subtitle_text = "Some classes may include group-based exercises or other graded work.",
    body          = "You will submit your work at the end of class for completion credit. I may or may not give advance notice for graded, in-class activities.",
    footer_text   = "You will not have any makeup opportunities for missed work, but I will drop at least one of your lowest scores.",
    card_class    = "info",
    heading       = "Assessment",
    icon_name     = "pen-to-square"
  )
}

late_work <- function() {
  content_card(
    title_text    = "Late Submissions",
    subtitle_text = "While I prefer to implement as flexible a deadline policy as possible, I also have to maintain reasonable processing times for assessing every student's submissions and submitting grades.",
    body          = p(span(tags$strong("To incentivize timely submissions, I will deduct 10% from the final grade on any assignment submitted after the deadline.")), "I will not accept any work from the first half of the semester submitted after our Midterm Exam, and I will not accept any work from the second half of the semester submitted after 5:00 PM on the last Friday of classes (i.e., the Friday before final exams begin). Unless Accessibility Services reaches out to discuss extremely extenuating circumstances with me, I will not make any exceptions to grade your work after the Final Exam."),
    footer_text   = "If you have arranged formal accommodation requests through Accessibility Services for your assignment deadlines (see above), then I will be happy to honor those to my full ability. Please do not hesitate to remind me if that is the case and I inadvertently apply any late penalties to a grade.",
    card_class    = "secondary",
    heading       = "Assessment",
    icon_name     = "pen-to-square"

  )
}


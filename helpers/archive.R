attendance <- function() {
  withTags(
    card(
      class = "card border-warning mb-3",
      card_header(
        class = "card-header text-white bg-warning",
        span(icon("circle-exclamation", class = "fa-solid")),
        span(strong("Important Note"))
      ),
      card_body(
        h5(
          class = "card-title text-warning-emphasis mb-0",
          style = "text-decoration:underline",
          "This is not an online or hybrid course."
        ),
        span(class = "text-tertiary mb-0",
             "I designed the curriculum and objectives specifically with that in mind, so to succeed you must attend and engage with every scheduled meeting."
        ),
        strong(
          class = "text-warning-emphasis",
          style = "text-decoration:underline",
          "You will not pass this class if you are unable to attend and take your own notes."
        ),
        p(
          class = "text-body",
          "Absences may only be truly excused if they have been registered through ",
          span(a(href = "https://www.unomaha.edu/student-life/accessibility/student-services.php", " Student Services."))
        ),
        p(
          class = "text-body",
          "Please do not provide me with any personal or medical details to explain your barriers to attendance or assignment completion. While I do care about accommodating these issues when they arise,",
          span(b("I also care about your privacy and respect that office's expertise in managing the conditions and complications that are outside of my purview."))
        ),
        p(
          class = "text-body",
          "I do my best to integrate a reasonable and universal degree of flexibility into our schedule and assessment plan to accommodate some work/life balance while also ensuring equity and reasonable progress in your learning. I am happy to receive requests and recommendations to help me update and improve this approach for everyone, but I will only create personalized accommodations by working directly with the ASC."
        )
      ),
      card_footer(
        class = "text-body-secondary",
        "Please also know that I genuinely appreciate opportunities to work with and around student barriers by collaborating with the ASC. If you know that you are coming into the semester with learning barriers (e.g., neurodivergence, visual/auditory processing differences), I strongly encourage you to register this with their office and then communicate with me about the ways I can work this into my approach from the beginning."
      )
    )
  )
}

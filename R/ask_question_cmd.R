#' Ask a question using the command-line.
#'
#' @param question Question.
#' @export
ask_question_cmd <- function(question) {
  cat(question$background)
  cat("\n")
  cat(question$prompt)
  cat("\n")

  answer <- readline2("RESPONSE - ")

  answer_texts <- sapply(
    question$answer,
    FUN = function(x) x$text)

  answer_feedbacks <- sapply(
    question$answer,
    FUN = function(x) x$feedback)

  answer_marks <- sapply(
    question$answer,
    FUN = function(x) x$mark)
  answer_texts

  match <- which(
    sapply(
      answer_texts,
      match_answer,
      response = answer))

  if(length(match) == 0) {
    return(
      list(
        input = answer,
        feedback = "Incorrect",
        mark = 0,
        correct_answers = question$answer[which(answer_marks == 1)]
      )
    )
  } else {
    match <- match[[1]]
    return(
      list(
        input = answer,
        feedback = answer_feedbacks[[match]],
        mark = answer_marks[[match]],
        correct_answers = question$answer[which(answer_marks == 1)]
      )
    )
  }
}


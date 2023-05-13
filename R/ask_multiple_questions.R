#' Ask multiple questions
#' @param question_templates Set of questions
#' @param n_questions Number of questions to ask
#' @param replace If replace, then same question can come up multiple times.
#' @param random If random, then questions are chosen at random.
#' @export
ask_multiple_questions_cmd <- function(
  question_templates,
  random = FALSE,
  n_questions = 1,
  replace = FALSE
) {

answers <- list()
# Non-random
if (!random) {
  for (i in seq_along(question_templates)) {
    question <- bake_question(
      question_templates[[i]]
    )
    result <- ask_question_cmd(question)
    cat(result$feedback)
    cat("\n")
    if (result$feedback == "Incorrect") {
      cat("Correct Answer:\n")
      cat(format_answer_text(result$correct_answers[[1]]$text))
      cat("\n")
      cat("\n")
    }
    answers[[i]] <- result 
  }
  return(answers)
}

if (n_questions > length(question_templates)) {
  stop("Not enough questions in list.")
}
question_ids <- sapply(question_templates, function(x){x[["id"]]})
previous_qs <- c()
i <- 1
while (i <= (n_questions)) {
subset <- subset_templates_by_id(
  question_templates,
  ids = question_ids)

sample_id <- sample(seq_along(subset), 1)
template <- subset[[sample_id]]
question <- bake_question(template)
if (!(template$id %in% previous_qs)) {
result <- ask_question_cmd(question)
cat(result$feedback)
cat("\n")
answers[[i]] <- result
i <- i + 1
previous_qs <- c(previous_qs, template$id)
}
}
answers
}

#' Summarise the results of `ask_multiple_questions_cmd`
#' @param answers Results
#' @export
summarise_question_results <- function(answers) {
inputs <- sapply(answers, function(x){x[["input"]]})
feedback <- sapply(answers, function(x){x[["feedback"]]})
correct_answer <- sapply(answers, function(x){x[["correct_answers"]][[1]][["text"]]})

answer_summary <- data.table::data.table(
  question = seq_along(answers),
  inputs,
  feedback,
  correct_answer)
  answer_summary
}
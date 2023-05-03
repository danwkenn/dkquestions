#' Ask multiple questions
#' @param question_templates Set of questions
#' @param n_questions Number of questions to ask
#' @export
ask_multiple_questions_cmd <- function(
  question_templates,
  n_questions = 1
) {

if (n_questions > length(question_templates)) {
  stop("Not enough questions in list.")
}
question_ids <- sapply(question_templates, function(x){x[["id"]]})
answers <- list()
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
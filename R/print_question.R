#' Print question
#' @param question a question
#' @export
print_question <- function(question) {
  cat(question$background)
  cat("\n")
  cat(question$prompt)
  cat("\n")
  cat("ANSWERS:\n")
  for (i in seq_along(question$answer)) {
    cat(paste0(
      " ", i,
      ". ",
    format_answer_text(question$answer[[i]]$text),
     " (",
     question$answer[[i]]$mark,
     "): ",
     question$answer[[i]]$feedback,
     "\n"))
  }
}

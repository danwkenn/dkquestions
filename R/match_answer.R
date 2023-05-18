#' Determines if a given response matches an answer text.
#' @param answer_text Text for a given answer.
#' @param response user response
match_answer <- function(
  answer_text,
  response
) {

  if (grepl(pattern = "^GREPL",answer_text)) {
    pattern = sub(
      "^GREPL(.*)$",
      "\\1",
      answer_text
    )

    pattern <- gsub(
      pattern = "<<<",
      "{",
      pattern
    )

    pattern <- gsub(
      pattern = ">>>",
      "}",
      pattern
    )
    return(grepl(
      pattern = pattern,
      x = response
    ))
  } else {
    return(answer_text == response)
  }

}
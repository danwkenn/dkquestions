#' Remove grepl details for answer text.
#' @param x answer text
prepare_answer_text <- function(x) {
x <- sub("^GREPL", "", x)
x <- gsub("\\(([^\\|]*)\\|.*\\)", "\\1", x)
x <- sub("^\\^", "", x)
x <- sub("\\$$", "", x)
x
}

#' Print answers to question
#' @param question A question
print_answers_as_md <- function(question) {
n_answers <- length(question$answer)
for (i in seq_len(n_answers)) {
    cat(paste0(
        "**ANSWER ",
        i,"**",
        paste0(" (Mark: ", scales::percent(question$answer[[i]]$mark),")"),
        ":\n\n"))
    cat(prepare_answer_text(question$answer[[i]]$text))
    cat("\n\n")
    cat("Feedback: ", question$answer[[i]]$feedback)
    cat("\n\n")
}
}

#' Print questions as a list of markdown questions
#' @param questions Question set
#' @export
print_questions_as_md <- function(questions) {
    
    n_questions <- length(questions)

    for (i in seq_len(n_questions)) {

        cat(paste0(
            i,
            ". ",
            questions[[i]]$background,
            "\n\n",
            questions[[i]]$prompt,
            "\n\n",
            '<input type="text" id="fname" name="fname"><br><br>',
            "\n\n"
        ))
        cat("<details>\n")
        print_answers_as_md(questions[[i]])
        cat("\n</details>")
    }
}
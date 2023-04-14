#' Load questions from a YAML file.
#' @param file YAML file
#' @export
load_templates <- function(file) {

    # Read in the YAML
    question_templates <- yaml::read_yaml(file)
    question_templates
}

#' Create questions
#' @param question_templates Set of templates
#' @param n_questions Number of questions to ask
#' @param duplicates Allow double-ups?
#' @export 
create_questions <- function(
    question_templates,
    n_questions = 10,
    duplicates = TRUE) {

    if (!duplicates && length(question_templates$templates) < n_questions) {
        stop("Not enough templates for n_questions")
    }

    ids <- sapply(question_templates$templates, function(x) x[["id"]])

    sample_ids <- sample(ids,n_questions,replace = duplicates)

    questions <- list()

    for (i in seq_len(n_questions)) {

        index <- which(ids == sample_ids[[i]])

        template <- question_templates$templates[[index]]
        # Source preprocessing
        if (length(template$preprocessing) > 0) {
            lapply(template$preprocessing, source)
        }

        # Replace datasets.
        for (j in seq_along(template$datasets)) {
            if (grepl("^FUNCTION", template$datasets[[j]])) {
                function_name <- sub("^FUNCTION\\s*(.*)$", "\\1", template$datasets[[j]])
                template$datasets[[j]] <- do.call(function_name, args = list())
            }
        }
        questions[[i]] <- create_question(template)
    }
    questions
}

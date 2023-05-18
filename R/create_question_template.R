#' Generate a random string.
#' @param length Number of characters
#' @param character_set Vector of characters
generate_random_string <- function(
  length = 20,
  character_set = c(LETTERS, letters, 0:9)) {

  paste0(
    sample(
      character_set,
      size = length,
      replace = FALSE),
    collapse = "")
}

#' Create a template
#' @param context Context for the conversation 
#' @param background context for question
#' @param prompt instruction to answer the question
#' @param preprocessing_script preprocessing script.
#' @param datasets The datasets.
#' @export
create_question_template <- function(
    context,
    background,
    prompt,
    preprocessing_script = NULL,
    datasets = NULL
){

    id <- generate_random_string()

    list(
        id = id,
        context = context,
        background = background,
        prompt = prompt,
        preprocessing = preprocessing_script,
        slots = list(),
        answer = list()
    )
}

#' Add a slot to a question template
#' @param question_template A question template created with `create_question_template`
#' @param name Name the slot will take
#' @param dataset The dataset
#' @param id The id of the row
#' @param field The field of the value
#' @export 
add_slot <- function(
    question_template,
    name,
    dataset,
    id,
    field
) {

    slot <- list(
        dataset = dataset,
        id = id,
        field = field
    )

    question_template$slots <- append(
        question_template$slots,
        setNames(list(slot), name)
    )

    question_template
}


#' Add a slot to a question template
#' @param question_template A question template created with `create_question_template`
#' @param text the response
#' @param feedback feedback to give if this response was given
#' @param mark Value between 0 and 1.
#' @export
add_answer <- function(
    question_template,
    text,
    feedback,
    mark
) {

    answer <- list(
        text = text,
        feedback = feedback,
        mark = mark
    )

    question_template$answer <- append(
        question_template$answer,
        list(answer)
    )

    question_template
}

#' Add a dataset resource to a question.
#' @param question_template template for a question.
#' @param name name the dataset will take within the question.
#' @param type type of dataset call (path or function)
#' @param value the call.
#' @export
add_dataset <- function(
    question_template,
    name,
    type,
    value) {

    if (is.null(question_template$datasets)) {
        question_template$datasets <- list()
    }

    if (type == "path") {
        question_template$datasets[[name]] <- 
            paste0(
                value
            )
    } else if (type == "function") {
        question_template$datasets[[name]] <- 
            paste0(
                "FUNCTION ",
                value
            )
    } else if (type == "expression") {
        question_template$datasets[[name]] <- 
            paste0(
                "EXPRESSION ",
                value
            )
    }
    question_template
}

#' Add a preprocessing script to be run for the question.
#' @param question_template a template for a question.
#' @param path path to script
#' @export 
add_preprocessing_script <- function(
    question_template,
    path) {

    if (is.null(question_template$preprocessing)) {
        question_template$preprocessing <- c()
    }

    question_template$preprocessing <- c(
        question_template$preprocessing,
        path
    )

    question_template
}

#' Add Additional Details
#' @param question_template
#' @param detail_name
#' @param value
#' @export
add_additional_detail <- function(
    question_template,
    detail_name,
    value) {
    if (is.null(question_template$additional_details)) {
        question_template$additional_details <- list()
    }

    question_template$additional_details <- c(
        question_template$additional_details,
        setNames(value, detail_name)
    )

    question_template
}

#' Get Additional Details
#' @param question_templates
#' @param detail_name
#' @export
get_additional_details <- function(
    question_templates,
    detail_name) {

    extract_fun <- function(x) {
        detail <- x[["additional_details"]][[detail_name]]
        id <- x[["id"]]
        if (is.null(detail)) {
            return(setNames(NA_character_, id))
        } else {
            return(setNames(detail, id))
        }
    }

    sapply(question_templates, extract_fun)
}
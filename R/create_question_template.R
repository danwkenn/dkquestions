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
#' 
#' @param background context for question
#' @param prompt instruction to answer the question
#' @param preprocessing_script preprocessing script.
#' @param datasets The datasets.
#' @export
create_question_template <- function(
    background,
    prompt,
    preprocessing_script = NULL,
    datasets = NULL
){

    id <- generate_random_string()

    list(
        id = id,
        background = background,
        prompt = prompt,
        preprocessing = preprocessing_script,
        slots = list(),
        answer = list()
    )
}

#' Add a slot to a question template
#' @param question_template A question template created with `create_question_template`
#' @param dataset The dataset
#' @param id The id of the row
#' @param field The field of the value
#' @export 
add_slot <- function(
    question_template,
    dataset,
    id,
    field
) {

    slot = list(
        dataset = dataset,
        id = id,
        field = field
    )

    question_template$slots <- append(
        question_template$slots,
        list(slot)
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

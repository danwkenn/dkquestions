
#' Internal function for single sampling.
#'
#' Basic.
#' @param x vector to sample from.
sample_from <- function(x) {
  x[[
    sample.int(
        length(x),
        1)
    ]]
}

#' Internal function for retrieving the value for a slot.
#'
#' @param data data-frame
#' @param sample_id Ranodmly selected ID.
#' @param slot Slot.
get_value <- function(data, sample_id, slot) {
  if (length(slot$field) == 1) {
   return(data[data$id == sample_id,][[slot[["field"]]]])
  } else {
    for (j in seq_along(slot$field)) {
      value <- data[data$id == sample_id,][[slot[["field"]][[j]]]]
      if (value != "") {
        return(value)
      }
    }
  }
}

#' Create a question from a template.
#' 
#' @param template A question template.
#' @import data.table
#' @export
create_question <- function(template) {
slots <- list()

for (i in seq_along(template$slots)) {

  # Extract data-set.
  dataset_name <-  template$slots[[i]]$dataset
  data <- data.table::fread(template$datasets[[dataset_name]])
  slot_name <- names(template$slots)[[i]]

  if (is.numeric(template$slots[[i]]$id)) {
    sample_id <- sample_from(template$slots[[i]]$id)
  } else if (grepl(template$slots[[i]]$id, pattern = "^NOT\\s")) {
   not_slot <- sub("^NOT\\s+(.*)$", "\\1", template$slots[[i]]$id)
   not_id <- slots[[paste0(not_slot, "_ID")]]
   remaining_ids <- setdiff(template$slots[[not_slot]]$id, not_id)
   sample_id <- sample_from(remaining_ids)
   } else if (grepl(template$slots[[i]]$id, pattern = "^DATATABLE\\s")) {
   expression <- sub("^DATATABLE\\s+(.*)$", "\\1", template$slots[[i]]$id)
   expression <- parse(text = expression)
   remaining_ids <- eval(expression)
   sample_id <- sample_from(remaining_ids)
   } else {
    sample_id <- slots[[paste0(template$slots[[i]]$id, "_ID")]] 
  }

  value <- get_value(
    data,
    sample_id,
    slot = template$slots[[i]])
  slots[[paste0(slot_name,"_ID")]] <- sample_id
  slots[[slot_name]] <- value

}
background <- with(
  slots,
  glue::glue(template$background))

prompt <- with(
  slots,
  glue::glue(template$prompt))

answer <- template$answer
for (i in seq_along(template$answer)) {
answer[[i]]$text <- with(
  slots,
  glue::glue(template$answer[[i]]$text))
answer[[i]]$feedback <- with(
  slots,
  glue::glue(template$answer[[i]]$feedback))
}

question <- list(
  background = background,
  prompt = prompt,
  answer = answer
)

question
}

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


#' Ask a question using the command-line.
#'
#' @param question Question.
#' @export
ask_question_cmd <- function(question) {
  cat(question$background)
  cat("\n")
  cat(question$prompt)
  cat("\n")

  answer <- readline("RESPONSE - ")

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

#' Subset by ID
#' @param template_list a list of question templates
#' @param ids Ids of the templates to be subsetted.
#' @export
subset_templates_by_id <- function(
  template_list,
  ids) {

  # Extract IDs
  template_ids <- sapply(template_list, function(x) x[["id"]])
  # Find items which match
  matches <- which(template_ids %in% ids)
  # Return the subset
  template_list[matches]
}


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
  if (is.character(template$datasets[[dataset_name]])) {
  data <- data.table::fread(template$datasets[[dataset_name]])
  } else if (data.table::is.data.table(template$datasets[[dataset_name]])) {
    data <- template$datasets[[dataset_name]]
  }
  slot_name <- names(template$slots)[[i]]

  if (is.numeric(template$slots[[i]]$id)) {
    sample_id <- sample_from(template$slots[[i]]$id)
  } else if (grepl(template$slots[[i]]$id, pattern = "^NOT\\s")) {
   not_slot <- sub("^NOT\\s+(.*)$", "\\1", template$slots[[i]]$id)
   not_id <- slots[[paste0(not_slot, "_ID")]]
   remaining_ids <- setdiff(template$slots[[not_slot]]$id, not_id)
   sample_id <- sample_from(remaining_ids)
  } else if (grepl(template$slots[[i]]$id, pattern = "^EXPRESSION\\s")) {
   expression <- sub("^EXPRESSION\\s+(.*)$", "\\1", template$slots[[i]]$id)
   expression <- parse(text = expression)
   ids <- eval(expression)
   sample_id <- sample_from(ids)
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

if (!is.null(template$context)) {
  template$background <- paste0(
    template$context, " ",
    template$background
  )
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
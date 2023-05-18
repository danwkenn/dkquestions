
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

#' Subset by ID
#' @param template_list a list of question templates
#' @param ids Ids of the templates to be subsetted.
#' @export
match_templates_by_id <- function(
  template_list,
  ids) {

  # Extract IDs
  template_ids <- sapply(template_list, function(x) x[["id"]])
  # Find items which match
  matches <- match(ids, template_ids)
  # Return the subset
  template_list[matches]
}

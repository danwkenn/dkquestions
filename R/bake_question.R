#' Bake the question
#' @param template question template
#' @export
bake_question <- function(
  template
){

  # Source preprocessing
  if (length(template$preprocessing) > 0) {
      lapply(template$preprocessing, source)
  }

  # Replace datasets.
  for (j in seq_along(template$datasets)) {
      if (grepl("^FUNCTION", template$datasets[[j]])) {
          function_name <- sub("^FUNCTION\\s*(.*)$", "\\1", template$datasets[[j]])
          template$datasets[[j]] <- do.call(function_name, args = list())
      } else if (grepl("^EXPRESSION", template$datasets[[j]])) {
          expression <- sub("^EXPRESSION\\s*(.*)$", "\\1", template$datasets[[j]])
          template$datasets[[j]] <- eval(parse(text = expression))
      }
  }
  create_question(template)
}
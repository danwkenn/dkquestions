replace_parentheses <- function(x) {
  # Regular expression pattern to match (a|b) style parentheses
  pattern <- "\\((.*?)\\|.*?\\)"

  # Replace the matches with the first option in the parentheses
  replaced_text <- gsub(pattern, "\\1", x)

  return(replaced_text)
}

unescape_regex <- function(x) {
  # List of characters to unescape
  unescape_chars <- c("\\\\", "\\^", "\\$", "\\.", "\\|", "\\?", "\\*", "\\+", "\\(", "\\)", "\\[", "\\]", "\\{", "\\}")

  # Replace the escaped characters with their unescaped counterparts
  for (char in unescape_chars) {
    x <- gsub(char, substr(char, 2,2), x)
  }

  return(x)
}

format_answer_text <- function(x) {
    
unescape_regex(replace_parentheses(x))

}

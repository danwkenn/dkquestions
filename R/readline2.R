readline2 <- function(prompt = "") {
  if (interactive()){
    readline(prompt)
  } else {
    #  non-interactive
    cat(prompt)
    readLines("stdin",n = 1)
  }
}
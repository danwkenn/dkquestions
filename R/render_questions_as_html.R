#' Render questions as some HTML
#' @param questions Questions to be rendered.
#' @param output file path to output result to.
#' @export
render_questions_as_html <- function(
    questions,
    output = tempfile(fileext = ".html")) {
    
    template_file <- system.file(
        package= "dkquestions",
        "question_set_template.Rmd")
    rmarkdown::render(
    input = template_file,
    params = list(questions = questions),
    output_file = output)
    print(output)
}

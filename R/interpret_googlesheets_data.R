#' Function converting Google Sheet data into questions.
#' 
#' @param question_data The question data from a row of the google-sheets table.
#' @export
interpret_googlesheets_data <- function(question_data) {
question_template <- list()
id <- generate_random_string()

question_template[["id"]] <- id
question_template[["context"]] <- question_data$Context
question_template[["background"]] <- question_data$Background
question_template[["prompt"]] <- question_data$Prompt
answer <- list()
answer[[1]] <- list()
answer[[1]][["text"]] <- question_data[["ANSWER 1"]]
answer[[1]][["feedback"]] <- question_data[["ANSWER 1 Feedback"]]
answer[[1]][["mark"]] <- question_data[["ANSWER 1 Mark"]] / 10

if (!is.na(question_data$`ANSWER 2`)) {
  answer[[2]] <- list()
  answer[[2]][["text"]] <- question_data[["ANSWER 2"]]
  answer[[2]][["feedback"]] <- question_data[["ANSWER 2 Feedback"]]
  answer[[2]][["mark"]] <- question_data[["ANSWER 2 Mark"]] / 10
}

if (!is.na(question_data$`ANSWER 3`)) {
  answer[[3]] <- list()
  answer[[3]][["text"]] <- question_data[["ANSWER 3"]]
  answer[[3]][["feedback"]] <- question_data[["ANSWER 3 Feedback"]]
  answer[[3]][["mark"]] <- question_data[["ANSWER 3 Mark"]] / 10
}

slots <- list()
if (!is.na(question_data$`Slot 1 - Name`)) {
  slots[[question_data$`Slot 1 - Name`]] <- list()
  slots[[question_data$`Slot 1 - Name`]][["dataset"]] <- 
    question_data$`Slot 1 - Dataset`
  slots[[question_data$`Slot 1 - Name`]][["field"]] <- 
    question_data$`Slot 1 - Field`
  slots[[question_data$`Slot 1 - Name`]][["id"]] <- 
    process_ids_from_string(question_data$`Slot 1 - ID`[[1]])
}

if (!is.na(question_data$`Slot 2 - Name`)) {
  slots[[question_data$`Slot 2 - Name`]] <- list()
  slots[[question_data$`Slot 2 - Name`]][["dataset"]] <- 
    question_data$`Slot 2 - Dataset`
  slots[[question_data$`Slot 2 - Name`]][["field"]] <- 
    question_data$`Slot 2 - Field`
  slots[[question_data$`Slot 2 - Name`]][["id"]] <- 
    process_ids_from_string(question_data$`Slot 2 - ID`[[1]])
}

if (!is.na(question_data$`Slot 3 - Name`)) {
  slots[[question_data$`Slot 3 - Name`]] <- list()
  slots[[question_data$`Slot 3 - Name`]][["dataset"]] <- 
    question_data$`Slot 3 - Dataset`
  slots[[question_data$`Slot 3 - Name`]][["field"]] <- 
    question_data$`Slot 3 - Field`
  slots[[question_data$`Slot 3 - Name`]][["id"]] <- 
    process_ids_from_string(question_data$`Slot 3 - ID`[[1]])
}

datasets <- list()
if (!is.na(question_data$`Dataset 1 - Name`)) {
  datasets[[question_data$`Dataset 1 - Name`]] <- 
    question_data$`Dataset 1 - Path`
}
if (!is.na(question_data$`Dataset 2 - Name`)) {
  datasets[[question_data$`Dataset 2 - Name`]] <- 
    question_data$`Dataset 2 - Path`
}

question_template[["answer"]] <- answer
question_template[["slots"]] <- slots
question_template[["datasets"]] <- datasets

preprocessing <- list()
if (!is.na(question_data$`Preprocessing 1`)) {
  preprocessing[[1]] <- question_data$`Preprocessing 1`
}
if (!is.na(question_data$`Preprocessing 2`)) {
  preprocessing[[2]] <- question_data$`Preprocessing 2`
}

question_template <- add_additional_detail(
  question_template,
  detail_name = "timestamp",
  value = as.character(question_data$Timestamp))

question_template
}


#' Internal function for processing IDs.
#' @param x Ids as string.
process_ids_from_string <- function(x) {
  x <- as.character(x)
  if (grepl("^(\\d+\\,)*\\d+$", x)) {
    as.integer(strsplit(x, "\\,")[[1]])
  } else {
    x
  }
}

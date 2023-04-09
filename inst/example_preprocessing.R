temp <- function() {

temp_file <- tempfile()
library(data.table)

conjugation_types <- fread("./inst/spanish_verb_conjugation_types.csv")
verb_forms <- fread("./inst/spanish_verb_form_glossary.csv")
pronouns <- fread("./inst/spanish_pronoun_glossary.csv")
verb_glossary <- fread("./inst/spanish_verbs.csv")

verb_forms <- verb_forms
temp <- pronouns[
    verb_forms,
    on = c(
        "person",
        "plural",
        "formal"),
    allow.cartesian = TRUE]
setnames(temp, "id", "pronoun_id")
temp[, id := seq_len(.N)]

temp[
    conjugation_types,
    on = c("tense_id" = "id"),
    `:=`(
        tense = i.name,
        tense_english = i.name_english)]

temp[
    tense_english == "imperative affirmative",
    .(
        paste(word,value),
        paste(english_word,english_participle)
    )]
setnames(
    temp,
    c("value","english_participle"),
    c("verb_form","english_form")
)

temp[
    verb_glossary,
    on = c("verb_id" = "id"),
    infinitive := i.word
]

fwrite(
    file = temp_file,
    x = temp)

return(temp_file)
}

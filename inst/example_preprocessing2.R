
translate_number.internal <- function(number) {
  
  # Convert the number to Malay words using an ifelse statement
  if (number == 1) {
    malay_word <- "satu"
  } else if (number == 2) {
    malay_word <- "dua"
  } else if (number == 3) {
    malay_word <- "tiga"
  } else if (number == 4) {
    malay_word <- "empat"
  } else if (number == 5) {
    malay_word <- "lima"
  } else if (number == 6) {
    malay_word <- "enam"
  } else if (number == 7) {
    malay_word <- "tujuh"
  } else if (number == 8) {
    malay_word <- "lapan"
  } else if (number == 9) {
    malay_word <- "sembilan"
  } else if (number == 10) {
    malay_word <- "sepuluh"
  } else if (number == 11) {
    malay_word <- "sebelas"
  } else if (number <= 19) {
    malay_word <- paste(translate_number.internal(number - 10), "belas")
  } else if (number %% 10 == 0 && number < 100) {
    malay_word <- paste(translate_number.internal(floor(number / 10)), "puluh")
  } else if (number <= 99) {
    malay_word <- paste(translate_number.internal(floor(number / 10)), "puluh", translate_number.internal(number %% 10))
  } else if (number == 100) {
    malay_word <- "seratus"
  } else if (number < 1000 && number %% 100 == 0) {
    malay_word <- paste(translate_number.internal(floor(number / 100)), "ratus")
  } else if (number < 1000) {
    hundreds <- floor(number / 100) * 100
    malay_word <- paste(
      translate_number.internal(hundreds),
      translate_number.internal(number - hundreds)
    )
  } else if (number == 1000) {
    malay_word <- "seribu"
  } else if (number < 1e6 & number %% 1000 == 0) {
    malay_word <- paste(translate_number.internal(floor(number / 1000)), "ribu")
  } else if (number < 1e6) {
    thousands <- floor(number / 1000) * 1000
    malay_word <- paste(
      translate_number.internal(thousands),
      translate_number.internal(number - thousands)
    )
  }
  
  return(malay_word)
}

temp <- function(){
    temp_file <- tempfile()

    int <- sample(1:99, size = 1)
    temp <- data.table::data.table(
        id = 1,
        number = int,
        translation = translate_number.internal(int))
    data.table::fwrite(
        file = temp_file,
        x = temp)

    return(temp_file)
}

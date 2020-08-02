#' Takes a noun and makes it plural
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export


sing_day <- function(dataset, line, phrase_col){
  phrases <- dataset %>% pull({{phrase_col}})
  last_day <- phrases[1]
  day_number <- dataset$Day.in.Words[line]
  twelve_days_words <- c("and a", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve")
  glue("On the {day_number} day of Christmas, my true love sent to me,") %>%
    cat(sep = "\n")
  map2_chr(rev(twelve_days_words[2:line]), rev(phrases[2:line]),paste)%>%
    cat(sep = "\n")
  glue(twelve_days_words[1], phrases[1])
}

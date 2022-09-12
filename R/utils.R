#' Reformat formular data from excel sheet
#'
#' rename col according to first row then remove unnecessary rows at the
#' beginning and at the end according to the landmark hash ("#")
#' put before the first and after the last lines
#'
#' @param formular formular data
#' @param formular_name formular name
#' @return formatted formular
reshape_formular = function(formular, formular_name) {
  names(formular) = c("question", "answer_options", "type_question",
                      "nb_info_decimal_pre", "nb_info_decimal_post",
                      "nb_info_unit", "nb_info_min", "nb_info_max",
                      "is_field_required", "automated_checks", "notes")
  ind_beginning_table = which(stringr::str_detect(unlist(formular[,1]), "#"))
  if ( diff(ind_beginning_table) > 4 ) {
    formular = formular[(ind_beginning_table[1]+4):(ind_beginning_table[2]-1), ]
    formular$formular = formular_name
  } else {
    formular = NULL
  }

  return(formular)
}

#' Take a string and build redcap variable name
#'
#' Take a string (usage is that it is the text of a question in the paramter) and
#' clean name (remove major case and space), then take from 1 to 26 (to comply
#' with RCP rules in terms of variable naming)
#'
#' @param strg a string
#' @return a redcap variable name
create_redcap_var_name = function(strg){
  strg %>% janitor::make_clean_names() %>%
    stringr::str_sub(1,24)
}

#' Table to translate field type from param list to RCp dictionary
#'
#' Table with same column as a Redcap dictionary and a start to translate
#' 'the information from the excel parameter list into data dictionary information
#' (field types and some form of validation for dates, integer and date + time)
#' @return a dataframes
get_table_recoding_for_redcap = function() {

  # create
  colnames_tbl =
    c(
      "type_question",
      "Variable / Field Name","Form Name","Section Header","Field Type","Field Label",
      "Choices, Calculations, OR Slider Labels","Field Note","Text Validation Type OR Show Slider Number",
      "Text Validation Min","Text Validation Max","Identifier?","Branching Logic (Show field only if...)",
      "Required Field?","Custom Alignment","Question Number (surveys only)","Matrix Group Name",
      "Matrix Ranking?","Field Annotation"
    )

  tbl = dplyr::as_tibble(data.frame(matrix(data = NA_character_, nrow=0,ncol=length(colnames_tbl))))
  names(tbl) = colnames_tbl

  tbl %>%
    dplyr::add_row(
      type_question =
        c(
          "Single selection (Yes / No)",
          "Date",
          "Number",
          "Score",
          "Single selection",
          "Multiple Selection",
          "Date + Time",
          "Text"
        ),
      `Field Type` =
        c(
          "yesno",
          "text", # date
          "text", # integer
          "calc",
          "radio",
          "checkbox",
          "text",
          "text"
        ),
      `Text Validation Type OR Show Slider Number` =
        c(
          "",
          "date_dmy", # date
          "integer",# integer
          "",
          "",
          "",
          "datetime_dmy", # date + time
          ""
        )
    )
}

#' Remove acccent from a string
#'
#' @param txt a string
#' @return a string without any accent
remove_accent <- function(txt) {

  txt %>%
    stringr::str_replace_all('ä', 'a') %>%
    stringr::str_replace_all('Ä', 'A') %>%
    stringr::str_replace_all('ö', 'o') %>%
    stringr::str_replace_all('Ö', 'O') %>%
    stringr::str_replace_all('Ü', 'U') %>%
    stringr::str_replace_all('ü', 'u') %>%
    stringr::str_replace_all('á', 'a') %>%
    stringr::str_replace_all('Á', 'A') %>%
    stringr::str_replace_all('à', 'a') %>%
    stringr::str_replace_all('À', 'A') %>%
    stringr::str_replace_all('â', 'a') %>%
    stringr::str_replace_all('Â', 'A') %>%
    stringr::str_replace_all('ã', 'a') %>%
    stringr::str_replace_all('Ã', 'A') %>%
    stringr::str_replace_all('é', 'e') %>%
    stringr::str_replace_all('É', 'E') %>%
    stringr::str_replace_all('è', 'e') %>%
    stringr::str_replace_all('È', 'E') %>%
    stringr::str_replace_all('ê', 'e') %>%
    stringr::str_replace_all('Ê', 'E') %>%
    stringr::str_replace_all('í', 'i') %>%
    stringr::str_replace_all('Í', 'I') %>%
    stringr::str_replace_all('ì', 'i') %>%
    stringr::str_replace_all('Ì', 'I') %>%
    stringr::str_replace_all('õ', 'o') %>%
    stringr::str_replace_all('Õ', 'O') %>%
    stringr::str_replace_all('ó', 'o') %>%
    stringr::str_replace_all('Ó', 'O') %>%
    stringr::str_replace_all('ô', 'o') %>%
    stringr::str_replace_all('Ô', 'O') %>%
    stringr::str_replace_all('ò', 'o') %>%
    stringr::str_replace_all('Ò', 'O') %>%
    stringr::str_replace_all('ú', 'u') %>%
    stringr::str_replace_all('Ú', 'U') %>%
    stringr::str_replace_all('ç', 'c') %>%
    stringr::str_replace_all('Ç', 'C') %>%
    stringr::str_replace_all('ñ', 'n') %>%
    stringr::str_replace_all('Ñ', 'N') %>%
    stringr::str_replace_all("\'", " ") %>%
    stringr::str_trim()
}

#' Format choice options for redcap
#'
#' take choice option as formatted in the parameter list and return a string with
#' choice options formatted for redcap
#'
#' @param string_choice string choice form parameter list
#' @return formatted choice options
split_and_enumerate_choice_options = function(string_choice){
  ch = string_choice %>% stringr::str_split(";") %>% unlist
  string_choice_out = ""
  for (i in 1:length(ch)){
    string_choice_out = paste0(string_choice_out, i-1, ", ", ch[i], " | ")
  }
  string_choice_out = stringr::str_sub(string_choice_out, 1, nchar(string_choice_out)-3)

  return(string_choice_out)
}

#' Count number of choice option
#'
#' take choice option as formatted in the parameter and return number of choices
#'
#' @param string_choice string choice form parameter list
#' @return an integer
split_and_count_choice_options = function(string_choice){
  string_choice %>% stringr::str_split(";") %>% unlist %>% length
}


#' Filter only redcap column
#'
#'
#' @param dic dataframe
#' @return dataframe
filter_only_redcap_columns = function(dic) {
  dic %>% dplyr::select(!!!rlang::syms(
    c("Variable / Field Name","Form Name","Section Header","Field Type","Field Label",
    "Choices, Calculations, OR Slider Labels","Field Note","Text Validation Type OR Show Slider Number",
    "Text Validation Min","Text Validation Max","Identifier?","Branching Logic (Show field only if...)",
    "Required Field?","Custom Alignment","Question Number (surveys only)","Matrix Group Name","Matrix Ranking?","Field Annotation"
  )))
}

#' Search index string in data.frame
#'
#'
#' @param string a string
#' @param df a dataframe
#' @return vector of index
search_index_str_in_df = function(str, df) {
  c(
    arrayInd(
      which(df == str),
      dim(df)
      )
    )
}

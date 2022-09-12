#' Transform a parameter list into a REDCap project
#'
#' This function takes a CTC-ZH parameter list as input and tranform it into
#' a REDCap project and an excel file that contain a. an excel sheet with the
#' corresponding data dictionary and b. an excel sheet with the corresponding
#' data dictionary + relevant annotations to finalize the REDCap project
#'
#'
#' @param credential_path Path where the credential file is stored
#' @param pid Project ID
#' @param path_parameter_list path to parameter list excel workbook
#' @param path_xlsx_output path where draft of data dictionary is saved
#' @param project_name project name (is used for naming the xlsx file)
#' @return list
#' @export
redcap_create_metadata_structure = function(
    credential_path,
    pid,
    path_parameter_list,
    path_xlsx_output,
    project_name
  ){

  # reada all sheet paramete list file
  parameter_list_raw = read_parameter_list(path_parameter_list)

  # reshape the sheet read above (crop begin and end of file, etc.)
  df = reshape_parameter_list(parameter_list_raw)

  # Transform forms into RCp data dictionary
  project_data = create_dictionary(df)

  # construct output path, check if exist and make output dir if needed
  dir_path = file.path(
    path_xlsx_output, project_name
  )

  if ( !dir.exists(dir_path) ) {
    dir.create(
      dir_path
    )
  }

  # Construct path for files (xlsx and RData)
  file_path = file.path(
    path_xlsx_output, project_name,
    paste0(
      paste(
        project_name, lubridate::now(), sep = "_") %>%
          janitor::make_clean_names()
      )
  )

  message(
    file_path
  )

  # save the outputs as files
  save_xlsx_and_rdata(project_data, file_path)
  # TODO: Let's see maybe I have time and skills to find a way to color the non-redcap columns so it is
  # easier to read

  # write metatadata in RCp database
  write_metadata_in_redcap(project_name, project_data, credential_path, pid)

  return(file_path)

}

#' Read parameter list
#'
#' @param path_parameter_excel_workbook path to parameter list excel workbook
#' @return list
read_parameter_list = function(path_parameter_excel_workbook){

  # read sheet names and files
  headers = readxl::excel_sheets(path_parameter_excel_workbook) %>% purrr::set_names()
  headers %>% purrr::map(readxl::read_excel, path = path_parameter_excel_workbook, col_names = FALSE)

}

#' Reshape parameter list
#'
#' @param parameter_list_raw list
#' @return list
reshape_parameter_list = function(parameter_list_raw){

  # remove sheet called Instructions or template
  sheet_names = names(parameter_list_raw)
  sheet_names = sheet_names[
    ! stringr::str_detect(sheet_names, "Instructions|Template")
  ]
  parameter_list_raw = parameter_list_raw[ sheet_names ]

  # get all possible field type from excel cell with data valiation
  field_types = parameter_list_raw$Listen[,1] %>% dplyr::pull()

  # get landmark in files
  ind_visit_nb = search_index_str_in_df("Visit Number (optional)", parameter_list_raw$`Study Schedule`)

  ind_hash = which(
    stringr::str_detect(
      unlist(parameter_list_raw$`Study Schedule`[,1]), "#"
    )
  )

  # check landmark congruent
  stopifnot(ind_visit_nb[1] == ind_hash[1] + 1)

  # get info on visits
  visits = dplyr::tibble(
    visit_number = parameter_list_raw$`Study Schedule`[
      ind_visit_nb[1],
      (ind_visit_nb[2]+1):ncol(parameter_list_raw$`Study Schedule`)
    ] %>% unlist %>% as.integer(),
    visit_name = parameter_list_raw$`Study Schedule`[
      ind_visit_nb[1]+1,
      (ind_visit_nb[2]+1):ncol(parameter_list_raw$`Study Schedule`)
      ] %>% unlist,
    timeline = parameter_list_raw$`Study Schedule`[
      ind_visit_nb[1]+2,
      (ind_visit_nb[2]+1):ncol(parameter_list_raw$`Study Schedule`)
    ] %>% unlist,
    is_event_repeated = parameter_list_raw$`Study Schedule`[
      ind_visit_nb[1]+3,
      (ind_visit_nb[2]+1):ncol(parameter_list_raw$`Study Schedule`)
    ] %>% unlist,
    repetition_period = parameter_list_raw$`Study Schedule`[
      ind_visit_nb[1]+4,
      (ind_visit_nb[2]+1):ncol(parameter_list_raw$`Study Schedule`)
    ] %>% unlist
  ) %>% dplyr::mutate(
    repetition_period = stringr::str_replace(repetition_period, "n.a.", NA_character_)
  )

  # get index where form names starts
  ind_form_name = search_index_str_in_df("Form name", parameter_list_raw$`Study Schedule`)

  # check same number of form names announced in the "Study Schedule" sheet and in sheet names
  stopifnot(
    assertive::are_same_length(
      parameter_list_raw$`Study Schedule`[
        (ind_form_name[1]+1):(ind_hash[2]-1),
        ind_form_name[2]
        ] %>% dplyr::pull(),
      sheet_names[
        (which(stringr::str_detect(sheet_names, "Study Schedule"))+1) : length(sheet_names)
        ]
    )
  )

  # get name formular from both sources
  formular_name = dplyr::tibble(
    formular_name = parameter_list_raw$`Study Schedule`[
        (ind_form_name[1]+1):(ind_hash[2]-1),
        ind_form_name[2]
      ] %>% dplyr::pull(),
    excel_sheet_name = sheet_names[
      (which(stringr::str_detect(sheet_names, "Study Schedule"))+1) : length(sheet_names)
    ]
  )

  # remove sheets that are not forms
  parameter_list_raw = parameter_list_raw[
    ! stringr::str_detect(names(parameter_list_raw), "Listen|Study Schedule")
  ]

  # reshape sheet with forms specification and create a unique tibble
  parameter_list_raw =
    purrr::map2(.x = parameter_list_raw, .y = names(parameter_list_raw), reshape_formular) %>%
    purrr::map_df(~.x)


  return(list(
    visits = visits,
    formular_name = formular_name,
    field_types = field_types,
    param_list = parameter_list_raw
  ))

}

#' Create data dictionary
#'
#' @param parameter_list_raw list
#' @return list
create_dictionary = function(df) {

  # join with table that make the link between info in parameter list and
  # specification for the REDCap dictionary
  param_list_joined = df$param_list %>%
    dplyr::left_join(

            get_table_recoding_for_redcap()

            ) %>%
    dplyr::mutate(
      # make variable name
      `Variable / Field Name` = create_redcap_var_name(question) %>%
        # another make_clean_names is performed outside the function to that variable
        # with the exact same are numbered (*_1, *_2, etc.)
        janitor::make_clean_names(),
      # Form name
      `Form Name` = formular %>%
        stringr::str_replace_all("\\+", "_plus") %>%
        snakecase::to_snake_case() %>%
        remove_accent(), # NOTE: maybe I need to remove the accent in the form name for redcap
      # Field Label
      `Field Label` = question,
      # Choice options
      # NOTE: if yesno field type remove choice options
      `Choices, Calculations, OR Slider Labels` =
        ifelse(`Field Type` == "yesno", NA_character_, answer_options),
      #TODO: HERE I only consider that the field note is used for unit, ok in this project but ...
      `Field Note` = nb_info_unit,
      `Text Validation Min` = nb_info_min,
      `Text Validation Max` = nb_info_max,
      # NOTE: here I detect if decimal in Text Validation Min/Max to change the type from integer to number
      `Text Validation Type OR Show Slider Number` =
        dplyr::case_when(
          stringr::str_detect(`Text Validation Min`, "\\.") ~ "number",
          stringr::str_detect(`Text Validation Max`, "\\.") ~ "number",
          TRUE ~`Text Validation Type OR Show Slider Number`
        ),
      ############
      # TODO : here I have to work on the detection of branching logic
      ############
      `Branching Logic (Show field only if...)` =
        ifelse(
          notes == "Erscheint nur bei entsprechender Auswahl.",
          "!!! BRANCHING LOGIC TO BE ADDED !!!",
          NA_character_
          ),
      `Required Field?` =
        ifelse(
          is_field_required == "Mandatory data",
          "y", NA_character_
          )
    ) %>%
    # then select and order column appropriately
    dplyr::select(
      `Variable / Field Name`, `Form Name`, `Section Header`,
      type_question, `Field Type`, `Field Label`, -question,
      answer_options, `Choices, Calculations, OR Slider Labels`,
      notes, automated_checks,
      `Field Note`, # -nb_info_unit,
      #
      `Text Validation Type OR Show Slider Number`,
      `Text Validation Min`,
      `Text Validation Max`,
      # info validation and other options
      # -nb_info_decimal_pre, -nb_info_decimal_post, -is_field_required
      `Identifier?`,
      `Branching Logic (Show field only if...)`, `Required Field?`,
      `Custom Alignment`, `Question Number (surveys only)`,
      `Matrix Group Name`, `Matrix Ranking?`,
      `Field Annotation`
    )

  # Reposition section headers into the Section header column of the variable
  # that is on the next line
  # get index where type_question == "HEADER"
  index_headers = which(param_list_joined$type_question == "HEADER")
  param_list_joined$`Section Header`[index_headers+1] = param_list_joined$`Field Label`[index_headers]
  param_list_joined = param_list_joined[-index_headers, ]
  rm(index_headers)

  # Create choice option in REDCap format + get number of choice option for
  # later deciding between radio button and dropdown field type
  param_list_joined =
    param_list_joined %>%
    tibble::add_column(number_of_choice_options = NA, .before = "Choices, Calculations, OR Slider Labels")

  for (row in 1:nrow(param_list_joined)) {
    if (!is.na(param_list_joined$`Choices, Calculations, OR Slider Labels`[row])){

      param_list_joined$`Choices, Calculations, OR Slider Labels`[row] =
        param_list_joined$answer_options[row] %>%
        split_and_enumerate_choice_options()

      param_list_joined$number_of_choice_options[row] =
        param_list_joined$answer_options[row] %>%
        split_and_count_choice_options()
    }
  }; rm(row)

  # change field type to dropdown if number of choice option is equal or more than five
  param_list_joined = param_list_joined %>%
    dplyr::mutate(
      `Field Type` =
        ifelse(
          `Field Type` == "radio" & number_of_choice_options >= 5,
          "dropdown",
          `Field Type`
        )
    ) %>%  dplyr::select(-number_of_choice_options) %>%
    # add "fake" calculations (= 99999) for calculated fields so no error when writing in RCp
    dplyr::mutate(
      `Choices, Calculations, OR Slider Labels` =
        ifelse(
          `Field Type` == "calc", "99999", `Choices, Calculations, OR Slider Labels`
        )
    )

  # final product is a dictionary annotated
  df$dictionary_annotated = param_list_joined
  # and a dictionary with only the columns that is required for RCp dictionary
  df$rcp_dictionary = param_list_joined %>% filter_only_redcap_columns

  return(df)

}

save_xlsx_and_rdata = function(project_data, file_path) {

  # save the outputs as files
  save(
    project_data,
    file = paste0(file_path, ".RData")
  )

  xlsx::write.xlsx(
    as.data.frame(project_data$dictionary_annotated),
    file = paste0(file_path, ".xlsx"),
    sheetName = "annotated_dic",
    row.names = FALSE,
    showNA = FALSE
  )

  xlsx::write.xlsx(
    as.data.frame(project_data$rcp_dictionary),
    file = paste0(file_path, ".xlsx"),
    sheetName = "dic",
    row.names = FALSE,
    showNA = FALSE,
    append = TRUE
  )

}

write_metadata_in_redcap = function(project_name, project_data, credential_path, pid) {

  # get credentials
  cred_info = suppressWarnings(
    REDCapR::retrieve_credential_local(
      credential_path, project_id = pid
    )
  )

  # Check project info
  project_info_from_rcp_server = redcap::project_info(
    conn = redcap::rconn(
      url = cred_info$redcap_uri,
      token = cred_info$token
    )
  ) %>% dplyr::select(
    project_id, project_title, creation_time, production_time
  )

  answer = svDialogs::dlg_input(
    paste(
      "You are about to write the metadata of a REDCap project,",
      "this operation will replace all metadata",
      "(and therefore DELETE ALL previous metadata).\n\n",
      "Please check carefully the information of the project you would like to modify:\n\n",
      "Title entered:", project_name, "\n",
      "Title from credentials' file", cred_info$comment, "\n\n",
      "Please check carefully the information of the REDCap database you are about to modify:\n\n",
      "REDCap's project title:", project_info_from_rcp_server$project_title, "\n",
      "PID:", project_info_from_rcp_server$project_id, "\n",
      "Created on the:", project_info_from_rcp_server$creation_time, "\n",
      "Released to production on the:", project_info_from_rcp_server$production_time, "\n\n",
      "To proceed, please confirm the PID number of the project (to cancel the operation type anything else):"
    )
  )

  # write metadata
  if (as.integer(cred_info$project_id) == as.integer(answer$res)) {
    REDCapR::redcap_metadata_write(
      ds = project_data$rcp_dictionary,
      redcap_uri = cred_info$redcap_uri,
      token = cred_info$token
    )
  } else {
    message(
      paste(
        "You did not write metadata into your REDCap's project, but the data dictionary was saved in:\n",
        file_path
      )
    )
  }


}


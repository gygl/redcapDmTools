# redcapDmTools
A package to facilitate the work of Data Managers using REDCap

## __`redcap_create_metadata_structure()` and Parameter List__

The template of the parameter list is found in `./inst/Parameter_List.xlsx`.

To install the package on your computer, clone the repository and open the project using `redcapDmTools.Rproj`. Once the project is open run in the console `devtools::install()`.

An example can be run by `redcapDmTools::redcap_create_metadata_structure(path_output = "test")`.

The output are:
- `test/test_YYYY_MM_DD_hh_mm_ss_data_dictionary.csv` a data dictionary ready to be imported in REDCap
- `test/test_YYYY_MM_DD_hh_mm_ss_dannotated_data_dictionary.csv` an annotated data dictionary designed to guide the Data Manager to implement features not automatically implemented
- `test/test_YYYY_MM_DD_hh_mm_ss.RData` an R data file that contains 2 tibbles corresponding to the two previous output

Information on other features: `?redcapDmTools::redcap_create_metadata_structure`

#' Reads an excel file exported from S-PRO, containing WIMU data
#'
#' This is a specific function for importing player-tracking data captured using
#' WIMU GPS devices, processed in S-PRO software and exported as a spreadsheet.
#' read_wimu_xls currently accepts data generated from the Intervals Pro
#' monitor.
#'
#' @param path Path to the xls file containing WIMU data
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an
#'   integer (the position of the sheet). Ignored if the sheet is specified via
#'   range. If neither argument specifies the sheet, defaults to the first
#'   sheet.
#' @param session Name of the session (character)
#' @param monitor Must be intervalspro.
#' @param ids Named vector containing the id associated to each wimu, in the
#'   style of c("P101" = "WIMU_1", ...)
#' @return A tibble
#' @export
read_wimu_xls <- function(path,
                           sheet = "Tables",
                           session,
                           monitor = c("intervalspro"),
                           ids = NULL){
  # Confirm this session has a name
  if (is.null(session)) {
   stop("You must provide a session name")
  } else if (class(session) != "character") {
    stop("Session should be a character")
  }

  # Check monitor
  if (monitor == "intervalspro") {
    raw_data = readxl::read_excel(path = path, sheet = sheet) %>%
      janitor::clean_names() %>%
      dplyr::mutate(session = UQ(session))
  } # end if monitor
  # Check id
  if (!is.null(ids)) {
    # Assign id codes
    if (class(ids) == "character") {
      raw_data = raw_data %>%
        dplyr::mutate(player = forcats::fct_recode(factor(player),
                                                   !!!ids))
    } else {
      warning("ids should be a character vector")
    }

  } # end if id
  return(tibble(raw_data))
}










#' Load claim files from NARA .txt fwf
#'
#' @param path path to the death file fwf .txt files
#' @return death record data.frame
#' @keywords internal
#' @import data.table
#' @export

read_claim_fwf <- function(path = "/global/scratch/p2p3/pl1_demography/censoc/input_data/nara_numident/claim_records/records") {

  claims_file_column_widths <- fwf_cols(
    NUMI_REFERENCE_NUM = c(1, 11),
    FILLER_1 = c(12, 12),
    NUMI_SSN = c(13, 21),
    FILLER_2 = c(22, 22),
    NUMI_DO_CODE = c(23, 25),
    NUMI_FORM_CODE = c(26, 26),
    NUMI_ENTRY_CODE = c(27, 27),
    FILLER_3 = c(28, 28),
    NUMI_CYCLE_DATE = c(29, 36),
    NUMI_NH_NAME_FIRST = c(37, 51),
    NUMI_NH_NAME_FIRST_OFLO = c(52, 52),
    NUMI_NH_NAME_MIDDLE = c(53, 67),
    NUMI_NH_NAME_MIDDLE_OFLO = c(68, 68),
    NUMI_NH_NAME_LAST = c(69, 88),
    NUMI_NH_NAME_LAST_OFLO = c(89, 89),
    NUMI_NH_NAME_SUFFIX = c(90, 93),
    FILLER_4 = c(94, 96),
    NUMI_DOB = c(97, 104),
    NUMI_SEX = c(105, 105),
    FILLER_5 = c(106, 106),
    NUMI_MTH_NAME_FIRST = c(107, 121),
    NUMI_MTH_NAME_FIRST_OFLO = c(122, 122),
    NUMI_MTH_NAME_MIDDLE = c(123, 137),
    NUMI_MTH_NAME_MIDDLE_OFLO = c(138, 138),
    NUMI_MTH_NAME_LAST = c(139, 158),
    NUMI_MTH_NAME_LAST_OFLO = c(159, 159),
    NUMI_MTH_NAME_SUFFIX = c(160, 163),
    NUMI_FTH_NAME_FIRST = c(164, 178),
    NUMI_FTH_NAME_FIRST_OFLO = c(179, 179),
    NUMI_FTH_NAME_MIDDLE = c(180, 194),
    NUMI_FTH_NAME_MIDDLE_OFLO = c(195, 195),
    NUMI_FTH_NAME_LAST = c(196, 215),
    NUMI_FTH_NAME_LAST_OFLO = c(216, 216),
    NUMI_FTH_NAME_SUFFIX = c(217, 220),
    NUMI_POB_CITY = c(221, 232),
    NUMI_POB_CITY_OFLO = c(233, 233),
    NUMI_POB_STATE_COUNTRY = c(234, 235),
    NUMI_POB_FOREIGN_IND = c(236, 236),
    NUMI_DO_ADDRESS = c(237, 281),
    NUMI_CLAIM_DATE = c(282, 289),
    NUMI_DOO = c(290, 297),
    NUMI_DOD_CLAIM = c(298, 305),
    FILLER_6 = c(306, 497),
    NUMI_CONVERSION_RECORD_IND = c(498, 498),
    FILLER_7 = c(499, 499)
    # NUMI_FLAG_DELETE_IND = c(500, 500) It appears this variable is not available.
  )

  ## make a list of all claims files
  claims_file <- list.files(path ="/data/josh/CenSoc/NUMICLAIM/Records/", pattern = "\\.txt", full.names = T)

  ## Create empty list to store data files
  list.dfs <- list()

  ## read in files; default to character classes for consistency
  ## loop over each file, read it, and store it as a data.frame in a list
  for (file in claims_file) {
    list.dfs[[file]] <- read_fwf(file, col_positions = claims_file_column_widths, col_types = cols(.default = col_character() ))
    cat("finished with file: ", file, "\n")
  }

  ## Combine the list of data.frames into a single data.frame
  claims <- bind_rows(list.dfs)

  return(claims)

}








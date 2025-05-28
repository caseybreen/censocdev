#' Load Application Files from NARA .txt fwf files
#'
#' @param path path to the app file fwf .txt files
#' @return death record data.frame
#' @keywords internal
#' @import data.table
#' @export

read_app_fwf <- function(path = "/global/scratch/p2p3/pl1_demography/censoc/input_data/nara_numident/application_records/records") {

  application_record_column_positions <- fwf_cols(
    NUMI_REFERENCE_NUM = c(1, 11),
    NUMI_INTERVIEW_CODE = c(12, 12),
    NUMI_SSN = c(13, 21),
    NUMI_CITIZENSHIP_CODE = c(22, 22),
    NUMI_DO_CODE = c(23, 25),
    NUMI_FORM_CODE = c(26, 26),
    NUMI_ENTRY_CODE = c(27, 27),
    NUMI_PRINT_CODE = c(28, 28),
    NUMI_CYCLE_DATE = c(29, 36),
    NUMI_NH_NAME_FIRST = c(37, 51),
    NUMI_NH_NAME_FIRST_OFLO = c(52, 52),
    NUMI_NH_NAME_MIDDLE = c(53, 67),
    NUMI_NH_NAME_MIDDLE_OFLO = c(68, 68),
    NUMI_NH_NAME_LAST = c(69, 88),
    NUMI_NH_NAME_LAST_OFLO = c(89, 89),
    NUMI_NH_NAME_SUFFIX = c(90, 93),
    FILLER_1 = c(94, 96),
    NUMI_DOB = c(97, 104),
    NUMI_SEX = c(105, 105),
    NUMI_RACE = c(106, 106),
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
    NUMI_OTH_NAME_FIRST = c(237, 251),
    NUMI_OTH_NAME_FIRST_OFLO = c(252, 252),
    NUMI_OTH_NAME_MIDDLE = c(253, 267),
    NUMI_OTH_NAME_MIDDLE_OFLO = c(268, 268),
    NUMI_OTH_NAME_LAST = c(269, 288),
    NUMI_OTH_NAME_LAST_OFLO = c(289, 289),
    NUMI_OTH_NAME_SUFFIX = c(290, 293),
    FILLER_2 = c(294, 296),
    NUMI_SIG_NAME_FIRST = c(297, 311),
    NUMI_SIG_NAME_FIRST_OFLO = c(312, 312),
    NUMI_SIG_NAME_MIDDLE = c(313, 327),
    NUMI_SIG_NAME_MIDDLE_OFLO = c(328, 328),
    NUMI_SIG_NAME_LAST = c(329, 348),
    NUMI_SIG_NAME_LAST_OFLO = c(349, 349),
    NUMI_SIG_NAME_SUFFIX = c(350, 353),
    NUMI_SIG_NAME_CODE = c(354, 354),
    FILLER_3 = c(355, 357),
    NUMI_NOTIFY = c(358, 359),
    NUMI_CARD_CODE = c(360, 360),
    NUMI_DIB_FREEZE_IND = c(361, 361),
    NUMI_DOB_CHANGE_IND = c(362, 362),
    NUMI_PRIOR_DOB = c(363, 370),
    NUMI_CORR_IND = c(371, 372),
    NUMI_BIRTH_CERTIFICATE_NUMBER = c(373, 383),
    FILLER_4 = c(384, 385),
    NUMI_ALIEN_REGISTRATION_NUMBER = c(386, 400),
    NUMI_ETHNIC_HISPANIC_OR_LATINO = c(401, 401),
    NUMI_ETHNIC_NOT_HISP_OR_LATINO = c(402, 402),
    NUMI_ETHNIC_UNUSED = c(403, 404),
    NUMI_RACE_AM_AK_NATIVE = c(405, 405),
    NUMI_RACE_ASIAN = c(406, 406),
    NUMI_RACE_BLACK_OR_AFRICAN_AM = c(407, 407),
    NUMI_RACE_HAWAIIAN_PACIFIC_IS = c(408, 408),
    NUMI_RACE_WHITE = c(409, 409),
    NUMI_RACE_UNUSED = c(410, 412),
    NUMI_CONTROL_NUMBER = c(413, 421),
    FILLER_5 = c(422, 496),
    NUMI_DIFFERENT_NUMBER_REASON = c(497, 497),
    NUMI_CONVERSION_RECORD_IND = c(498, 498),
    NUMI_ENTRY_SUBCODE = c(499, 499),
    NUMI_FLAG_DELETE_IND = c(500, 500))

  ## make a list of all claims files
  application_file <- list.files(path = path, pattern = "\\.txt", full.names = T)

  ## Create empty list to store data files
  list.dfs <- list()

  ## read in files; default to character
  for (file in application_file) {
    list.dfs[[file]] <- read_fwf(file, col_positions = application_record_column_positions, col_types = cols(.default = col_character() ))
    cat("finished with file: ", file, "\n")
  }

  application_records <- bind_rows(list.dfs)

  return(application_records)


}

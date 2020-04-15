#' Load death files from NARA .txt fwf
#'
#' @param path path to the death file fwf .txt files
#' @return death record data.frame
#' @keywords internal
#' @import data.table
#' @export

read_death_fwf <- function(path = "/data/josh/CenSoc/NUMDEATH/") {

  death_record_column_positions <- fwf_cols(
    NUMI_REFERENCE_NUM = c(1, 11),
    FILLER_1 = c(12, 12),
    NUMI_SSN = c(13, 21),
    FILLER_2 = c(22, 26),
    NUMI_ENTRY_CODE = c(27, 27),
    FILLER_3 = c(28, 28),
    NUMI_CYCLE_DATE = c(29, 36),
    NUMI_NH_NAME_FIRST_DTH_1 = c(37, 52),
    NUMI_NH_NAME_MIDDLE_DTH_1 = c(53, 68),
    NUMI_NH_NAME_LAST_DTH_1 = c(69, 89),
    NUMI_NH_NAME_SUFFIX_DTH_1 = c(90, 93),
    FILLER_4 = c(94, 96),
    NUMI_DOB_DTH_1 = c(97, 104),
    NUMI_SEX_DTH_1 = c(105, 105),
    FILLER_5 = c(106, 106),
    NUMI_INDIVIDUALS_OTHER_NUM_1 = c(107, 115),
    NUMI_RECORD_ID_CODE_1 = c(116, 116),
    NUMI_PROOF_OF_DEATH_1 = c(117, 117),
    NUMI_PGM_INVOLVEMENT_IND_1 = c(118, 118),
    NUMI_ZIP_RESIDENCE_1 = c(119, 127),
    NUMI_ZIP_LUMP_SUM_1 = c(128, 136),
    NUMI_PLUS_5_DOB_EX_IND_1 = c(137, 137),
    NUMI_PLUS_5_EXCEPT_CHANGE_IND_1 = c(138, 138),
    NUMI_PLUS_5_MBR_DOB_EX_IND_1 = c(139, 139),
    FILLER_6 = c(140, 141),
    NUMI_SOURCE_OF_DEATH_1 = c(142, 143),
    NUMI_VERIFIED_EDR_IND = c(144, 144),
    NUMI_DOD_DTH_1 = c(145, 152),
    NUMI_DTH_CERTIFICATE_NUM_1 = c(153, 178),
    NUMI_DTH_POSTING_DATE_1 = c(179, 186),
    NUMI_NH_NAME_FIRST_DTH_2 = c(187, 202),
    NUMI_NH_NAME_MIDDLE_DTH_2 = c(203, 218),
    NUMI_NH_NAME_LAST_DTH_2 = c(219, 239),
    NUMI_NH_NAME_SUFFIX_DTH_2 = c(240, 243),
    FILLER_7 = c(244, 246),
    NUMI_DOB_DTH_2 = c(247, 254),
    NUMI_SEX_DTH_2 = c(255, 255),
    FILLER_8 = c(256, 256),
    NUMI_INDIVIDUALS_OTHER_NUM_2 = c(257, 265),
    NUMI_RECORD_ID_CODE_2 = c(266, 266),
    NUMI_PROOF_OF_DEATH_2 = c(267, 267),
    NUMI_PGM_INVOLVEMENT_IND_2 = c(268, 268),
    NUMI_ZIP_RESIDENCE_2 = c(269, 277),
    NUMI_ZIP_LUMP_SUM_2 = c(278, 286),
    NUMI_PLUS_5_INDS_2 = c(287, 291),
    NUMI_PLUS_5_DOB_EX_IND_2 = c(287, 287),
    NUMI_PLUS_5_EXCEPT_CHANGE_IND_2 = c(288, 288),
    FILLER_9 = c(289, 289),
    NUMI_PLUS_5_MBR_DOB_EX_IND_2 = c(290, 291),
    NUMI_SOURCE_OF_DEATH_2 = c(292, 293),
    NUMI_VERIFIED_EDR_IND_2 = c(294, 294),
    NUMI_DOD_DTH_2 = c(295, 302),
    NUMI_DTH_CERTIFICATE_NUM_2 = c(303, 328),
    NUMI_DTH_POSTING_DATE_2 = c(329, 336),
    NUMI_NH_NAME_FIRST_DTH_3 = c(337, 352),
    NUMI_NH_NAME_MIDDLE_DTH_3 = c(353, 368),
    NUMI_NH_NAME_LAST_DTH_3 = c(369, 389),
    NUMI_NH_NAME_SUFFIX_DTH_3 = c(390, 393),
    FILLER_10 = c(394, 396),
    NUMI_DOB_DTH_3 = c(397, 404),
    NUMI_SEX_DTH_3 = c(405, 405),
    FILLER_11 = c(406, 406),
    NUMI_INDIVIDUALS_OTHER_NUM_3 = c(407, 415),
    NUMI_RECORD_ID_CODE_3 = c(416, 416),
    NUMI_PROOF_OF_DEATH_3 = c(417, 417),
    NUMI_PGM_INVOLVEMENT_IND_3 = c(418, 418),
    NUMI_ZIP_RESIDENCE_3 = c(419, 427),
    NUMI_ZIP_LUMP_SUM_3 = c(428, 436),
    NUMI_PLUS_5_DOB_EX_IND_3 = c(437, 437),
    NUMI_PLUS_5_EXCEPT_CHANGE_IND_3 = c(438, 438),
    FILLER_12 = c(439, 439),
    NUMI_PLUS_5_MBR_DOB_EX_IND_3 = c(440, 441),
    NUMI_SOURCE_OF_DEATH_3 = c(442, 443),
    NUMI_VERIFIED_EDR_IND_3 = c(444, 444),
    NUMI_DOD_DTH_3 = c(445, 452),
    NUMI_DTH_CERTIFICATE_NUM_3 = c(453, 478),
    NUMI_DTH_POSTING_DATE_3 = c(479, 486),
    NUMI_DTH_NOE = c(487, 487))

  ## make a list of all claims files
  death_record_files <- list.files(path = path, pattern = "\\.TXT", full.names = T)

  ## Create empty list to store data files
  list.dfs <- list()

  ## read in files; default to character
  for (file in death_record_files) {
    list.dfs[[file]] <- read_fwf(file, col_positions = death_record_column_positions, col_types = cols(.default = col_character() ))
    cat("finished with file: ", file, "\n")
  }

  death_record <- bind_rows(list.dfs)

  return(death_record)

}

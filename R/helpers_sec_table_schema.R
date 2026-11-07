#' Expected columns and types of the SEC Form 13F data set tables
#'
#' The single source of truth for bronze: column names in file order and their types,
#' from the SEC readme (`FORM13F_readme.htm`, section 5) and checked against the
#' 2014q1, 2023q1 and 2025 archives (identical headers). Dates arrive as text such as
#' `31-MAR-2025`. Identifiers (CIK, CUSIP, file numbers) stay character so leading
#' zeros are kept; amounts and counts are double because NUMBER(16) can exceed R's
#' integer range.
#'
#' @return Named list of `readr::cols()` specifications, one per table, in the order
#'   SUBMISSION, COVERPAGE, SUMMARYPAGE, OTHERMANAGER, OTHERMANAGER2, INFOTABLE.
#' @noRd
sec_table_schema <- function() {
  chr <- readr::col_character()
  dbl <- readr::col_double()
  dte <- readr::col_date(format = "%d-%b-%Y")

  list(
    SUBMISSION = readr::cols(
      ACCESSION_NUMBER = chr, FILING_DATE = dte, SUBMISSIONTYPE = chr, CIK = chr,
      PERIODOFREPORT = dte
    ),
    COVERPAGE = readr::cols(
      ACCESSION_NUMBER = chr, REPORTCALENDARORQUARTER = dte, ISAMENDMENT = chr,
      AMENDMENTNO = dbl, AMENDMENTTYPE = chr, CONFDENIEDEXPIRED = chr,
      DATEDENIEDEXPIRED = dte, DATEREPORTED = dte, REASONFORNONCONFIDENTIALITY = chr,
      FILINGMANAGER_NAME = chr, FILINGMANAGER_STREET1 = chr, FILINGMANAGER_STREET2 = chr,
      FILINGMANAGER_CITY = chr, FILINGMANAGER_STATEORCOUNTRY = chr,
      FILINGMANAGER_ZIPCODE = chr, REPORTTYPE = chr, FORM13FFILENUMBER = chr,
      CRDNUMBER = chr, SECFILENUMBER = chr, PROVIDEINFOFORINSTRUCTION5 = chr,
      ADDITIONALINFORMATION = chr
    ),
    SUMMARYPAGE = readr::cols(
      ACCESSION_NUMBER = chr, OTHERINCLUDEDMANAGERSCOUNT = dbl, TABLEENTRYTOTAL = dbl,
      TABLEVALUETOTAL = dbl, ISCONFIDENTIALOMITTED = chr
    ),
    OTHERMANAGER = readr::cols(
      ACCESSION_NUMBER = chr, OTHERMANAGER_SK = dbl, CIK = chr, FORM13FFILENUMBER = chr,
      CRDNUMBER = chr, SECFILENUMBER = chr, NAME = chr
    ),
    OTHERMANAGER2 = readr::cols(
      ACCESSION_NUMBER = chr, SEQUENCENUMBER = dbl, CIK = chr, FORM13FFILENUMBER = chr,
      CRDNUMBER = chr, SECFILENUMBER = chr, NAME = chr
    ),
    INFOTABLE = readr::cols(
      ACCESSION_NUMBER = chr, INFOTABLE_SK = dbl, NAMEOFISSUER = chr, TITLEOFCLASS = chr,
      CUSIP = chr, FIGI = chr, VALUE = dbl, SSHPRNAMT = dbl, SSHPRNAMTTYPE = chr,
      PUTCALL = chr, INVESTMENTDISCRETION = chr, OTHERMANAGER = chr,
      VOTING_AUTH_SOLE = dbl, VOTING_AUTH_SHARED = dbl, VOTING_AUTH_NONE = dbl
    )
  )
}

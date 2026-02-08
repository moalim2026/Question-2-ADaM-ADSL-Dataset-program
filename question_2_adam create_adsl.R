#Question 2: ADaM ADSL Dataset Creation 

library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

library(tibble) # added 


dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

dm <- convert_blanks_to_na(dm)
vs <- convert_blanks_to_na(vs)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)

adsl <- dm
  
  
#AGEGR9 & AGEGR9N Age grouping into the following categories: “<18”, “18 - 50”, “>50” 

# Create lookup tables
agegr9_lookup <- exprs(
  ~condition,           ~AGEGR9,
  AGE < 18,               "<18",
  between(AGE, 18, 50), "18-50",
  AGE > 50,               ">50",
  is.na(AGE),         "Missing"
)

agegr9N_lookup <- exprs(
  ~condition,           ~AGEGR9N,
  AGE < 18,               1,
  between(AGE, 18, 50),   2,
  AGE > 50,               3,
  is.na(AGE),             .
)

adsl <- adsl %>%
  derive_vars_cat(
    definition = agegr9_lookup
  ) 

adsl <- adsl %>%
  mutate(
    AGEGR9N = case_when(
      AGE < 18 ~ 1,
      between(AGE, 18, 50) ~ 2,
      AGE > 50 ~ 3,
      is.na(AGE) ~ NA_real_   # Use NA_real_ for numeric missing values
    )
  )


# Impute start and end time of exposure to first and last respectively,
# Do not impute date
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

#ITTFL  “Y”/”N” flag identifying patients who have been randomized, that is, 
#where ARM is populated in pharmaversesdtm::dm domain. 
adsl <- adsl %>%
  derive_var_merged_exist_flag(
    dataset_add = dm,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = ITTFL,
    false_value = "N",
    missing_value = "N",
    condition = !is.na(ARM)
  )

#Derive the Last Date Known Alive (LSTAVLDT)
#LSTAVLDT 
#Last known alive date using any vital signs visit date, any adverse event 
#start date, any disposition record and any exposure record.

adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition = !is.na(VSDTC) & !is.na(VSSTRESN) & !is.na(VSSTRESC),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(VSDTC, highest_imputation = "M"),
          seq = VSSEQ
        )
      ),
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(AESTDTC),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "M"),
          seq = AESEQ
        )
      ),
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "M"),
          seq = DSSEQ
        )
      ),
      event(
        dataset_name = "ex",
        order = exprs(EXSTDTC, EXSEQ),
        condition = !is.na(EXSTDTC),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(EXSTDTC, highest_imputation = "M"),
          seq = EXSEQ
        )
      )
    ),
    source_datasets = list(vs = vs, ae = ae, ds = ds, ex = ex),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTAVLDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTAVLDT)
    )


# Load the package
library(writexl)

# Save as Excel
write_xlsx(adsl, path = "C:\\Users\\ja-mo\\OneDrive\\Desktop\\R programming\\Data\\question 2 ADaM ADSL\\adam.adsl.xlsx")

# Set path for the log file
log_file <- "C:\\Users\\ja-mo\\OneDrive\\Desktop\\R programming\\Data\\adam.adsl.xlsx" 

# Start capturing console output
sink(log_file, split = TRUE)   # split=TRUE also keeps showing in console

## Run the logged
summary(adsl)
sessionInfo()

# Stop capturing
sink()
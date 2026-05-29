# random_dataset_generator.R
# =====================================================================
# Reads the full Swespine cervical Excel workbook, derives ~48
# analysis-ready variables, randomly splits records into 4 simulated
# country sites (Sweden 40 %, Norway 25 %, Denmark 20 %, Finland 15 %),
# and writes one CSV per country to  data/
#
# Output files:
#   data/sweden.csv   data/norway.csv   data/denmark.csv   data/finland.csv
#
# The generated CSVs are already in data/ and tracked in git.
# Re-run this script only if you need to regenerate them from a new
# version of the source Excel workbook.
#
# Prerequisites:
#   1. Place the Swespine Excel file at the path set in DATA_FILE below.
#   2. Run from the project root:
#        Rscript tools/random_dataset_generator.R
# =====================================================================

suppressPackageStartupMessages(library(readxl))

set.seed(2026)

# Update this path if the Excel workbook lives elsewhere on your system.
DATA_FILE <- file.path(
  "Swespine_cervical_2006_2026_unprotected.xlsx"
)
SHEET   <- "master_sheet"
OUT_DIR <- "data"

if (!file.exists(DATA_FILE))
  stop(
    "Source Excel not found: ", DATA_FILE, "\n",
    "Place the Swespine workbook at the path above and re-run."
  )

cat("Reading workbook … ")
raw <- as.data.frame(read_excel(DATA_FILE, sheet = SHEET,
                                na = c("", "NA", "NaN", "NULL")))
cat(sprintf("%d rows × %d cols\n", nrow(raw), ncol(raw)))

num <- function(x) suppressWarnings(as.numeric(x))
bin <- function(x) {
  # Convert any non-zero numeric value to 1, 0 stays 0, NA stays NA
  v <- num(x); ifelse(is.na(v), NA_real_, ifelse(v > 0, 1, 0))
}

# =====================================================================
# Variable derivation
# =====================================================================

# ---- Identifiers ----
clinic <- as.character(raw[["DG-Klinik"]])
clinic[clinic == "" | clinic == "NA"] <- NA

# ---- Demographics ----
age  <- num(raw[["DG-Alder"]])
age[!is.na(age) & (age < 10 | age > 110)] <- NA   # obvious coding errors

sex_code <- num(raw[["PAT-Kon"]])
sexM     <- ifelse(sex_code == 1, 1L, ifelse(sex_code == 2, 0L, NA_integer_))

height <- num(raw[["PreopHR-Langd"]])
weight <- num(raw[["PreopHR-Vikt"]])
height[!is.na(height) & (height < 100 | height > 220)] <- NA  # cm
weight[!is.na(weight) & (weight < 30  | weight > 300)] <- NA  # kg
bmi    <- weight / (height / 100)^2
bmi[!is.na(bmi) & (bmi < 10 | bmi > 80)] <- NA

smoker <- bin(raw[["PreopHR-Rokare"]])   # 0 = no, 1 = yes

# ---- Diagnosis ----
diag_code  <- num(raw[["OpHR-Neuro"]])
diag_grp   <- ifelse(diag_code == 2, "radiculopathy",
              ifelse(diag_code == 3, "myelopathy", NA_character_))
diag_myelo <- ifelse(diag_code == 3, 1L,
              ifelse(diag_code == 2, 0L, NA_integer_))

# ---- Surgical / clinical severity ----
asa    <- num(raw[["OpHR-ASA"]])
asa[!is.na(asa) & (asa < 1 | asa > 5)] <- NA

nurick <- num(raw[["OpHR-Nurick"]])
nurick[!is.na(nurick) & (nurick < 0 | nurick > 5)] <- NA

# ---- Comorbidities (binary) ----
comorbid_cardiac <- bin(raw[["PreopHR-LidsjukdHjarta"]])
comorbid_neuro   <- bin(raw[["PreopHR-LidsjukdNeuro"]])
comorbid_gait    <- bin(raw[["PreopHR-LidsjukdGang"]])
comorbid_pain    <- bin(raw[["PreopHR-LidsjukdSmart"]])
opioids_preop       <- bin(raw[["PreopHR-MedicinMorfinliknande"]])
neuropathic_preop   <- bin(raw[["PreopHR-MedicinNervsmarta"]])

# ---- Work status pre-op (binary: any degree → 1) ----
unemployed_preop         <- bin(raw[["PreopHR-ArbLos"]])
sick_leave_preop         <- bin(raw[["PreopHR-SjukPen"]])
work_incapacity_preop    <- bin(raw[["PreopHR-ArbOfor"]])

# ---- Pre-operative PROMs ----
nrs_arm_preop  <- num(raw[["PreopHR-NRSArm"]])
nrs_arm_preop[!is.na(nrs_arm_preop) & (nrs_arm_preop < 0 | nrs_arm_preop > 10)] <- NA

nrs_neck_preop <- num(raw[["PreopHR-NRSHalsBrost"]])
nrs_neck_preop[!is.na(nrs_neck_preop) & (nrs_neck_preop < 0 | nrs_neck_preop > 10)] <- NA

eq5d_index_preop <- num(raw[["PreopHR-EQ5DIndex"]])
eq5d_index_preop[!is.na(eq5d_index_preop) &
                 (eq5d_index_preop < -1 | eq5d_index_preop > 1)] <- NA

eq5d_vas_preop <- num(raw[["PreopHR-EQ5DVAS"]])
eq5d_vas_preop[!is.na(eq5d_vas_preop) &
               (eq5d_vas_preop < 0 | eq5d_vas_preop > 100)] <- NA

ndi_index_preop <- num(raw[["PreopHR-NDIIndex"]])
ndi_index_preop[!is.na(ndi_index_preop) &
                (ndi_index_preop < 0 | ndi_index_preop > 100)] <- NA

myelopathy_index_preop <- num(raw[["PreopHR-MyelopatiIndex"]])
myelopathy_index_preop[!is.na(myelopathy_index_preop) &
                       (myelopathy_index_preop < 0 | myelopathy_index_preop > 18)] <- NA

pmjoa_index_preop <- num(raw[["PreopHR-PmjoaIndex"]])
pmjoa_index_preop[!is.na(pmjoa_index_preop) &
                  (pmjoa_index_preop < 0 | pmjoa_index_preop > 18)] <- NA

# ---- Operation outcomes ----
any_complication <- bin(raw[["OpHR-Komp"]])
reoperated       <- ifelse(!is.na(raw[["ReopHR-Dat_1"]]), 1L, 0L)

# ---- 12-month follow-up PROMs ----
nrs_arm_12m  <- num(raw[["UppHR-NRSArm_1"]])
nrs_arm_12m[!is.na(nrs_arm_12m) & (nrs_arm_12m < 0 | nrs_arm_12m > 10)] <- NA

nrs_neck_12m <- num(raw[["UppHR-NRSHalsBrost_1"]])
nrs_neck_12m[!is.na(nrs_neck_12m) & (nrs_neck_12m < 0 | nrs_neck_12m > 10)] <- NA

eq5d_index_12m <- num(raw[["UppHR-EQ5DIndex_1"]])
eq5d_index_12m[!is.na(eq5d_index_12m) &
               (eq5d_index_12m < -1 | eq5d_index_12m > 1)] <- NA

eq5d_vas_12m <- num(raw[["UppHR-EQ5DVAS_1"]])
eq5d_vas_12m[!is.na(eq5d_vas_12m) &
             (eq5d_vas_12m < 0 | eq5d_vas_12m > 100)] <- NA

ndi_index_12m <- num(raw[["UppHR-NDIIndex_1"]])
ndi_index_12m[!is.na(ndi_index_12m) &
              (ndi_index_12m < 0 | ndi_index_12m > 100)] <- NA

myelopathy_index_12m <- num(raw[["UppHR-MyelopatiIndex_1"]])
myelopathy_index_12m[!is.na(myelopathy_index_12m) &
                     (myelopathy_index_12m < 0 | myelopathy_index_12m > 18)] <- NA

pmjoa_index_12m <- num(raw[["UppHR-PmjoaIndex_1"]])
pmjoa_index_12m[!is.na(pmjoa_index_12m) &
                (pmjoa_index_12m < 0 | pmjoa_index_12m > 18)] <- NA

# patient_satisfied_12m: 1 in the data = satisfied (already 0/1 binary)
patient_satisfied_12m <- bin(raw[["UppHR-SuccessNojdhet_1"]])

# ---- 24-month follow-up PROMs ----
nrs_arm_24m  <- num(raw[["UppHR-NRSArm_2"]])
nrs_arm_24m[!is.na(nrs_arm_24m) & (nrs_arm_24m < 0 | nrs_arm_24m > 10)] <- NA

nrs_neck_24m <- num(raw[["UppHR-NRSHalsBrost_2"]])
nrs_neck_24m[!is.na(nrs_neck_24m) & (nrs_neck_24m < 0 | nrs_neck_24m > 10)] <- NA

eq5d_index_24m <- num(raw[["UppHR-EQ5DIndex_2"]])
eq5d_index_24m[!is.na(eq5d_index_24m) &
               (eq5d_index_24m < -1 | eq5d_index_24m > 1)] <- NA

ndi_index_24m <- num(raw[["UppHR-NDIIndex_2"]])
ndi_index_24m[!is.na(ndi_index_24m) &
              (ndi_index_24m < 0 | ndi_index_24m > 100)] <- NA

# ---- Derived change scores & MCID outcomes ----
delta_nrs_arm_12m  <- nrs_arm_preop  - nrs_arm_12m   # positive = improvement
delta_nrs_neck_12m <- nrs_neck_preop - nrs_neck_12m
delta_eq5d_12m     <- eq5d_index_12m - eq5d_index_preop  # positive = improvement
delta_ndi_12m      <- ndi_index_preop - ndi_index_12m    # positive = improvement

mcid_arm_12m  <- ifelse(is.na(delta_nrs_arm_12m),  NA_integer_,
                 ifelse(delta_nrs_arm_12m  >= 3, 1L, 0L))
mcid_neck_12m <- ifelse(is.na(delta_nrs_neck_12m), NA_integer_,
                 ifelse(delta_nrs_neck_12m >= 3, 1L, 0L))
mcid_eq5d_12m <- ifelse(is.na(delta_eq5d_12m),     NA_integer_,
                 ifelse(delta_eq5d_12m     >= 0.07, 1L, 0L))
mcid_ndi_12m  <- ifelse(is.na(delta_ndi_12m),      NA_integer_,
                 ifelse(delta_ndi_12m      >= 10,   1L, 0L))

delta_nrs_arm_24m  <- nrs_arm_preop  - nrs_arm_24m
mcid_arm_24m       <- ifelse(is.na(delta_nrs_arm_24m), NA_integer_,
                      ifelse(delta_nrs_arm_24m >= 3, 1L, 0L))

# =====================================================================
# Assemble clean data frame
# =====================================================================
dat <- data.frame(
  # identifiers
  clinic                 = clinic,

  # demographics
  age                    = age,
  sexM                   = sexM,
  bmi                    = bmi,
  smoker                 = smoker,

  # diagnosis
  diag_grp               = diag_grp,
  diag_myelo             = diag_myelo,

  # pre-op clinical
  asa                    = asa,
  nurick                 = nurick,
  any_complication       = any_complication,
  reoperated             = reoperated,

  # comorbidities
  comorbid_cardiac       = comorbid_cardiac,
  comorbid_neuro         = comorbid_neuro,
  comorbid_gait          = comorbid_gait,
  comorbid_pain          = comorbid_pain,
  opioids_preop          = opioids_preop,
  neuropathic_preop      = neuropathic_preop,

  # work status pre-op
  unemployed_preop       = unemployed_preop,
  sick_leave_preop       = sick_leave_preop,
  work_incapacity_preop  = work_incapacity_preop,

  # pre-op PROMs
  nrs_arm_preop          = nrs_arm_preop,
  nrs_neck_preop         = nrs_neck_preop,
  eq5d_index_preop       = eq5d_index_preop,
  eq5d_vas_preop         = eq5d_vas_preop,
  ndi_index_preop        = ndi_index_preop,
  myelopathy_index_preop = myelopathy_index_preop,
  pmjoa_index_preop      = pmjoa_index_preop,

  # 12m outcomes
  nrs_arm_12m            = nrs_arm_12m,
  nrs_neck_12m           = nrs_neck_12m,
  eq5d_index_12m         = eq5d_index_12m,
  eq5d_vas_12m           = eq5d_vas_12m,
  ndi_index_12m          = ndi_index_12m,
  myelopathy_index_12m   = myelopathy_index_12m,
  pmjoa_index_12m        = pmjoa_index_12m,
  patient_satisfied_12m  = patient_satisfied_12m,

  # 12m change scores
  delta_nrs_arm_12m      = delta_nrs_arm_12m,
  delta_nrs_neck_12m     = delta_nrs_neck_12m,
  delta_eq5d_12m         = delta_eq5d_12m,
  delta_ndi_12m          = delta_ndi_12m,

  # 12m MCID outcomes
  mcid_arm_12m           = mcid_arm_12m,
  mcid_neck_12m          = mcid_neck_12m,
  mcid_eq5d_12m          = mcid_eq5d_12m,
  mcid_ndi_12m           = mcid_ndi_12m,

  # 24m outcomes
  nrs_arm_24m            = nrs_arm_24m,
  nrs_neck_24m           = nrs_neck_24m,
  eq5d_index_24m         = eq5d_index_24m,
  ndi_index_24m          = ndi_index_24m,
  mcid_arm_24m           = mcid_arm_24m,

  stringsAsFactors = FALSE
)

# Keep rows with minimum required keys (clinic, age, sex, diagnosis)
dat <- dat[!is.na(dat$clinic) & !is.na(dat$age) &
           !is.na(dat$sexM)   & !is.na(dat$diag_grp), ]

cat(sprintf("Clean dataset: %d rows × %d columns\n", nrow(dat), ncol(dat)))

# =====================================================================
# Random country assignment (mimics multi-registry federation)
# =====================================================================
countries <- c("sweden", "norway", "denmark", "finland")
probs     <- c(0.40,     0.25,     0.20,      0.15)

dat$country_site <- sample(countries, nrow(dat), replace = TRUE, prob = probs)

# =====================================================================
# Write one CSV per country
# =====================================================================
dir.create(OUT_DIR, showWarnings = FALSE)

for (ct in countries) {
  sub <- dat[dat$country_site == ct, ]
  out <- file.path(OUT_DIR, paste0(ct, ".csv"))
  write.csv(sub, out, row.names = FALSE, na = "")
  cat(sprintf("  %-8s  %5d rows  →  %s\n", ct, nrow(sub), out))
}

cat(sprintf("\nTotal rows written: %d\n", nrow(dat)))
cat(sprintf("Columns per file:   %d\n",  ncol(dat)))
cat("\nVariables included:\n")
cat(paste(sprintf("  %s", names(dat)[names(dat) != "country_site"]),
          collapse = "\n"), "\n")

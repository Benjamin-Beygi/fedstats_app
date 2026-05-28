# run.R
# =====================================================================
# Swespine cervical-spine cohort - federated analysis
# ---------------------------------------------------------------------
# Reproduces, in-process, a federated study across surgical centres:
# every centre becomes an independent site server (server.R) and the
# client (client.R) combines their aggregate outputs into global
# estimates. No record-level data is ever pooled.
#
# Pipeline:
#   1. load the Swespine workbook
#   2. derive the analysis variables
#   3. split patients by centre  -> one site server each
#   4. federated descriptive statistics
#   5. federated linear regression   (sufficient statistics)
#   6. federated logistic regression (Newton-Raphson, method 3b)
#
# Run from the project root (open federated_statistics.Rproj first):
#   source("run.R")
# =====================================================================

suppressPackageStartupMessages(library(readxl))

source("server.R")
source("client.R")

# ---------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------
DATA_FILE  <- file.path("server/data", "Swespine_cervical_2006_2026_unprotected.xlsx")
SHEET      <- "master_sheet"
MIN_SITE_N <- 20            # centres with fewer patients are excluded

# Swespine column names
COL_CLINIC    <- "DG-Klinik"
COL_SEX       <- "PAT-Kon"          # 1 = male, 2 = female
COL_DIAGNOSIS <- "OpHR-Neuro"       # 2 = radiculopathy, 3 = myelopathy
COL_HEIGHT    <- "PreopHR-Langd"    # cm
COL_WEIGHT    <- "PreopHR-Vikt"     # kg
COL_NRS_PREOP <- "PreopHR-NRSArm"   # arm-pain NRS, pre-operative
COL_NRS_12M   <- "UppHR-NRSArm_1"   # arm-pain NRS, 12-month follow-up
AGE_CANDIDATES <- c("DG-Alder", "Age", "age", "Alder", "Ålder",
                    "PreopHR_Alder", "PreopHR_AlderAr")

RADICULOPATHY <- 2
MYELOPATHY    <- 3

# ---------------------------------------------------------------------
# 1. Load the Swespine workbook
# ---------------------------------------------------------------------
if (!file.exists(DATA_FILE)) {
  stop("Data file not found: ", DATA_FILE,
       "\nPlace the Swespine workbook inside the data/ folder.")
}
raw <- as.data.frame(read_excel(DATA_FILE, sheet = SHEET))

num <- function(x) suppressWarnings(as.numeric(x))

# ---------------------------------------------------------------------
# 2. Derive the analysis variables
# ---------------------------------------------------------------------
age_col <- AGE_CANDIDATES[AGE_CANDIDATES %in% names(raw)][1]
if (is.na(age_col)) stop("No age column found in the workbook.")

age <- num(raw[[age_col]])

sex_code <- num(raw[[COL_SEX]])
sexM     <- ifelse(sex_code == 1, 1, ifelse(sex_code == 2, 0, NA))

diag_code  <- num(raw[[COL_DIAGNOSIS]])
diag_grp   <- ifelse(diag_code == RADICULOPATHY, "radiculopathy",
              ifelse(diag_code == MYELOPATHY,    "myelopathy", NA))
diag_myelo <- ifelse(diag_code == MYELOPATHY,    1,
              ifelse(diag_code == RADICULOPATHY, 0, NA))

height <- num(raw[[COL_HEIGHT]])
weight <- num(raw[[COL_WEIGHT]])
bmi    <- weight / (height / 100)^2
bmi[!is.finite(bmi) | bmi < 10 | bmi > 80] <- NA

nrs_preop <- num(raw[[COL_NRS_PREOP]])
nrs_12m   <- num(raw[[COL_NRS_12M]])
nrs_preop[nrs_preop < 0 | nrs_preop > 10] <- NA
nrs_12m[nrs_12m     < 0 | nrs_12m     > 10] <- NA

# MCID: arm-pain NRS improvement of at least 3 points at 12 months
delta_nrs <- nrs_preop - nrs_12m
mcid_12m  <- ifelse(is.na(delta_nrs), NA, ifelse(delta_nrs >= 3, 1, 0))

clinic <- as.character(raw[[COL_CLINIC]])
clinic[clinic == ""] <- NA

dat <- data.frame(
  clinic       = clinic,
  diag_grp     = diag_grp,
  diag_myelo   = diag_myelo,
  age          = age,
  sexM         = sexM,
  bmi          = bmi,
  nrs_arm_12m  = nrs_12m,
  mcid_arm_12m = mcid_12m,
  stringsAsFactors = FALSE
)

# Keep records with complete keys; per-analysis missingness is handled
# downstream (numeric summaries drop NAs, model.frame uses na.omit).
dat <- dat[!is.na(dat$clinic)     & !is.na(dat$age)      &
           !is.na(dat$sexM)       & !is.na(dat$diag_grp) &
           !is.na(dat$diag_myelo), ]

# ---------------------------------------------------------------------
# 3. Split patients by centre -> one site server each
# ---------------------------------------------------------------------
site_size <- table(dat$clinic)
keep      <- names(site_size)[site_size >= MIN_SITE_N]
dat       <- dat[dat$clinic %in% keep, ]

sites   <- split(dat, dat$clinic)
servers <- lapply(sites, create_server)

cat("=====================================================\n")
cat(" Swespine federated analysis\n")
cat("=====================================================\n")
cat(sprintf("Patients : %d\n", nrow(dat)))
cat(sprintf("Centres  : %d (>= %d patients each)\n",
            length(servers), MIN_SITE_N))
cat(sprintf("Centre sizes: %s\n",
            paste(sort(as.integer(site_size[keep]), decreasing = TRUE),
                  collapse = ", ")))

# ---------------------------------------------------------------------
# 4. Federated descriptive statistics
# ---------------------------------------------------------------------
cat("\n----- Descriptive statistics -----\n")

age_summary <- fed_numeric(servers, "age")
bmi_summary <- fed_numeric(servers, "bmi")
cat(sprintf("Age : n = %4d   mean = %6.2f   sd = %5.2f\n",
            age_summary$n, age_summary$mean, age_summary$sd))
cat(sprintf("BMI : n = %4d   mean = %6.2f   sd = %5.2f\n",
            bmi_summary$n, bmi_summary$mean, bmi_summary$sd))

tt <- fed_welch_t(servers, "age", "diag_grp", "radiculopathy", "myelopathy")
cat("\nWelch t-test - age by diagnosis:\n")
cat(sprintf("  radiculopathy: mean = %.2f (n = %d)\n", tt$mean1, tt$n1))
cat(sprintf("  myelopathy   : mean = %.2f (n = %d)\n", tt$mean2, tt$n2))
cat(sprintf("  t = %.3f, df = %.1f, p = %.4g\n", tt$t, tt$df, tt$p))

chi <- fed_chisq_2x2(servers, "sexM", "diag_myelo", correct = TRUE)
cat("\nChi-square - sex vs diagnosis (Yates corrected):\n")
print(chi$table)
sex_counts <- rowSums(chi$table)
cat(sprintf("  sex: female = %d, male = %d\n",
            sex_counts["sexM=0"], sex_counts["sexM=1"]))
cat(sprintf("  X-squared = %.3f, df = %d, p = %.4g\n",
            chi$statistic, chi$df, chi$p))

# ---------------------------------------------------------------------
# 5. Federated linear regression (sufficient statistics)
# ---------------------------------------------------------------------
cat("\n----- Linear regression: nrs_arm_12m ~ age + sexM -----\n")
linear <- fed_lm(servers, nrs_arm_12m ~ age + sexM)
print(round(data.frame(
  Estimate  = linear$coefficients,
  Std.Error = linear$se,
  t.value   = linear$t,
  p.value   = linear$p
), 6))
cat(sprintf("Residual SE = %.4f on %d degrees of freedom (N = %d).\n",
            linear$sigma, linear$df, linear$N))

# ---------------------------------------------------------------------
# 6. Federated logistic regression (Newton-Raphson, method 3b)
# ---------------------------------------------------------------------
cat("\n----- Logistic regression: mcid_arm_12m ~ age + sexM -----\n")
logit <- fed_logistic_newton(servers, mcid_arm_12m ~ age + sexM,
                             robust_cluster = TRUE)

beta <- logit$coefficients
se   <- logit$se
z    <- beta / se
print(round(data.frame(
  Estimate  = beta,
  Std.Error = se,
  z.value   = z,
  p.value   = 2 * (1 - pnorm(abs(z))),
  OR        = exp(beta),
  CI.lower  = exp(beta - 1.96 * se),
  CI.upper  = exp(beta + 1.96 * se)
), 6))
cat(sprintf("%s in %d Newton iterations | logLik = %.3f | N = %d\n",
            if (logit$converged) "Converged" else "Did NOT converge",
            logit$iterations, logit$logLik, logit$N))

if (!is.null(logit$se_robust)) {
  cat(sprintf("\nCluster-robust SE (CR1; %d centres as clusters):\n",
              logit$clusters))
  z_robust <- beta / logit$se_robust
  print(round(data.frame(
    Estimate  = beta,
    Robust.SE = logit$se_robust,
    z.value   = z_robust,
    p.value   = 2 * (1 - pnorm(abs(z_robust)))
  ), 6))
}

cat("\n=====================================================\n")
cat(" Done.\n")
cat("=====================================================\n")





# run.R
# =====================================================================
# Federated analysis
#
# MODE (set via env var FED_MODE or edit below):
#   local       — simulate federation inside one R session (default)
#   localhost   — real HTTP, sites running on localhost:8001, 8002, ...
#   remote      — real HTTP over Tailscale (or any network)
# =====================================================================

suppressPackageStartupMessages({
  library(readxl)
})

source("server.R")
source("client.R")

MODE      <- Sys.getenv("FED_MODE", "local")
MIN_SITE_N <- 20

DATA_FILE <- file.path(
  "server/data",
  "Swespine_cervical_2006_2026_unprotected.xlsx"
)
SHEET <- "master_sheet"

# ---- Column names ------------------------------------------------
COL_CLINIC    <- "DG-Klinik"
COL_SEX       <- "PAT-Kon"
COL_DIAGNOSIS <- "OpHR-Neuro"
COL_HEIGHT    <- "PreopHR-Langd"
COL_WEIGHT    <- "PreopHR-Vikt"
COL_NRS_PREOP <- "PreopHR-NRSArm"
COL_NRS_12M   <- "UppHR-NRSArm_1"

AGE_CANDIDATES <- c(
  "DG-Alder", "Age", "age", "Alder", "Ålder",
  "PreopHR_Alder", "PreopHR_AlderAr"
)

RADICULOPATHY <- 2
MYELOPATHY    <- 3
num <- function(x) suppressWarnings(as.numeric(x))

# ---- Expected variable specifications (used by fed_validate) -----
VARS_SPEC <- list(
  age           = list(type = "numeric",     min = 18,  max = 100),
  sexM          = list(type = "binary"),
  bmi           = list(type = "numeric",     min = 10,  max = 80),
  nrs_arm_preop = list(type = "numeric",     min = 0,   max = 10),
  nrs_arm_12m   = list(type = "numeric",     min = 0,   max = 10),
  mcid_arm_12m  = list(type = "binary"),
  diag_grp      = list(type = "categorical",
                        levels = c("radiculopathy", "myelopathy")),
  diag_myelo    = list(type = "binary")
)

# ---- Data preparation (local mode only) --------------------------
make_analysis_data <- function(raw) {
  age_col <- AGE_CANDIDATES[AGE_CANDIDATES %in% names(raw)][1]
  if (is.na(age_col)) stop("No age column found.")
  age      <- num(raw[[age_col]])
  sex_code <- num(raw[[COL_SEX]])
  sexM     <- ifelse(sex_code == 1, 1, ifelse(sex_code == 2, 0, NA))
  diag_code <- num(raw[[COL_DIAGNOSIS]])
  diag_grp  <- ifelse(diag_code == RADICULOPATHY, "radiculopathy",
                      ifelse(diag_code == MYELOPATHY,    "myelopathy",   NA))
  diag_myelo <- ifelse(diag_code == MYELOPATHY,    1,
                       ifelse(diag_code == RADICULOPATHY, 0, NA))
  height <- num(raw[[COL_HEIGHT]])
  weight <- num(raw[[COL_WEIGHT]])
  bmi    <- weight / (height / 100)^2
  bmi[!is.finite(bmi) | bmi < 10 | bmi > 80] <- NA
  nrs_preop <- num(raw[[COL_NRS_PREOP]]); nrs_preop[nrs_preop < 0 | nrs_preop > 10] <- NA
  nrs_12m   <- num(raw[[COL_NRS_12M]]);   nrs_12m[nrs_12m   < 0 | nrs_12m   > 10] <- NA
  delta_nrs <- nrs_preop - nrs_12m
  mcid_12m  <- ifelse(is.na(delta_nrs), NA, ifelse(delta_nrs >= 3, 1, 0))
  clinic    <- as.character(raw[[COL_CLINIC]]); clinic[clinic == ""] <- NA
  
  dat <- data.frame(
    clinic = clinic, diag_grp = diag_grp, diag_myelo = diag_myelo,
    age = age, sexM = sexM, bmi = bmi,
    nrs_arm_preop = nrs_preop, nrs_arm_12m = nrs_12m, mcid_arm_12m = mcid_12m,
    stringsAsFactors = FALSE
  )
  dat[!is.na(dat$clinic) & !is.na(dat$age) & !is.na(dat$sexM) &
        !is.na(dat$diag_grp) & !is.na(dat$diag_myelo), ]
}

# ==================================================================
# Server / site setup
# ==================================================================

if (MODE == "local") {
  
  if (!file.exists(DATA_FILE)) stop("Data file not found: ", DATA_FILE)
  raw <- as.data.frame(read_excel(DATA_FILE, sheet = SHEET))
  dat <- make_analysis_data(raw)
  
  site_size <- table(dat$clinic)
  keep      <- names(site_size)[site_size >= MIN_SITE_N]
  dat       <- dat[dat$clinic %in% keep, ]
  
  sites   <- split(dat, dat$clinic)
  servers <- lapply(sites, create_server)
  
  cat("=====================================================\n")
  cat(" Swespine federated analysis — LOCAL SIMULATION\n")
  cat("=====================================================\n")
  cat(sprintf("Patients : %d\n", nrow(dat)))
  cat(sprintf("Centres  : %d\n", length(servers)))
  
} else if (MODE == "localhost") {
  
  # ----------------------------------------------------------------
  # Localhost test: spin up one api_server.R per site on this machine.
  #
  # Before running run.R in this mode, start each site in a separate
  # terminal:
  #   FED_DATA_FILE=site1.csv FED_PORT=8001 Rscript api_server.R
  #   FED_DATA_FILE=site2.csv FED_PORT=8002 Rscript api_server.R
  #   ...
  #
  # Then set:
  #   FED_SITE_URLS=http://localhost:8001,http://localhost:8002
  #   FED_MODE=localhost
  # ----------------------------------------------------------------
  site_urls <- strsplit(Sys.getenv("FED_SITE_URLS"), ",")[[1]]
  site_urls <- trimws(site_urls[nzchar(trimws(site_urls))])
  if (length(site_urls) == 0)
    stop("Set FED_SITE_URLS, e.g. http://localhost:8001,http://localhost:8002")
  
  servers <- lapply(site_urls, create_remote_server)
  
  cat("=====================================================\n")
  cat(" Swespine federated analysis — LOCALHOST TEST\n")
  cat("=====================================================\n")
  cat(sprintf("Local HTTP sites: %d\n", length(servers)))
  
} else if (MODE == "remote") {
  
  # ----------------------------------------------------------------
  # Real remote federation over Tailscale (or any private network).
  # Each site runs:
  #   FED_DATA_FILE=data.csv FED_PORT=8000 FED_TOKEN=secret \
  #     Rscript api_server.R
  #
  # Set on coordinator:
  #   FED_SITE_URLS=http://100.x.y.z:8000,http://100.a.b.c:8000
  #   FED_API_TOKEN=secret     (same token as on sites)
  #   FED_MODE=remote
  # ----------------------------------------------------------------
  site_urls <- strsplit(Sys.getenv("FED_SITE_URLS"), ",")[[1]]
  site_urls <- trimws(site_urls[nzchar(trimws(site_urls))])
  if (length(site_urls) == 0)
    stop("Set FED_SITE_URLS, e.g. http://100.x.y.z:8000,http://100.a.b.c:8000")
  
  servers <- lapply(site_urls, create_remote_server)
  
  cat("=====================================================\n")
  cat(" Swespine federated analysis — REMOTE / Tailscale\n")
  cat("=====================================================\n")
  cat(sprintf("Remote sites: %d\n", length(servers)))
  
} else {
  stop("Unknown FED_MODE: ", MODE, ". Use 'local', 'localhost', or 'remote'.")
}

# ==================================================================
# Pre-analysis validation
# ==================================================================

cat("\n----- Data validation -----\n")
validation <- fed_validate(
  servers,
  VARS_SPEC,
  formula = mcid_arm_12m ~ age + sexM,
  min_n   = MIN_SITE_N
)
print_validation_report(validation)

if (!validation$ok) {
  if (MODE == "local") {
    cat("[INFO] Errors above are from the local simulation (individual clinics with\n")
    cat("[INFO] sparse data). Proceeding — those sites will contribute nothing to\n")
    cat("[INFO] the model (handled gracefully by na.omit inside each server).\n\n")
  } else {
    stop("Validation failed — fix the errors above before running the analysis.")
  }
}

# ==================================================================
# Analysis — identical for all three modes
# ==================================================================

cat("\n----- Descriptive statistics -----\n")

age_summary  <- fed_numeric(servers, "age")
bmi_summary  <- fed_numeric(servers, "bmi")
nrs_pre_summary <- fed_numeric(servers, "nrs_arm_preop")
nrs12_summary   <- fed_numeric(servers, "nrs_arm_12m")
mcid_summary    <- fed_numeric(servers, "mcid_arm_12m")

cat(sprintf("Age: n = %d | mean = %.2f | SD = %.2f\n",
            age_summary$n, age_summary$mean, age_summary$sd))
cat(sprintf("BMI: n = %d | mean = %.2f | SD = %.2f\n",
            bmi_summary$n, bmi_summary$mean, bmi_summary$sd))
cat(sprintf("NRS preop: n = %d | mean = %.2f | SD = %.2f\n",
            nrs_pre_summary$n, nrs_pre_summary$mean, nrs_pre_summary$sd))
cat(sprintf("NRS 12m: n = %d | mean = %.2f | SD = %.2f\n",
            nrs12_summary$n, nrs12_summary$mean, nrs12_summary$sd))
cat(sprintf("MCID achieved: %.0f / %d (%.1f%%)\n",
            mcid_summary$sum, mcid_summary$n, 100 * mcid_summary$mean))

tt <- fed_welch_t(servers, "age", "diag_grp", "radiculopathy", "myelopathy")
cat("\nWelch t-test — age by diagnosis:\n")
cat(sprintf("  radiculopathy mean = %.2f | n = %d\n", tt$mean1, tt$n1))
cat(sprintf("  myelopathy    mean = %.2f | n = %d\n", tt$mean2, tt$n2))
cat(sprintf("  t = %.3f | df = %.1f | p = %.4g\n",   tt$t, tt$df, tt$p))

chi <- fed_chisq_2x2(servers, "sexM", "diag_myelo", correct = TRUE)
cat("\nChi-square — sex vs diagnosis:\n")
print(chi$table)
cat(sprintf("  X-squared = %.3f | df = %d | p = %.4g\n",
            chi$statistic, chi$df, chi$p))

cat("\n----- Linear regression: nrs_arm_12m ~ age + sexM -----\n")
linear <- fed_lm(servers, nrs_arm_12m ~ age + sexM)

table3 <- data.frame(
  Term      = names(linear$coefficients),
  Estimate  = linear$coefficients,
  Std.Error = linear$se,
  t.value   = linear$t,
  p.value   = linear$p
)
table3[, -1] <- round(table3[, -1], 6)
print(table3)
cat(sprintf("N = %d | Residual SE = %.4f | df = %d\n",
            linear$N, linear$sigma, linear$df))

cat("\n----- Logistic regression: mcid_arm_12m ~ age + sexM -----\n")
logit <- fed_logistic_newton(
  servers, mcid_arm_12m ~ age + sexM,
  robust_cluster = TRUE, verbose = FALSE
)

beta <- logit$coefficients
se   <- logit$se
z    <- beta / se
p    <- 2 * (1 - pnorm(abs(z)))

table4 <- data.frame(
  Term      = names(beta),
  Estimate  = beta,
  Std.Error = se,
  z.value   = z,
  p.value   = p,
  OR        = exp(beta),
  CI.lower  = exp(beta - 1.96 * se),
  CI.upper  = exp(beta + 1.96 * se)
)
table4[, -1] <- round(table4[, -1], 6)
print(table4)
cat(sprintf("N = %d | logLik = %.3f | iterations = %d | converged = %s\n",
            logit$N, logit$logLik, logit$iterations, logit$converged))

# ------------------------------------------------------------------
# Local-only validation against pooled glm / lm
# ------------------------------------------------------------------
if (MODE == "local") {
  cat("\n----- Local pooled validation -----\n")
  pooled_lm  <- lm(nrs_arm_12m ~ age + sexM,
                   data = na.omit(dat[, c("nrs_arm_12m", "age", "sexM")]))
  pooled_glm <- glm(mcid_arm_12m ~ age + sexM, family = binomial(),
                    data = na.omit(dat[, c("mcid_arm_12m", "age", "sexM")]))
  lm_diff  <- max(abs(linear$coefficients - coef(pooled_lm)))
  glm_diff <- max(abs(logit$coefficients  - coef(pooled_glm)))
  cat(sprintf("Max LM  coefficient difference : %.2e\n", lm_diff))
  cat(sprintf("Max GLM coefficient difference : %.2e\n", glm_diff))
}

cat("\n=====================================================\n")
cat(" Done.\n")
cat("=====================================================\n")








# =====================================================
# OPTIONAL: CREATE 4 THEORETICAL COUNTRY DATASETS
# =====================================================

# cat("\n=====================================================\n")
# cat(" Creating theoretical country datasets\n")
# cat("=====================================================\n")
# 
# OUT_DIR <- "federated_sites"
# dir.create(OUT_DIR, showWarnings = FALSE)
# 
# set.seed(2026)
# 
# # Randomly split current analysis dataset into 4 theoretical registries
# country_labels <- c("sweden", "norway", "denmark", "finland")
# 
# dat_country <- dat
# dat_country$country_site <- sample(
#   country_labels,
#   size = nrow(dat_country),
#   replace = TRUE,
#   prob = c(0.40, 0.25, 0.20, 0.15)
# )
# 
# for (ct in country_labels) {
#   country_data <- dat_country[dat_country$country_site == ct, ]
#   
#   out_file <- file.path(OUT_DIR, paste0(ct, ".csv"))
#   
#   write.csv(
#     country_data,
#     file = out_file,
#     row.names = FALSE
#   )
#   
#   cat(sprintf(
#     "%-8s: %5d rows -> %s\n",
#     ct,
#     nrow(country_data),
#     out_file
#   ))
# }
# 
# cat("=====================================================\n")
# cat(" Done creating theoretical country datasets.\n")
# cat("=====================================================\n")
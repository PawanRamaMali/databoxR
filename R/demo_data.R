#' Generate Synthetic SDTM Demo Data
#'
#' Creates synthetic clinical trial datasets for demonstration purposes
#'
#' @param n_subjects Number of subjects (default: 100)
#' @return List of demo datasets (DM, AE, VS)
#' @export
generate_demo_data <- function(n_subjects = 100) {
  
  # Generate Demographics (DM) data
  dm_data <- data.frame(
    STUDYID = rep("DEMO001", n_subjects),
    DOMAIN = rep("DM", n_subjects),
    USUBJID = paste0("DEMO001-", sprintf("%03d", 1:n_subjects)),
    SUBJID = sprintf("%03d", 1:n_subjects),
    RFSTDTC = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), n_subjects),
    RFENDTC = NA,
    SITEID = sample(sprintf("SITE%02d", 1:10), n_subjects, replace = TRUE),
    AGE = sample(18:80, n_subjects, replace = TRUE),
    AGEU = rep("YEARS", n_subjects),
    SEX = sample(c("M", "F"), n_subjects, replace = TRUE, prob = c(0.48, 0.52)),
    RACE = sample(c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER"), 
                  n_subjects, replace = TRUE, prob = c(0.7, 0.15, 0.1, 0.05)),
    ETHNIC = sample(c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"), 
                    n_subjects, replace = TRUE, prob = c(0.85, 0.15)),
    ARMCD = sample(c("TRT", "PLB"), n_subjects, replace = TRUE),
    ARM = ifelse(sample(c("TRT", "PLB"), n_subjects, replace = TRUE) == "TRT", 
                 "Treatment", "Placebo"),
    ACTARMCD = sample(c("TRT", "PLB"), n_subjects, replace = TRUE),
    ACTARM = ifelse(sample(c("TRT", "PLB"), n_subjects, replace = TRUE) == "TRT", 
                    "Treatment", "Placebo"),
    stringsAsFactors = FALSE
  )
  
  # Generate Adverse Events (AE) data
  ae_terms <- c("Headache", "Nausea", "Dizziness", "Fatigue", "Diarrhea", 
                "Constipation", "Insomnia", "Anxiety", "Cough", "Rash")
  body_systems <- c("NERVOUS SYSTEM DISORDERS", "GASTROINTESTINAL DISORDERS",
                    "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                    "PSYCHIATRIC DISORDERS", "RESPIRATORY DISORDERS", 
                    "SKIN AND SUBCUTANEOUS TISSUE DISORDERS")
  
  # Each subject has 0-3 AEs
  ae_counts <- sample(0:3, n_subjects, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
  total_aes <- sum(ae_counts)
  
  if (total_aes > 0) {
    ae_subjects <- rep(1:n_subjects, ae_counts)
    ae_data <- data.frame(
      STUDYID = rep("DEMO001", total_aes),
      DOMAIN = rep("AE", total_aes),
      USUBJID = paste0("DEMO001-", sprintf("%03d", ae_subjects)),
      AESEQ = unlist(sapply(ae_counts[ae_counts > 0], function(x) 1:x)),
      AETERM = sample(ae_terms, total_aes, replace = TRUE),
      AEDECOD = sample(ae_terms, total_aes, replace = TRUE),
      AEBODSYS = sample(body_systems, total_aes, replace = TRUE),
      AESEV = sample(c("MILD", "MODERATE", "SEVERE"), total_aes, 
                     replace = TRUE, prob = c(0.6, 0.3, 0.1)),
      AESER = sample(c("N", "Y"), total_aes, replace = TRUE, prob = c(0.95, 0.05)),
      AEREL = sample(c("NOT RELATED", "UNLIKELY", "POSSIBLE", "PROBABLE", "DEFINITE"),
                     total_aes, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.1, 0.05)),
      AEACN = sample(c("NONE", "DOSE REDUCED", "DRUG WITHDRAWN"), 
                     total_aes, replace = TRUE, prob = c(0.8, 0.15, 0.05)),
      AEOUT = sample(c("RECOVERED/RESOLVED", "RECOVERING/RESOLVING", "NOT RECOVERED/NOT RESOLVED", "ONGOING"),
                     total_aes, replace = TRUE, prob = c(0.7, 0.15, 0.1, 0.05)),
      AESTDTC = sample(seq(as.Date("2023-02-01"), as.Date("2023-11-30"), by = "day"), 
                       total_aes, replace = TRUE),
      AEENDTC = NA,
      stringsAsFactors = FALSE
    )
  } else {
    # Create empty AE dataset with proper structure
    ae_data <- data.frame(
      STUDYID = character(0), DOMAIN = character(0), USUBJID = character(0),
      AESEQ = numeric(0), AETERM = character(0), AEDECOD = character(0),
      AEBODSYS = character(0), AESEV = character(0), AESER = character(0),
      AEREL = character(0), AEACN = character(0), AEOUT = character(0),
      AESTDTC = as.Date(character(0)), AEENDTC = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
  }
  
  # Generate Vital Signs (VS) data
  vs_tests <- c("SYSBP", "DIABP", "PULSE", "TEMP", "RESP", "WEIGHT", "HEIGHT")
  vs_testnames <- c("Systolic Blood Pressure", "Diastolic Blood Pressure", 
                    "Pulse Rate", "Temperature", "Respiratory Rate", 
                    "Weight", "Height")
  vs_units <- c("mmHg", "mmHg", "beats/min", "C", "breaths/min", "kg", "cm")
  
  # Each subject has multiple VS measurements
  vs_per_subject <- 4  # 4 visits per subject
  total_vs <- n_subjects * length(vs_tests) * vs_per_subject
  
  vs_data <- data.frame(
    STUDYID = rep("DEMO001", total_vs),
    DOMAIN = rep("VS", total_vs),
    USUBJID = rep(paste0("DEMO001-", sprintf("%03d", 1:n_subjects)), 
                  each = length(vs_tests) * vs_per_subject),
    VSSEQ = rep(1:(length(vs_tests) * vs_per_subject), n_subjects),
    VSTESTCD = rep(rep(vs_tests, vs_per_subject), n_subjects),
    VSTEST = rep(rep(vs_testnames, vs_per_subject), n_subjects),
    VSORRES = NA,
    VSORRESU = rep(rep(vs_units, vs_per_subject), n_subjects),
    VSSTRESC = NA,
    VSSTRESN = NA,
    VSSTRESU = rep(rep(vs_units, vs_per_subject), n_subjects),
    VISITNUM = rep(rep(1:vs_per_subject, each = length(vs_tests)), n_subjects),
    VISIT = rep(rep(paste("Visit", 1:vs_per_subject), each = length(vs_tests)), n_subjects),
    VSDTC = rep(sample(seq(as.Date("2023-01-15"), as.Date("2023-12-15"), by = "day"), 
                       vs_per_subject, replace = TRUE), each = length(vs_tests) * n_subjects),
    stringsAsFactors = FALSE
  )
  
  # Add realistic vital signs values
  for (i in 1:nrow(vs_data)) {
    test <- vs_data$VSTESTCD[i]
    if (test == "SYSBP") {
      val <- round(rnorm(1, 120, 15))
    } else if (test == "DIABP") {
      val <- round(rnorm(1, 80, 10))
    } else if (test == "PULSE") {
      val <- round(rnorm(1, 70, 12))
    } else if (test == "TEMP") {
      val <- round(rnorm(1, 36.5, 0.5), 1)
    } else if (test == "RESP") {
      val <- round(rnorm(1, 16, 3))
    } else if (test == "WEIGHT") {
      val <- round(rnorm(1, 70, 15), 1)
    } else if (test == "HEIGHT") {
      val <- round(rnorm(1, 170, 10))
    } else {
      val <- NA
    }
    vs_data$VSORRES[i] <- as.character(val)
    vs_data$VSSTRESC[i] <- as.character(val)
    vs_data$VSSTRESN[i] <- val
  }
  
  return(list(
    DM = dm_data,
    AE = ae_data,
    VS = vs_data
  ))
}

#' Get Demo Dataset by Name
#'
#' Retrieve a specific demo dataset
#'
#' @param dataset_name Name of dataset ("DM", "AE", "VS")
#' @param n_subjects Number of subjects for generation
#' @return data.frame with demo data
#' @export
get_demo_dataset <- function(dataset_name = "DM", n_subjects = 100) {
  demo_data <- generate_demo_data(n_subjects)
  
  if (toupper(dataset_name) %in% names(demo_data)) {
    return(demo_data[[toupper(dataset_name)]])
  } else {
    stop("Dataset name must be one of: DM, AE, VS")
  }
}
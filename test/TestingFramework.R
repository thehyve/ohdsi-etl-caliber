initFramework <- function() {
  insertDf <- data.frame(table = character(), sql = character(), stringsAsFactors = FALSE)
  insertDf[nrow(insertDf) + 1,] <- c('patient', 'TRUNCATE TABLE patient;')
  insertDf[nrow(insertDf) + 1,] <- c('practice', 'TRUNCATE TABLE practice;')
  insertDf[nrow(insertDf) + 1,] <- c('consultation', 'TRUNCATE TABLE consultation;')
  insertDf[nrow(insertDf) + 1,] <- c('clinical', 'TRUNCATE TABLE clinical;')
  insertDf[nrow(insertDf) + 1,] <- c('referral', 'TRUNCATE TABLE referral;')
  insertDf[nrow(insertDf) + 1,] <- c('test', 'TRUNCATE TABLE test;')
  insertDf[nrow(insertDf) + 1,] <- c('immunisation', 'TRUNCATE TABLE immunisation;')
  insertDf[nrow(insertDf) + 1,] <- c('therapy', 'TRUNCATE TABLE therapy;')
  insertDf[nrow(insertDf) + 1,] <- c('additional', 'TRUNCATE TABLE additional;')
  insertDf[nrow(insertDf) + 1,] <- c('ons_death', 'TRUNCATE TABLE ons_death;')
  insertDf[nrow(insertDf) + 1,] <- c('ons_imd', 'TRUNCATE TABLE ons_imd;')
  insertDf[nrow(insertDf) + 1,] <- c('hes_patient', 'TRUNCATE TABLE hes_patient;')
  insertDf[nrow(insertDf) + 1,] <- c('hes_diag_hosp', 'TRUNCATE TABLE hes_diag_hosp;')
  insertDf[nrow(insertDf) + 1,] <- c('hes_proc_epi', 'TRUNCATE TABLE hes_proc_epi;')
  insertDf[nrow(insertDf) + 1,] <- c('hes_diag_epi', 'TRUNCATE TABLE hes_diag_epi;')
  insertDf[nrow(insertDf) + 1,] <- c('hes_op_clinical', 'TRUNCATE TABLE hes_op_clinical;')
  assign('insertDf', insertDf, envir = globalenv())
  testSql <- c()
  testSql <- c(testSql, "IF OBJECT_ID('test_results', 'U') IS NOT NULL DROP TABLE test_results;")
  testSql <- c(testSql, '')
  testSql <- c(testSql, "CREATE TABLE test_results (id INT, description VARCHAR(512), test VARCHAR(256), status VARCHAR(5));")
  testSql <- c(testSql, '')
  assign('testSql', testSql, envir = globalenv())
  assign('testId', 1, envir = globalenv())
  assign('testDescription', '', envir = globalenv())

  defaultValues <- new.env(parent = globalenv())
  assign('defaultValues', defaultValues, envir = globalenv())

  defaults <- list()
  defaults$pracid <- '463'
  defaults$gender <- '2'
  defaults$yob <- '1971'
  defaults$mob <- '0'
  defaults$crd <- '1948-07-05'
  defaults$marital <- '6'
  defaults$famnum <- '658'
  defaults$chsreg <- '2'
  defaults$prescr <- '0'
  defaults$capsup <- '0'
  defaults$ses <- '0'
  defaults$frd <- '1948-07-05'
  defaults$regstat <- '0'
  defaults$reggap <- '0'
  defaults$internal <- '0'
  defaults$toreason <- '0'
  defaults$accept <- '1'
  defaults$prac_region <- '8'
  defaults$prac_lcd <- '2016-12-19'
  defaults$prac_uts <- '1996-10-02'
  defaults$linkable <- '1'
  assign('patient', defaults, envir = defaultValues)

  defaults <- list()
  defaults$region <- '9'
  defaults$lcd <- '2016-12-19'
  assign('practice', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '7969436'
  defaults$constype <- 'Surgery consultation'
  defaults$consid <- '23316'
  defaults$staffid <- '0'
  defaults$duration <- '0'
  assign('consultation', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '7514463'
  defaults$constype <- 'Diagnosis'
  defaults$consid <- '1277697'
  defaults$staffid <- '0'
  defaults$episode <- 'Other'
  defaults$medcode <- '1'
  defaults$enttype <- '2'
  defaults$adid <- '0'
  assign('clinical', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '14895632'
  defaults$eventdate <- '2009-04-27'
  defaults$constype <- 'Intervention'
  defaults$consid <- '47886'
  defaults$staffid <- '0'
  defaults$source <- 'GP Referral'
  defaults$medcode <- '91'
  defaults$nhsspec <- 'No Data Entered'
  defaults$fhsaspec <- 'Pathology'
  defaults$inpatient <- 'Out Patient'
  defaults$attendance <- 'Data Not Entered'
  defaults$urgency <- 'Data Not Entered'
  assign('referral', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '7690607'
  defaults$eventdate <- '2010-01-19'
  defaults$consid <- '2296793'
  defaults$medcode <- '5'
  defaults$constype <- 'Examination'
  defaults$enttype <- '165'
  defaults$staffid <- '0'
  defaults$data1 <- '3'
  defaults$data3 <- '96'
  defaults$data4 <- '0'
  defaults$data7 <- '0'
  assign('test', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '10055293'
  defaults$eventdate <- '2007-10-13'
  defaults$constype <- 'Intervention'
  defaults$consid <- '3696832'
  defaults$staffid <- '0'
  defaults$immstype <- 'FLU'
  defaults$medcode <- '6'
  defaults$stage <- '0'
  defaults$status <- 'Given'
  defaults$compound <- 'Data Not Entered'
  defaults$source <- 'In this Practice'
  defaults$reason <- 'Routine Measure'
  defaults$method <- 'Intramuscular'
  defaults$batch <- '0'
  assign('immunisation', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '1481442'
  defaults$eventdate <- '2012-12-17'
  defaults$numdays <- '0'
  defaults$prodcode <- '3'
  defaults$consid <- '1811207'
  defaults$staffid <- '0'
  defaults$bnfcode <- '3010101'
  defaults$qty <- '28.0000000000000'
  defaults$ndd <- '1.0000000000000'
  defaults$numpacks <- '0'
  defaults$packtype <- 'TABLET(S)'
  defaults$issueseq <- '0'
  assign('therapy', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '24318149'
  defaults$enttype <- '1'
  defaults$adid <- '13691'
  defaults$data1 <- '1'
  defaults$data2 <- '0'
  assign('additional', defaults, envir = defaultValues)

  defaults <- list()
  defaults$pracid <- '416'
  defaults$match_rank <- '1'
  defaults$dod <- '2014-11-28'
  defaults$cause <- 'C34.9'
  defaults$cause1 <- 'J18.0'
  assign('ons_death', defaults, envir = defaultValues)

  defaults <- list()
  defaults$pracid <- '229'
  defaults$imd <- '1'
  assign('ons_imd', defaults, envir = defaultValues)

  defaults <- list()
  defaults$pracid <- '670'
  defaults$gen_hesid <- '1610640'
  defaults$n_patid_hes <- '1'
  defaults$gen_ethnicity <- 'White'
  defaults$match_rank <- '1'
  assign('hes_patient', defaults, envir = defaultValues)

  defaults <- list()
  defaults$spno <- '28521421'
  defaults$patid <- '25763246'
  defaults$admidate <- '2014-04-07'
  defaults$icd <- 'I10'
  assign('hes_diag_hosp', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '13190042'
  defaults$spno <- '12477299'
  defaults$epikey <- '110773063428'
  defaults$admidate <- '2015-05-11'
  defaults$epistart <- '2015-06-09'
  defaults$epiend <- '2015-02-13'
  defaults$opcs <- 'X403'
  defaults$p_order <- '1'
  assign('hes_proc_epi', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '25763246'
  defaults$spno <- '27717462'
  defaults$epikey <- '502660782828'
  defaults$epistart <- '2014-04-07'
  defaults$epiend <- '2015-08-25'
  defaults$icd <- 'I10'
  defaults$d_order <- '1'
  assign('hes_diag_epi', defaults, envir = defaultValues)

  defaults <- list()
  defaults$patid <- '9668022'
  defaults$attendkey <- '309023494104'
  defaults$operstat <- '8'
  defaults$tretspef <- '320'
  defaults$mainspef <- '110'
  defaults$hes_yr <- '2014'
  defaults$diag_01 <- 'R69X6'
  defaults$opertn_01 <- 'X997'
  assign('hes_op_clinical', defaults, envir = defaultValues)
}

initFramework()

generateInsertSql <- function()
{
  insertSql <<- c()
  tables <- unique(insertDf$table)
  lapply(tables, function(t) {
    insertSql <<- c(insertSql, subset(insertDf, table == t)$sql)
  })
  insertSql
}

declareTest <- function(id, description) {
  assign('testId', id, envir = globalenv()) 
  assign('testDescription', description, envir = globalenv()) 
  assign('testNewAdded', TRUE, envir = globalenv()) 
  assign('testNewExpected', TRUE, envir = globalenv()) 
}

set_defaults_patient <- function(patid, pracid, gender, yob, mob, crd, tod, deathdate, marital, famnum, chsreg, chsdate, prescr, capsup, ses, frd, regstat, reggap, internal, toreason, accept, prac_region, prac_lcd, prac_uts, linkable) {
  defaults <- get('patient', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(pracid)) {
    defaults$pracid <- pracid
  }
  if (!missing(gender)) {
    defaults$gender <- gender
  }
  if (!missing(yob)) {
    defaults$yob <- yob
  }
  if (!missing(mob)) {
    defaults$mob <- mob
  }
  if (!missing(crd)) {
    defaults$crd <- crd
  }
  if (!missing(tod)) {
    defaults$tod <- tod
  }
  if (!missing(deathdate)) {
    defaults$deathdate <- deathdate
  }
  if (!missing(marital)) {
    defaults$marital <- marital
  }
  if (!missing(famnum)) {
    defaults$famnum <- famnum
  }
  if (!missing(chsreg)) {
    defaults$chsreg <- chsreg
  }
  if (!missing(chsdate)) {
    defaults$chsdate <- chsdate
  }
  if (!missing(prescr)) {
    defaults$prescr <- prescr
  }
  if (!missing(capsup)) {
    defaults$capsup <- capsup
  }
  if (!missing(ses)) {
    defaults$ses <- ses
  }
  if (!missing(frd)) {
    defaults$frd <- frd
  }
  if (!missing(regstat)) {
    defaults$regstat <- regstat
  }
  if (!missing(reggap)) {
    defaults$reggap <- reggap
  }
  if (!missing(internal)) {
    defaults$internal <- internal
  }
  if (!missing(toreason)) {
    defaults$toreason <- toreason
  }
  if (!missing(accept)) {
    defaults$accept <- accept
  }
  if (!missing(prac_region)) {
    defaults$prac_region <- prac_region
  }
  if (!missing(prac_lcd)) {
    defaults$prac_lcd <- prac_lcd
  }
  if (!missing(prac_uts)) {
    defaults$prac_uts <- prac_uts
  }
  if (!missing(linkable)) {
    defaults$linkable <- linkable
  }
  assign('patient', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_practice <- function(pracid, region, uts, lcd) {
  defaults <- get('practice', envir = defaultValues)
  if (!missing(pracid)) {
    defaults$pracid <- pracid
  }
  if (!missing(region)) {
    defaults$region <- region
  }
  if (!missing(uts)) {
    defaults$uts <- uts
  }
  if (!missing(lcd)) {
    defaults$lcd <- lcd
  }
  assign('practice', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_consultation <- function(patid, eventdate, constype, consid, staffid, duration) {
  defaults <- get('consultation', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(eventdate)) {
    defaults$eventdate <- eventdate
  }
  if (!missing(constype)) {
    defaults$constype <- constype
  }
  if (!missing(consid)) {
    defaults$consid <- consid
  }
  if (!missing(staffid)) {
    defaults$staffid <- staffid
  }
  if (!missing(duration)) {
    defaults$duration <- duration
  }
  assign('consultation', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_clinical <- function(patid, eventdate, constype, consid, staffid, episode, medcode, enttype, adid, data1, data2, data3, data4, data5, data6, data7) {
  defaults <- get('clinical', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(eventdate)) {
    defaults$eventdate <- eventdate
  }
  if (!missing(constype)) {
    defaults$constype <- constype
  }
  if (!missing(consid)) {
    defaults$consid <- consid
  }
  if (!missing(staffid)) {
    defaults$staffid <- staffid
  }
  if (!missing(episode)) {
    defaults$episode <- episode
  }
  if (!missing(medcode)) {
    defaults$medcode <- medcode
  }
  if (!missing(enttype)) {
    defaults$enttype <- enttype
  }
  if (!missing(adid)) {
    defaults$adid <- adid
  }
  if (!missing(data1)) {
    defaults$data1 <- data1
  }
  if (!missing(data2)) {
    defaults$data2 <- data2
  }
  if (!missing(data3)) {
    defaults$data3 <- data3
  }
  if (!missing(data4)) {
    defaults$data4 <- data4
  }
  if (!missing(data5)) {
    defaults$data5 <- data5
  }
  if (!missing(data6)) {
    defaults$data6 <- data6
  }
  if (!missing(data7)) {
    defaults$data7 <- data7
  }
  assign('clinical', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_referral <- function(patid, eventdate, constype, consid, staffid, source, medcode, nhsspec, fhsaspec, inpatient, attendance, urgency) {
  defaults <- get('referral', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(eventdate)) {
    defaults$eventdate <- eventdate
  }
  if (!missing(constype)) {
    defaults$constype <- constype
  }
  if (!missing(consid)) {
    defaults$consid <- consid
  }
  if (!missing(staffid)) {
    defaults$staffid <- staffid
  }
  if (!missing(source)) {
    defaults$source <- source
  }
  if (!missing(medcode)) {
    defaults$medcode <- medcode
  }
  if (!missing(nhsspec)) {
    defaults$nhsspec <- nhsspec
  }
  if (!missing(fhsaspec)) {
    defaults$fhsaspec <- fhsaspec
  }
  if (!missing(inpatient)) {
    defaults$inpatient <- inpatient
  }
  if (!missing(attendance)) {
    defaults$attendance <- attendance
  }
  if (!missing(urgency)) {
    defaults$urgency <- urgency
  }
  assign('referral', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_test <- function(patid, eventdate, consid, medcode, constype, textid, enttype, staffid, data1, data2, data3, data4, data5, data6, data7, data8) {
  defaults <- get('test', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(eventdate)) {
    defaults$eventdate <- eventdate
  }
  if (!missing(consid)) {
    defaults$consid <- consid
  }
  if (!missing(medcode)) {
    defaults$medcode <- medcode
  }
  if (!missing(constype)) {
    defaults$constype <- constype
  }
  if (!missing(textid)) {
    defaults$textid <- textid
  }
  if (!missing(enttype)) {
    defaults$enttype <- enttype
  }
  if (!missing(staffid)) {
    defaults$staffid <- staffid
  }
  if (!missing(data1)) {
    defaults$data1 <- data1
  }
  if (!missing(data2)) {
    defaults$data2 <- data2
  }
  if (!missing(data3)) {
    defaults$data3 <- data3
  }
  if (!missing(data4)) {
    defaults$data4 <- data4
  }
  if (!missing(data5)) {
    defaults$data5 <- data5
  }
  if (!missing(data6)) {
    defaults$data6 <- data6
  }
  if (!missing(data7)) {
    defaults$data7 <- data7
  }
  if (!missing(data8)) {
    defaults$data8 <- data8
  }
  assign('test', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_immunisation <- function(patid, eventdate, sysdate, constype, consid, staffid, immstype, medcode, stage, status, compound, source, reason, method, batch) {
  defaults <- get('immunisation', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(eventdate)) {
    defaults$eventdate <- eventdate
  }
  if (!missing(sysdate)) {
    defaults$sysdate <- sysdate
  }
  if (!missing(constype)) {
    defaults$constype <- constype
  }
  if (!missing(consid)) {
    defaults$consid <- consid
  }
  if (!missing(staffid)) {
    defaults$staffid <- staffid
  }
  if (!missing(immstype)) {
    defaults$immstype <- immstype
  }
  if (!missing(medcode)) {
    defaults$medcode <- medcode
  }
  if (!missing(stage)) {
    defaults$stage <- stage
  }
  if (!missing(status)) {
    defaults$status <- status
  }
  if (!missing(compound)) {
    defaults$compound <- compound
  }
  if (!missing(source)) {
    defaults$source <- source
  }
  if (!missing(reason)) {
    defaults$reason <- reason
  }
  if (!missing(method)) {
    defaults$method <- method
  }
  if (!missing(batch)) {
    defaults$batch <- batch
  }
  assign('immunisation', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_therapy <- function(patid, eventdate, numdays, prodcode, consid, staffid, bnfcode, qty, ndd, numpacks, packtype, issueseq) {
  defaults <- get('therapy', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(eventdate)) {
    defaults$eventdate <- eventdate
  }
  if (!missing(numdays)) {
    defaults$numdays <- numdays
  }
  if (!missing(prodcode)) {
    defaults$prodcode <- prodcode
  }
  if (!missing(consid)) {
    defaults$consid <- consid
  }
  if (!missing(staffid)) {
    defaults$staffid <- staffid
  }
  if (!missing(bnfcode)) {
    defaults$bnfcode <- bnfcode
  }
  if (!missing(qty)) {
    defaults$qty <- qty
  }
  if (!missing(ndd)) {
    defaults$ndd <- ndd
  }
  if (!missing(numpacks)) {
    defaults$numpacks <- numpacks
  }
  if (!missing(packtype)) {
    defaults$packtype <- packtype
  }
  if (!missing(issueseq)) {
    defaults$issueseq <- issueseq
  }
  assign('therapy', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_additional <- function(patid, enttype, adid, data1, data2, data3, data4, data5, data6, data7) {
  defaults <- get('additional', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(enttype)) {
    defaults$enttype <- enttype
  }
  if (!missing(adid)) {
    defaults$adid <- adid
  }
  if (!missing(data1)) {
    defaults$data1 <- data1
  }
  if (!missing(data2)) {
    defaults$data2 <- data2
  }
  if (!missing(data3)) {
    defaults$data3 <- data3
  }
  if (!missing(data4)) {
    defaults$data4 <- data4
  }
  if (!missing(data5)) {
    defaults$data5 <- data5
  }
  if (!missing(data6)) {
    defaults$data6 <- data6
  }
  if (!missing(data7)) {
    defaults$data7 <- data7
  }
  assign('additional', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_ons_death <- function(patid, pracid, match_rank, dod, dod_partial, cause, cause1, cause2, cause3, cause4, cause5, cause6, cause7, cause8, cause9, cause10, cause11, cause12, cause13, cause14, cause15, cause_neonatal1, cause_neonatal2, cause_neonatal3, cause_neonatal4, cause_neonatal5, cause_neonatal6, cause_neonatal7, cause_neonatal8) {
  defaults <- get('ons_death', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(pracid)) {
    defaults$pracid <- pracid
  }
  if (!missing(match_rank)) {
    defaults$match_rank <- match_rank
  }
  if (!missing(dod)) {
    defaults$dod <- dod
  }
  if (!missing(dod_partial)) {
    defaults$dod_partial <- dod_partial
  }
  if (!missing(cause)) {
    defaults$cause <- cause
  }
  if (!missing(cause1)) {
    defaults$cause1 <- cause1
  }
  if (!missing(cause2)) {
    defaults$cause2 <- cause2
  }
  if (!missing(cause3)) {
    defaults$cause3 <- cause3
  }
  if (!missing(cause4)) {
    defaults$cause4 <- cause4
  }
  if (!missing(cause5)) {
    defaults$cause5 <- cause5
  }
  if (!missing(cause6)) {
    defaults$cause6 <- cause6
  }
  if (!missing(cause7)) {
    defaults$cause7 <- cause7
  }
  if (!missing(cause8)) {
    defaults$cause8 <- cause8
  }
  if (!missing(cause9)) {
    defaults$cause9 <- cause9
  }
  if (!missing(cause10)) {
    defaults$cause10 <- cause10
  }
  if (!missing(cause11)) {
    defaults$cause11 <- cause11
  }
  if (!missing(cause12)) {
    defaults$cause12 <- cause12
  }
  if (!missing(cause13)) {
    defaults$cause13 <- cause13
  }
  if (!missing(cause14)) {
    defaults$cause14 <- cause14
  }
  if (!missing(cause15)) {
    defaults$cause15 <- cause15
  }
  if (!missing(cause_neonatal1)) {
    defaults$cause_neonatal1 <- cause_neonatal1
  }
  if (!missing(cause_neonatal2)) {
    defaults$cause_neonatal2 <- cause_neonatal2
  }
  if (!missing(cause_neonatal3)) {
    defaults$cause_neonatal3 <- cause_neonatal3
  }
  if (!missing(cause_neonatal4)) {
    defaults$cause_neonatal4 <- cause_neonatal4
  }
  if (!missing(cause_neonatal5)) {
    defaults$cause_neonatal5 <- cause_neonatal5
  }
  if (!missing(cause_neonatal6)) {
    defaults$cause_neonatal6 <- cause_neonatal6
  }
  if (!missing(cause_neonatal7)) {
    defaults$cause_neonatal7 <- cause_neonatal7
  }
  if (!missing(cause_neonatal8)) {
    defaults$cause_neonatal8 <- cause_neonatal8
  }
  assign('ons_death', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_ons_imd <- function(patid, pracid, imd) {
  defaults <- get('ons_imd', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(pracid)) {
    defaults$pracid <- pracid
  }
  if (!missing(imd)) {
    defaults$imd <- imd
  }
  assign('ons_imd', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_hes_patient <- function(patid, pracid, gen_hesid, n_patid_hes, gen_ethnicity, match_rank) {
  defaults <- get('hes_patient', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(pracid)) {
    defaults$pracid <- pracid
  }
  if (!missing(gen_hesid)) {
    defaults$gen_hesid <- gen_hesid
  }
  if (!missing(n_patid_hes)) {
    defaults$n_patid_hes <- n_patid_hes
  }
  if (!missing(gen_ethnicity)) {
    defaults$gen_ethnicity <- gen_ethnicity
  }
  if (!missing(match_rank)) {
    defaults$match_rank <- match_rank
  }
  assign('hes_patient', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_hes_diag_hosp <- function(spno, patid, admidate, discharged, icd, icdx) {
  defaults <- get('hes_diag_hosp', envir = defaultValues)
  if (!missing(spno)) {
    defaults$spno <- spno
  }
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(admidate)) {
    defaults$admidate <- admidate
  }
  if (!missing(discharged)) {
    defaults$discharged <- discharged
  }
  if (!missing(icd)) {
    defaults$icd <- icd
  }
  if (!missing(icdx)) {
    defaults$icdx <- icdx
  }
  assign('hes_diag_hosp', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_hes_proc_epi <- function(patid, spno, epikey, admidate, epistart, epiend, discharged, opcs, evdate, p_order) {
  defaults <- get('hes_proc_epi', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(spno)) {
    defaults$spno <- spno
  }
  if (!missing(epikey)) {
    defaults$epikey <- epikey
  }
  if (!missing(admidate)) {
    defaults$admidate <- admidate
  }
  if (!missing(epistart)) {
    defaults$epistart <- epistart
  }
  if (!missing(epiend)) {
    defaults$epiend <- epiend
  }
  if (!missing(discharged)) {
    defaults$discharged <- discharged
  }
  if (!missing(opcs)) {
    defaults$opcs <- opcs
  }
  if (!missing(evdate)) {
    defaults$evdate <- evdate
  }
  if (!missing(p_order)) {
    defaults$p_order <- p_order
  }
  assign('hes_proc_epi', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_hes_diag_epi <- function(patid, spno, epikey, epistart, epiend, icd, icdx, d_order) {
  defaults <- get('hes_diag_epi', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(spno)) {
    defaults$spno <- spno
  }
  if (!missing(epikey)) {
    defaults$epikey <- epikey
  }
  if (!missing(epistart)) {
    defaults$epistart <- epistart
  }
  if (!missing(epiend)) {
    defaults$epiend <- epiend
  }
  if (!missing(icd)) {
    defaults$icd <- icd
  }
  if (!missing(icdx)) {
    defaults$icdx <- icdx
  }
  if (!missing(d_order)) {
    defaults$d_order <- d_order
  }
  assign('hes_diag_epi', defaults, envir = defaultValues)
  invisible(defaults)
}

set_defaults_hes_op_clinical <- function(patid, attendkey, operstat, tretspef, mainspef, hes_yr, diag_01, diag_02, diag_03, diag_04, diag_05, diag_06, diag_07, diag_08, diag_09, diag_10, diag_11, diag_12, opertn_01, opertn_02, opertn_03, opertn_04, opertn_05, opertn_06, opertn_07, opertn_08, opertn_09, opertn_10, opertn_11, opertn_12, opertn_13, opertn_14, opertn_15, opertn_16, opertn_17, opertn_18, opertn_19, opertn_20, opertn_21, opertn_22, opertn_23, opertn_24) {
  defaults <- get('hes_op_clinical', envir = defaultValues)
  if (!missing(patid)) {
    defaults$patid <- patid
  }
  if (!missing(attendkey)) {
    defaults$attendkey <- attendkey
  }
  if (!missing(operstat)) {
    defaults$operstat <- operstat
  }
  if (!missing(tretspef)) {
    defaults$tretspef <- tretspef
  }
  if (!missing(mainspef)) {
    defaults$mainspef <- mainspef
  }
  if (!missing(hes_yr)) {
    defaults$hes_yr <- hes_yr
  }
  if (!missing(diag_01)) {
    defaults$diag_01 <- diag_01
  }
  if (!missing(diag_02)) {
    defaults$diag_02 <- diag_02
  }
  if (!missing(diag_03)) {
    defaults$diag_03 <- diag_03
  }
  if (!missing(diag_04)) {
    defaults$diag_04 <- diag_04
  }
  if (!missing(diag_05)) {
    defaults$diag_05 <- diag_05
  }
  if (!missing(diag_06)) {
    defaults$diag_06 <- diag_06
  }
  if (!missing(diag_07)) {
    defaults$diag_07 <- diag_07
  }
  if (!missing(diag_08)) {
    defaults$diag_08 <- diag_08
  }
  if (!missing(diag_09)) {
    defaults$diag_09 <- diag_09
  }
  if (!missing(diag_10)) {
    defaults$diag_10 <- diag_10
  }
  if (!missing(diag_11)) {
    defaults$diag_11 <- diag_11
  }
  if (!missing(diag_12)) {
    defaults$diag_12 <- diag_12
  }
  if (!missing(opertn_01)) {
    defaults$opertn_01 <- opertn_01
  }
  if (!missing(opertn_02)) {
    defaults$opertn_02 <- opertn_02
  }
  if (!missing(opertn_03)) {
    defaults$opertn_03 <- opertn_03
  }
  if (!missing(opertn_04)) {
    defaults$opertn_04 <- opertn_04
  }
  if (!missing(opertn_05)) {
    defaults$opertn_05 <- opertn_05
  }
  if (!missing(opertn_06)) {
    defaults$opertn_06 <- opertn_06
  }
  if (!missing(opertn_07)) {
    defaults$opertn_07 <- opertn_07
  }
  if (!missing(opertn_08)) {
    defaults$opertn_08 <- opertn_08
  }
  if (!missing(opertn_09)) {
    defaults$opertn_09 <- opertn_09
  }
  if (!missing(opertn_10)) {
    defaults$opertn_10 <- opertn_10
  }
  if (!missing(opertn_11)) {
    defaults$opertn_11 <- opertn_11
  }
  if (!missing(opertn_12)) {
    defaults$opertn_12 <- opertn_12
  }
  if (!missing(opertn_13)) {
    defaults$opertn_13 <- opertn_13
  }
  if (!missing(opertn_14)) {
    defaults$opertn_14 <- opertn_14
  }
  if (!missing(opertn_15)) {
    defaults$opertn_15 <- opertn_15
  }
  if (!missing(opertn_16)) {
    defaults$opertn_16 <- opertn_16
  }
  if (!missing(opertn_17)) {
    defaults$opertn_17 <- opertn_17
  }
  if (!missing(opertn_18)) {
    defaults$opertn_18 <- opertn_18
  }
  if (!missing(opertn_19)) {
    defaults$opertn_19 <- opertn_19
  }
  if (!missing(opertn_20)) {
    defaults$opertn_20 <- opertn_20
  }
  if (!missing(opertn_21)) {
    defaults$opertn_21 <- opertn_21
  }
  if (!missing(opertn_22)) {
    defaults$opertn_22 <- opertn_22
  }
  if (!missing(opertn_23)) {
    defaults$opertn_23 <- opertn_23
  }
  if (!missing(opertn_24)) {
    defaults$opertn_24 <- opertn_24
  }
  assign('hes_op_clinical', defaults, envir = defaultValues)
  invisible(defaults)
}

get_defaults_patient <- function() {
  defaults <- get('patient', envir = defaultValues)
  return(defaults)
}

get_defaults_practice <- function() {
  defaults <- get('practice', envir = defaultValues)
  return(defaults)
}

get_defaults_consultation <- function() {
  defaults <- get('consultation', envir = defaultValues)
  return(defaults)
}

get_defaults_clinical <- function() {
  defaults <- get('clinical', envir = defaultValues)
  return(defaults)
}

get_defaults_referral <- function() {
  defaults <- get('referral', envir = defaultValues)
  return(defaults)
}

get_defaults_test <- function() {
  defaults <- get('test', envir = defaultValues)
  return(defaults)
}

get_defaults_immunisation <- function() {
  defaults <- get('immunisation', envir = defaultValues)
  return(defaults)
}

get_defaults_therapy <- function() {
  defaults <- get('therapy', envir = defaultValues)
  return(defaults)
}

get_defaults_additional <- function() {
  defaults <- get('additional', envir = defaultValues)
  return(defaults)
}

get_defaults_ons_death <- function() {
  defaults <- get('ons_death', envir = defaultValues)
  return(defaults)
}

get_defaults_ons_imd <- function() {
  defaults <- get('ons_imd', envir = defaultValues)
  return(defaults)
}

get_defaults_hes_patient <- function() {
  defaults <- get('hes_patient', envir = defaultValues)
  return(defaults)
}

get_defaults_hes_diag_hosp <- function() {
  defaults <- get('hes_diag_hosp', envir = defaultValues)
  return(defaults)
}

get_defaults_hes_proc_epi <- function() {
  defaults <- get('hes_proc_epi', envir = defaultValues)
  return(defaults)
}

get_defaults_hes_diag_epi <- function() {
  defaults <- get('hes_diag_epi', envir = defaultValues)
  return(defaults)
}

get_defaults_hes_op_clinical <- function() {
  defaults <- get('hes_op_clinical', envir = defaultValues)
  return(defaults)
}

add_patient <- function(patid, pracid, gender, yob, mob, crd, tod, deathdate, marital, famnum, chsreg, chsdate, prescr, capsup, ses, frd, regstat, reggap, internal, toreason, accept, prac_region, prac_lcd, prac_uts, linkable) {
  defaults <- get('patient', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(pracid)) {
    pracid <- defaults$pracid
  }
  if (!is.null(pracid)) {
    insertFields <- c(insertFields, "pracid")
    insertValues <- c(insertValues, pracid)
  }

  if (missing(gender)) {
    gender <- defaults$gender
  }
  if (!is.null(gender)) {
    insertFields <- c(insertFields, "gender")
    insertValues <- c(insertValues, gender)
  }

  if (missing(yob)) {
    yob <- defaults$yob
  }
  if (!is.null(yob)) {
    insertFields <- c(insertFields, "yob")
    insertValues <- c(insertValues, yob)
  }

  if (missing(mob)) {
    mob <- defaults$mob
  }
  if (!is.null(mob)) {
    insertFields <- c(insertFields, "mob")
    insertValues <- c(insertValues, mob)
  }

  if (missing(crd)) {
    crd <- defaults$crd
  }
  if (!is.null(crd)) {
    insertFields <- c(insertFields, "crd")
    insertValues <- c(insertValues, crd)
  }

  if (missing(tod)) {
    tod <- defaults$tod
  }
  if (!is.null(tod)) {
    insertFields <- c(insertFields, "tod")
    insertValues <- c(insertValues, tod)
  }

  if (missing(deathdate)) {
    deathdate <- defaults$deathdate
  }
  if (!is.null(deathdate)) {
    insertFields <- c(insertFields, "deathdate")
    insertValues <- c(insertValues, deathdate)
  }

  if (missing(marital)) {
    marital <- defaults$marital
  }
  if (!is.null(marital)) {
    insertFields <- c(insertFields, "marital")
    insertValues <- c(insertValues, marital)
  }

  if (missing(famnum)) {
    famnum <- defaults$famnum
  }
  if (!is.null(famnum)) {
    insertFields <- c(insertFields, "famnum")
    insertValues <- c(insertValues, famnum)
  }

  if (missing(chsreg)) {
    chsreg <- defaults$chsreg
  }
  if (!is.null(chsreg)) {
    insertFields <- c(insertFields, "chsreg")
    insertValues <- c(insertValues, chsreg)
  }

  if (missing(chsdate)) {
    chsdate <- defaults$chsdate
  }
  if (!is.null(chsdate)) {
    insertFields <- c(insertFields, "chsdate")
    insertValues <- c(insertValues, chsdate)
  }

  if (missing(prescr)) {
    prescr <- defaults$prescr
  }
  if (!is.null(prescr)) {
    insertFields <- c(insertFields, "prescr")
    insertValues <- c(insertValues, prescr)
  }

  if (missing(capsup)) {
    capsup <- defaults$capsup
  }
  if (!is.null(capsup)) {
    insertFields <- c(insertFields, "capsup")
    insertValues <- c(insertValues, capsup)
  }

  if (missing(ses)) {
    ses <- defaults$ses
  }
  if (!is.null(ses)) {
    insertFields <- c(insertFields, "ses")
    insertValues <- c(insertValues, ses)
  }

  if (missing(frd)) {
    frd <- defaults$frd
  }
  if (!is.null(frd)) {
    insertFields <- c(insertFields, "frd")
    insertValues <- c(insertValues, frd)
  }

  if (missing(regstat)) {
    regstat <- defaults$regstat
  }
  if (!is.null(regstat)) {
    insertFields <- c(insertFields, "regstat")
    insertValues <- c(insertValues, regstat)
  }

  if (missing(reggap)) {
    reggap <- defaults$reggap
  }
  if (!is.null(reggap)) {
    insertFields <- c(insertFields, "reggap")
    insertValues <- c(insertValues, reggap)
  }

  if (missing(internal)) {
    internal <- defaults$internal
  }
  if (!is.null(internal)) {
    insertFields <- c(insertFields, "internal")
    insertValues <- c(insertValues, internal)
  }

  if (missing(toreason)) {
    toreason <- defaults$toreason
  }
  if (!is.null(toreason)) {
    insertFields <- c(insertFields, "toreason")
    insertValues <- c(insertValues, toreason)
  }

  if (missing(accept)) {
    accept <- defaults$accept
  }
  if (!is.null(accept)) {
    insertFields <- c(insertFields, "accept")
    insertValues <- c(insertValues, accept)
  }

  if (missing(prac_region)) {
    prac_region <- defaults$prac_region
  }
  if (!is.null(prac_region)) {
    insertFields <- c(insertFields, "prac_region")
    insertValues <- c(insertValues, prac_region)
  }

  if (missing(prac_lcd)) {
    prac_lcd <- defaults$prac_lcd
  }
  if (!is.null(prac_lcd)) {
    insertFields <- c(insertFields, "prac_lcd")
    insertValues <- c(insertValues, prac_lcd)
  }

  if (missing(prac_uts)) {
    prac_uts <- defaults$prac_uts
  }
  if (!is.null(prac_uts)) {
    insertFields <- c(insertFields, "prac_uts")
    insertValues <- c(insertValues, prac_uts)
  }

  if (missing(linkable)) {
    linkable <- defaults$linkable
  }
  if (!is.null(linkable)) {
    insertFields <- c(insertFields, "linkable")
    insertValues <- c(insertValues, linkable)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('patient', comment)
  }
  statement <- paste0("INSERT INTO patient (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('patient', statement)
  invisible(statement)
}

add_practice <- function(pracid, region, uts, lcd) {
  defaults <- get('practice', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(pracid)) {
    pracid <- defaults$pracid
  }
  if (!is.null(pracid)) {
    insertFields <- c(insertFields, "pracid")
    insertValues <- c(insertValues, pracid)
  }

  if (missing(region)) {
    region <- defaults$region
  }
  if (!is.null(region)) {
    insertFields <- c(insertFields, "region")
    insertValues <- c(insertValues, region)
  }

  if (missing(uts)) {
    uts <- defaults$uts
  }
  if (!is.null(uts)) {
    insertFields <- c(insertFields, "uts")
    insertValues <- c(insertValues, uts)
  }

  if (missing(lcd)) {
    lcd <- defaults$lcd
  }
  if (!is.null(lcd)) {
    insertFields <- c(insertFields, "lcd")
    insertValues <- c(insertValues, lcd)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('practice', comment)
  }
  statement <- paste0("INSERT INTO practice (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('practice', statement)
  invisible(statement)
}

add_consultation <- function(patid, eventdate, constype, consid, staffid, duration) {
  defaults <- get('consultation', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(eventdate)) {
    eventdate <- defaults$eventdate
  }
  if (!is.null(eventdate)) {
    insertFields <- c(insertFields, "eventdate")
    insertValues <- c(insertValues, eventdate)
  }

  if (missing(constype)) {
    constype <- defaults$constype
  }
  if (!is.null(constype)) {
    insertFields <- c(insertFields, "constype")
    insertValues <- c(insertValues, constype)
  }

  if (missing(consid)) {
    consid <- defaults$consid
  }
  if (!is.null(consid)) {
    insertFields <- c(insertFields, "consid")
    insertValues <- c(insertValues, consid)
  }

  if (missing(staffid)) {
    staffid <- defaults$staffid
  }
  if (!is.null(staffid)) {
    insertFields <- c(insertFields, "staffid")
    insertValues <- c(insertValues, staffid)
  }

  if (missing(duration)) {
    duration <- defaults$duration
  }
  if (!is.null(duration)) {
    insertFields <- c(insertFields, "duration")
    insertValues <- c(insertValues, duration)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('consultation', comment)
  }
  statement <- paste0("INSERT INTO consultation (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('consultation', statement)
  invisible(statement)
}

add_clinical <- function(patid, eventdate, constype, consid, staffid, episode, medcode, enttype, adid, data1, data2, data3, data4, data5, data6, data7) {
  defaults <- get('clinical', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(eventdate)) {
    eventdate <- defaults$eventdate
  }
  if (!is.null(eventdate)) {
    insertFields <- c(insertFields, "eventdate")
    insertValues <- c(insertValues, eventdate)
  }

  if (missing(constype)) {
    constype <- defaults$constype
  }
  if (!is.null(constype)) {
    insertFields <- c(insertFields, "constype")
    insertValues <- c(insertValues, constype)
  }

  if (missing(consid)) {
    consid <- defaults$consid
  }
  if (!is.null(consid)) {
    insertFields <- c(insertFields, "consid")
    insertValues <- c(insertValues, consid)
  }

  if (missing(staffid)) {
    staffid <- defaults$staffid
  }
  if (!is.null(staffid)) {
    insertFields <- c(insertFields, "staffid")
    insertValues <- c(insertValues, staffid)
  }

  if (missing(episode)) {
    episode <- defaults$episode
  }
  if (!is.null(episode)) {
    insertFields <- c(insertFields, "episode")
    insertValues <- c(insertValues, episode)
  }

  if (missing(medcode)) {
    medcode <- defaults$medcode
  }
  if (!is.null(medcode)) {
    insertFields <- c(insertFields, "medcode")
    insertValues <- c(insertValues, medcode)
  }

  if (missing(enttype)) {
    enttype <- defaults$enttype
  }
  if (!is.null(enttype)) {
    insertFields <- c(insertFields, "enttype")
    insertValues <- c(insertValues, enttype)
  }

  if (missing(adid)) {
    adid <- defaults$adid
  }
  if (!is.null(adid)) {
    insertFields <- c(insertFields, "adid")
    insertValues <- c(insertValues, adid)
  }

  if (missing(data1)) {
    data1 <- defaults$data1
  }
  if (!is.null(data1)) {
    insertFields <- c(insertFields, "data1")
    insertValues <- c(insertValues, data1)
  }

  if (missing(data2)) {
    data2 <- defaults$data2
  }
  if (!is.null(data2)) {
    insertFields <- c(insertFields, "data2")
    insertValues <- c(insertValues, data2)
  }

  if (missing(data3)) {
    data3 <- defaults$data3
  }
  if (!is.null(data3)) {
    insertFields <- c(insertFields, "data3")
    insertValues <- c(insertValues, data3)
  }

  if (missing(data4)) {
    data4 <- defaults$data4
  }
  if (!is.null(data4)) {
    insertFields <- c(insertFields, "data4")
    insertValues <- c(insertValues, data4)
  }

  if (missing(data5)) {
    data5 <- defaults$data5
  }
  if (!is.null(data5)) {
    insertFields <- c(insertFields, "data5")
    insertValues <- c(insertValues, data5)
  }

  if (missing(data6)) {
    data6 <- defaults$data6
  }
  if (!is.null(data6)) {
    insertFields <- c(insertFields, "data6")
    insertValues <- c(insertValues, data6)
  }

  if (missing(data7)) {
    data7 <- defaults$data7
  }
  if (!is.null(data7)) {
    insertFields <- c(insertFields, "data7")
    insertValues <- c(insertValues, data7)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('clinical', comment)
  }
  statement <- paste0("INSERT INTO clinical (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('clinical', statement)
  invisible(statement)
}

add_referral <- function(patid, eventdate, constype, consid, staffid, source, medcode, nhsspec, fhsaspec, inpatient, attendance, urgency) {
  defaults <- get('referral', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(eventdate)) {
    eventdate <- defaults$eventdate
  }
  if (!is.null(eventdate)) {
    insertFields <- c(insertFields, "eventdate")
    insertValues <- c(insertValues, eventdate)
  }

  if (missing(constype)) {
    constype <- defaults$constype
  }
  if (!is.null(constype)) {
    insertFields <- c(insertFields, "constype")
    insertValues <- c(insertValues, constype)
  }

  if (missing(consid)) {
    consid <- defaults$consid
  }
  if (!is.null(consid)) {
    insertFields <- c(insertFields, "consid")
    insertValues <- c(insertValues, consid)
  }

  if (missing(staffid)) {
    staffid <- defaults$staffid
  }
  if (!is.null(staffid)) {
    insertFields <- c(insertFields, "staffid")
    insertValues <- c(insertValues, staffid)
  }

  if (missing(source)) {
    source <- defaults$source
  }
  if (!is.null(source)) {
    insertFields <- c(insertFields, "source")
    insertValues <- c(insertValues, source)
  }

  if (missing(medcode)) {
    medcode <- defaults$medcode
  }
  if (!is.null(medcode)) {
    insertFields <- c(insertFields, "medcode")
    insertValues <- c(insertValues, medcode)
  }

  if (missing(nhsspec)) {
    nhsspec <- defaults$nhsspec
  }
  if (!is.null(nhsspec)) {
    insertFields <- c(insertFields, "nhsspec")
    insertValues <- c(insertValues, nhsspec)
  }

  if (missing(fhsaspec)) {
    fhsaspec <- defaults$fhsaspec
  }
  if (!is.null(fhsaspec)) {
    insertFields <- c(insertFields, "fhsaspec")
    insertValues <- c(insertValues, fhsaspec)
  }

  if (missing(inpatient)) {
    inpatient <- defaults$inpatient
  }
  if (!is.null(inpatient)) {
    insertFields <- c(insertFields, "inpatient")
    insertValues <- c(insertValues, inpatient)
  }

  if (missing(attendance)) {
    attendance <- defaults$attendance
  }
  if (!is.null(attendance)) {
    insertFields <- c(insertFields, "attendance")
    insertValues <- c(insertValues, attendance)
  }

  if (missing(urgency)) {
    urgency <- defaults$urgency
  }
  if (!is.null(urgency)) {
    insertFields <- c(insertFields, "urgency")
    insertValues <- c(insertValues, urgency)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('referral', comment)
  }
  statement <- paste0("INSERT INTO referral (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('referral', statement)
  invisible(statement)
}

add_test <- function(patid, eventdate, consid, medcode, constype, textid, enttype, staffid, data1, data2, data3, data4, data5, data6, data7, data8) {
  defaults <- get('test', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(eventdate)) {
    eventdate <- defaults$eventdate
  }
  if (!is.null(eventdate)) {
    insertFields <- c(insertFields, "eventdate")
    insertValues <- c(insertValues, eventdate)
  }

  if (missing(consid)) {
    consid <- defaults$consid
  }
  if (!is.null(consid)) {
    insertFields <- c(insertFields, "consid")
    insertValues <- c(insertValues, consid)
  }

  if (missing(medcode)) {
    medcode <- defaults$medcode
  }
  if (!is.null(medcode)) {
    insertFields <- c(insertFields, "medcode")
    insertValues <- c(insertValues, medcode)
  }

  if (missing(constype)) {
    constype <- defaults$constype
  }
  if (!is.null(constype)) {
    insertFields <- c(insertFields, "constype")
    insertValues <- c(insertValues, constype)
  }

  if (missing(textid)) {
    textid <- defaults$textid
  }
  if (!is.null(textid)) {
    insertFields <- c(insertFields, "textid")
    insertValues <- c(insertValues, textid)
  }

  if (missing(enttype)) {
    enttype <- defaults$enttype
  }
  if (!is.null(enttype)) {
    insertFields <- c(insertFields, "enttype")
    insertValues <- c(insertValues, enttype)
  }

  if (missing(staffid)) {
    staffid <- defaults$staffid
  }
  if (!is.null(staffid)) {
    insertFields <- c(insertFields, "staffid")
    insertValues <- c(insertValues, staffid)
  }

  if (missing(data1)) {
    data1 <- defaults$data1
  }
  if (!is.null(data1)) {
    insertFields <- c(insertFields, "data1")
    insertValues <- c(insertValues, data1)
  }

  if (missing(data2)) {
    data2 <- defaults$data2
  }
  if (!is.null(data2)) {
    insertFields <- c(insertFields, "data2")
    insertValues <- c(insertValues, data2)
  }

  if (missing(data3)) {
    data3 <- defaults$data3
  }
  if (!is.null(data3)) {
    insertFields <- c(insertFields, "data3")
    insertValues <- c(insertValues, data3)
  }

  if (missing(data4)) {
    data4 <- defaults$data4
  }
  if (!is.null(data4)) {
    insertFields <- c(insertFields, "data4")
    insertValues <- c(insertValues, data4)
  }

  if (missing(data5)) {
    data5 <- defaults$data5
  }
  if (!is.null(data5)) {
    insertFields <- c(insertFields, "data5")
    insertValues <- c(insertValues, data5)
  }

  if (missing(data6)) {
    data6 <- defaults$data6
  }
  if (!is.null(data6)) {
    insertFields <- c(insertFields, "data6")
    insertValues <- c(insertValues, data6)
  }

  if (missing(data7)) {
    data7 <- defaults$data7
  }
  if (!is.null(data7)) {
    insertFields <- c(insertFields, "data7")
    insertValues <- c(insertValues, data7)
  }

  if (missing(data8)) {
    data8 <- defaults$data8
  }
  if (!is.null(data8)) {
    insertFields <- c(insertFields, "data8")
    insertValues <- c(insertValues, data8)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('test', comment)
  }
  statement <- paste0("INSERT INTO test (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('test', statement)
  invisible(statement)
}

add_immunisation <- function(patid, eventdate, sysdate, constype, consid, staffid, immstype, medcode, stage, status, compound, source, reason, method, batch) {
  defaults <- get('immunisation', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(eventdate)) {
    eventdate <- defaults$eventdate
  }
  if (!is.null(eventdate)) {
    insertFields <- c(insertFields, "eventdate")
    insertValues <- c(insertValues, eventdate)
  }

  if (missing(sysdate)) {
    sysdate <- defaults$sysdate
  }
  if (!is.null(sysdate)) {
    insertFields <- c(insertFields, "sysdate")
    insertValues <- c(insertValues, sysdate)
  }

  if (missing(constype)) {
    constype <- defaults$constype
  }
  if (!is.null(constype)) {
    insertFields <- c(insertFields, "constype")
    insertValues <- c(insertValues, constype)
  }

  if (missing(consid)) {
    consid <- defaults$consid
  }
  if (!is.null(consid)) {
    insertFields <- c(insertFields, "consid")
    insertValues <- c(insertValues, consid)
  }

  if (missing(staffid)) {
    staffid <- defaults$staffid
  }
  if (!is.null(staffid)) {
    insertFields <- c(insertFields, "staffid")
    insertValues <- c(insertValues, staffid)
  }

  if (missing(immstype)) {
    immstype <- defaults$immstype
  }
  if (!is.null(immstype)) {
    insertFields <- c(insertFields, "immstype")
    insertValues <- c(insertValues, immstype)
  }

  if (missing(medcode)) {
    medcode <- defaults$medcode
  }
  if (!is.null(medcode)) {
    insertFields <- c(insertFields, "medcode")
    insertValues <- c(insertValues, medcode)
  }

  if (missing(stage)) {
    stage <- defaults$stage
  }
  if (!is.null(stage)) {
    insertFields <- c(insertFields, "stage")
    insertValues <- c(insertValues, stage)
  }

  if (missing(status)) {
    status <- defaults$status
  }
  if (!is.null(status)) {
    insertFields <- c(insertFields, "status")
    insertValues <- c(insertValues, status)
  }

  if (missing(compound)) {
    compound <- defaults$compound
  }
  if (!is.null(compound)) {
    insertFields <- c(insertFields, "compound")
    insertValues <- c(insertValues, compound)
  }

  if (missing(source)) {
    source <- defaults$source
  }
  if (!is.null(source)) {
    insertFields <- c(insertFields, "source")
    insertValues <- c(insertValues, source)
  }

  if (missing(reason)) {
    reason <- defaults$reason
  }
  if (!is.null(reason)) {
    insertFields <- c(insertFields, "reason")
    insertValues <- c(insertValues, reason)
  }

  if (missing(method)) {
    method <- defaults$method
  }
  if (!is.null(method)) {
    insertFields <- c(insertFields, "method")
    insertValues <- c(insertValues, method)
  }

  if (missing(batch)) {
    batch <- defaults$batch
  }
  if (!is.null(batch)) {
    insertFields <- c(insertFields, "batch")
    insertValues <- c(insertValues, batch)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('immunisation', comment)
  }
  statement <- paste0("INSERT INTO immunisation (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('immunisation', statement)
  invisible(statement)
}

add_therapy <- function(patid, eventdate, numdays, prodcode, consid, staffid, bnfcode, qty, ndd, numpacks, packtype, issueseq) {
  defaults <- get('therapy', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(eventdate)) {
    eventdate <- defaults$eventdate
  }
  if (!is.null(eventdate)) {
    insertFields <- c(insertFields, "eventdate")
    insertValues <- c(insertValues, eventdate)
  }

  if (missing(numdays)) {
    numdays <- defaults$numdays
  }
  if (!is.null(numdays)) {
    insertFields <- c(insertFields, "numdays")
    insertValues <- c(insertValues, numdays)
  }

  if (missing(prodcode)) {
    prodcode <- defaults$prodcode
  }
  if (!is.null(prodcode)) {
    insertFields <- c(insertFields, "prodcode")
    insertValues <- c(insertValues, prodcode)
  }

  if (missing(consid)) {
    consid <- defaults$consid
  }
  if (!is.null(consid)) {
    insertFields <- c(insertFields, "consid")
    insertValues <- c(insertValues, consid)
  }

  if (missing(staffid)) {
    staffid <- defaults$staffid
  }
  if (!is.null(staffid)) {
    insertFields <- c(insertFields, "staffid")
    insertValues <- c(insertValues, staffid)
  }

  if (missing(bnfcode)) {
    bnfcode <- defaults$bnfcode
  }
  if (!is.null(bnfcode)) {
    insertFields <- c(insertFields, "bnfcode")
    insertValues <- c(insertValues, bnfcode)
  }

  if (missing(qty)) {
    qty <- defaults$qty
  }
  if (!is.null(qty)) {
    insertFields <- c(insertFields, "qty")
    insertValues <- c(insertValues, qty)
  }

  if (missing(ndd)) {
    ndd <- defaults$ndd
  }
  if (!is.null(ndd)) {
    insertFields <- c(insertFields, "ndd")
    insertValues <- c(insertValues, ndd)
  }

  if (missing(numpacks)) {
    numpacks <- defaults$numpacks
  }
  if (!is.null(numpacks)) {
    insertFields <- c(insertFields, "numpacks")
    insertValues <- c(insertValues, numpacks)
  }

  if (missing(packtype)) {
    packtype <- defaults$packtype
  }
  if (!is.null(packtype)) {
    insertFields <- c(insertFields, "packtype")
    insertValues <- c(insertValues, packtype)
  }

  if (missing(issueseq)) {
    issueseq <- defaults$issueseq
  }
  if (!is.null(issueseq)) {
    insertFields <- c(insertFields, "issueseq")
    insertValues <- c(insertValues, issueseq)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('therapy', comment)
  }
  statement <- paste0("INSERT INTO therapy (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('therapy', statement)
  invisible(statement)
}

add_additional <- function(patid, enttype, adid, data1, data2, data3, data4, data5, data6, data7) {
  defaults <- get('additional', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(enttype)) {
    enttype <- defaults$enttype
  }
  if (!is.null(enttype)) {
    insertFields <- c(insertFields, "enttype")
    insertValues <- c(insertValues, enttype)
  }

  if (missing(adid)) {
    adid <- defaults$adid
  }
  if (!is.null(adid)) {
    insertFields <- c(insertFields, "adid")
    insertValues <- c(insertValues, adid)
  }

  if (missing(data1)) {
    data1 <- defaults$data1
  }
  if (!is.null(data1)) {
    insertFields <- c(insertFields, "data1")
    insertValues <- c(insertValues, data1)
  }

  if (missing(data2)) {
    data2 <- defaults$data2
  }
  if (!is.null(data2)) {
    insertFields <- c(insertFields, "data2")
    insertValues <- c(insertValues, data2)
  }

  if (missing(data3)) {
    data3 <- defaults$data3
  }
  if (!is.null(data3)) {
    insertFields <- c(insertFields, "data3")
    insertValues <- c(insertValues, data3)
  }

  if (missing(data4)) {
    data4 <- defaults$data4
  }
  if (!is.null(data4)) {
    insertFields <- c(insertFields, "data4")
    insertValues <- c(insertValues, data4)
  }

  if (missing(data5)) {
    data5 <- defaults$data5
  }
  if (!is.null(data5)) {
    insertFields <- c(insertFields, "data5")
    insertValues <- c(insertValues, data5)
  }

  if (missing(data6)) {
    data6 <- defaults$data6
  }
  if (!is.null(data6)) {
    insertFields <- c(insertFields, "data6")
    insertValues <- c(insertValues, data6)
  }

  if (missing(data7)) {
    data7 <- defaults$data7
  }
  if (!is.null(data7)) {
    insertFields <- c(insertFields, "data7")
    insertValues <- c(insertValues, data7)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('additional', comment)
  }
  statement <- paste0("INSERT INTO additional (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('additional', statement)
  invisible(statement)
}

add_ons_death <- function(patid, pracid, match_rank, dod, dod_partial, cause, cause1, cause2, cause3, cause4, cause5, cause6, cause7, cause8, cause9, cause10, cause11, cause12, cause13, cause14, cause15, cause_neonatal1, cause_neonatal2, cause_neonatal3, cause_neonatal4, cause_neonatal5, cause_neonatal6, cause_neonatal7, cause_neonatal8) {
  defaults <- get('ons_death', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(pracid)) {
    pracid <- defaults$pracid
  }
  if (!is.null(pracid)) {
    insertFields <- c(insertFields, "pracid")
    insertValues <- c(insertValues, pracid)
  }

  if (missing(match_rank)) {
    match_rank <- defaults$match_rank
  }
  if (!is.null(match_rank)) {
    insertFields <- c(insertFields, "match_rank")
    insertValues <- c(insertValues, match_rank)
  }

  if (missing(dod)) {
    dod <- defaults$dod
  }
  if (!is.null(dod)) {
    insertFields <- c(insertFields, "dod")
    insertValues <- c(insertValues, dod)
  }

  if (missing(dod_partial)) {
    dod_partial <- defaults$dod_partial
  }
  if (!is.null(dod_partial)) {
    insertFields <- c(insertFields, "dod_partial")
    insertValues <- c(insertValues, dod_partial)
  }

  if (missing(cause)) {
    cause <- defaults$cause
  }
  if (!is.null(cause)) {
    insertFields <- c(insertFields, "cause")
    insertValues <- c(insertValues, cause)
  }

  if (missing(cause1)) {
    cause1 <- defaults$cause1
  }
  if (!is.null(cause1)) {
    insertFields <- c(insertFields, "cause1")
    insertValues <- c(insertValues, cause1)
  }

  if (missing(cause2)) {
    cause2 <- defaults$cause2
  }
  if (!is.null(cause2)) {
    insertFields <- c(insertFields, "cause2")
    insertValues <- c(insertValues, cause2)
  }

  if (missing(cause3)) {
    cause3 <- defaults$cause3
  }
  if (!is.null(cause3)) {
    insertFields <- c(insertFields, "cause3")
    insertValues <- c(insertValues, cause3)
  }

  if (missing(cause4)) {
    cause4 <- defaults$cause4
  }
  if (!is.null(cause4)) {
    insertFields <- c(insertFields, "cause4")
    insertValues <- c(insertValues, cause4)
  }

  if (missing(cause5)) {
    cause5 <- defaults$cause5
  }
  if (!is.null(cause5)) {
    insertFields <- c(insertFields, "cause5")
    insertValues <- c(insertValues, cause5)
  }

  if (missing(cause6)) {
    cause6 <- defaults$cause6
  }
  if (!is.null(cause6)) {
    insertFields <- c(insertFields, "cause6")
    insertValues <- c(insertValues, cause6)
  }

  if (missing(cause7)) {
    cause7 <- defaults$cause7
  }
  if (!is.null(cause7)) {
    insertFields <- c(insertFields, "cause7")
    insertValues <- c(insertValues, cause7)
  }

  if (missing(cause8)) {
    cause8 <- defaults$cause8
  }
  if (!is.null(cause8)) {
    insertFields <- c(insertFields, "cause8")
    insertValues <- c(insertValues, cause8)
  }

  if (missing(cause9)) {
    cause9 <- defaults$cause9
  }
  if (!is.null(cause9)) {
    insertFields <- c(insertFields, "cause9")
    insertValues <- c(insertValues, cause9)
  }

  if (missing(cause10)) {
    cause10 <- defaults$cause10
  }
  if (!is.null(cause10)) {
    insertFields <- c(insertFields, "cause10")
    insertValues <- c(insertValues, cause10)
  }

  if (missing(cause11)) {
    cause11 <- defaults$cause11
  }
  if (!is.null(cause11)) {
    insertFields <- c(insertFields, "cause11")
    insertValues <- c(insertValues, cause11)
  }

  if (missing(cause12)) {
    cause12 <- defaults$cause12
  }
  if (!is.null(cause12)) {
    insertFields <- c(insertFields, "cause12")
    insertValues <- c(insertValues, cause12)
  }

  if (missing(cause13)) {
    cause13 <- defaults$cause13
  }
  if (!is.null(cause13)) {
    insertFields <- c(insertFields, "cause13")
    insertValues <- c(insertValues, cause13)
  }

  if (missing(cause14)) {
    cause14 <- defaults$cause14
  }
  if (!is.null(cause14)) {
    insertFields <- c(insertFields, "cause14")
    insertValues <- c(insertValues, cause14)
  }

  if (missing(cause15)) {
    cause15 <- defaults$cause15
  }
  if (!is.null(cause15)) {
    insertFields <- c(insertFields, "cause15")
    insertValues <- c(insertValues, cause15)
  }

  if (missing(cause_neonatal1)) {
    cause_neonatal1 <- defaults$cause_neonatal1
  }
  if (!is.null(cause_neonatal1)) {
    insertFields <- c(insertFields, "cause_neonatal1")
    insertValues <- c(insertValues, cause_neonatal1)
  }

  if (missing(cause_neonatal2)) {
    cause_neonatal2 <- defaults$cause_neonatal2
  }
  if (!is.null(cause_neonatal2)) {
    insertFields <- c(insertFields, "cause_neonatal2")
    insertValues <- c(insertValues, cause_neonatal2)
  }

  if (missing(cause_neonatal3)) {
    cause_neonatal3 <- defaults$cause_neonatal3
  }
  if (!is.null(cause_neonatal3)) {
    insertFields <- c(insertFields, "cause_neonatal3")
    insertValues <- c(insertValues, cause_neonatal3)
  }

  if (missing(cause_neonatal4)) {
    cause_neonatal4 <- defaults$cause_neonatal4
  }
  if (!is.null(cause_neonatal4)) {
    insertFields <- c(insertFields, "cause_neonatal4")
    insertValues <- c(insertValues, cause_neonatal4)
  }

  if (missing(cause_neonatal5)) {
    cause_neonatal5 <- defaults$cause_neonatal5
  }
  if (!is.null(cause_neonatal5)) {
    insertFields <- c(insertFields, "cause_neonatal5")
    insertValues <- c(insertValues, cause_neonatal5)
  }

  if (missing(cause_neonatal6)) {
    cause_neonatal6 <- defaults$cause_neonatal6
  }
  if (!is.null(cause_neonatal6)) {
    insertFields <- c(insertFields, "cause_neonatal6")
    insertValues <- c(insertValues, cause_neonatal6)
  }

  if (missing(cause_neonatal7)) {
    cause_neonatal7 <- defaults$cause_neonatal7
  }
  if (!is.null(cause_neonatal7)) {
    insertFields <- c(insertFields, "cause_neonatal7")
    insertValues <- c(insertValues, cause_neonatal7)
  }

  if (missing(cause_neonatal8)) {
    cause_neonatal8 <- defaults$cause_neonatal8
  }
  if (!is.null(cause_neonatal8)) {
    insertFields <- c(insertFields, "cause_neonatal8")
    insertValues <- c(insertValues, cause_neonatal8)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('ons_death', comment)
  }
  statement <- paste0("INSERT INTO ons_death (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('ons_death', statement)
  invisible(statement)
}

add_ons_imd <- function(patid, pracid, imd) {
  defaults <- get('ons_imd', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(pracid)) {
    pracid <- defaults$pracid
  }
  if (!is.null(pracid)) {
    insertFields <- c(insertFields, "pracid")
    insertValues <- c(insertValues, pracid)
  }

  if (missing(imd)) {
    imd <- defaults$imd
  }
  if (!is.null(imd)) {
    insertFields <- c(insertFields, "imd")
    insertValues <- c(insertValues, imd)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('ons_imd', comment)
  }
  statement <- paste0("INSERT INTO ons_imd (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('ons_imd', statement)
  invisible(statement)
}

add_hes_patient <- function(patid, pracid, gen_hesid, n_patid_hes, gen_ethnicity, match_rank) {
  defaults <- get('hes_patient', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(pracid)) {
    pracid <- defaults$pracid
  }
  if (!is.null(pracid)) {
    insertFields <- c(insertFields, "pracid")
    insertValues <- c(insertValues, pracid)
  }

  if (missing(gen_hesid)) {
    gen_hesid <- defaults$gen_hesid
  }
  if (!is.null(gen_hesid)) {
    insertFields <- c(insertFields, "gen_hesid")
    insertValues <- c(insertValues, gen_hesid)
  }

  if (missing(n_patid_hes)) {
    n_patid_hes <- defaults$n_patid_hes
  }
  if (!is.null(n_patid_hes)) {
    insertFields <- c(insertFields, "n_patid_hes")
    insertValues <- c(insertValues, n_patid_hes)
  }

  if (missing(gen_ethnicity)) {
    gen_ethnicity <- defaults$gen_ethnicity
  }
  if (!is.null(gen_ethnicity)) {
    insertFields <- c(insertFields, "gen_ethnicity")
    insertValues <- c(insertValues, gen_ethnicity)
  }

  if (missing(match_rank)) {
    match_rank <- defaults$match_rank
  }
  if (!is.null(match_rank)) {
    insertFields <- c(insertFields, "match_rank")
    insertValues <- c(insertValues, match_rank)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('hes_patient', comment)
  }
  statement <- paste0("INSERT INTO hes_patient (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('hes_patient', statement)
  invisible(statement)
}

add_hes_diag_hosp <- function(spno, patid, admidate, discharged, icd, icdx) {
  defaults <- get('hes_diag_hosp', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(spno)) {
    spno <- defaults$spno
  }
  if (!is.null(spno)) {
    insertFields <- c(insertFields, "spno")
    insertValues <- c(insertValues, spno)
  }

  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(admidate)) {
    admidate <- defaults$admidate
  }
  if (!is.null(admidate)) {
    insertFields <- c(insertFields, "admidate")
    insertValues <- c(insertValues, admidate)
  }

  if (missing(discharged)) {
    discharged <- defaults$discharged
  }
  if (!is.null(discharged)) {
    insertFields <- c(insertFields, "discharged")
    insertValues <- c(insertValues, discharged)
  }

  if (missing(icd)) {
    icd <- defaults$icd
  }
  if (!is.null(icd)) {
    insertFields <- c(insertFields, "icd")
    insertValues <- c(insertValues, icd)
  }

  if (missing(icdx)) {
    icdx <- defaults$icdx
  }
  if (!is.null(icdx)) {
    insertFields <- c(insertFields, "icdx")
    insertValues <- c(insertValues, icdx)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('hes_diag_hosp', comment)
  }
  statement <- paste0("INSERT INTO hes_diag_hosp (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('hes_diag_hosp', statement)
  invisible(statement)
}

add_hes_proc_epi <- function(patid, spno, epikey, admidate, epistart, epiend, discharged, opcs, evdate, p_order) {
  defaults <- get('hes_proc_epi', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(spno)) {
    spno <- defaults$spno
  }
  if (!is.null(spno)) {
    insertFields <- c(insertFields, "spno")
    insertValues <- c(insertValues, spno)
  }

  if (missing(epikey)) {
    epikey <- defaults$epikey
  }
  if (!is.null(epikey)) {
    insertFields <- c(insertFields, "epikey")
    insertValues <- c(insertValues, epikey)
  }

  if (missing(admidate)) {
    admidate <- defaults$admidate
  }
  if (!is.null(admidate)) {
    insertFields <- c(insertFields, "admidate")
    insertValues <- c(insertValues, admidate)
  }

  if (missing(epistart)) {
    epistart <- defaults$epistart
  }
  if (!is.null(epistart)) {
    insertFields <- c(insertFields, "epistart")
    insertValues <- c(insertValues, epistart)
  }

  if (missing(epiend)) {
    epiend <- defaults$epiend
  }
  if (!is.null(epiend)) {
    insertFields <- c(insertFields, "epiend")
    insertValues <- c(insertValues, epiend)
  }

  if (missing(discharged)) {
    discharged <- defaults$discharged
  }
  if (!is.null(discharged)) {
    insertFields <- c(insertFields, "discharged")
    insertValues <- c(insertValues, discharged)
  }

  if (missing(opcs)) {
    opcs <- defaults$opcs
  }
  if (!is.null(opcs)) {
    insertFields <- c(insertFields, "opcs")
    insertValues <- c(insertValues, opcs)
  }

  if (missing(evdate)) {
    evdate <- defaults$evdate
  }
  if (!is.null(evdate)) {
    insertFields <- c(insertFields, "evdate")
    insertValues <- c(insertValues, evdate)
  }

  if (missing(p_order)) {
    p_order <- defaults$p_order
  }
  if (!is.null(p_order)) {
    insertFields <- c(insertFields, "p_order")
    insertValues <- c(insertValues, p_order)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('hes_proc_epi', comment)
  }
  statement <- paste0("INSERT INTO hes_proc_epi (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('hes_proc_epi', statement)
  invisible(statement)
}

add_hes_diag_epi <- function(patid, spno, epikey, epistart, epiend, icd, icdx, d_order) {
  defaults <- get('hes_diag_epi', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(spno)) {
    spno <- defaults$spno
  }
  if (!is.null(spno)) {
    insertFields <- c(insertFields, "spno")
    insertValues <- c(insertValues, spno)
  }

  if (missing(epikey)) {
    epikey <- defaults$epikey
  }
  if (!is.null(epikey)) {
    insertFields <- c(insertFields, "epikey")
    insertValues <- c(insertValues, epikey)
  }

  if (missing(epistart)) {
    epistart <- defaults$epistart
  }
  if (!is.null(epistart)) {
    insertFields <- c(insertFields, "epistart")
    insertValues <- c(insertValues, epistart)
  }

  if (missing(epiend)) {
    epiend <- defaults$epiend
  }
  if (!is.null(epiend)) {
    insertFields <- c(insertFields, "epiend")
    insertValues <- c(insertValues, epiend)
  }

  if (missing(icd)) {
    icd <- defaults$icd
  }
  if (!is.null(icd)) {
    insertFields <- c(insertFields, "icd")
    insertValues <- c(insertValues, icd)
  }

  if (missing(icdx)) {
    icdx <- defaults$icdx
  }
  if (!is.null(icdx)) {
    insertFields <- c(insertFields, "icdx")
    insertValues <- c(insertValues, icdx)
  }

  if (missing(d_order)) {
    d_order <- defaults$d_order
  }
  if (!is.null(d_order)) {
    insertFields <- c(insertFields, "d_order")
    insertValues <- c(insertValues, d_order)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('hes_diag_epi', comment)
  }
  statement <- paste0("INSERT INTO hes_diag_epi (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('hes_diag_epi', statement)
  invisible(statement)
}

add_hes_op_clinical <- function(patid, attendkey, operstat, tretspef, mainspef, hes_yr, diag_01, diag_02, diag_03, diag_04, diag_05, diag_06, diag_07, diag_08, diag_09, diag_10, diag_11, diag_12, opertn_01, opertn_02, opertn_03, opertn_04, opertn_05, opertn_06, opertn_07, opertn_08, opertn_09, opertn_10, opertn_11, opertn_12, opertn_13, opertn_14, opertn_15, opertn_16, opertn_17, opertn_18, opertn_19, opertn_20, opertn_21, opertn_22, opertn_23, opertn_24) {
  defaults <- get('hes_op_clinical', envir = defaultValues)
  insertFields <- c()
  insertValues <- c()
  if (missing(patid)) {
    patid <- defaults$patid
  }
  if (!is.null(patid)) {
    insertFields <- c(insertFields, "patid")
    insertValues <- c(insertValues, patid)
  }

  if (missing(attendkey)) {
    attendkey <- defaults$attendkey
  }
  if (!is.null(attendkey)) {
    insertFields <- c(insertFields, "attendkey")
    insertValues <- c(insertValues, attendkey)
  }

  if (missing(operstat)) {
    operstat <- defaults$operstat
  }
  if (!is.null(operstat)) {
    insertFields <- c(insertFields, "operstat")
    insertValues <- c(insertValues, operstat)
  }

  if (missing(tretspef)) {
    tretspef <- defaults$tretspef
  }
  if (!is.null(tretspef)) {
    insertFields <- c(insertFields, "tretspef")
    insertValues <- c(insertValues, tretspef)
  }

  if (missing(mainspef)) {
    mainspef <- defaults$mainspef
  }
  if (!is.null(mainspef)) {
    insertFields <- c(insertFields, "mainspef")
    insertValues <- c(insertValues, mainspef)
  }

  if (missing(hes_yr)) {
    hes_yr <- defaults$hes_yr
  }
  if (!is.null(hes_yr)) {
    insertFields <- c(insertFields, "hes_yr")
    insertValues <- c(insertValues, hes_yr)
  }

  if (missing(diag_01)) {
    diag_01 <- defaults$diag_01
  }
  if (!is.null(diag_01)) {
    insertFields <- c(insertFields, "diag_01")
    insertValues <- c(insertValues, diag_01)
  }

  if (missing(diag_02)) {
    diag_02 <- defaults$diag_02
  }
  if (!is.null(diag_02)) {
    insertFields <- c(insertFields, "diag_02")
    insertValues <- c(insertValues, diag_02)
  }

  if (missing(diag_03)) {
    diag_03 <- defaults$diag_03
  }
  if (!is.null(diag_03)) {
    insertFields <- c(insertFields, "diag_03")
    insertValues <- c(insertValues, diag_03)
  }

  if (missing(diag_04)) {
    diag_04 <- defaults$diag_04
  }
  if (!is.null(diag_04)) {
    insertFields <- c(insertFields, "diag_04")
    insertValues <- c(insertValues, diag_04)
  }

  if (missing(diag_05)) {
    diag_05 <- defaults$diag_05
  }
  if (!is.null(diag_05)) {
    insertFields <- c(insertFields, "diag_05")
    insertValues <- c(insertValues, diag_05)
  }

  if (missing(diag_06)) {
    diag_06 <- defaults$diag_06
  }
  if (!is.null(diag_06)) {
    insertFields <- c(insertFields, "diag_06")
    insertValues <- c(insertValues, diag_06)
  }

  if (missing(diag_07)) {
    diag_07 <- defaults$diag_07
  }
  if (!is.null(diag_07)) {
    insertFields <- c(insertFields, "diag_07")
    insertValues <- c(insertValues, diag_07)
  }

  if (missing(diag_08)) {
    diag_08 <- defaults$diag_08
  }
  if (!is.null(diag_08)) {
    insertFields <- c(insertFields, "diag_08")
    insertValues <- c(insertValues, diag_08)
  }

  if (missing(diag_09)) {
    diag_09 <- defaults$diag_09
  }
  if (!is.null(diag_09)) {
    insertFields <- c(insertFields, "diag_09")
    insertValues <- c(insertValues, diag_09)
  }

  if (missing(diag_10)) {
    diag_10 <- defaults$diag_10
  }
  if (!is.null(diag_10)) {
    insertFields <- c(insertFields, "diag_10")
    insertValues <- c(insertValues, diag_10)
  }

  if (missing(diag_11)) {
    diag_11 <- defaults$diag_11
  }
  if (!is.null(diag_11)) {
    insertFields <- c(insertFields, "diag_11")
    insertValues <- c(insertValues, diag_11)
  }

  if (missing(diag_12)) {
    diag_12 <- defaults$diag_12
  }
  if (!is.null(diag_12)) {
    insertFields <- c(insertFields, "diag_12")
    insertValues <- c(insertValues, diag_12)
  }

  if (missing(opertn_01)) {
    opertn_01 <- defaults$opertn_01
  }
  if (!is.null(opertn_01)) {
    insertFields <- c(insertFields, "opertn_01")
    insertValues <- c(insertValues, opertn_01)
  }

  if (missing(opertn_02)) {
    opertn_02 <- defaults$opertn_02
  }
  if (!is.null(opertn_02)) {
    insertFields <- c(insertFields, "opertn_02")
    insertValues <- c(insertValues, opertn_02)
  }

  if (missing(opertn_03)) {
    opertn_03 <- defaults$opertn_03
  }
  if (!is.null(opertn_03)) {
    insertFields <- c(insertFields, "opertn_03")
    insertValues <- c(insertValues, opertn_03)
  }

  if (missing(opertn_04)) {
    opertn_04 <- defaults$opertn_04
  }
  if (!is.null(opertn_04)) {
    insertFields <- c(insertFields, "opertn_04")
    insertValues <- c(insertValues, opertn_04)
  }

  if (missing(opertn_05)) {
    opertn_05 <- defaults$opertn_05
  }
  if (!is.null(opertn_05)) {
    insertFields <- c(insertFields, "opertn_05")
    insertValues <- c(insertValues, opertn_05)
  }

  if (missing(opertn_06)) {
    opertn_06 <- defaults$opertn_06
  }
  if (!is.null(opertn_06)) {
    insertFields <- c(insertFields, "opertn_06")
    insertValues <- c(insertValues, opertn_06)
  }

  if (missing(opertn_07)) {
    opertn_07 <- defaults$opertn_07
  }
  if (!is.null(opertn_07)) {
    insertFields <- c(insertFields, "opertn_07")
    insertValues <- c(insertValues, opertn_07)
  }

  if (missing(opertn_08)) {
    opertn_08 <- defaults$opertn_08
  }
  if (!is.null(opertn_08)) {
    insertFields <- c(insertFields, "opertn_08")
    insertValues <- c(insertValues, opertn_08)
  }

  if (missing(opertn_09)) {
    opertn_09 <- defaults$opertn_09
  }
  if (!is.null(opertn_09)) {
    insertFields <- c(insertFields, "opertn_09")
    insertValues <- c(insertValues, opertn_09)
  }

  if (missing(opertn_10)) {
    opertn_10 <- defaults$opertn_10
  }
  if (!is.null(opertn_10)) {
    insertFields <- c(insertFields, "opertn_10")
    insertValues <- c(insertValues, opertn_10)
  }

  if (missing(opertn_11)) {
    opertn_11 <- defaults$opertn_11
  }
  if (!is.null(opertn_11)) {
    insertFields <- c(insertFields, "opertn_11")
    insertValues <- c(insertValues, opertn_11)
  }

  if (missing(opertn_12)) {
    opertn_12 <- defaults$opertn_12
  }
  if (!is.null(opertn_12)) {
    insertFields <- c(insertFields, "opertn_12")
    insertValues <- c(insertValues, opertn_12)
  }

  if (missing(opertn_13)) {
    opertn_13 <- defaults$opertn_13
  }
  if (!is.null(opertn_13)) {
    insertFields <- c(insertFields, "opertn_13")
    insertValues <- c(insertValues, opertn_13)
  }

  if (missing(opertn_14)) {
    opertn_14 <- defaults$opertn_14
  }
  if (!is.null(opertn_14)) {
    insertFields <- c(insertFields, "opertn_14")
    insertValues <- c(insertValues, opertn_14)
  }

  if (missing(opertn_15)) {
    opertn_15 <- defaults$opertn_15
  }
  if (!is.null(opertn_15)) {
    insertFields <- c(insertFields, "opertn_15")
    insertValues <- c(insertValues, opertn_15)
  }

  if (missing(opertn_16)) {
    opertn_16 <- defaults$opertn_16
  }
  if (!is.null(opertn_16)) {
    insertFields <- c(insertFields, "opertn_16")
    insertValues <- c(insertValues, opertn_16)
  }

  if (missing(opertn_17)) {
    opertn_17 <- defaults$opertn_17
  }
  if (!is.null(opertn_17)) {
    insertFields <- c(insertFields, "opertn_17")
    insertValues <- c(insertValues, opertn_17)
  }

  if (missing(opertn_18)) {
    opertn_18 <- defaults$opertn_18
  }
  if (!is.null(opertn_18)) {
    insertFields <- c(insertFields, "opertn_18")
    insertValues <- c(insertValues, opertn_18)
  }

  if (missing(opertn_19)) {
    opertn_19 <- defaults$opertn_19
  }
  if (!is.null(opertn_19)) {
    insertFields <- c(insertFields, "opertn_19")
    insertValues <- c(insertValues, opertn_19)
  }

  if (missing(opertn_20)) {
    opertn_20 <- defaults$opertn_20
  }
  if (!is.null(opertn_20)) {
    insertFields <- c(insertFields, "opertn_20")
    insertValues <- c(insertValues, opertn_20)
  }

  if (missing(opertn_21)) {
    opertn_21 <- defaults$opertn_21
  }
  if (!is.null(opertn_21)) {
    insertFields <- c(insertFields, "opertn_21")
    insertValues <- c(insertValues, opertn_21)
  }

  if (missing(opertn_22)) {
    opertn_22 <- defaults$opertn_22
  }
  if (!is.null(opertn_22)) {
    insertFields <- c(insertFields, "opertn_22")
    insertValues <- c(insertValues, opertn_22)
  }

  if (missing(opertn_23)) {
    opertn_23 <- defaults$opertn_23
  }
  if (!is.null(opertn_23)) {
    insertFields <- c(insertFields, "opertn_23")
    insertValues <- c(insertValues, opertn_23)
  }

  if (missing(opertn_24)) {
    opertn_24 <- defaults$opertn_24
  }
  if (!is.null(opertn_24)) {
    insertFields <- c(insertFields, "opertn_24")
    insertValues <- c(insertValues, opertn_24)
  }

  if (exists('testNewAdded', where = globalenv()) && get('testNewAdded'))
  {
    assign('testNewAdded', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    insertDf[nrow(insertDf) + 1,] <<- c('hes_op_clinical', comment)
  }
  statement <- paste0("INSERT INTO hes_op_clinical (", paste(insertFields, collapse = ", "), ") VALUES ('", paste(insertValues, collapse = "', '"), "');")
  insertDf[nrow(insertDf) + 1,] <<- c('hes_op_clinical', statement)
  invisible(statement)
}

expect_person <- function(person_id, person_source_value, care_site_id, gender_concept_id, gender_source_value, year_of_birth, month_of_birth, day_of_birth, birth_datetime, race_concept_id, race_source_value, ethnicity_concept_id, ethnicity_source_value, location_id, provider_id, gender_source_concept_id, race_source_concept_id, ethnicity_source_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect person' AS test, CASE WHEN(SELECT COUNT(*) FROM person WHERE")
  first <- TRUE
  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(person_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_source_value)) {
      statement <- paste0(statement, ' person_source_value IS NULL')
    } else if (is(person_source_value, 'subQuery')){
      statement <- paste0(statement, ' person_source_value = (', as.character(person_source_value), ')')
    } else {
      statement <- paste0(statement, " person_source_value = '", person_source_value,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(gender_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_concept_id)) {
      statement <- paste0(statement, ' gender_concept_id IS NULL')
    } else if (is(gender_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_concept_id = (', as.character(gender_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_concept_id = '", gender_concept_id,"'")
    }
  }

  if (!missing(gender_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_value)) {
      statement <- paste0(statement, ' gender_source_value IS NULL')
    } else if (is(gender_source_value, 'subQuery')){
      statement <- paste0(statement, ' gender_source_value = (', as.character(gender_source_value), ')')
    } else {
      statement <- paste0(statement, " gender_source_value = '", gender_source_value,"'")
    }
  }

  if (!missing(year_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(year_of_birth)) {
      statement <- paste0(statement, ' year_of_birth IS NULL')
    } else if (is(year_of_birth, 'subQuery')){
      statement <- paste0(statement, ' year_of_birth = (', as.character(year_of_birth), ')')
    } else {
      statement <- paste0(statement, " year_of_birth = '", year_of_birth,"'")
    }
  }

  if (!missing(month_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(month_of_birth)) {
      statement <- paste0(statement, ' month_of_birth IS NULL')
    } else if (is(month_of_birth, 'subQuery')){
      statement <- paste0(statement, ' month_of_birth = (', as.character(month_of_birth), ')')
    } else {
      statement <- paste0(statement, " month_of_birth = '", month_of_birth,"'")
    }
  }

  if (!missing(day_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(day_of_birth)) {
      statement <- paste0(statement, ' day_of_birth IS NULL')
    } else if (is(day_of_birth, 'subQuery')){
      statement <- paste0(statement, ' day_of_birth = (', as.character(day_of_birth), ')')
    } else {
      statement <- paste0(statement, " day_of_birth = '", day_of_birth,"'")
    }
  }

  if (!missing(birth_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(birth_datetime)) {
      statement <- paste0(statement, ' birth_datetime IS NULL')
    } else if (is(birth_datetime, 'subQuery')){
      statement <- paste0(statement, ' birth_datetime = (', as.character(birth_datetime), ')')
    } else {
      statement <- paste0(statement, " birth_datetime = '", birth_datetime,"'")
    }
  }

  if (!missing(race_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_concept_id)) {
      statement <- paste0(statement, ' race_concept_id IS NULL')
    } else if (is(race_concept_id, 'subQuery')){
      statement <- paste0(statement, ' race_concept_id = (', as.character(race_concept_id), ')')
    } else {
      statement <- paste0(statement, " race_concept_id = '", race_concept_id,"'")
    }
  }

  if (!missing(race_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_source_value)) {
      statement <- paste0(statement, ' race_source_value IS NULL')
    } else if (is(race_source_value, 'subQuery')){
      statement <- paste0(statement, ' race_source_value = (', as.character(race_source_value), ')')
    } else {
      statement <- paste0(statement, " race_source_value = '", race_source_value,"'")
    }
  }

  if (!missing(ethnicity_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_concept_id)) {
      statement <- paste0(statement, ' ethnicity_concept_id IS NULL')
    } else if (is(ethnicity_concept_id, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_concept_id = (', as.character(ethnicity_concept_id), ')')
    } else {
      statement <- paste0(statement, " ethnicity_concept_id = '", ethnicity_concept_id,"'")
    }
  }

  if (!missing(ethnicity_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_source_value)) {
      statement <- paste0(statement, ' ethnicity_source_value IS NULL')
    } else if (is(ethnicity_source_value, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_source_value = (', as.character(ethnicity_source_value), ')')
    } else {
      statement <- paste0(statement, " ethnicity_source_value = '", ethnicity_source_value,"'")
    }
  }

  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(gender_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_concept_id)) {
      statement <- paste0(statement, ' gender_source_concept_id IS NULL')
    } else if (is(gender_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_source_concept_id = (', as.character(gender_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_source_concept_id = '", gender_source_concept_id,"'")
    }
  }

  if (!missing(race_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_source_concept_id)) {
      statement <- paste0(statement, ' race_source_concept_id IS NULL')
    } else if (is(race_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' race_source_concept_id = (', as.character(race_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " race_source_concept_id = '", race_source_concept_id,"'")
    }
  }

  if (!missing(ethnicity_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_source_concept_id)) {
      statement <- paste0(statement, ' ethnicity_source_concept_id IS NULL')
    } else if (is(ethnicity_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_source_concept_id = (', as.character(ethnicity_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " ethnicity_source_concept_id = '", ethnicity_source_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_observation_period <- function(observation_period_id, person_id, observation_period_start_date, observation_period_end_date, period_type_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect observation_period' AS test, CASE WHEN(SELECT COUNT(*) FROM observation_period WHERE")
  first <- TRUE
  if (!missing(observation_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_id)) {
      statement <- paste0(statement, ' observation_period_id IS NULL')
    } else if (is(observation_period_id, 'subQuery')){
      statement <- paste0(statement, ' observation_period_id = (', as.character(observation_period_id), ')')
    } else {
      statement <- paste0(statement, " observation_period_id = '", observation_period_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(observation_period_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_start_date)) {
      statement <- paste0(statement, ' observation_period_start_date IS NULL')
    } else if (is(observation_period_start_date, 'subQuery')){
      statement <- paste0(statement, ' observation_period_start_date = (', as.character(observation_period_start_date), ')')
    } else {
      statement <- paste0(statement, " observation_period_start_date = '", observation_period_start_date,"'")
    }
  }

  if (!missing(observation_period_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_end_date)) {
      statement <- paste0(statement, ' observation_period_end_date IS NULL')
    } else if (is(observation_period_end_date, 'subQuery')){
      statement <- paste0(statement, ' observation_period_end_date = (', as.character(observation_period_end_date), ')')
    } else {
      statement <- paste0(statement, " observation_period_end_date = '", observation_period_end_date,"'")
    }
  }

  if (!missing(period_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(period_type_concept_id)) {
      statement <- paste0(statement, ' period_type_concept_id IS NULL')
    } else if (is(period_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' period_type_concept_id = (', as.character(period_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " period_type_concept_id = '", period_type_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_visit_occurrence <- function(visit_occurrence_id, visit_source_value, person_id, care_site_id, visit_start_date, visit_end_date, visit_concept_id, provider_id, visit_type_concept_id, visit_source_concept_id, admitting_source_value, discharge_to_concept_id, discharge_to_source_value, preceding_visit_occurrence_id, visit_start_datetime, visit_end_datetime) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect visit_occurrence' AS test, CASE WHEN(SELECT COUNT(*) FROM visit_occurrence WHERE")
  first <- TRUE
  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_value)) {
      statement <- paste0(statement, ' visit_source_value IS NULL')
    } else if (is(visit_source_value, 'subQuery')){
      statement <- paste0(statement, ' visit_source_value = (', as.character(visit_source_value), ')')
    } else {
      statement <- paste0(statement, " visit_source_value = '", visit_source_value,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(visit_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_date)) {
      statement <- paste0(statement, ' visit_start_date IS NULL')
    } else if (is(visit_start_date, 'subQuery')){
      statement <- paste0(statement, ' visit_start_date = (', as.character(visit_start_date), ')')
    } else {
      statement <- paste0(statement, " visit_start_date = '", visit_start_date,"'")
    }
  }

  if (!missing(visit_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_date)) {
      statement <- paste0(statement, ' visit_end_date IS NULL')
    } else if (is(visit_end_date, 'subQuery')){
      statement <- paste0(statement, ' visit_end_date = (', as.character(visit_end_date), ')')
    } else {
      statement <- paste0(statement, " visit_end_date = '", visit_end_date,"'")
    }
  }

  if (!missing(visit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_concept_id)) {
      statement <- paste0(statement, ' visit_concept_id IS NULL')
    } else if (is(visit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_concept_id = (', as.character(visit_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_concept_id = '", visit_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_type_concept_id)) {
      statement <- paste0(statement, ' visit_type_concept_id IS NULL')
    } else if (is(visit_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_type_concept_id = (', as.character(visit_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_type_concept_id = '", visit_type_concept_id,"'")
    }
  }

  if (!missing(visit_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_concept_id)) {
      statement <- paste0(statement, ' visit_source_concept_id IS NULL')
    } else if (is(visit_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_source_concept_id = (', as.character(visit_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_source_concept_id = '", visit_source_concept_id,"'")
    }
  }

  if (!missing(admitting_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(admitting_source_value)) {
      statement <- paste0(statement, ' admitting_source_value IS NULL')
    } else if (is(admitting_source_value, 'subQuery')){
      statement <- paste0(statement, ' admitting_source_value = (', as.character(admitting_source_value), ')')
    } else {
      statement <- paste0(statement, " admitting_source_value = '", admitting_source_value,"'")
    }
  }

  if (!missing(discharge_to_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_concept_id)) {
      statement <- paste0(statement, ' discharge_to_concept_id IS NULL')
    } else if (is(discharge_to_concept_id, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_concept_id = (', as.character(discharge_to_concept_id), ')')
    } else {
      statement <- paste0(statement, " discharge_to_concept_id = '", discharge_to_concept_id,"'")
    }
  }

  if (!missing(discharge_to_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_source_value)) {
      statement <- paste0(statement, ' discharge_to_source_value IS NULL')
    } else if (is(discharge_to_source_value, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_source_value = (', as.character(discharge_to_source_value), ')')
    } else {
      statement <- paste0(statement, " discharge_to_source_value = '", discharge_to_source_value,"'")
    }
  }

  if (!missing(preceding_visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(preceding_visit_occurrence_id)) {
      statement <- paste0(statement, ' preceding_visit_occurrence_id IS NULL')
    } else if (is(preceding_visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' preceding_visit_occurrence_id = (', as.character(preceding_visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " preceding_visit_occurrence_id = '", preceding_visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_datetime)) {
      statement <- paste0(statement, ' visit_start_datetime IS NULL')
    } else if (is(visit_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_start_datetime = (', as.character(visit_start_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_start_datetime = '", visit_start_datetime,"'")
    }
  }

  if (!missing(visit_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_datetime)) {
      statement <- paste0(statement, ' visit_end_datetime IS NULL')
    } else if (is(visit_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_end_datetime = (', as.character(visit_end_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_end_datetime = '", visit_end_datetime,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_visit_detail <- function(visit_detail_id, person_id, visit_detail_concept_id, visit_start_date, visit_start_datetime, visit_end_date, visit_end_datetime, visit_type_concept_id, provider_id, care_site_id, visit_source_value, visit_source_concept_id, admitting_source_value, discharge_to_source_value, discharge_to_concept_id, preceding_visit_detail_id, visit_detail_parent_id, visit_occurrence_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect visit_detail' AS test, CASE WHEN(SELECT COUNT(*) FROM visit_detail WHERE")
  first <- TRUE
  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(visit_detail_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_concept_id)) {
      statement <- paste0(statement, ' visit_detail_concept_id IS NULL')
    } else if (is(visit_detail_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_concept_id = (', as.character(visit_detail_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_concept_id = '", visit_detail_concept_id,"'")
    }
  }

  if (!missing(visit_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_date)) {
      statement <- paste0(statement, ' visit_start_date IS NULL')
    } else if (is(visit_start_date, 'subQuery')){
      statement <- paste0(statement, ' visit_start_date = (', as.character(visit_start_date), ')')
    } else {
      statement <- paste0(statement, " visit_start_date = '", visit_start_date,"'")
    }
  }

  if (!missing(visit_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_datetime)) {
      statement <- paste0(statement, ' visit_start_datetime IS NULL')
    } else if (is(visit_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_start_datetime = (', as.character(visit_start_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_start_datetime = '", visit_start_datetime,"'")
    }
  }

  if (!missing(visit_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_date)) {
      statement <- paste0(statement, ' visit_end_date IS NULL')
    } else if (is(visit_end_date, 'subQuery')){
      statement <- paste0(statement, ' visit_end_date = (', as.character(visit_end_date), ')')
    } else {
      statement <- paste0(statement, " visit_end_date = '", visit_end_date,"'")
    }
  }

  if (!missing(visit_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_datetime)) {
      statement <- paste0(statement, ' visit_end_datetime IS NULL')
    } else if (is(visit_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_end_datetime = (', as.character(visit_end_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_end_datetime = '", visit_end_datetime,"'")
    }
  }

  if (!missing(visit_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_type_concept_id)) {
      statement <- paste0(statement, ' visit_type_concept_id IS NULL')
    } else if (is(visit_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_type_concept_id = (', as.character(visit_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_type_concept_id = '", visit_type_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(visit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_value)) {
      statement <- paste0(statement, ' visit_source_value IS NULL')
    } else if (is(visit_source_value, 'subQuery')){
      statement <- paste0(statement, ' visit_source_value = (', as.character(visit_source_value), ')')
    } else {
      statement <- paste0(statement, " visit_source_value = '", visit_source_value,"'")
    }
  }

  if (!missing(visit_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_concept_id)) {
      statement <- paste0(statement, ' visit_source_concept_id IS NULL')
    } else if (is(visit_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_source_concept_id = (', as.character(visit_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_source_concept_id = '", visit_source_concept_id,"'")
    }
  }

  if (!missing(admitting_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(admitting_source_value)) {
      statement <- paste0(statement, ' admitting_source_value IS NULL')
    } else if (is(admitting_source_value, 'subQuery')){
      statement <- paste0(statement, ' admitting_source_value = (', as.character(admitting_source_value), ')')
    } else {
      statement <- paste0(statement, " admitting_source_value = '", admitting_source_value,"'")
    }
  }

  if (!missing(discharge_to_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_source_value)) {
      statement <- paste0(statement, ' discharge_to_source_value IS NULL')
    } else if (is(discharge_to_source_value, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_source_value = (', as.character(discharge_to_source_value), ')')
    } else {
      statement <- paste0(statement, " discharge_to_source_value = '", discharge_to_source_value,"'")
    }
  }

  if (!missing(discharge_to_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_concept_id)) {
      statement <- paste0(statement, ' discharge_to_concept_id IS NULL')
    } else if (is(discharge_to_concept_id, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_concept_id = (', as.character(discharge_to_concept_id), ')')
    } else {
      statement <- paste0(statement, " discharge_to_concept_id = '", discharge_to_concept_id,"'")
    }
  }

  if (!missing(preceding_visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(preceding_visit_detail_id)) {
      statement <- paste0(statement, ' preceding_visit_detail_id IS NULL')
    } else if (is(preceding_visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' preceding_visit_detail_id = (', as.character(preceding_visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " preceding_visit_detail_id = '", preceding_visit_detail_id,"'")
    }
  }

  if (!missing(visit_detail_parent_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_parent_id)) {
      statement <- paste0(statement, ' visit_detail_parent_id IS NULL')
    } else if (is(visit_detail_parent_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_parent_id = (', as.character(visit_detail_parent_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_parent_id = '", visit_detail_parent_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_condition_occurrence <- function(condition_occurrence_id, person_id, condition_start_date, visit_occurrence_id, condition_concept_id, condition_start_datetime, condition_end_date, condition_end_datetime, condition_type_concept_id, stop_reason, provider_id, visit_detail_id, condition_source_concept_id, condition_source_value, condition_status_source_value, condition_status_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect condition_occurrence' AS test, CASE WHEN(SELECT COUNT(*) FROM condition_occurrence WHERE")
  first <- TRUE
  if (!missing(condition_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_occurrence_id)) {
      statement <- paste0(statement, ' condition_occurrence_id IS NULL')
    } else if (is(condition_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' condition_occurrence_id = (', as.character(condition_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " condition_occurrence_id = '", condition_occurrence_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(condition_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_start_date)) {
      statement <- paste0(statement, ' condition_start_date IS NULL')
    } else if (is(condition_start_date, 'subQuery')){
      statement <- paste0(statement, ' condition_start_date = (', as.character(condition_start_date), ')')
    } else {
      statement <- paste0(statement, " condition_start_date = '", condition_start_date,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(condition_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_concept_id)) {
      statement <- paste0(statement, ' condition_concept_id IS NULL')
    } else if (is(condition_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_concept_id = (', as.character(condition_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_concept_id = '", condition_concept_id,"'")
    }
  }

  if (!missing(condition_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_start_datetime)) {
      statement <- paste0(statement, ' condition_start_datetime IS NULL')
    } else if (is(condition_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' condition_start_datetime = (', as.character(condition_start_datetime), ')')
    } else {
      statement <- paste0(statement, " condition_start_datetime = '", condition_start_datetime,"'")
    }
  }

  if (!missing(condition_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_end_date)) {
      statement <- paste0(statement, ' condition_end_date IS NULL')
    } else if (is(condition_end_date, 'subQuery')){
      statement <- paste0(statement, ' condition_end_date = (', as.character(condition_end_date), ')')
    } else {
      statement <- paste0(statement, " condition_end_date = '", condition_end_date,"'")
    }
  }

  if (!missing(condition_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_end_datetime)) {
      statement <- paste0(statement, ' condition_end_datetime IS NULL')
    } else if (is(condition_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' condition_end_datetime = (', as.character(condition_end_datetime), ')')
    } else {
      statement <- paste0(statement, " condition_end_datetime = '", condition_end_datetime,"'")
    }
  }

  if (!missing(condition_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_type_concept_id)) {
      statement <- paste0(statement, ' condition_type_concept_id IS NULL')
    } else if (is(condition_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_type_concept_id = (', as.character(condition_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_type_concept_id = '", condition_type_concept_id,"'")
    }
  }

  if (!missing(stop_reason)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(stop_reason)) {
      statement <- paste0(statement, ' stop_reason IS NULL')
    } else if (is(stop_reason, 'subQuery')){
      statement <- paste0(statement, ' stop_reason = (', as.character(stop_reason), ')')
    } else {
      statement <- paste0(statement, " stop_reason = '", stop_reason,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(condition_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_source_concept_id)) {
      statement <- paste0(statement, ' condition_source_concept_id IS NULL')
    } else if (is(condition_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_source_concept_id = (', as.character(condition_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_source_concept_id = '", condition_source_concept_id,"'")
    }
  }

  if (!missing(condition_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_source_value)) {
      statement <- paste0(statement, ' condition_source_value IS NULL')
    } else if (is(condition_source_value, 'subQuery')){
      statement <- paste0(statement, ' condition_source_value = (', as.character(condition_source_value), ')')
    } else {
      statement <- paste0(statement, " condition_source_value = '", condition_source_value,"'")
    }
  }

  if (!missing(condition_status_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_status_source_value)) {
      statement <- paste0(statement, ' condition_status_source_value IS NULL')
    } else if (is(condition_status_source_value, 'subQuery')){
      statement <- paste0(statement, ' condition_status_source_value = (', as.character(condition_status_source_value), ')')
    } else {
      statement <- paste0(statement, " condition_status_source_value = '", condition_status_source_value,"'")
    }
  }

  if (!missing(condition_status_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_status_concept_id)) {
      statement <- paste0(statement, ' condition_status_concept_id IS NULL')
    } else if (is(condition_status_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_status_concept_id = (', as.character(condition_status_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_status_concept_id = '", condition_status_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_procedure_occurrence <- function(procedure_occurrence_id, visit_occurrence_id, person_id, procedure_concept_id, procedure_date, procedure_datetime, procedure_type_concept_id, modifier_concept_id, quantity, provider_id, procedure_source_value, procedure_source_concept_id, qualifier_source_value, visit_detail_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect procedure_occurrence' AS test, CASE WHEN(SELECT COUNT(*) FROM procedure_occurrence WHERE")
  first <- TRUE
  if (!missing(procedure_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_occurrence_id)) {
      statement <- paste0(statement, ' procedure_occurrence_id IS NULL')
    } else if (is(procedure_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_occurrence_id = (', as.character(procedure_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " procedure_occurrence_id = '", procedure_occurrence_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(procedure_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_concept_id)) {
      statement <- paste0(statement, ' procedure_concept_id IS NULL')
    } else if (is(procedure_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_concept_id = (', as.character(procedure_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_concept_id = '", procedure_concept_id,"'")
    }
  }

  if (!missing(procedure_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_date)) {
      statement <- paste0(statement, ' procedure_date IS NULL')
    } else if (is(procedure_date, 'subQuery')){
      statement <- paste0(statement, ' procedure_date = (', as.character(procedure_date), ')')
    } else {
      statement <- paste0(statement, " procedure_date = '", procedure_date,"'")
    }
  }

  if (!missing(procedure_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_datetime)) {
      statement <- paste0(statement, ' procedure_datetime IS NULL')
    } else if (is(procedure_datetime, 'subQuery')){
      statement <- paste0(statement, ' procedure_datetime = (', as.character(procedure_datetime), ')')
    } else {
      statement <- paste0(statement, " procedure_datetime = '", procedure_datetime,"'")
    }
  }

  if (!missing(procedure_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_type_concept_id)) {
      statement <- paste0(statement, ' procedure_type_concept_id IS NULL')
    } else if (is(procedure_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_type_concept_id = (', as.character(procedure_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_type_concept_id = '", procedure_type_concept_id,"'")
    }
  }

  if (!missing(modifier_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(modifier_concept_id)) {
      statement <- paste0(statement, ' modifier_concept_id IS NULL')
    } else if (is(modifier_concept_id, 'subQuery')){
      statement <- paste0(statement, ' modifier_concept_id = (', as.character(modifier_concept_id), ')')
    } else {
      statement <- paste0(statement, " modifier_concept_id = '", modifier_concept_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(procedure_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_source_value)) {
      statement <- paste0(statement, ' procedure_source_value IS NULL')
    } else if (is(procedure_source_value, 'subQuery')){
      statement <- paste0(statement, ' procedure_source_value = (', as.character(procedure_source_value), ')')
    } else {
      statement <- paste0(statement, " procedure_source_value = '", procedure_source_value,"'")
    }
  }

  if (!missing(procedure_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_source_concept_id)) {
      statement <- paste0(statement, ' procedure_source_concept_id IS NULL')
    } else if (is(procedure_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_source_concept_id = (', as.character(procedure_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_source_concept_id = '", procedure_source_concept_id,"'")
    }
  }

  if (!missing(qualifier_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_source_value)) {
      statement <- paste0(statement, ' qualifier_source_value IS NULL')
    } else if (is(qualifier_source_value, 'subQuery')){
      statement <- paste0(statement, ' qualifier_source_value = (', as.character(qualifier_source_value), ')')
    } else {
      statement <- paste0(statement, " qualifier_source_value = '", qualifier_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_drug_exposure <- function(drug_exposure_id, person_id, drug_exposure_start_date, drug_exposure_end_date, days_supply, drug_concept_id, drug_source_value, drug_source_concept_id, provider_id, drug_type_concept_id, quantity, sig, refills, visit_occurrence_id, route_source_value, dose_unit_source_value, visit_detail_id, drug_exposure_start_datetime, drug_exposure_end_datetime, verbatim_end_date, route_concept_id, stop_reason, lot_number) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect drug_exposure' AS test, CASE WHEN(SELECT COUNT(*) FROM drug_exposure WHERE")
  first <- TRUE
  if (!missing(drug_exposure_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_id)) {
      statement <- paste0(statement, ' drug_exposure_id IS NULL')
    } else if (is(drug_exposure_id, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_id = (', as.character(drug_exposure_id), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_id = '", drug_exposure_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_exposure_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_start_date)) {
      statement <- paste0(statement, ' drug_exposure_start_date IS NULL')
    } else if (is(drug_exposure_start_date, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_start_date = (', as.character(drug_exposure_start_date), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_start_date = '", drug_exposure_start_date,"'")
    }
  }

  if (!missing(drug_exposure_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_end_date)) {
      statement <- paste0(statement, ' drug_exposure_end_date IS NULL')
    } else if (is(drug_exposure_end_date, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_end_date = (', as.character(drug_exposure_end_date), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_end_date = '", drug_exposure_end_date,"'")
    }
  }

  if (!missing(days_supply)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(days_supply)) {
      statement <- paste0(statement, ' days_supply IS NULL')
    } else if (is(days_supply, 'subQuery')){
      statement <- paste0(statement, ' days_supply = (', as.character(days_supply), ')')
    } else {
      statement <- paste0(statement, " days_supply = '", days_supply,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(drug_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_source_value)) {
      statement <- paste0(statement, ' drug_source_value IS NULL')
    } else if (is(drug_source_value, 'subQuery')){
      statement <- paste0(statement, ' drug_source_value = (', as.character(drug_source_value), ')')
    } else {
      statement <- paste0(statement, " drug_source_value = '", drug_source_value,"'")
    }
  }

  if (!missing(drug_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_source_concept_id)) {
      statement <- paste0(statement, ' drug_source_concept_id IS NULL')
    } else if (is(drug_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_source_concept_id = (', as.character(drug_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_source_concept_id = '", drug_source_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(drug_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_type_concept_id)) {
      statement <- paste0(statement, ' drug_type_concept_id IS NULL')
    } else if (is(drug_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_type_concept_id = (', as.character(drug_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_type_concept_id = '", drug_type_concept_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(sig)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(sig)) {
      statement <- paste0(statement, ' sig IS NULL')
    } else if (is(sig, 'subQuery')){
      statement <- paste0(statement, ' sig = (', as.character(sig), ')')
    } else {
      statement <- paste0(statement, " sig = '", sig,"'")
    }
  }

  if (!missing(refills)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(refills)) {
      statement <- paste0(statement, ' refills IS NULL')
    } else if (is(refills, 'subQuery')){
      statement <- paste0(statement, ' refills = (', as.character(refills), ')')
    } else {
      statement <- paste0(statement, " refills = '", refills,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(route_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(route_source_value)) {
      statement <- paste0(statement, ' route_source_value IS NULL')
    } else if (is(route_source_value, 'subQuery')){
      statement <- paste0(statement, ' route_source_value = (', as.character(route_source_value), ')')
    } else {
      statement <- paste0(statement, " route_source_value = '", route_source_value,"'")
    }
  }

  if (!missing(dose_unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_unit_source_value)) {
      statement <- paste0(statement, ' dose_unit_source_value IS NULL')
    } else if (is(dose_unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' dose_unit_source_value = (', as.character(dose_unit_source_value), ')')
    } else {
      statement <- paste0(statement, " dose_unit_source_value = '", dose_unit_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(drug_exposure_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_start_datetime)) {
      statement <- paste0(statement, ' drug_exposure_start_datetime IS NULL')
    } else if (is(drug_exposure_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_start_datetime = (', as.character(drug_exposure_start_datetime), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_start_datetime = '", drug_exposure_start_datetime,"'")
    }
  }

  if (!missing(drug_exposure_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_end_datetime)) {
      statement <- paste0(statement, ' drug_exposure_end_datetime IS NULL')
    } else if (is(drug_exposure_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_end_datetime = (', as.character(drug_exposure_end_datetime), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_end_datetime = '", drug_exposure_end_datetime,"'")
    }
  }

  if (!missing(verbatim_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(verbatim_end_date)) {
      statement <- paste0(statement, ' verbatim_end_date IS NULL')
    } else if (is(verbatim_end_date, 'subQuery')){
      statement <- paste0(statement, ' verbatim_end_date = (', as.character(verbatim_end_date), ')')
    } else {
      statement <- paste0(statement, " verbatim_end_date = '", verbatim_end_date,"'")
    }
  }

  if (!missing(route_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(route_concept_id)) {
      statement <- paste0(statement, ' route_concept_id IS NULL')
    } else if (is(route_concept_id, 'subQuery')){
      statement <- paste0(statement, ' route_concept_id = (', as.character(route_concept_id), ')')
    } else {
      statement <- paste0(statement, " route_concept_id = '", route_concept_id,"'")
    }
  }

  if (!missing(stop_reason)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(stop_reason)) {
      statement <- paste0(statement, ' stop_reason IS NULL')
    } else if (is(stop_reason, 'subQuery')){
      statement <- paste0(statement, ' stop_reason = (', as.character(stop_reason), ')')
    } else {
      statement <- paste0(statement, " stop_reason = '", stop_reason,"'")
    }
  }

  if (!missing(lot_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(lot_number)) {
      statement <- paste0(statement, ' lot_number IS NULL')
    } else if (is(lot_number, 'subQuery')){
      statement <- paste0(statement, ' lot_number = (', as.character(lot_number), ')')
    } else {
      statement <- paste0(statement, " lot_number = '", lot_number,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_measurement <- function(measurement_id, person_id, measurement_concept_id, measurement_date, measurement_datetime, measurement_type_concept_id, operator_concept_id, value_as_number, value_as_concept_id, unit_concept_id, range_low, range_high, provider_id, visit_occurrence_id, visit_detail_id, measurement_source_value, measurement_source_concept_id, unit_source_value, value_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect measurement' AS test, CASE WHEN(SELECT COUNT(*) FROM measurement WHERE")
  first <- TRUE
  if (!missing(measurement_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_id)) {
      statement <- paste0(statement, ' measurement_id IS NULL')
    } else if (is(measurement_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_id = (', as.character(measurement_id), ')')
    } else {
      statement <- paste0(statement, " measurement_id = '", measurement_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(measurement_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_concept_id)) {
      statement <- paste0(statement, ' measurement_concept_id IS NULL')
    } else if (is(measurement_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_concept_id = (', as.character(measurement_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_concept_id = '", measurement_concept_id,"'")
    }
  }

  if (!missing(measurement_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_date)) {
      statement <- paste0(statement, ' measurement_date IS NULL')
    } else if (is(measurement_date, 'subQuery')){
      statement <- paste0(statement, ' measurement_date = (', as.character(measurement_date), ')')
    } else {
      statement <- paste0(statement, " measurement_date = '", measurement_date,"'")
    }
  }

  if (!missing(measurement_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_datetime)) {
      statement <- paste0(statement, ' measurement_datetime IS NULL')
    } else if (is(measurement_datetime, 'subQuery')){
      statement <- paste0(statement, ' measurement_datetime = (', as.character(measurement_datetime), ')')
    } else {
      statement <- paste0(statement, " measurement_datetime = '", measurement_datetime,"'")
    }
  }

  if (!missing(measurement_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_type_concept_id)) {
      statement <- paste0(statement, ' measurement_type_concept_id IS NULL')
    } else if (is(measurement_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_type_concept_id = (', as.character(measurement_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_type_concept_id = '", measurement_type_concept_id,"'")
    }
  }

  if (!missing(operator_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(operator_concept_id)) {
      statement <- paste0(statement, ' operator_concept_id IS NULL')
    } else if (is(operator_concept_id, 'subQuery')){
      statement <- paste0(statement, ' operator_concept_id = (', as.character(operator_concept_id), ')')
    } else {
      statement <- paste0(statement, " operator_concept_id = '", operator_concept_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(range_low)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(range_low)) {
      statement <- paste0(statement, ' range_low IS NULL')
    } else if (is(range_low, 'subQuery')){
      statement <- paste0(statement, ' range_low = (', as.character(range_low), ')')
    } else {
      statement <- paste0(statement, " range_low = '", range_low,"'")
    }
  }

  if (!missing(range_high)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(range_high)) {
      statement <- paste0(statement, ' range_high IS NULL')
    } else if (is(range_high, 'subQuery')){
      statement <- paste0(statement, ' range_high = (', as.character(range_high), ')')
    } else {
      statement <- paste0(statement, " range_high = '", range_high,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(measurement_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_source_value)) {
      statement <- paste0(statement, ' measurement_source_value IS NULL')
    } else if (is(measurement_source_value, 'subQuery')){
      statement <- paste0(statement, ' measurement_source_value = (', as.character(measurement_source_value), ')')
    } else {
      statement <- paste0(statement, " measurement_source_value = '", measurement_source_value,"'")
    }
  }

  if (!missing(measurement_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_source_concept_id)) {
      statement <- paste0(statement, ' measurement_source_concept_id IS NULL')
    } else if (is(measurement_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_source_concept_id = (', as.character(measurement_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_source_concept_id = '", measurement_source_concept_id,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(value_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_source_value)) {
      statement <- paste0(statement, ' value_source_value IS NULL')
    } else if (is(value_source_value, 'subQuery')){
      statement <- paste0(statement, ' value_source_value = (', as.character(value_source_value), ')')
    } else {
      statement <- paste0(statement, " value_source_value = '", value_source_value,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_observation <- function(observation_id, person_id, visit_occurrence_id, observation_date, observation_concept_id, observation_source_value, provider_id, observation_type_concept_id, value_as_number, value_as_string, qualifier_concept_id, qualifier_source_value, unit_concept_id, unit_source_value, visit_detail_id, observation_source_concept_id, observation_datetime, value_as_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect observation' AS test, CASE WHEN(SELECT COUNT(*) FROM observation WHERE")
  first <- TRUE
  if (!missing(observation_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_id)) {
      statement <- paste0(statement, ' observation_id IS NULL')
    } else if (is(observation_id, 'subQuery')){
      statement <- paste0(statement, ' observation_id = (', as.character(observation_id), ')')
    } else {
      statement <- paste0(statement, " observation_id = '", observation_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(observation_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_date)) {
      statement <- paste0(statement, ' observation_date IS NULL')
    } else if (is(observation_date, 'subQuery')){
      statement <- paste0(statement, ' observation_date = (', as.character(observation_date), ')')
    } else {
      statement <- paste0(statement, " observation_date = '", observation_date,"'")
    }
  }

  if (!missing(observation_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_concept_id)) {
      statement <- paste0(statement, ' observation_concept_id IS NULL')
    } else if (is(observation_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_concept_id = (', as.character(observation_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_concept_id = '", observation_concept_id,"'")
    }
  }

  if (!missing(observation_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_source_value)) {
      statement <- paste0(statement, ' observation_source_value IS NULL')
    } else if (is(observation_source_value, 'subQuery')){
      statement <- paste0(statement, ' observation_source_value = (', as.character(observation_source_value), ')')
    } else {
      statement <- paste0(statement, " observation_source_value = '", observation_source_value,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(observation_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_type_concept_id)) {
      statement <- paste0(statement, ' observation_type_concept_id IS NULL')
    } else if (is(observation_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_type_concept_id = (', as.character(observation_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_type_concept_id = '", observation_type_concept_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_string)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_string)) {
      statement <- paste0(statement, ' value_as_string IS NULL')
    } else if (is(value_as_string, 'subQuery')){
      statement <- paste0(statement, ' value_as_string = (', as.character(value_as_string), ')')
    } else {
      statement <- paste0(statement, " value_as_string = '", value_as_string,"'")
    }
  }

  if (!missing(qualifier_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_concept_id)) {
      statement <- paste0(statement, ' qualifier_concept_id IS NULL')
    } else if (is(qualifier_concept_id, 'subQuery')){
      statement <- paste0(statement, ' qualifier_concept_id = (', as.character(qualifier_concept_id), ')')
    } else {
      statement <- paste0(statement, " qualifier_concept_id = '", qualifier_concept_id,"'")
    }
  }

  if (!missing(qualifier_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_source_value)) {
      statement <- paste0(statement, ' qualifier_source_value IS NULL')
    } else if (is(qualifier_source_value, 'subQuery')){
      statement <- paste0(statement, ' qualifier_source_value = (', as.character(qualifier_source_value), ')')
    } else {
      statement <- paste0(statement, " qualifier_source_value = '", qualifier_source_value,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(observation_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_source_concept_id)) {
      statement <- paste0(statement, ' observation_source_concept_id IS NULL')
    } else if (is(observation_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_source_concept_id = (', as.character(observation_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_source_concept_id = '", observation_source_concept_id,"'")
    }
  }

  if (!missing(observation_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_datetime)) {
      statement <- paste0(statement, ' observation_datetime IS NULL')
    } else if (is(observation_datetime, 'subQuery')){
      statement <- paste0(statement, ' observation_datetime = (', as.character(observation_datetime), ')')
    } else {
      statement <- paste0(statement, " observation_datetime = '", observation_datetime,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_device_exposure <- function(device_exposure_id, person_id, device_exposure_start_date, device_concept_id, device_exposure_start_datetime, device_exposure_end_date, device_exposure_end_datetime, device_type_concept_id, unique_device_id, quantity, provider_id, visit_occurrence_id, device_source_value, device_source__concept_id, visit_detail_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect device_exposure' AS test, CASE WHEN(SELECT COUNT(*) FROM device_exposure WHERE")
  first <- TRUE
  if (!missing(device_exposure_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_id)) {
      statement <- paste0(statement, ' device_exposure_id IS NULL')
    } else if (is(device_exposure_id, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_id = (', as.character(device_exposure_id), ')')
    } else {
      statement <- paste0(statement, " device_exposure_id = '", device_exposure_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(device_exposure_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_start_date)) {
      statement <- paste0(statement, ' device_exposure_start_date IS NULL')
    } else if (is(device_exposure_start_date, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_start_date = (', as.character(device_exposure_start_date), ')')
    } else {
      statement <- paste0(statement, " device_exposure_start_date = '", device_exposure_start_date,"'")
    }
  }

  if (!missing(device_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_concept_id)) {
      statement <- paste0(statement, ' device_concept_id IS NULL')
    } else if (is(device_concept_id, 'subQuery')){
      statement <- paste0(statement, ' device_concept_id = (', as.character(device_concept_id), ')')
    } else {
      statement <- paste0(statement, " device_concept_id = '", device_concept_id,"'")
    }
  }

  if (!missing(device_exposure_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_start_datetime)) {
      statement <- paste0(statement, ' device_exposure_start_datetime IS NULL')
    } else if (is(device_exposure_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_start_datetime = (', as.character(device_exposure_start_datetime), ')')
    } else {
      statement <- paste0(statement, " device_exposure_start_datetime = '", device_exposure_start_datetime,"'")
    }
  }

  if (!missing(device_exposure_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_end_date)) {
      statement <- paste0(statement, ' device_exposure_end_date IS NULL')
    } else if (is(device_exposure_end_date, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_end_date = (', as.character(device_exposure_end_date), ')')
    } else {
      statement <- paste0(statement, " device_exposure_end_date = '", device_exposure_end_date,"'")
    }
  }

  if (!missing(device_exposure_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_end_datetime)) {
      statement <- paste0(statement, ' device_exposure_end_datetime IS NULL')
    } else if (is(device_exposure_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_end_datetime = (', as.character(device_exposure_end_datetime), ')')
    } else {
      statement <- paste0(statement, " device_exposure_end_datetime = '", device_exposure_end_datetime,"'")
    }
  }

  if (!missing(device_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_type_concept_id)) {
      statement <- paste0(statement, ' device_type_concept_id IS NULL')
    } else if (is(device_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' device_type_concept_id = (', as.character(device_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " device_type_concept_id = '", device_type_concept_id,"'")
    }
  }

  if (!missing(unique_device_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unique_device_id)) {
      statement <- paste0(statement, ' unique_device_id IS NULL')
    } else if (is(unique_device_id, 'subQuery')){
      statement <- paste0(statement, ' unique_device_id = (', as.character(unique_device_id), ')')
    } else {
      statement <- paste0(statement, " unique_device_id = '", unique_device_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(device_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_source_value)) {
      statement <- paste0(statement, ' device_source_value IS NULL')
    } else if (is(device_source_value, 'subQuery')){
      statement <- paste0(statement, ' device_source_value = (', as.character(device_source_value), ')')
    } else {
      statement <- paste0(statement, " device_source_value = '", device_source_value,"'")
    }
  }

  if (!missing(device_source__concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_source__concept_id)) {
      statement <- paste0(statement, ' [device_source_ concept_id] IS NULL')
    } else if (is(device_source__concept_id, 'subQuery')){
      statement <- paste0(statement, ' [device_source_ concept_id] = (', as.character(device_source__concept_id), ')')
    } else {
      statement <- paste0(statement, " [device_source_ concept_id] = '", device_source__concept_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_death <- function(person_id, death_date, death_datetime, death_type_concept_id, cause_concept_id, cause_source_value, cause_source_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect death' AS test, CASE WHEN(SELECT COUNT(*) FROM death WHERE")
  first <- TRUE
  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(death_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_date)) {
      statement <- paste0(statement, ' death_date IS NULL')
    } else if (is(death_date, 'subQuery')){
      statement <- paste0(statement, ' death_date = (', as.character(death_date), ')')
    } else {
      statement <- paste0(statement, " death_date = '", death_date,"'")
    }
  }

  if (!missing(death_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_datetime)) {
      statement <- paste0(statement, ' death_datetime IS NULL')
    } else if (is(death_datetime, 'subQuery')){
      statement <- paste0(statement, ' death_datetime = (', as.character(death_datetime), ')')
    } else {
      statement <- paste0(statement, " death_datetime = '", death_datetime,"'")
    }
  }

  if (!missing(death_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_type_concept_id)) {
      statement <- paste0(statement, ' death_type_concept_id IS NULL')
    } else if (is(death_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' death_type_concept_id = (', as.character(death_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " death_type_concept_id = '", death_type_concept_id,"'")
    }
  }

  if (!missing(cause_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_concept_id)) {
      statement <- paste0(statement, ' cause_concept_id IS NULL')
    } else if (is(cause_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cause_concept_id = (', as.character(cause_concept_id), ')')
    } else {
      statement <- paste0(statement, " cause_concept_id = '", cause_concept_id,"'")
    }
  }

  if (!missing(cause_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_source_value)) {
      statement <- paste0(statement, ' cause_source_value IS NULL')
    } else if (is(cause_source_value, 'subQuery')){
      statement <- paste0(statement, ' cause_source_value = (', as.character(cause_source_value), ')')
    } else {
      statement <- paste0(statement, " cause_source_value = '", cause_source_value,"'")
    }
  }

  if (!missing(cause_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_source_concept_id)) {
      statement <- paste0(statement, ' cause_source_concept_id IS NULL')
    } else if (is(cause_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cause_source_concept_id = (', as.character(cause_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " cause_source_concept_id = '", cause_source_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_location <- function(location_id, address_1, address_2, city, state, zip, county, location_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect location' AS test, CASE WHEN(SELECT COUNT(*) FROM location WHERE")
  first <- TRUE
  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(address_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(address_1)) {
      statement <- paste0(statement, ' address_1 IS NULL')
    } else if (is(address_1, 'subQuery')){
      statement <- paste0(statement, ' address_1 = (', as.character(address_1), ')')
    } else {
      statement <- paste0(statement, " address_1 = '", address_1,"'")
    }
  }

  if (!missing(address_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(address_2)) {
      statement <- paste0(statement, ' address_2 IS NULL')
    } else if (is(address_2, 'subQuery')){
      statement <- paste0(statement, ' address_2 = (', as.character(address_2), ')')
    } else {
      statement <- paste0(statement, " address_2 = '", address_2,"'")
    }
  }

  if (!missing(city)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(city)) {
      statement <- paste0(statement, ' city IS NULL')
    } else if (is(city, 'subQuery')){
      statement <- paste0(statement, ' city = (', as.character(city), ')')
    } else {
      statement <- paste0(statement, " city = '", city,"'")
    }
  }

  if (!missing(state)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(state)) {
      statement <- paste0(statement, ' state IS NULL')
    } else if (is(state, 'subQuery')){
      statement <- paste0(statement, ' state = (', as.character(state), ')')
    } else {
      statement <- paste0(statement, " state = '", state,"'")
    }
  }

  if (!missing(zip)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(zip)) {
      statement <- paste0(statement, ' zip IS NULL')
    } else if (is(zip, 'subQuery')){
      statement <- paste0(statement, ' zip = (', as.character(zip), ')')
    } else {
      statement <- paste0(statement, " zip = '", zip,"'")
    }
  }

  if (!missing(county)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(county)) {
      statement <- paste0(statement, ' county IS NULL')
    } else if (is(county, 'subQuery')){
      statement <- paste0(statement, ' county = (', as.character(county), ')')
    } else {
      statement <- paste0(statement, " county = '", county,"'")
    }
  }

  if (!missing(location_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_source_value)) {
      statement <- paste0(statement, ' location_source_value IS NULL')
    } else if (is(location_source_value, 'subQuery')){
      statement <- paste0(statement, ' location_source_value = (', as.character(location_source_value), ')')
    } else {
      statement <- paste0(statement, " location_source_value = '", location_source_value,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_care_site <- function(care_site_id, care_site_source_value, location_id, care_site_name, place_of_service_concept_id, place_of_service_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect care_site' AS test, CASE WHEN(SELECT COUNT(*) FROM care_site WHERE")
  first <- TRUE
  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(care_site_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_source_value)) {
      statement <- paste0(statement, ' care_site_source_value IS NULL')
    } else if (is(care_site_source_value, 'subQuery')){
      statement <- paste0(statement, ' care_site_source_value = (', as.character(care_site_source_value), ')')
    } else {
      statement <- paste0(statement, " care_site_source_value = '", care_site_source_value,"'")
    }
  }

  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(care_site_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_name)) {
      statement <- paste0(statement, ' care_site_name IS NULL')
    } else if (is(care_site_name, 'subQuery')){
      statement <- paste0(statement, ' care_site_name = (', as.character(care_site_name), ')')
    } else {
      statement <- paste0(statement, " care_site_name = '", care_site_name,"'")
    }
  }

  if (!missing(place_of_service_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(place_of_service_concept_id)) {
      statement <- paste0(statement, ' place_of_service_concept_id IS NULL')
    } else if (is(place_of_service_concept_id, 'subQuery')){
      statement <- paste0(statement, ' place_of_service_concept_id = (', as.character(place_of_service_concept_id), ')')
    } else {
      statement <- paste0(statement, " place_of_service_concept_id = '", place_of_service_concept_id,"'")
    }
  }

  if (!missing(place_of_service_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(place_of_service_source_value)) {
      statement <- paste0(statement, ' place_of_service_source_value IS NULL')
    } else if (is(place_of_service_source_value, 'subQuery')){
      statement <- paste0(statement, ' place_of_service_source_value = (', as.character(place_of_service_source_value), ')')
    } else {
      statement <- paste0(statement, " place_of_service_source_value = '", place_of_service_source_value,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_provider <- function(provider_id, specialty_concept_id, specialty_source_value, provider_name, npi, dea, care_site_id, year_of_birth, gender_concept_id, provider_source_value, specialty_source_concept_id, gender_source_value, gender_source_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect provider' AS test, CASE WHEN(SELECT COUNT(*) FROM provider WHERE")
  first <- TRUE
  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(specialty_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_concept_id)) {
      statement <- paste0(statement, ' specialty_concept_id IS NULL')
    } else if (is(specialty_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specialty_concept_id = (', as.character(specialty_concept_id), ')')
    } else {
      statement <- paste0(statement, " specialty_concept_id = '", specialty_concept_id,"'")
    }
  }

  if (!missing(specialty_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_source_value)) {
      statement <- paste0(statement, ' specialty_source_value IS NULL')
    } else if (is(specialty_source_value, 'subQuery')){
      statement <- paste0(statement, ' specialty_source_value = (', as.character(specialty_source_value), ')')
    } else {
      statement <- paste0(statement, " specialty_source_value = '", specialty_source_value,"'")
    }
  }

  if (!missing(provider_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_name)) {
      statement <- paste0(statement, ' provider_name IS NULL')
    } else if (is(provider_name, 'subQuery')){
      statement <- paste0(statement, ' provider_name = (', as.character(provider_name), ')')
    } else {
      statement <- paste0(statement, " provider_name = '", provider_name,"'")
    }
  }

  if (!missing(npi)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(npi)) {
      statement <- paste0(statement, ' npi IS NULL')
    } else if (is(npi, 'subQuery')){
      statement <- paste0(statement, ' npi = (', as.character(npi), ')')
    } else {
      statement <- paste0(statement, " npi = '", npi,"'")
    }
  }

  if (!missing(dea)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dea)) {
      statement <- paste0(statement, ' dea IS NULL')
    } else if (is(dea, 'subQuery')){
      statement <- paste0(statement, ' dea = (', as.character(dea), ')')
    } else {
      statement <- paste0(statement, " dea = '", dea,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(year_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(year_of_birth)) {
      statement <- paste0(statement, ' year_of_birth IS NULL')
    } else if (is(year_of_birth, 'subQuery')){
      statement <- paste0(statement, ' year_of_birth = (', as.character(year_of_birth), ')')
    } else {
      statement <- paste0(statement, " year_of_birth = '", year_of_birth,"'")
    }
  }

  if (!missing(gender_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_concept_id)) {
      statement <- paste0(statement, ' gender_concept_id IS NULL')
    } else if (is(gender_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_concept_id = (', as.character(gender_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_concept_id = '", gender_concept_id,"'")
    }
  }

  if (!missing(provider_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_source_value)) {
      statement <- paste0(statement, ' provider_source_value IS NULL')
    } else if (is(provider_source_value, 'subQuery')){
      statement <- paste0(statement, ' provider_source_value = (', as.character(provider_source_value), ')')
    } else {
      statement <- paste0(statement, " provider_source_value = '", provider_source_value,"'")
    }
  }

  if (!missing(specialty_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_source_concept_id)) {
      statement <- paste0(statement, ' specialty_source_concept_id IS NULL')
    } else if (is(specialty_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specialty_source_concept_id = (', as.character(specialty_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " specialty_source_concept_id = '", specialty_source_concept_id,"'")
    }
  }

  if (!missing(gender_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_value)) {
      statement <- paste0(statement, ' gender_source_value IS NULL')
    } else if (is(gender_source_value, 'subQuery')){
      statement <- paste0(statement, ' gender_source_value = (', as.character(gender_source_value), ')')
    } else {
      statement <- paste0(statement, " gender_source_value = '", gender_source_value,"'")
    }
  }

  if (!missing(gender_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_concept_id)) {
      statement <- paste0(statement, ' gender_source_concept_id IS NULL')
    } else if (is(gender_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_source_concept_id = (', as.character(gender_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_source_concept_id = '", gender_source_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_note <- function(note_id, person_id, note_date, note_datetime, note_type_concept_id, note_class_concept_id, note_title, note_text, encoding_concept_id, language_concept_id, provider_id, note_source_value, visit_occurrence_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect note' AS test, CASE WHEN(SELECT COUNT(*) FROM note WHERE")
  first <- TRUE
  if (!missing(note_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_id)) {
      statement <- paste0(statement, ' note_id IS NULL')
    } else if (is(note_id, 'subQuery')){
      statement <- paste0(statement, ' note_id = (', as.character(note_id), ')')
    } else {
      statement <- paste0(statement, " note_id = '", note_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(note_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_date)) {
      statement <- paste0(statement, ' note_date IS NULL')
    } else if (is(note_date, 'subQuery')){
      statement <- paste0(statement, ' note_date = (', as.character(note_date), ')')
    } else {
      statement <- paste0(statement, " note_date = '", note_date,"'")
    }
  }

  if (!missing(note_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_datetime)) {
      statement <- paste0(statement, ' note_datetime IS NULL')
    } else if (is(note_datetime, 'subQuery')){
      statement <- paste0(statement, ' note_datetime = (', as.character(note_datetime), ')')
    } else {
      statement <- paste0(statement, " note_datetime = '", note_datetime,"'")
    }
  }

  if (!missing(note_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_type_concept_id)) {
      statement <- paste0(statement, ' note_type_concept_id IS NULL')
    } else if (is(note_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_type_concept_id = (', as.character(note_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_type_concept_id = '", note_type_concept_id,"'")
    }
  }

  if (!missing(note_class_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_class_concept_id)) {
      statement <- paste0(statement, ' note_class_concept_id IS NULL')
    } else if (is(note_class_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_class_concept_id = (', as.character(note_class_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_class_concept_id = '", note_class_concept_id,"'")
    }
  }

  if (!missing(note_title)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_title)) {
      statement <- paste0(statement, ' note_title IS NULL')
    } else if (is(note_title, 'subQuery')){
      statement <- paste0(statement, ' note_title = (', as.character(note_title), ')')
    } else {
      statement <- paste0(statement, " note_title = '", note_title,"'")
    }
  }

  if (!missing(note_text)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_text)) {
      statement <- paste0(statement, ' note_text IS NULL')
    } else if (is(note_text, 'subQuery')){
      statement <- paste0(statement, ' note_text = (', as.character(note_text), ')')
    } else {
      statement <- paste0(statement, " note_text = '", note_text,"'")
    }
  }

  if (!missing(encoding_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(encoding_concept_id)) {
      statement <- paste0(statement, ' encoding_concept_id IS NULL')
    } else if (is(encoding_concept_id, 'subQuery')){
      statement <- paste0(statement, ' encoding_concept_id = (', as.character(encoding_concept_id), ')')
    } else {
      statement <- paste0(statement, " encoding_concept_id = '", encoding_concept_id,"'")
    }
  }

  if (!missing(language_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(language_concept_id)) {
      statement <- paste0(statement, ' language_concept_id IS NULL')
    } else if (is(language_concept_id, 'subQuery')){
      statement <- paste0(statement, ' language_concept_id = (', as.character(language_concept_id), ')')
    } else {
      statement <- paste0(statement, " language_concept_id = '", language_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(note_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_source_value)) {
      statement <- paste0(statement, ' note_source_value IS NULL')
    } else if (is(note_source_value, 'subQuery')){
      statement <- paste0(statement, ' note_source_value = (', as.character(note_source_value), ')')
    } else {
      statement <- paste0(statement, " note_source_value = '", note_source_value,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_fact_relationship <- function(domain_concept_id_1, fact_id_1, domain_concept_id_2, fact_id_2, relationship_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect fact_relationship' AS test, CASE WHEN(SELECT COUNT(*) FROM fact_relationship WHERE")
  first <- TRUE
  if (!missing(domain_concept_id_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(domain_concept_id_1)) {
      statement <- paste0(statement, ' domain_concept_id_1 IS NULL')
    } else if (is(domain_concept_id_1, 'subQuery')){
      statement <- paste0(statement, ' domain_concept_id_1 = (', as.character(domain_concept_id_1), ')')
    } else {
      statement <- paste0(statement, " domain_concept_id_1 = '", domain_concept_id_1,"'")
    }
  }

  if (!missing(fact_id_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(fact_id_1)) {
      statement <- paste0(statement, ' fact_id_1 IS NULL')
    } else if (is(fact_id_1, 'subQuery')){
      statement <- paste0(statement, ' fact_id_1 = (', as.character(fact_id_1), ')')
    } else {
      statement <- paste0(statement, " fact_id_1 = '", fact_id_1,"'")
    }
  }

  if (!missing(domain_concept_id_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(domain_concept_id_2)) {
      statement <- paste0(statement, ' domain_concept_id_2 IS NULL')
    } else if (is(domain_concept_id_2, 'subQuery')){
      statement <- paste0(statement, ' domain_concept_id_2 = (', as.character(domain_concept_id_2), ')')
    } else {
      statement <- paste0(statement, " domain_concept_id_2 = '", domain_concept_id_2,"'")
    }
  }

  if (!missing(fact_id_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(fact_id_2)) {
      statement <- paste0(statement, ' fact_id_2 IS NULL')
    } else if (is(fact_id_2, 'subQuery')){
      statement <- paste0(statement, ' fact_id_2 = (', as.character(fact_id_2), ')')
    } else {
      statement <- paste0(statement, " fact_id_2 = '", fact_id_2,"'")
    }
  }

  if (!missing(relationship_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(relationship_concept_id)) {
      statement <- paste0(statement, ' relationship_concept_id IS NULL')
    } else if (is(relationship_concept_id, 'subQuery')){
      statement <- paste0(statement, ' relationship_concept_id = (', as.character(relationship_concept_id), ')')
    } else {
      statement <- paste0(statement, " relationship_concept_id = '", relationship_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_note_nlp <- function(note_nlp_id, note_id, section_concept_id, snippet, offset, lexical_variant, note_nlp_concept_id, note_nlp_source_concept_id, nlp_system, nlp_date, nlp_date_time, term_exists, term_temporal, term_modifiers) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect note_nlp' AS test, CASE WHEN(SELECT COUNT(*) FROM note_nlp WHERE")
  first <- TRUE
  if (!missing(note_nlp_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_id)) {
      statement <- paste0(statement, ' note_nlp_id IS NULL')
    } else if (is(note_nlp_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_id = (', as.character(note_nlp_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_id = '", note_nlp_id,"'")
    }
  }

  if (!missing(note_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_id)) {
      statement <- paste0(statement, ' note_id IS NULL')
    } else if (is(note_id, 'subQuery')){
      statement <- paste0(statement, ' note_id = (', as.character(note_id), ')')
    } else {
      statement <- paste0(statement, " note_id = '", note_id,"'")
    }
  }

  if (!missing(section_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(section_concept_id)) {
      statement <- paste0(statement, ' section_concept_id IS NULL')
    } else if (is(section_concept_id, 'subQuery')){
      statement <- paste0(statement, ' section_concept_id = (', as.character(section_concept_id), ')')
    } else {
      statement <- paste0(statement, " section_concept_id = '", section_concept_id,"'")
    }
  }

  if (!missing(snippet)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(snippet)) {
      statement <- paste0(statement, ' snippet IS NULL')
    } else if (is(snippet, 'subQuery')){
      statement <- paste0(statement, ' snippet = (', as.character(snippet), ')')
    } else {
      statement <- paste0(statement, " snippet = '", snippet,"'")
    }
  }

  if (!missing(offset)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(offset)) {
      statement <- paste0(statement, ' offset IS NULL')
    } else if (is(offset, 'subQuery')){
      statement <- paste0(statement, ' offset = (', as.character(offset), ')')
    } else {
      statement <- paste0(statement, " offset = '", offset,"'")
    }
  }

  if (!missing(lexical_variant)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(lexical_variant)) {
      statement <- paste0(statement, ' lexical_variant IS NULL')
    } else if (is(lexical_variant, 'subQuery')){
      statement <- paste0(statement, ' lexical_variant = (', as.character(lexical_variant), ')')
    } else {
      statement <- paste0(statement, " lexical_variant = '", lexical_variant,"'")
    }
  }

  if (!missing(note_nlp_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_concept_id)) {
      statement <- paste0(statement, ' note_nlp_concept_id IS NULL')
    } else if (is(note_nlp_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_concept_id = (', as.character(note_nlp_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_concept_id = '", note_nlp_concept_id,"'")
    }
  }

  if (!missing(note_nlp_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_source_concept_id)) {
      statement <- paste0(statement, ' note_nlp_source_concept_id IS NULL')
    } else if (is(note_nlp_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_source_concept_id = (', as.character(note_nlp_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_source_concept_id = '", note_nlp_source_concept_id,"'")
    }
  }

  if (!missing(nlp_system)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_system)) {
      statement <- paste0(statement, ' nlp_system IS NULL')
    } else if (is(nlp_system, 'subQuery')){
      statement <- paste0(statement, ' nlp_system = (', as.character(nlp_system), ')')
    } else {
      statement <- paste0(statement, " nlp_system = '", nlp_system,"'")
    }
  }

  if (!missing(nlp_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_date)) {
      statement <- paste0(statement, ' nlp_date IS NULL')
    } else if (is(nlp_date, 'subQuery')){
      statement <- paste0(statement, ' nlp_date = (', as.character(nlp_date), ')')
    } else {
      statement <- paste0(statement, " nlp_date = '", nlp_date,"'")
    }
  }

  if (!missing(nlp_date_time)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_date_time)) {
      statement <- paste0(statement, ' nlp_date_time IS NULL')
    } else if (is(nlp_date_time, 'subQuery')){
      statement <- paste0(statement, ' nlp_date_time = (', as.character(nlp_date_time), ')')
    } else {
      statement <- paste0(statement, " nlp_date_time = '", nlp_date_time,"'")
    }
  }

  if (!missing(term_exists)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_exists)) {
      statement <- paste0(statement, ' term_exists IS NULL')
    } else if (is(term_exists, 'subQuery')){
      statement <- paste0(statement, ' term_exists = (', as.character(term_exists), ')')
    } else {
      statement <- paste0(statement, " term_exists = '", term_exists,"'")
    }
  }

  if (!missing(term_temporal)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_temporal)) {
      statement <- paste0(statement, ' term_temporal IS NULL')
    } else if (is(term_temporal, 'subQuery')){
      statement <- paste0(statement, ' term_temporal = (', as.character(term_temporal), ')')
    } else {
      statement <- paste0(statement, " term_temporal = '", term_temporal,"'")
    }
  }

  if (!missing(term_modifiers)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_modifiers)) {
      statement <- paste0(statement, ' term_modifiers IS NULL')
    } else if (is(term_modifiers, 'subQuery')){
      statement <- paste0(statement, ' term_modifiers = (', as.character(term_modifiers), ')')
    } else {
      statement <- paste0(statement, " term_modifiers = '", term_modifiers,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_specimen <- function(specimen_id, person_id, specimen_concept_id, specimen_type_concept_id, specimen_date, specimen_datetime, quantity, unit_concept_id, anatomic_site_concept_id, disease_status_concept_id, specimen_source_id, specimen_source_value, unit_source_value, anatomic_site_source_value, disease_status_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect specimen' AS test, CASE WHEN(SELECT COUNT(*) FROM specimen WHERE")
  first <- TRUE
  if (!missing(specimen_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_id)) {
      statement <- paste0(statement, ' specimen_id IS NULL')
    } else if (is(specimen_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_id = (', as.character(specimen_id), ')')
    } else {
      statement <- paste0(statement, " specimen_id = '", specimen_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(specimen_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_concept_id)) {
      statement <- paste0(statement, ' specimen_concept_id IS NULL')
    } else if (is(specimen_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_concept_id = (', as.character(specimen_concept_id), ')')
    } else {
      statement <- paste0(statement, " specimen_concept_id = '", specimen_concept_id,"'")
    }
  }

  if (!missing(specimen_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_type_concept_id)) {
      statement <- paste0(statement, ' specimen_type_concept_id IS NULL')
    } else if (is(specimen_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_type_concept_id = (', as.character(specimen_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " specimen_type_concept_id = '", specimen_type_concept_id,"'")
    }
  }

  if (!missing(specimen_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_date)) {
      statement <- paste0(statement, ' specimen_date IS NULL')
    } else if (is(specimen_date, 'subQuery')){
      statement <- paste0(statement, ' specimen_date = (', as.character(specimen_date), ')')
    } else {
      statement <- paste0(statement, " specimen_date = '", specimen_date,"'")
    }
  }

  if (!missing(specimen_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_datetime)) {
      statement <- paste0(statement, ' specimen_datetime IS NULL')
    } else if (is(specimen_datetime, 'subQuery')){
      statement <- paste0(statement, ' specimen_datetime = (', as.character(specimen_datetime), ')')
    } else {
      statement <- paste0(statement, " specimen_datetime = '", specimen_datetime,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(anatomic_site_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(anatomic_site_concept_id)) {
      statement <- paste0(statement, ' anatomic_site_concept_id IS NULL')
    } else if (is(anatomic_site_concept_id, 'subQuery')){
      statement <- paste0(statement, ' anatomic_site_concept_id = (', as.character(anatomic_site_concept_id), ')')
    } else {
      statement <- paste0(statement, " anatomic_site_concept_id = '", anatomic_site_concept_id,"'")
    }
  }

  if (!missing(disease_status_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(disease_status_concept_id)) {
      statement <- paste0(statement, ' disease_status_concept_id IS NULL')
    } else if (is(disease_status_concept_id, 'subQuery')){
      statement <- paste0(statement, ' disease_status_concept_id = (', as.character(disease_status_concept_id), ')')
    } else {
      statement <- paste0(statement, " disease_status_concept_id = '", disease_status_concept_id,"'")
    }
  }

  if (!missing(specimen_source_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_source_id)) {
      statement <- paste0(statement, ' specimen_source_id IS NULL')
    } else if (is(specimen_source_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_source_id = (', as.character(specimen_source_id), ')')
    } else {
      statement <- paste0(statement, " specimen_source_id = '", specimen_source_id,"'")
    }
  }

  if (!missing(specimen_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_source_value)) {
      statement <- paste0(statement, ' specimen_source_value IS NULL')
    } else if (is(specimen_source_value, 'subQuery')){
      statement <- paste0(statement, ' specimen_source_value = (', as.character(specimen_source_value), ')')
    } else {
      statement <- paste0(statement, " specimen_source_value = '", specimen_source_value,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(anatomic_site_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(anatomic_site_source_value)) {
      statement <- paste0(statement, ' anatomic_site_source_value IS NULL')
    } else if (is(anatomic_site_source_value, 'subQuery')){
      statement <- paste0(statement, ' anatomic_site_source_value = (', as.character(anatomic_site_source_value), ')')
    } else {
      statement <- paste0(statement, " anatomic_site_source_value = '", anatomic_site_source_value,"'")
    }
  }

  if (!missing(disease_status_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(disease_status_source_value)) {
      statement <- paste0(statement, ' disease_status_source_value IS NULL')
    } else if (is(disease_status_source_value, 'subQuery')){
      statement <- paste0(statement, ' disease_status_source_value = (', as.character(disease_status_source_value), ')')
    } else {
      statement <- paste0(statement, " disease_status_source_value = '", disease_status_source_value,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_cohort <- function(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cohort' AS test, CASE WHEN(SELECT COUNT(*) FROM cohort WHERE")
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(subject_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_id)) {
      statement <- paste0(statement, ' subject_id IS NULL')
    } else if (is(subject_id, 'subQuery')){
      statement <- paste0(statement, ' subject_id = (', as.character(subject_id), ')')
    } else {
      statement <- paste0(statement, " subject_id = '", subject_id,"'")
    }
  }

  if (!missing(cohort_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_start_date)) {
      statement <- paste0(statement, ' cohort_start_date IS NULL')
    } else if (is(cohort_start_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_start_date = (', as.character(cohort_start_date), ')')
    } else {
      statement <- paste0(statement, " cohort_start_date = '", cohort_start_date,"'")
    }
  }

  if (!missing(cohort_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_end_date)) {
      statement <- paste0(statement, ' cohort_end_date IS NULL')
    } else if (is(cohort_end_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_end_date = (', as.character(cohort_end_date), ')')
    } else {
      statement <- paste0(statement, " cohort_end_date = '", cohort_end_date,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_cohort_attribute <- function(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, attribute_definition_id, value_as_number, value_as_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cohort_attribute' AS test, CASE WHEN(SELECT COUNT(*) FROM cohort_attribute WHERE")
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(subject_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_id)) {
      statement <- paste0(statement, ' subject_id IS NULL')
    } else if (is(subject_id, 'subQuery')){
      statement <- paste0(statement, ' subject_id = (', as.character(subject_id), ')')
    } else {
      statement <- paste0(statement, " subject_id = '", subject_id,"'")
    }
  }

  if (!missing(cohort_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_start_date)) {
      statement <- paste0(statement, ' cohort_start_date IS NULL')
    } else if (is(cohort_start_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_start_date = (', as.character(cohort_start_date), ')')
    } else {
      statement <- paste0(statement, " cohort_start_date = '", cohort_start_date,"'")
    }
  }

  if (!missing(cohort_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_end_date)) {
      statement <- paste0(statement, ' cohort_end_date IS NULL')
    } else if (is(cohort_end_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_end_date = (', as.character(cohort_end_date), ')')
    } else {
      statement <- paste0(statement, " cohort_end_date = '", cohort_end_date,"'")
    }
  }

  if (!missing(attribute_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_definition_id)) {
      statement <- paste0(statement, ' attribute_definition_id IS NULL')
    } else if (is(attribute_definition_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_definition_id = (', as.character(attribute_definition_id), ')')
    } else {
      statement <- paste0(statement, " attribute_definition_id = '", attribute_definition_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_drug_era <- function(drug_era_id, person_id, drug_concept_id, drug_era_start_date, drug_era_end_date, drug_exposure_count, gap_days) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect drug_era' AS test, CASE WHEN(SELECT COUNT(*) FROM drug_era WHERE")
  first <- TRUE
  if (!missing(drug_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_id)) {
      statement <- paste0(statement, ' drug_era_id IS NULL')
    } else if (is(drug_era_id, 'subQuery')){
      statement <- paste0(statement, ' drug_era_id = (', as.character(drug_era_id), ')')
    } else {
      statement <- paste0(statement, " drug_era_id = '", drug_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(drug_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_start_date)) {
      statement <- paste0(statement, ' drug_era_start_date IS NULL')
    } else if (is(drug_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' drug_era_start_date = (', as.character(drug_era_start_date), ')')
    } else {
      statement <- paste0(statement, " drug_era_start_date = '", drug_era_start_date,"'")
    }
  }

  if (!missing(drug_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_end_date)) {
      statement <- paste0(statement, ' drug_era_end_date IS NULL')
    } else if (is(drug_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' drug_era_end_date = (', as.character(drug_era_end_date), ')')
    } else {
      statement <- paste0(statement, " drug_era_end_date = '", drug_era_end_date,"'")
    }
  }

  if (!missing(drug_exposure_count)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_count)) {
      statement <- paste0(statement, ' drug_exposure_count IS NULL')
    } else if (is(drug_exposure_count, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_count = (', as.character(drug_exposure_count), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_count = '", drug_exposure_count,"'")
    }
  }

  if (!missing(gap_days)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gap_days)) {
      statement <- paste0(statement, ' gap_days IS NULL')
    } else if (is(gap_days, 'subQuery')){
      statement <- paste0(statement, ' gap_days = (', as.character(gap_days), ')')
    } else {
      statement <- paste0(statement, " gap_days = '", gap_days,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_condition_era <- function(condition_era_id, person_id, condition_concept_id, condition_era_start_date, condition_era_end_date, condition_occurrence_count) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect condition_era' AS test, CASE WHEN(SELECT COUNT(*) FROM condition_era WHERE")
  first <- TRUE
  if (!missing(condition_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_id)) {
      statement <- paste0(statement, ' condition_era_id IS NULL')
    } else if (is(condition_era_id, 'subQuery')){
      statement <- paste0(statement, ' condition_era_id = (', as.character(condition_era_id), ')')
    } else {
      statement <- paste0(statement, " condition_era_id = '", condition_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(condition_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_concept_id)) {
      statement <- paste0(statement, ' condition_concept_id IS NULL')
    } else if (is(condition_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_concept_id = (', as.character(condition_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_concept_id = '", condition_concept_id,"'")
    }
  }

  if (!missing(condition_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_start_date)) {
      statement <- paste0(statement, ' condition_era_start_date IS NULL')
    } else if (is(condition_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' condition_era_start_date = (', as.character(condition_era_start_date), ')')
    } else {
      statement <- paste0(statement, " condition_era_start_date = '", condition_era_start_date,"'")
    }
  }

  if (!missing(condition_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_end_date)) {
      statement <- paste0(statement, ' condition_era_end_date IS NULL')
    } else if (is(condition_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' condition_era_end_date = (', as.character(condition_era_end_date), ')')
    } else {
      statement <- paste0(statement, " condition_era_end_date = '", condition_era_end_date,"'")
    }
  }

  if (!missing(condition_occurrence_count)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_occurrence_count)) {
      statement <- paste0(statement, ' condition_occurrence_count IS NULL')
    } else if (is(condition_occurrence_count, 'subQuery')){
      statement <- paste0(statement, ' condition_occurrence_count = (', as.character(condition_occurrence_count), ')')
    } else {
      statement <- paste0(statement, " condition_occurrence_count = '", condition_occurrence_count,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_dose_era <- function(dose_era_id, person_id, drug_concept_id, unit_concept_id, dose_value, dose_era_start_date, dose_era_end_date) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect dose_era' AS test, CASE WHEN(SELECT COUNT(*) FROM dose_era WHERE")
  first <- TRUE
  if (!missing(dose_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_id)) {
      statement <- paste0(statement, ' dose_era_id IS NULL')
    } else if (is(dose_era_id, 'subQuery')){
      statement <- paste0(statement, ' dose_era_id = (', as.character(dose_era_id), ')')
    } else {
      statement <- paste0(statement, " dose_era_id = '", dose_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(dose_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_value)) {
      statement <- paste0(statement, ' dose_value IS NULL')
    } else if (is(dose_value, 'subQuery')){
      statement <- paste0(statement, ' dose_value = (', as.character(dose_value), ')')
    } else {
      statement <- paste0(statement, " dose_value = '", dose_value,"'")
    }
  }

  if (!missing(dose_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_start_date)) {
      statement <- paste0(statement, ' dose_era_start_date IS NULL')
    } else if (is(dose_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' dose_era_start_date = (', as.character(dose_era_start_date), ')')
    } else {
      statement <- paste0(statement, " dose_era_start_date = '", dose_era_start_date,"'")
    }
  }

  if (!missing(dose_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_end_date)) {
      statement <- paste0(statement, ' dose_era_end_date IS NULL')
    } else if (is(dose_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' dose_era_end_date = (', as.character(dose_era_end_date), ')')
    } else {
      statement <- paste0(statement, " dose_era_end_date = '", dose_era_end_date,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_cost <- function(cost_id, cost_event_id, cost_domain_id, cost_type_concept_id, currency_concept_id, total_charge, total_cost, total_paid, paid_by_payer, paid_by_patient, paid_patient_copay, paid_patient_coinsurance, paid_patient_deductible, paid_by_primary, paid_ingredient_cost, paid_dispensing_fee, payer_plan_period_id, amount_allowed, revenue_code_concept_id, revenue_code_source_value, drg_concept_id, drg_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cost' AS test, CASE WHEN(SELECT COUNT(*) FROM cost WHERE")
  first <- TRUE
  if (!missing(cost_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_id)) {
      statement <- paste0(statement, ' cost_id IS NULL')
    } else if (is(cost_id, 'subQuery')){
      statement <- paste0(statement, ' cost_id = (', as.character(cost_id), ')')
    } else {
      statement <- paste0(statement, " cost_id = '", cost_id,"'")
    }
  }

  if (!missing(cost_event_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_event_id)) {
      statement <- paste0(statement, ' cost_event_id IS NULL')
    } else if (is(cost_event_id, 'subQuery')){
      statement <- paste0(statement, ' cost_event_id = (', as.character(cost_event_id), ')')
    } else {
      statement <- paste0(statement, " cost_event_id = '", cost_event_id,"'")
    }
  }

  if (!missing(cost_domain_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_domain_id)) {
      statement <- paste0(statement, ' cost_domain_id IS NULL')
    } else if (is(cost_domain_id, 'subQuery')){
      statement <- paste0(statement, ' cost_domain_id = (', as.character(cost_domain_id), ')')
    } else {
      statement <- paste0(statement, " cost_domain_id = '", cost_domain_id,"'")
    }
  }

  if (!missing(cost_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_type_concept_id)) {
      statement <- paste0(statement, ' cost_type_concept_id IS NULL')
    } else if (is(cost_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cost_type_concept_id = (', as.character(cost_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " cost_type_concept_id = '", cost_type_concept_id,"'")
    }
  }

  if (!missing(currency_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(currency_concept_id)) {
      statement <- paste0(statement, ' currency_concept_id IS NULL')
    } else if (is(currency_concept_id, 'subQuery')){
      statement <- paste0(statement, ' currency_concept_id = (', as.character(currency_concept_id), ')')
    } else {
      statement <- paste0(statement, " currency_concept_id = '", currency_concept_id,"'")
    }
  }

  if (!missing(total_charge)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_charge)) {
      statement <- paste0(statement, ' total_charge IS NULL')
    } else if (is(total_charge, 'subQuery')){
      statement <- paste0(statement, ' total_charge = (', as.character(total_charge), ')')
    } else {
      statement <- paste0(statement, " total_charge = '", total_charge,"'")
    }
  }

  if (!missing(total_cost)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_cost)) {
      statement <- paste0(statement, ' total_cost IS NULL')
    } else if (is(total_cost, 'subQuery')){
      statement <- paste0(statement, ' total_cost = (', as.character(total_cost), ')')
    } else {
      statement <- paste0(statement, " total_cost = '", total_cost,"'")
    }
  }

  if (!missing(total_paid)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_paid)) {
      statement <- paste0(statement, ' total_paid IS NULL')
    } else if (is(total_paid, 'subQuery')){
      statement <- paste0(statement, ' total_paid = (', as.character(total_paid), ')')
    } else {
      statement <- paste0(statement, " total_paid = '", total_paid,"'")
    }
  }

  if (!missing(paid_by_payer)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_payer)) {
      statement <- paste0(statement, ' paid_by_payer IS NULL')
    } else if (is(paid_by_payer, 'subQuery')){
      statement <- paste0(statement, ' paid_by_payer = (', as.character(paid_by_payer), ')')
    } else {
      statement <- paste0(statement, " paid_by_payer = '", paid_by_payer,"'")
    }
  }

  if (!missing(paid_by_patient)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_patient)) {
      statement <- paste0(statement, ' paid_by_patient IS NULL')
    } else if (is(paid_by_patient, 'subQuery')){
      statement <- paste0(statement, ' paid_by_patient = (', as.character(paid_by_patient), ')')
    } else {
      statement <- paste0(statement, " paid_by_patient = '", paid_by_patient,"'")
    }
  }

  if (!missing(paid_patient_copay)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_copay)) {
      statement <- paste0(statement, ' paid_patient_copay IS NULL')
    } else if (is(paid_patient_copay, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_copay = (', as.character(paid_patient_copay), ')')
    } else {
      statement <- paste0(statement, " paid_patient_copay = '", paid_patient_copay,"'")
    }
  }

  if (!missing(paid_patient_coinsurance)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_coinsurance)) {
      statement <- paste0(statement, ' paid_patient_coinsurance IS NULL')
    } else if (is(paid_patient_coinsurance, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_coinsurance = (', as.character(paid_patient_coinsurance), ')')
    } else {
      statement <- paste0(statement, " paid_patient_coinsurance = '", paid_patient_coinsurance,"'")
    }
  }

  if (!missing(paid_patient_deductible)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_deductible)) {
      statement <- paste0(statement, ' paid_patient_deductible IS NULL')
    } else if (is(paid_patient_deductible, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_deductible = (', as.character(paid_patient_deductible), ')')
    } else {
      statement <- paste0(statement, " paid_patient_deductible = '", paid_patient_deductible,"'")
    }
  }

  if (!missing(paid_by_primary)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_primary)) {
      statement <- paste0(statement, ' paid_by_primary IS NULL')
    } else if (is(paid_by_primary, 'subQuery')){
      statement <- paste0(statement, ' paid_by_primary = (', as.character(paid_by_primary), ')')
    } else {
      statement <- paste0(statement, " paid_by_primary = '", paid_by_primary,"'")
    }
  }

  if (!missing(paid_ingredient_cost)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_ingredient_cost)) {
      statement <- paste0(statement, ' paid_ingredient_cost IS NULL')
    } else if (is(paid_ingredient_cost, 'subQuery')){
      statement <- paste0(statement, ' paid_ingredient_cost = (', as.character(paid_ingredient_cost), ')')
    } else {
      statement <- paste0(statement, " paid_ingredient_cost = '", paid_ingredient_cost,"'")
    }
  }

  if (!missing(paid_dispensing_fee)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_dispensing_fee)) {
      statement <- paste0(statement, ' paid_dispensing_fee IS NULL')
    } else if (is(paid_dispensing_fee, 'subQuery')){
      statement <- paste0(statement, ' paid_dispensing_fee = (', as.character(paid_dispensing_fee), ')')
    } else {
      statement <- paste0(statement, " paid_dispensing_fee = '", paid_dispensing_fee,"'")
    }
  }

  if (!missing(payer_plan_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_id)) {
      statement <- paste0(statement, ' payer_plan_period_id IS NULL')
    } else if (is(payer_plan_period_id, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_id = (', as.character(payer_plan_period_id), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_id = '", payer_plan_period_id,"'")
    }
  }

  if (!missing(amount_allowed)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(amount_allowed)) {
      statement <- paste0(statement, ' amount_allowed IS NULL')
    } else if (is(amount_allowed, 'subQuery')){
      statement <- paste0(statement, ' amount_allowed = (', as.character(amount_allowed), ')')
    } else {
      statement <- paste0(statement, " amount_allowed = '", amount_allowed,"'")
    }
  }

  if (!missing(revenue_code_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(revenue_code_concept_id)) {
      statement <- paste0(statement, ' revenue_code_concept_id IS NULL')
    } else if (is(revenue_code_concept_id, 'subQuery')){
      statement <- paste0(statement, ' revenue_code_concept_id = (', as.character(revenue_code_concept_id), ')')
    } else {
      statement <- paste0(statement, " revenue_code_concept_id = '", revenue_code_concept_id,"'")
    }
  }

  if (!missing(revenue_code_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(revenue_code_source_value)) {
      statement <- paste0(statement, ' revenue_code_source_value IS NULL')
    } else if (is(revenue_code_source_value, 'subQuery')){
      statement <- paste0(statement, ' revenue_code_source_value = (', as.character(revenue_code_source_value), ')')
    } else {
      statement <- paste0(statement, " revenue_code_source_value = '", revenue_code_source_value,"'")
    }
  }

  if (!missing(drg_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drg_concept_id)) {
      statement <- paste0(statement, ' drg_concept_id IS NULL')
    } else if (is(drg_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drg_concept_id = (', as.character(drg_concept_id), ')')
    } else {
      statement <- paste0(statement, " drg_concept_id = '", drg_concept_id,"'")
    }
  }

  if (!missing(drg_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drg_source_value)) {
      statement <- paste0(statement, ' drg_source_value IS NULL')
    } else if (is(drg_source_value, 'subQuery')){
      statement <- paste0(statement, ' drg_source_value = (', as.character(drg_source_value), ')')
    } else {
      statement <- paste0(statement, " drg_source_value = '", drg_source_value,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_payer_plan_period <- function(payer_plan_period_id, person_id, payer_plan_period_start_date, payer_plan_period_end_date, payer_source_value, plan_source_value, family_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect payer_plan_period' AS test, CASE WHEN(SELECT COUNT(*) FROM payer_plan_period WHERE")
  first <- TRUE
  if (!missing(payer_plan_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_id)) {
      statement <- paste0(statement, ' payer_plan_period_id IS NULL')
    } else if (is(payer_plan_period_id, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_id = (', as.character(payer_plan_period_id), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_id = '", payer_plan_period_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(payer_plan_period_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_start_date)) {
      statement <- paste0(statement, ' payer_plan_period_start_date IS NULL')
    } else if (is(payer_plan_period_start_date, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_start_date = (', as.character(payer_plan_period_start_date), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_start_date = '", payer_plan_period_start_date,"'")
    }
  }

  if (!missing(payer_plan_period_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_end_date)) {
      statement <- paste0(statement, ' payer_plan_period_end_date IS NULL')
    } else if (is(payer_plan_period_end_date, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_end_date = (', as.character(payer_plan_period_end_date), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_end_date = '", payer_plan_period_end_date,"'")
    }
  }

  if (!missing(payer_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_source_value)) {
      statement <- paste0(statement, ' payer_source_value IS NULL')
    } else if (is(payer_source_value, 'subQuery')){
      statement <- paste0(statement, ' payer_source_value = (', as.character(payer_source_value), ')')
    } else {
      statement <- paste0(statement, " payer_source_value = '", payer_source_value,"'")
    }
  }

  if (!missing(plan_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(plan_source_value)) {
      statement <- paste0(statement, ' plan_source_value IS NULL')
    } else if (is(plan_source_value, 'subQuery')){
      statement <- paste0(statement, ' plan_source_value = (', as.character(plan_source_value), ')')
    } else {
      statement <- paste0(statement, " plan_source_value = '", plan_source_value,"'")
    }
  }

  if (!missing(family_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(family_source_value)) {
      statement <- paste0(statement, ' family_source_value IS NULL')
    } else if (is(family_source_value, 'subQuery')){
      statement <- paste0(statement, ' family_source_value = (', as.character(family_source_value), ')')
    } else {
      statement <- paste0(statement, " family_source_value = '", family_source_value,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_cdm_source <- function(cdm_source_name, cdm_source_abbreviation, cdm_holder, source_description, source_documentation_reference, cdm_etl__reference, source_release_date, cdm_release_date, cdm_version, vocabulary_version) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cdm_source' AS test, CASE WHEN(SELECT COUNT(*) FROM cdm_source WHERE")
  first <- TRUE
  if (!missing(cdm_source_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_source_name)) {
      statement <- paste0(statement, ' cdm_source_name IS NULL')
    } else if (is(cdm_source_name, 'subQuery')){
      statement <- paste0(statement, ' cdm_source_name = (', as.character(cdm_source_name), ')')
    } else {
      statement <- paste0(statement, " cdm_source_name = '", cdm_source_name,"'")
    }
  }

  if (!missing(cdm_source_abbreviation)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_source_abbreviation)) {
      statement <- paste0(statement, ' cdm_source_abbreviation IS NULL')
    } else if (is(cdm_source_abbreviation, 'subQuery')){
      statement <- paste0(statement, ' cdm_source_abbreviation = (', as.character(cdm_source_abbreviation), ')')
    } else {
      statement <- paste0(statement, " cdm_source_abbreviation = '", cdm_source_abbreviation,"'")
    }
  }

  if (!missing(cdm_holder)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_holder)) {
      statement <- paste0(statement, ' cdm_holder IS NULL')
    } else if (is(cdm_holder, 'subQuery')){
      statement <- paste0(statement, ' cdm_holder = (', as.character(cdm_holder), ')')
    } else {
      statement <- paste0(statement, " cdm_holder = '", cdm_holder,"'")
    }
  }

  if (!missing(source_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_description)) {
      statement <- paste0(statement, ' source_description IS NULL')
    } else if (is(source_description, 'subQuery')){
      statement <- paste0(statement, ' source_description = (', as.character(source_description), ')')
    } else {
      statement <- paste0(statement, " source_description = '", source_description,"'")
    }
  }

  if (!missing(source_documentation_reference)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_documentation_reference)) {
      statement <- paste0(statement, ' source_documentation_reference IS NULL')
    } else if (is(source_documentation_reference, 'subQuery')){
      statement <- paste0(statement, ' source_documentation_reference = (', as.character(source_documentation_reference), ')')
    } else {
      statement <- paste0(statement, " source_documentation_reference = '", source_documentation_reference,"'")
    }
  }

  if (!missing(cdm_etl__reference)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_etl__reference)) {
      statement <- paste0(statement, ' [cdm_etl _reference] IS NULL')
    } else if (is(cdm_etl__reference, 'subQuery')){
      statement <- paste0(statement, ' [cdm_etl _reference] = (', as.character(cdm_etl__reference), ')')
    } else {
      statement <- paste0(statement, " [cdm_etl _reference] = '", cdm_etl__reference,"'")
    }
  }

  if (!missing(source_release_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_release_date)) {
      statement <- paste0(statement, ' source_release_date IS NULL')
    } else if (is(source_release_date, 'subQuery')){
      statement <- paste0(statement, ' source_release_date = (', as.character(source_release_date), ')')
    } else {
      statement <- paste0(statement, " source_release_date = '", source_release_date,"'")
    }
  }

  if (!missing(cdm_release_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_release_date)) {
      statement <- paste0(statement, ' cdm_release_date IS NULL')
    } else if (is(cdm_release_date, 'subQuery')){
      statement <- paste0(statement, ' cdm_release_date = (', as.character(cdm_release_date), ')')
    } else {
      statement <- paste0(statement, " cdm_release_date = '", cdm_release_date,"'")
    }
  }

  if (!missing(cdm_version)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_version)) {
      statement <- paste0(statement, ' cdm_version IS NULL')
    } else if (is(cdm_version, 'subQuery')){
      statement <- paste0(statement, ' cdm_version = (', as.character(cdm_version), ')')
    } else {
      statement <- paste0(statement, " cdm_version = '", cdm_version,"'")
    }
  }

  if (!missing(vocabulary_version)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(vocabulary_version)) {
      statement <- paste0(statement, ' vocabulary_version IS NULL')
    } else if (is(vocabulary_version, 'subQuery')){
      statement <- paste0(statement, ' vocabulary_version = (', as.character(vocabulary_version), ')')
    } else {
      statement <- paste0(statement, " vocabulary_version = '", vocabulary_version,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_attribute_definition <- function(attribute_definition_id, attribute_name, attribute_description, attribute_type_concept_id, attribute_syntax) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect attribute_definition' AS test, CASE WHEN(SELECT COUNT(*) FROM attribute_definition WHERE")
  first <- TRUE
  if (!missing(attribute_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_definition_id)) {
      statement <- paste0(statement, ' attribute_definition_id IS NULL')
    } else if (is(attribute_definition_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_definition_id = (', as.character(attribute_definition_id), ')')
    } else {
      statement <- paste0(statement, " attribute_definition_id = '", attribute_definition_id,"'")
    }
  }

  if (!missing(attribute_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_name)) {
      statement <- paste0(statement, ' attribute_name IS NULL')
    } else if (is(attribute_name, 'subQuery')){
      statement <- paste0(statement, ' attribute_name = (', as.character(attribute_name), ')')
    } else {
      statement <- paste0(statement, " attribute_name = '", attribute_name,"'")
    }
  }

  if (!missing(attribute_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_description)) {
      statement <- paste0(statement, ' attribute_description IS NULL')
    } else if (is(attribute_description, 'subQuery')){
      statement <- paste0(statement, ' attribute_description = (', as.character(attribute_description), ')')
    } else {
      statement <- paste0(statement, " attribute_description = '", attribute_description,"'")
    }
  }

  if (!missing(attribute_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_type_concept_id)) {
      statement <- paste0(statement, ' attribute_type_concept_id IS NULL')
    } else if (is(attribute_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_type_concept_id = (', as.character(attribute_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " attribute_type_concept_id = '", attribute_type_concept_id,"'")
    }
  }

  if (!missing(attribute_syntax)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_syntax)) {
      statement <- paste0(statement, ' attribute_syntax IS NULL')
    } else if (is(attribute_syntax, 'subQuery')){
      statement <- paste0(statement, ' attribute_syntax = (', as.character(attribute_syntax), ')')
    } else {
      statement <- paste0(statement, " attribute_syntax = '", attribute_syntax,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_cohort_definition <- function(cohort_definition_id, cohort_definition_name, cohort_definition_description, definition_type_concept_id, cohort_definition_syntax, subject_concept_id, cohort_instantiation_date) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cohort_definition' AS test, CASE WHEN(SELECT COUNT(*) FROM cohort_definition WHERE")
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(cohort_definition_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_name)) {
      statement <- paste0(statement, ' cohort_definition_name IS NULL')
    } else if (is(cohort_definition_name, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_name = (', as.character(cohort_definition_name), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_name = '", cohort_definition_name,"'")
    }
  }

  if (!missing(cohort_definition_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_description)) {
      statement <- paste0(statement, ' cohort_definition_description IS NULL')
    } else if (is(cohort_definition_description, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_description = (', as.character(cohort_definition_description), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_description = '", cohort_definition_description,"'")
    }
  }

  if (!missing(definition_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(definition_type_concept_id)) {
      statement <- paste0(statement, ' definition_type_concept_id IS NULL')
    } else if (is(definition_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' definition_type_concept_id = (', as.character(definition_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " definition_type_concept_id = '", definition_type_concept_id,"'")
    }
  }

  if (!missing(cohort_definition_syntax)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_syntax)) {
      statement <- paste0(statement, ' cohort_definition_syntax IS NULL')
    } else if (is(cohort_definition_syntax, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_syntax = (', as.character(cohort_definition_syntax), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_syntax = '", cohort_definition_syntax,"'")
    }
  }

  if (!missing(subject_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_concept_id)) {
      statement <- paste0(statement, ' subject_concept_id IS NULL')
    } else if (is(subject_concept_id, 'subQuery')){
      statement <- paste0(statement, ' subject_concept_id = (', as.character(subject_concept_id), ')')
    } else {
      statement <- paste0(statement, " subject_concept_id = '", subject_concept_id,"'")
    }
  }

  if (!missing(cohort_instantiation_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_instantiation_date)) {
      statement <- paste0(statement, ' cohort_instantiation_date IS NULL')
    } else if (is(cohort_instantiation_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_instantiation_date = (', as.character(cohort_instantiation_date), ')')
    } else {
      statement <- paste0(statement, " cohort_instantiation_date = '", cohort_instantiation_date,"'")
    }
  }

  statement <- paste0(statement, ") = 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_person <- function(person_id, person_source_value, care_site_id, gender_concept_id, gender_source_value, year_of_birth, month_of_birth, day_of_birth, birth_datetime, race_concept_id, race_source_value, ethnicity_concept_id, ethnicity_source_value, location_id, provider_id, gender_source_concept_id, race_source_concept_id, ethnicity_source_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect person' AS test, CASE WHEN(SELECT COUNT(*) FROM person WHERE")
  first <- TRUE
  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(person_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_source_value)) {
      statement <- paste0(statement, ' person_source_value IS NULL')
    } else if (is(person_source_value, 'subQuery')){
      statement <- paste0(statement, ' person_source_value = (', as.character(person_source_value), ')')
    } else {
      statement <- paste0(statement, " person_source_value = '", person_source_value,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(gender_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_concept_id)) {
      statement <- paste0(statement, ' gender_concept_id IS NULL')
    } else if (is(gender_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_concept_id = (', as.character(gender_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_concept_id = '", gender_concept_id,"'")
    }
  }

  if (!missing(gender_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_value)) {
      statement <- paste0(statement, ' gender_source_value IS NULL')
    } else if (is(gender_source_value, 'subQuery')){
      statement <- paste0(statement, ' gender_source_value = (', as.character(gender_source_value), ')')
    } else {
      statement <- paste0(statement, " gender_source_value = '", gender_source_value,"'")
    }
  }

  if (!missing(year_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(year_of_birth)) {
      statement <- paste0(statement, ' year_of_birth IS NULL')
    } else if (is(year_of_birth, 'subQuery')){
      statement <- paste0(statement, ' year_of_birth = (', as.character(year_of_birth), ')')
    } else {
      statement <- paste0(statement, " year_of_birth = '", year_of_birth,"'")
    }
  }

  if (!missing(month_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(month_of_birth)) {
      statement <- paste0(statement, ' month_of_birth IS NULL')
    } else if (is(month_of_birth, 'subQuery')){
      statement <- paste0(statement, ' month_of_birth = (', as.character(month_of_birth), ')')
    } else {
      statement <- paste0(statement, " month_of_birth = '", month_of_birth,"'")
    }
  }

  if (!missing(day_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(day_of_birth)) {
      statement <- paste0(statement, ' day_of_birth IS NULL')
    } else if (is(day_of_birth, 'subQuery')){
      statement <- paste0(statement, ' day_of_birth = (', as.character(day_of_birth), ')')
    } else {
      statement <- paste0(statement, " day_of_birth = '", day_of_birth,"'")
    }
  }

  if (!missing(birth_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(birth_datetime)) {
      statement <- paste0(statement, ' birth_datetime IS NULL')
    } else if (is(birth_datetime, 'subQuery')){
      statement <- paste0(statement, ' birth_datetime = (', as.character(birth_datetime), ')')
    } else {
      statement <- paste0(statement, " birth_datetime = '", birth_datetime,"'")
    }
  }

  if (!missing(race_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_concept_id)) {
      statement <- paste0(statement, ' race_concept_id IS NULL')
    } else if (is(race_concept_id, 'subQuery')){
      statement <- paste0(statement, ' race_concept_id = (', as.character(race_concept_id), ')')
    } else {
      statement <- paste0(statement, " race_concept_id = '", race_concept_id,"'")
    }
  }

  if (!missing(race_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_source_value)) {
      statement <- paste0(statement, ' race_source_value IS NULL')
    } else if (is(race_source_value, 'subQuery')){
      statement <- paste0(statement, ' race_source_value = (', as.character(race_source_value), ')')
    } else {
      statement <- paste0(statement, " race_source_value = '", race_source_value,"'")
    }
  }

  if (!missing(ethnicity_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_concept_id)) {
      statement <- paste0(statement, ' ethnicity_concept_id IS NULL')
    } else if (is(ethnicity_concept_id, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_concept_id = (', as.character(ethnicity_concept_id), ')')
    } else {
      statement <- paste0(statement, " ethnicity_concept_id = '", ethnicity_concept_id,"'")
    }
  }

  if (!missing(ethnicity_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_source_value)) {
      statement <- paste0(statement, ' ethnicity_source_value IS NULL')
    } else if (is(ethnicity_source_value, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_source_value = (', as.character(ethnicity_source_value), ')')
    } else {
      statement <- paste0(statement, " ethnicity_source_value = '", ethnicity_source_value,"'")
    }
  }

  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(gender_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_concept_id)) {
      statement <- paste0(statement, ' gender_source_concept_id IS NULL')
    } else if (is(gender_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_source_concept_id = (', as.character(gender_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_source_concept_id = '", gender_source_concept_id,"'")
    }
  }

  if (!missing(race_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_source_concept_id)) {
      statement <- paste0(statement, ' race_source_concept_id IS NULL')
    } else if (is(race_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' race_source_concept_id = (', as.character(race_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " race_source_concept_id = '", race_source_concept_id,"'")
    }
  }

  if (!missing(ethnicity_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_source_concept_id)) {
      statement <- paste0(statement, ' ethnicity_source_concept_id IS NULL')
    } else if (is(ethnicity_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_source_concept_id = (', as.character(ethnicity_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " ethnicity_source_concept_id = '", ethnicity_source_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_observation_period <- function(observation_period_id, person_id, observation_period_start_date, observation_period_end_date, period_type_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect observation_period' AS test, CASE WHEN(SELECT COUNT(*) FROM observation_period WHERE")
  first <- TRUE
  if (!missing(observation_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_id)) {
      statement <- paste0(statement, ' observation_period_id IS NULL')
    } else if (is(observation_period_id, 'subQuery')){
      statement <- paste0(statement, ' observation_period_id = (', as.character(observation_period_id), ')')
    } else {
      statement <- paste0(statement, " observation_period_id = '", observation_period_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(observation_period_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_start_date)) {
      statement <- paste0(statement, ' observation_period_start_date IS NULL')
    } else if (is(observation_period_start_date, 'subQuery')){
      statement <- paste0(statement, ' observation_period_start_date = (', as.character(observation_period_start_date), ')')
    } else {
      statement <- paste0(statement, " observation_period_start_date = '", observation_period_start_date,"'")
    }
  }

  if (!missing(observation_period_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_end_date)) {
      statement <- paste0(statement, ' observation_period_end_date IS NULL')
    } else if (is(observation_period_end_date, 'subQuery')){
      statement <- paste0(statement, ' observation_period_end_date = (', as.character(observation_period_end_date), ')')
    } else {
      statement <- paste0(statement, " observation_period_end_date = '", observation_period_end_date,"'")
    }
  }

  if (!missing(period_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(period_type_concept_id)) {
      statement <- paste0(statement, ' period_type_concept_id IS NULL')
    } else if (is(period_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' period_type_concept_id = (', as.character(period_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " period_type_concept_id = '", period_type_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_visit_occurrence <- function(visit_occurrence_id, visit_source_value, person_id, care_site_id, visit_start_date, visit_end_date, visit_concept_id, provider_id, visit_type_concept_id, visit_source_concept_id, admitting_source_value, discharge_to_concept_id, discharge_to_source_value, preceding_visit_occurrence_id, visit_start_datetime, visit_end_datetime) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect visit_occurrence' AS test, CASE WHEN(SELECT COUNT(*) FROM visit_occurrence WHERE")
  first <- TRUE
  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_value)) {
      statement <- paste0(statement, ' visit_source_value IS NULL')
    } else if (is(visit_source_value, 'subQuery')){
      statement <- paste0(statement, ' visit_source_value = (', as.character(visit_source_value), ')')
    } else {
      statement <- paste0(statement, " visit_source_value = '", visit_source_value,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(visit_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_date)) {
      statement <- paste0(statement, ' visit_start_date IS NULL')
    } else if (is(visit_start_date, 'subQuery')){
      statement <- paste0(statement, ' visit_start_date = (', as.character(visit_start_date), ')')
    } else {
      statement <- paste0(statement, " visit_start_date = '", visit_start_date,"'")
    }
  }

  if (!missing(visit_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_date)) {
      statement <- paste0(statement, ' visit_end_date IS NULL')
    } else if (is(visit_end_date, 'subQuery')){
      statement <- paste0(statement, ' visit_end_date = (', as.character(visit_end_date), ')')
    } else {
      statement <- paste0(statement, " visit_end_date = '", visit_end_date,"'")
    }
  }

  if (!missing(visit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_concept_id)) {
      statement <- paste0(statement, ' visit_concept_id IS NULL')
    } else if (is(visit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_concept_id = (', as.character(visit_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_concept_id = '", visit_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_type_concept_id)) {
      statement <- paste0(statement, ' visit_type_concept_id IS NULL')
    } else if (is(visit_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_type_concept_id = (', as.character(visit_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_type_concept_id = '", visit_type_concept_id,"'")
    }
  }

  if (!missing(visit_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_concept_id)) {
      statement <- paste0(statement, ' visit_source_concept_id IS NULL')
    } else if (is(visit_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_source_concept_id = (', as.character(visit_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_source_concept_id = '", visit_source_concept_id,"'")
    }
  }

  if (!missing(admitting_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(admitting_source_value)) {
      statement <- paste0(statement, ' admitting_source_value IS NULL')
    } else if (is(admitting_source_value, 'subQuery')){
      statement <- paste0(statement, ' admitting_source_value = (', as.character(admitting_source_value), ')')
    } else {
      statement <- paste0(statement, " admitting_source_value = '", admitting_source_value,"'")
    }
  }

  if (!missing(discharge_to_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_concept_id)) {
      statement <- paste0(statement, ' discharge_to_concept_id IS NULL')
    } else if (is(discharge_to_concept_id, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_concept_id = (', as.character(discharge_to_concept_id), ')')
    } else {
      statement <- paste0(statement, " discharge_to_concept_id = '", discharge_to_concept_id,"'")
    }
  }

  if (!missing(discharge_to_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_source_value)) {
      statement <- paste0(statement, ' discharge_to_source_value IS NULL')
    } else if (is(discharge_to_source_value, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_source_value = (', as.character(discharge_to_source_value), ')')
    } else {
      statement <- paste0(statement, " discharge_to_source_value = '", discharge_to_source_value,"'")
    }
  }

  if (!missing(preceding_visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(preceding_visit_occurrence_id)) {
      statement <- paste0(statement, ' preceding_visit_occurrence_id IS NULL')
    } else if (is(preceding_visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' preceding_visit_occurrence_id = (', as.character(preceding_visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " preceding_visit_occurrence_id = '", preceding_visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_datetime)) {
      statement <- paste0(statement, ' visit_start_datetime IS NULL')
    } else if (is(visit_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_start_datetime = (', as.character(visit_start_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_start_datetime = '", visit_start_datetime,"'")
    }
  }

  if (!missing(visit_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_datetime)) {
      statement <- paste0(statement, ' visit_end_datetime IS NULL')
    } else if (is(visit_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_end_datetime = (', as.character(visit_end_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_end_datetime = '", visit_end_datetime,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_visit_detail <- function(visit_detail_id, person_id, visit_detail_concept_id, visit_start_date, visit_start_datetime, visit_end_date, visit_end_datetime, visit_type_concept_id, provider_id, care_site_id, visit_source_value, visit_source_concept_id, admitting_source_value, discharge_to_source_value, discharge_to_concept_id, preceding_visit_detail_id, visit_detail_parent_id, visit_occurrence_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect visit_detail' AS test, CASE WHEN(SELECT COUNT(*) FROM visit_detail WHERE")
  first <- TRUE
  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(visit_detail_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_concept_id)) {
      statement <- paste0(statement, ' visit_detail_concept_id IS NULL')
    } else if (is(visit_detail_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_concept_id = (', as.character(visit_detail_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_concept_id = '", visit_detail_concept_id,"'")
    }
  }

  if (!missing(visit_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_date)) {
      statement <- paste0(statement, ' visit_start_date IS NULL')
    } else if (is(visit_start_date, 'subQuery')){
      statement <- paste0(statement, ' visit_start_date = (', as.character(visit_start_date), ')')
    } else {
      statement <- paste0(statement, " visit_start_date = '", visit_start_date,"'")
    }
  }

  if (!missing(visit_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_datetime)) {
      statement <- paste0(statement, ' visit_start_datetime IS NULL')
    } else if (is(visit_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_start_datetime = (', as.character(visit_start_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_start_datetime = '", visit_start_datetime,"'")
    }
  }

  if (!missing(visit_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_date)) {
      statement <- paste0(statement, ' visit_end_date IS NULL')
    } else if (is(visit_end_date, 'subQuery')){
      statement <- paste0(statement, ' visit_end_date = (', as.character(visit_end_date), ')')
    } else {
      statement <- paste0(statement, " visit_end_date = '", visit_end_date,"'")
    }
  }

  if (!missing(visit_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_datetime)) {
      statement <- paste0(statement, ' visit_end_datetime IS NULL')
    } else if (is(visit_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_end_datetime = (', as.character(visit_end_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_end_datetime = '", visit_end_datetime,"'")
    }
  }

  if (!missing(visit_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_type_concept_id)) {
      statement <- paste0(statement, ' visit_type_concept_id IS NULL')
    } else if (is(visit_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_type_concept_id = (', as.character(visit_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_type_concept_id = '", visit_type_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(visit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_value)) {
      statement <- paste0(statement, ' visit_source_value IS NULL')
    } else if (is(visit_source_value, 'subQuery')){
      statement <- paste0(statement, ' visit_source_value = (', as.character(visit_source_value), ')')
    } else {
      statement <- paste0(statement, " visit_source_value = '", visit_source_value,"'")
    }
  }

  if (!missing(visit_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_concept_id)) {
      statement <- paste0(statement, ' visit_source_concept_id IS NULL')
    } else if (is(visit_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_source_concept_id = (', as.character(visit_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_source_concept_id = '", visit_source_concept_id,"'")
    }
  }

  if (!missing(admitting_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(admitting_source_value)) {
      statement <- paste0(statement, ' admitting_source_value IS NULL')
    } else if (is(admitting_source_value, 'subQuery')){
      statement <- paste0(statement, ' admitting_source_value = (', as.character(admitting_source_value), ')')
    } else {
      statement <- paste0(statement, " admitting_source_value = '", admitting_source_value,"'")
    }
  }

  if (!missing(discharge_to_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_source_value)) {
      statement <- paste0(statement, ' discharge_to_source_value IS NULL')
    } else if (is(discharge_to_source_value, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_source_value = (', as.character(discharge_to_source_value), ')')
    } else {
      statement <- paste0(statement, " discharge_to_source_value = '", discharge_to_source_value,"'")
    }
  }

  if (!missing(discharge_to_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_concept_id)) {
      statement <- paste0(statement, ' discharge_to_concept_id IS NULL')
    } else if (is(discharge_to_concept_id, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_concept_id = (', as.character(discharge_to_concept_id), ')')
    } else {
      statement <- paste0(statement, " discharge_to_concept_id = '", discharge_to_concept_id,"'")
    }
  }

  if (!missing(preceding_visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(preceding_visit_detail_id)) {
      statement <- paste0(statement, ' preceding_visit_detail_id IS NULL')
    } else if (is(preceding_visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' preceding_visit_detail_id = (', as.character(preceding_visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " preceding_visit_detail_id = '", preceding_visit_detail_id,"'")
    }
  }

  if (!missing(visit_detail_parent_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_parent_id)) {
      statement <- paste0(statement, ' visit_detail_parent_id IS NULL')
    } else if (is(visit_detail_parent_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_parent_id = (', as.character(visit_detail_parent_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_parent_id = '", visit_detail_parent_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_condition_occurrence <- function(condition_occurrence_id, person_id, condition_start_date, visit_occurrence_id, condition_concept_id, condition_start_datetime, condition_end_date, condition_end_datetime, condition_type_concept_id, stop_reason, provider_id, visit_detail_id, condition_source_concept_id, condition_source_value, condition_status_source_value, condition_status_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect condition_occurrence' AS test, CASE WHEN(SELECT COUNT(*) FROM condition_occurrence WHERE")
  first <- TRUE
  if (!missing(condition_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_occurrence_id)) {
      statement <- paste0(statement, ' condition_occurrence_id IS NULL')
    } else if (is(condition_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' condition_occurrence_id = (', as.character(condition_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " condition_occurrence_id = '", condition_occurrence_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(condition_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_start_date)) {
      statement <- paste0(statement, ' condition_start_date IS NULL')
    } else if (is(condition_start_date, 'subQuery')){
      statement <- paste0(statement, ' condition_start_date = (', as.character(condition_start_date), ')')
    } else {
      statement <- paste0(statement, " condition_start_date = '", condition_start_date,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(condition_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_concept_id)) {
      statement <- paste0(statement, ' condition_concept_id IS NULL')
    } else if (is(condition_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_concept_id = (', as.character(condition_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_concept_id = '", condition_concept_id,"'")
    }
  }

  if (!missing(condition_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_start_datetime)) {
      statement <- paste0(statement, ' condition_start_datetime IS NULL')
    } else if (is(condition_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' condition_start_datetime = (', as.character(condition_start_datetime), ')')
    } else {
      statement <- paste0(statement, " condition_start_datetime = '", condition_start_datetime,"'")
    }
  }

  if (!missing(condition_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_end_date)) {
      statement <- paste0(statement, ' condition_end_date IS NULL')
    } else if (is(condition_end_date, 'subQuery')){
      statement <- paste0(statement, ' condition_end_date = (', as.character(condition_end_date), ')')
    } else {
      statement <- paste0(statement, " condition_end_date = '", condition_end_date,"'")
    }
  }

  if (!missing(condition_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_end_datetime)) {
      statement <- paste0(statement, ' condition_end_datetime IS NULL')
    } else if (is(condition_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' condition_end_datetime = (', as.character(condition_end_datetime), ')')
    } else {
      statement <- paste0(statement, " condition_end_datetime = '", condition_end_datetime,"'")
    }
  }

  if (!missing(condition_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_type_concept_id)) {
      statement <- paste0(statement, ' condition_type_concept_id IS NULL')
    } else if (is(condition_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_type_concept_id = (', as.character(condition_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_type_concept_id = '", condition_type_concept_id,"'")
    }
  }

  if (!missing(stop_reason)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(stop_reason)) {
      statement <- paste0(statement, ' stop_reason IS NULL')
    } else if (is(stop_reason, 'subQuery')){
      statement <- paste0(statement, ' stop_reason = (', as.character(stop_reason), ')')
    } else {
      statement <- paste0(statement, " stop_reason = '", stop_reason,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(condition_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_source_concept_id)) {
      statement <- paste0(statement, ' condition_source_concept_id IS NULL')
    } else if (is(condition_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_source_concept_id = (', as.character(condition_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_source_concept_id = '", condition_source_concept_id,"'")
    }
  }

  if (!missing(condition_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_source_value)) {
      statement <- paste0(statement, ' condition_source_value IS NULL')
    } else if (is(condition_source_value, 'subQuery')){
      statement <- paste0(statement, ' condition_source_value = (', as.character(condition_source_value), ')')
    } else {
      statement <- paste0(statement, " condition_source_value = '", condition_source_value,"'")
    }
  }

  if (!missing(condition_status_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_status_source_value)) {
      statement <- paste0(statement, ' condition_status_source_value IS NULL')
    } else if (is(condition_status_source_value, 'subQuery')){
      statement <- paste0(statement, ' condition_status_source_value = (', as.character(condition_status_source_value), ')')
    } else {
      statement <- paste0(statement, " condition_status_source_value = '", condition_status_source_value,"'")
    }
  }

  if (!missing(condition_status_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_status_concept_id)) {
      statement <- paste0(statement, ' condition_status_concept_id IS NULL')
    } else if (is(condition_status_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_status_concept_id = (', as.character(condition_status_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_status_concept_id = '", condition_status_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_procedure_occurrence <- function(procedure_occurrence_id, visit_occurrence_id, person_id, procedure_concept_id, procedure_date, procedure_datetime, procedure_type_concept_id, modifier_concept_id, quantity, provider_id, procedure_source_value, procedure_source_concept_id, qualifier_source_value, visit_detail_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect procedure_occurrence' AS test, CASE WHEN(SELECT COUNT(*) FROM procedure_occurrence WHERE")
  first <- TRUE
  if (!missing(procedure_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_occurrence_id)) {
      statement <- paste0(statement, ' procedure_occurrence_id IS NULL')
    } else if (is(procedure_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_occurrence_id = (', as.character(procedure_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " procedure_occurrence_id = '", procedure_occurrence_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(procedure_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_concept_id)) {
      statement <- paste0(statement, ' procedure_concept_id IS NULL')
    } else if (is(procedure_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_concept_id = (', as.character(procedure_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_concept_id = '", procedure_concept_id,"'")
    }
  }

  if (!missing(procedure_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_date)) {
      statement <- paste0(statement, ' procedure_date IS NULL')
    } else if (is(procedure_date, 'subQuery')){
      statement <- paste0(statement, ' procedure_date = (', as.character(procedure_date), ')')
    } else {
      statement <- paste0(statement, " procedure_date = '", procedure_date,"'")
    }
  }

  if (!missing(procedure_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_datetime)) {
      statement <- paste0(statement, ' procedure_datetime IS NULL')
    } else if (is(procedure_datetime, 'subQuery')){
      statement <- paste0(statement, ' procedure_datetime = (', as.character(procedure_datetime), ')')
    } else {
      statement <- paste0(statement, " procedure_datetime = '", procedure_datetime,"'")
    }
  }

  if (!missing(procedure_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_type_concept_id)) {
      statement <- paste0(statement, ' procedure_type_concept_id IS NULL')
    } else if (is(procedure_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_type_concept_id = (', as.character(procedure_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_type_concept_id = '", procedure_type_concept_id,"'")
    }
  }

  if (!missing(modifier_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(modifier_concept_id)) {
      statement <- paste0(statement, ' modifier_concept_id IS NULL')
    } else if (is(modifier_concept_id, 'subQuery')){
      statement <- paste0(statement, ' modifier_concept_id = (', as.character(modifier_concept_id), ')')
    } else {
      statement <- paste0(statement, " modifier_concept_id = '", modifier_concept_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(procedure_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_source_value)) {
      statement <- paste0(statement, ' procedure_source_value IS NULL')
    } else if (is(procedure_source_value, 'subQuery')){
      statement <- paste0(statement, ' procedure_source_value = (', as.character(procedure_source_value), ')')
    } else {
      statement <- paste0(statement, " procedure_source_value = '", procedure_source_value,"'")
    }
  }

  if (!missing(procedure_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_source_concept_id)) {
      statement <- paste0(statement, ' procedure_source_concept_id IS NULL')
    } else if (is(procedure_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_source_concept_id = (', as.character(procedure_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_source_concept_id = '", procedure_source_concept_id,"'")
    }
  }

  if (!missing(qualifier_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_source_value)) {
      statement <- paste0(statement, ' qualifier_source_value IS NULL')
    } else if (is(qualifier_source_value, 'subQuery')){
      statement <- paste0(statement, ' qualifier_source_value = (', as.character(qualifier_source_value), ')')
    } else {
      statement <- paste0(statement, " qualifier_source_value = '", qualifier_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_drug_exposure <- function(drug_exposure_id, person_id, drug_exposure_start_date, drug_exposure_end_date, days_supply, drug_concept_id, drug_source_value, drug_source_concept_id, provider_id, drug_type_concept_id, quantity, sig, refills, visit_occurrence_id, route_source_value, dose_unit_source_value, visit_detail_id, drug_exposure_start_datetime, drug_exposure_end_datetime, verbatim_end_date, route_concept_id, stop_reason, lot_number) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect drug_exposure' AS test, CASE WHEN(SELECT COUNT(*) FROM drug_exposure WHERE")
  first <- TRUE
  if (!missing(drug_exposure_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_id)) {
      statement <- paste0(statement, ' drug_exposure_id IS NULL')
    } else if (is(drug_exposure_id, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_id = (', as.character(drug_exposure_id), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_id = '", drug_exposure_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_exposure_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_start_date)) {
      statement <- paste0(statement, ' drug_exposure_start_date IS NULL')
    } else if (is(drug_exposure_start_date, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_start_date = (', as.character(drug_exposure_start_date), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_start_date = '", drug_exposure_start_date,"'")
    }
  }

  if (!missing(drug_exposure_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_end_date)) {
      statement <- paste0(statement, ' drug_exposure_end_date IS NULL')
    } else if (is(drug_exposure_end_date, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_end_date = (', as.character(drug_exposure_end_date), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_end_date = '", drug_exposure_end_date,"'")
    }
  }

  if (!missing(days_supply)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(days_supply)) {
      statement <- paste0(statement, ' days_supply IS NULL')
    } else if (is(days_supply, 'subQuery')){
      statement <- paste0(statement, ' days_supply = (', as.character(days_supply), ')')
    } else {
      statement <- paste0(statement, " days_supply = '", days_supply,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(drug_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_source_value)) {
      statement <- paste0(statement, ' drug_source_value IS NULL')
    } else if (is(drug_source_value, 'subQuery')){
      statement <- paste0(statement, ' drug_source_value = (', as.character(drug_source_value), ')')
    } else {
      statement <- paste0(statement, " drug_source_value = '", drug_source_value,"'")
    }
  }

  if (!missing(drug_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_source_concept_id)) {
      statement <- paste0(statement, ' drug_source_concept_id IS NULL')
    } else if (is(drug_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_source_concept_id = (', as.character(drug_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_source_concept_id = '", drug_source_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(drug_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_type_concept_id)) {
      statement <- paste0(statement, ' drug_type_concept_id IS NULL')
    } else if (is(drug_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_type_concept_id = (', as.character(drug_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_type_concept_id = '", drug_type_concept_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(sig)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(sig)) {
      statement <- paste0(statement, ' sig IS NULL')
    } else if (is(sig, 'subQuery')){
      statement <- paste0(statement, ' sig = (', as.character(sig), ')')
    } else {
      statement <- paste0(statement, " sig = '", sig,"'")
    }
  }

  if (!missing(refills)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(refills)) {
      statement <- paste0(statement, ' refills IS NULL')
    } else if (is(refills, 'subQuery')){
      statement <- paste0(statement, ' refills = (', as.character(refills), ')')
    } else {
      statement <- paste0(statement, " refills = '", refills,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(route_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(route_source_value)) {
      statement <- paste0(statement, ' route_source_value IS NULL')
    } else if (is(route_source_value, 'subQuery')){
      statement <- paste0(statement, ' route_source_value = (', as.character(route_source_value), ')')
    } else {
      statement <- paste0(statement, " route_source_value = '", route_source_value,"'")
    }
  }

  if (!missing(dose_unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_unit_source_value)) {
      statement <- paste0(statement, ' dose_unit_source_value IS NULL')
    } else if (is(dose_unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' dose_unit_source_value = (', as.character(dose_unit_source_value), ')')
    } else {
      statement <- paste0(statement, " dose_unit_source_value = '", dose_unit_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(drug_exposure_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_start_datetime)) {
      statement <- paste0(statement, ' drug_exposure_start_datetime IS NULL')
    } else if (is(drug_exposure_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_start_datetime = (', as.character(drug_exposure_start_datetime), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_start_datetime = '", drug_exposure_start_datetime,"'")
    }
  }

  if (!missing(drug_exposure_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_end_datetime)) {
      statement <- paste0(statement, ' drug_exposure_end_datetime IS NULL')
    } else if (is(drug_exposure_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_end_datetime = (', as.character(drug_exposure_end_datetime), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_end_datetime = '", drug_exposure_end_datetime,"'")
    }
  }

  if (!missing(verbatim_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(verbatim_end_date)) {
      statement <- paste0(statement, ' verbatim_end_date IS NULL')
    } else if (is(verbatim_end_date, 'subQuery')){
      statement <- paste0(statement, ' verbatim_end_date = (', as.character(verbatim_end_date), ')')
    } else {
      statement <- paste0(statement, " verbatim_end_date = '", verbatim_end_date,"'")
    }
  }

  if (!missing(route_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(route_concept_id)) {
      statement <- paste0(statement, ' route_concept_id IS NULL')
    } else if (is(route_concept_id, 'subQuery')){
      statement <- paste0(statement, ' route_concept_id = (', as.character(route_concept_id), ')')
    } else {
      statement <- paste0(statement, " route_concept_id = '", route_concept_id,"'")
    }
  }

  if (!missing(stop_reason)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(stop_reason)) {
      statement <- paste0(statement, ' stop_reason IS NULL')
    } else if (is(stop_reason, 'subQuery')){
      statement <- paste0(statement, ' stop_reason = (', as.character(stop_reason), ')')
    } else {
      statement <- paste0(statement, " stop_reason = '", stop_reason,"'")
    }
  }

  if (!missing(lot_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(lot_number)) {
      statement <- paste0(statement, ' lot_number IS NULL')
    } else if (is(lot_number, 'subQuery')){
      statement <- paste0(statement, ' lot_number = (', as.character(lot_number), ')')
    } else {
      statement <- paste0(statement, " lot_number = '", lot_number,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_measurement <- function(measurement_id, person_id, measurement_concept_id, measurement_date, measurement_datetime, measurement_type_concept_id, operator_concept_id, value_as_number, value_as_concept_id, unit_concept_id, range_low, range_high, provider_id, visit_occurrence_id, visit_detail_id, measurement_source_value, measurement_source_concept_id, unit_source_value, value_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect measurement' AS test, CASE WHEN(SELECT COUNT(*) FROM measurement WHERE")
  first <- TRUE
  if (!missing(measurement_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_id)) {
      statement <- paste0(statement, ' measurement_id IS NULL')
    } else if (is(measurement_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_id = (', as.character(measurement_id), ')')
    } else {
      statement <- paste0(statement, " measurement_id = '", measurement_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(measurement_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_concept_id)) {
      statement <- paste0(statement, ' measurement_concept_id IS NULL')
    } else if (is(measurement_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_concept_id = (', as.character(measurement_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_concept_id = '", measurement_concept_id,"'")
    }
  }

  if (!missing(measurement_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_date)) {
      statement <- paste0(statement, ' measurement_date IS NULL')
    } else if (is(measurement_date, 'subQuery')){
      statement <- paste0(statement, ' measurement_date = (', as.character(measurement_date), ')')
    } else {
      statement <- paste0(statement, " measurement_date = '", measurement_date,"'")
    }
  }

  if (!missing(measurement_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_datetime)) {
      statement <- paste0(statement, ' measurement_datetime IS NULL')
    } else if (is(measurement_datetime, 'subQuery')){
      statement <- paste0(statement, ' measurement_datetime = (', as.character(measurement_datetime), ')')
    } else {
      statement <- paste0(statement, " measurement_datetime = '", measurement_datetime,"'")
    }
  }

  if (!missing(measurement_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_type_concept_id)) {
      statement <- paste0(statement, ' measurement_type_concept_id IS NULL')
    } else if (is(measurement_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_type_concept_id = (', as.character(measurement_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_type_concept_id = '", measurement_type_concept_id,"'")
    }
  }

  if (!missing(operator_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(operator_concept_id)) {
      statement <- paste0(statement, ' operator_concept_id IS NULL')
    } else if (is(operator_concept_id, 'subQuery')){
      statement <- paste0(statement, ' operator_concept_id = (', as.character(operator_concept_id), ')')
    } else {
      statement <- paste0(statement, " operator_concept_id = '", operator_concept_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(range_low)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(range_low)) {
      statement <- paste0(statement, ' range_low IS NULL')
    } else if (is(range_low, 'subQuery')){
      statement <- paste0(statement, ' range_low = (', as.character(range_low), ')')
    } else {
      statement <- paste0(statement, " range_low = '", range_low,"'")
    }
  }

  if (!missing(range_high)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(range_high)) {
      statement <- paste0(statement, ' range_high IS NULL')
    } else if (is(range_high, 'subQuery')){
      statement <- paste0(statement, ' range_high = (', as.character(range_high), ')')
    } else {
      statement <- paste0(statement, " range_high = '", range_high,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(measurement_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_source_value)) {
      statement <- paste0(statement, ' measurement_source_value IS NULL')
    } else if (is(measurement_source_value, 'subQuery')){
      statement <- paste0(statement, ' measurement_source_value = (', as.character(measurement_source_value), ')')
    } else {
      statement <- paste0(statement, " measurement_source_value = '", measurement_source_value,"'")
    }
  }

  if (!missing(measurement_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_source_concept_id)) {
      statement <- paste0(statement, ' measurement_source_concept_id IS NULL')
    } else if (is(measurement_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_source_concept_id = (', as.character(measurement_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_source_concept_id = '", measurement_source_concept_id,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(value_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_source_value)) {
      statement <- paste0(statement, ' value_source_value IS NULL')
    } else if (is(value_source_value, 'subQuery')){
      statement <- paste0(statement, ' value_source_value = (', as.character(value_source_value), ')')
    } else {
      statement <- paste0(statement, " value_source_value = '", value_source_value,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_observation <- function(observation_id, person_id, visit_occurrence_id, observation_date, observation_concept_id, observation_source_value, provider_id, observation_type_concept_id, value_as_number, value_as_string, qualifier_concept_id, qualifier_source_value, unit_concept_id, unit_source_value, visit_detail_id, observation_source_concept_id, observation_datetime, value_as_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect observation' AS test, CASE WHEN(SELECT COUNT(*) FROM observation WHERE")
  first <- TRUE
  if (!missing(observation_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_id)) {
      statement <- paste0(statement, ' observation_id IS NULL')
    } else if (is(observation_id, 'subQuery')){
      statement <- paste0(statement, ' observation_id = (', as.character(observation_id), ')')
    } else {
      statement <- paste0(statement, " observation_id = '", observation_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(observation_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_date)) {
      statement <- paste0(statement, ' observation_date IS NULL')
    } else if (is(observation_date, 'subQuery')){
      statement <- paste0(statement, ' observation_date = (', as.character(observation_date), ')')
    } else {
      statement <- paste0(statement, " observation_date = '", observation_date,"'")
    }
  }

  if (!missing(observation_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_concept_id)) {
      statement <- paste0(statement, ' observation_concept_id IS NULL')
    } else if (is(observation_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_concept_id = (', as.character(observation_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_concept_id = '", observation_concept_id,"'")
    }
  }

  if (!missing(observation_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_source_value)) {
      statement <- paste0(statement, ' observation_source_value IS NULL')
    } else if (is(observation_source_value, 'subQuery')){
      statement <- paste0(statement, ' observation_source_value = (', as.character(observation_source_value), ')')
    } else {
      statement <- paste0(statement, " observation_source_value = '", observation_source_value,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(observation_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_type_concept_id)) {
      statement <- paste0(statement, ' observation_type_concept_id IS NULL')
    } else if (is(observation_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_type_concept_id = (', as.character(observation_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_type_concept_id = '", observation_type_concept_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_string)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_string)) {
      statement <- paste0(statement, ' value_as_string IS NULL')
    } else if (is(value_as_string, 'subQuery')){
      statement <- paste0(statement, ' value_as_string = (', as.character(value_as_string), ')')
    } else {
      statement <- paste0(statement, " value_as_string = '", value_as_string,"'")
    }
  }

  if (!missing(qualifier_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_concept_id)) {
      statement <- paste0(statement, ' qualifier_concept_id IS NULL')
    } else if (is(qualifier_concept_id, 'subQuery')){
      statement <- paste0(statement, ' qualifier_concept_id = (', as.character(qualifier_concept_id), ')')
    } else {
      statement <- paste0(statement, " qualifier_concept_id = '", qualifier_concept_id,"'")
    }
  }

  if (!missing(qualifier_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_source_value)) {
      statement <- paste0(statement, ' qualifier_source_value IS NULL')
    } else if (is(qualifier_source_value, 'subQuery')){
      statement <- paste0(statement, ' qualifier_source_value = (', as.character(qualifier_source_value), ')')
    } else {
      statement <- paste0(statement, " qualifier_source_value = '", qualifier_source_value,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(observation_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_source_concept_id)) {
      statement <- paste0(statement, ' observation_source_concept_id IS NULL')
    } else if (is(observation_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_source_concept_id = (', as.character(observation_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_source_concept_id = '", observation_source_concept_id,"'")
    }
  }

  if (!missing(observation_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_datetime)) {
      statement <- paste0(statement, ' observation_datetime IS NULL')
    } else if (is(observation_datetime, 'subQuery')){
      statement <- paste0(statement, ' observation_datetime = (', as.character(observation_datetime), ')')
    } else {
      statement <- paste0(statement, " observation_datetime = '", observation_datetime,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_device_exposure <- function(device_exposure_id, person_id, device_exposure_start_date, device_concept_id, device_exposure_start_datetime, device_exposure_end_date, device_exposure_end_datetime, device_type_concept_id, unique_device_id, quantity, provider_id, visit_occurrence_id, device_source_value, device_source__concept_id, visit_detail_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect device_exposure' AS test, CASE WHEN(SELECT COUNT(*) FROM device_exposure WHERE")
  first <- TRUE
  if (!missing(device_exposure_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_id)) {
      statement <- paste0(statement, ' device_exposure_id IS NULL')
    } else if (is(device_exposure_id, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_id = (', as.character(device_exposure_id), ')')
    } else {
      statement <- paste0(statement, " device_exposure_id = '", device_exposure_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(device_exposure_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_start_date)) {
      statement <- paste0(statement, ' device_exposure_start_date IS NULL')
    } else if (is(device_exposure_start_date, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_start_date = (', as.character(device_exposure_start_date), ')')
    } else {
      statement <- paste0(statement, " device_exposure_start_date = '", device_exposure_start_date,"'")
    }
  }

  if (!missing(device_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_concept_id)) {
      statement <- paste0(statement, ' device_concept_id IS NULL')
    } else if (is(device_concept_id, 'subQuery')){
      statement <- paste0(statement, ' device_concept_id = (', as.character(device_concept_id), ')')
    } else {
      statement <- paste0(statement, " device_concept_id = '", device_concept_id,"'")
    }
  }

  if (!missing(device_exposure_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_start_datetime)) {
      statement <- paste0(statement, ' device_exposure_start_datetime IS NULL')
    } else if (is(device_exposure_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_start_datetime = (', as.character(device_exposure_start_datetime), ')')
    } else {
      statement <- paste0(statement, " device_exposure_start_datetime = '", device_exposure_start_datetime,"'")
    }
  }

  if (!missing(device_exposure_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_end_date)) {
      statement <- paste0(statement, ' device_exposure_end_date IS NULL')
    } else if (is(device_exposure_end_date, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_end_date = (', as.character(device_exposure_end_date), ')')
    } else {
      statement <- paste0(statement, " device_exposure_end_date = '", device_exposure_end_date,"'")
    }
  }

  if (!missing(device_exposure_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_end_datetime)) {
      statement <- paste0(statement, ' device_exposure_end_datetime IS NULL')
    } else if (is(device_exposure_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_end_datetime = (', as.character(device_exposure_end_datetime), ')')
    } else {
      statement <- paste0(statement, " device_exposure_end_datetime = '", device_exposure_end_datetime,"'")
    }
  }

  if (!missing(device_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_type_concept_id)) {
      statement <- paste0(statement, ' device_type_concept_id IS NULL')
    } else if (is(device_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' device_type_concept_id = (', as.character(device_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " device_type_concept_id = '", device_type_concept_id,"'")
    }
  }

  if (!missing(unique_device_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unique_device_id)) {
      statement <- paste0(statement, ' unique_device_id IS NULL')
    } else if (is(unique_device_id, 'subQuery')){
      statement <- paste0(statement, ' unique_device_id = (', as.character(unique_device_id), ')')
    } else {
      statement <- paste0(statement, " unique_device_id = '", unique_device_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(device_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_source_value)) {
      statement <- paste0(statement, ' device_source_value IS NULL')
    } else if (is(device_source_value, 'subQuery')){
      statement <- paste0(statement, ' device_source_value = (', as.character(device_source_value), ')')
    } else {
      statement <- paste0(statement, " device_source_value = '", device_source_value,"'")
    }
  }

  if (!missing(device_source__concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_source__concept_id)) {
      statement <- paste0(statement, ' [device_source_ concept_id] IS NULL')
    } else if (is(device_source__concept_id, 'subQuery')){
      statement <- paste0(statement, ' [device_source_ concept_id] = (', as.character(device_source__concept_id), ')')
    } else {
      statement <- paste0(statement, " [device_source_ concept_id] = '", device_source__concept_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_death <- function(person_id, death_date, death_datetime, death_type_concept_id, cause_concept_id, cause_source_value, cause_source_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect death' AS test, CASE WHEN(SELECT COUNT(*) FROM death WHERE")
  first <- TRUE
  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(death_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_date)) {
      statement <- paste0(statement, ' death_date IS NULL')
    } else if (is(death_date, 'subQuery')){
      statement <- paste0(statement, ' death_date = (', as.character(death_date), ')')
    } else {
      statement <- paste0(statement, " death_date = '", death_date,"'")
    }
  }

  if (!missing(death_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_datetime)) {
      statement <- paste0(statement, ' death_datetime IS NULL')
    } else if (is(death_datetime, 'subQuery')){
      statement <- paste0(statement, ' death_datetime = (', as.character(death_datetime), ')')
    } else {
      statement <- paste0(statement, " death_datetime = '", death_datetime,"'")
    }
  }

  if (!missing(death_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_type_concept_id)) {
      statement <- paste0(statement, ' death_type_concept_id IS NULL')
    } else if (is(death_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' death_type_concept_id = (', as.character(death_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " death_type_concept_id = '", death_type_concept_id,"'")
    }
  }

  if (!missing(cause_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_concept_id)) {
      statement <- paste0(statement, ' cause_concept_id IS NULL')
    } else if (is(cause_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cause_concept_id = (', as.character(cause_concept_id), ')')
    } else {
      statement <- paste0(statement, " cause_concept_id = '", cause_concept_id,"'")
    }
  }

  if (!missing(cause_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_source_value)) {
      statement <- paste0(statement, ' cause_source_value IS NULL')
    } else if (is(cause_source_value, 'subQuery')){
      statement <- paste0(statement, ' cause_source_value = (', as.character(cause_source_value), ')')
    } else {
      statement <- paste0(statement, " cause_source_value = '", cause_source_value,"'")
    }
  }

  if (!missing(cause_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_source_concept_id)) {
      statement <- paste0(statement, ' cause_source_concept_id IS NULL')
    } else if (is(cause_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cause_source_concept_id = (', as.character(cause_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " cause_source_concept_id = '", cause_source_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_location <- function(location_id, address_1, address_2, city, state, zip, county, location_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect location' AS test, CASE WHEN(SELECT COUNT(*) FROM location WHERE")
  first <- TRUE
  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(address_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(address_1)) {
      statement <- paste0(statement, ' address_1 IS NULL')
    } else if (is(address_1, 'subQuery')){
      statement <- paste0(statement, ' address_1 = (', as.character(address_1), ')')
    } else {
      statement <- paste0(statement, " address_1 = '", address_1,"'")
    }
  }

  if (!missing(address_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(address_2)) {
      statement <- paste0(statement, ' address_2 IS NULL')
    } else if (is(address_2, 'subQuery')){
      statement <- paste0(statement, ' address_2 = (', as.character(address_2), ')')
    } else {
      statement <- paste0(statement, " address_2 = '", address_2,"'")
    }
  }

  if (!missing(city)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(city)) {
      statement <- paste0(statement, ' city IS NULL')
    } else if (is(city, 'subQuery')){
      statement <- paste0(statement, ' city = (', as.character(city), ')')
    } else {
      statement <- paste0(statement, " city = '", city,"'")
    }
  }

  if (!missing(state)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(state)) {
      statement <- paste0(statement, ' state IS NULL')
    } else if (is(state, 'subQuery')){
      statement <- paste0(statement, ' state = (', as.character(state), ')')
    } else {
      statement <- paste0(statement, " state = '", state,"'")
    }
  }

  if (!missing(zip)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(zip)) {
      statement <- paste0(statement, ' zip IS NULL')
    } else if (is(zip, 'subQuery')){
      statement <- paste0(statement, ' zip = (', as.character(zip), ')')
    } else {
      statement <- paste0(statement, " zip = '", zip,"'")
    }
  }

  if (!missing(county)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(county)) {
      statement <- paste0(statement, ' county IS NULL')
    } else if (is(county, 'subQuery')){
      statement <- paste0(statement, ' county = (', as.character(county), ')')
    } else {
      statement <- paste0(statement, " county = '", county,"'")
    }
  }

  if (!missing(location_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_source_value)) {
      statement <- paste0(statement, ' location_source_value IS NULL')
    } else if (is(location_source_value, 'subQuery')){
      statement <- paste0(statement, ' location_source_value = (', as.character(location_source_value), ')')
    } else {
      statement <- paste0(statement, " location_source_value = '", location_source_value,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_care_site <- function(care_site_id, care_site_source_value, location_id, care_site_name, place_of_service_concept_id, place_of_service_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect care_site' AS test, CASE WHEN(SELECT COUNT(*) FROM care_site WHERE")
  first <- TRUE
  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(care_site_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_source_value)) {
      statement <- paste0(statement, ' care_site_source_value IS NULL')
    } else if (is(care_site_source_value, 'subQuery')){
      statement <- paste0(statement, ' care_site_source_value = (', as.character(care_site_source_value), ')')
    } else {
      statement <- paste0(statement, " care_site_source_value = '", care_site_source_value,"'")
    }
  }

  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(care_site_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_name)) {
      statement <- paste0(statement, ' care_site_name IS NULL')
    } else if (is(care_site_name, 'subQuery')){
      statement <- paste0(statement, ' care_site_name = (', as.character(care_site_name), ')')
    } else {
      statement <- paste0(statement, " care_site_name = '", care_site_name,"'")
    }
  }

  if (!missing(place_of_service_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(place_of_service_concept_id)) {
      statement <- paste0(statement, ' place_of_service_concept_id IS NULL')
    } else if (is(place_of_service_concept_id, 'subQuery')){
      statement <- paste0(statement, ' place_of_service_concept_id = (', as.character(place_of_service_concept_id), ')')
    } else {
      statement <- paste0(statement, " place_of_service_concept_id = '", place_of_service_concept_id,"'")
    }
  }

  if (!missing(place_of_service_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(place_of_service_source_value)) {
      statement <- paste0(statement, ' place_of_service_source_value IS NULL')
    } else if (is(place_of_service_source_value, 'subQuery')){
      statement <- paste0(statement, ' place_of_service_source_value = (', as.character(place_of_service_source_value), ')')
    } else {
      statement <- paste0(statement, " place_of_service_source_value = '", place_of_service_source_value,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_provider <- function(provider_id, specialty_concept_id, specialty_source_value, provider_name, npi, dea, care_site_id, year_of_birth, gender_concept_id, provider_source_value, specialty_source_concept_id, gender_source_value, gender_source_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect provider' AS test, CASE WHEN(SELECT COUNT(*) FROM provider WHERE")
  first <- TRUE
  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(specialty_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_concept_id)) {
      statement <- paste0(statement, ' specialty_concept_id IS NULL')
    } else if (is(specialty_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specialty_concept_id = (', as.character(specialty_concept_id), ')')
    } else {
      statement <- paste0(statement, " specialty_concept_id = '", specialty_concept_id,"'")
    }
  }

  if (!missing(specialty_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_source_value)) {
      statement <- paste0(statement, ' specialty_source_value IS NULL')
    } else if (is(specialty_source_value, 'subQuery')){
      statement <- paste0(statement, ' specialty_source_value = (', as.character(specialty_source_value), ')')
    } else {
      statement <- paste0(statement, " specialty_source_value = '", specialty_source_value,"'")
    }
  }

  if (!missing(provider_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_name)) {
      statement <- paste0(statement, ' provider_name IS NULL')
    } else if (is(provider_name, 'subQuery')){
      statement <- paste0(statement, ' provider_name = (', as.character(provider_name), ')')
    } else {
      statement <- paste0(statement, " provider_name = '", provider_name,"'")
    }
  }

  if (!missing(npi)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(npi)) {
      statement <- paste0(statement, ' npi IS NULL')
    } else if (is(npi, 'subQuery')){
      statement <- paste0(statement, ' npi = (', as.character(npi), ')')
    } else {
      statement <- paste0(statement, " npi = '", npi,"'")
    }
  }

  if (!missing(dea)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dea)) {
      statement <- paste0(statement, ' dea IS NULL')
    } else if (is(dea, 'subQuery')){
      statement <- paste0(statement, ' dea = (', as.character(dea), ')')
    } else {
      statement <- paste0(statement, " dea = '", dea,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(year_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(year_of_birth)) {
      statement <- paste0(statement, ' year_of_birth IS NULL')
    } else if (is(year_of_birth, 'subQuery')){
      statement <- paste0(statement, ' year_of_birth = (', as.character(year_of_birth), ')')
    } else {
      statement <- paste0(statement, " year_of_birth = '", year_of_birth,"'")
    }
  }

  if (!missing(gender_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_concept_id)) {
      statement <- paste0(statement, ' gender_concept_id IS NULL')
    } else if (is(gender_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_concept_id = (', as.character(gender_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_concept_id = '", gender_concept_id,"'")
    }
  }

  if (!missing(provider_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_source_value)) {
      statement <- paste0(statement, ' provider_source_value IS NULL')
    } else if (is(provider_source_value, 'subQuery')){
      statement <- paste0(statement, ' provider_source_value = (', as.character(provider_source_value), ')')
    } else {
      statement <- paste0(statement, " provider_source_value = '", provider_source_value,"'")
    }
  }

  if (!missing(specialty_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_source_concept_id)) {
      statement <- paste0(statement, ' specialty_source_concept_id IS NULL')
    } else if (is(specialty_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specialty_source_concept_id = (', as.character(specialty_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " specialty_source_concept_id = '", specialty_source_concept_id,"'")
    }
  }

  if (!missing(gender_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_value)) {
      statement <- paste0(statement, ' gender_source_value IS NULL')
    } else if (is(gender_source_value, 'subQuery')){
      statement <- paste0(statement, ' gender_source_value = (', as.character(gender_source_value), ')')
    } else {
      statement <- paste0(statement, " gender_source_value = '", gender_source_value,"'")
    }
  }

  if (!missing(gender_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_concept_id)) {
      statement <- paste0(statement, ' gender_source_concept_id IS NULL')
    } else if (is(gender_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_source_concept_id = (', as.character(gender_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_source_concept_id = '", gender_source_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_note <- function(note_id, person_id, note_date, note_datetime, note_type_concept_id, note_class_concept_id, note_title, note_text, encoding_concept_id, language_concept_id, provider_id, note_source_value, visit_occurrence_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect note' AS test, CASE WHEN(SELECT COUNT(*) FROM note WHERE")
  first <- TRUE
  if (!missing(note_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_id)) {
      statement <- paste0(statement, ' note_id IS NULL')
    } else if (is(note_id, 'subQuery')){
      statement <- paste0(statement, ' note_id = (', as.character(note_id), ')')
    } else {
      statement <- paste0(statement, " note_id = '", note_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(note_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_date)) {
      statement <- paste0(statement, ' note_date IS NULL')
    } else if (is(note_date, 'subQuery')){
      statement <- paste0(statement, ' note_date = (', as.character(note_date), ')')
    } else {
      statement <- paste0(statement, " note_date = '", note_date,"'")
    }
  }

  if (!missing(note_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_datetime)) {
      statement <- paste0(statement, ' note_datetime IS NULL')
    } else if (is(note_datetime, 'subQuery')){
      statement <- paste0(statement, ' note_datetime = (', as.character(note_datetime), ')')
    } else {
      statement <- paste0(statement, " note_datetime = '", note_datetime,"'")
    }
  }

  if (!missing(note_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_type_concept_id)) {
      statement <- paste0(statement, ' note_type_concept_id IS NULL')
    } else if (is(note_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_type_concept_id = (', as.character(note_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_type_concept_id = '", note_type_concept_id,"'")
    }
  }

  if (!missing(note_class_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_class_concept_id)) {
      statement <- paste0(statement, ' note_class_concept_id IS NULL')
    } else if (is(note_class_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_class_concept_id = (', as.character(note_class_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_class_concept_id = '", note_class_concept_id,"'")
    }
  }

  if (!missing(note_title)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_title)) {
      statement <- paste0(statement, ' note_title IS NULL')
    } else if (is(note_title, 'subQuery')){
      statement <- paste0(statement, ' note_title = (', as.character(note_title), ')')
    } else {
      statement <- paste0(statement, " note_title = '", note_title,"'")
    }
  }

  if (!missing(note_text)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_text)) {
      statement <- paste0(statement, ' note_text IS NULL')
    } else if (is(note_text, 'subQuery')){
      statement <- paste0(statement, ' note_text = (', as.character(note_text), ')')
    } else {
      statement <- paste0(statement, " note_text = '", note_text,"'")
    }
  }

  if (!missing(encoding_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(encoding_concept_id)) {
      statement <- paste0(statement, ' encoding_concept_id IS NULL')
    } else if (is(encoding_concept_id, 'subQuery')){
      statement <- paste0(statement, ' encoding_concept_id = (', as.character(encoding_concept_id), ')')
    } else {
      statement <- paste0(statement, " encoding_concept_id = '", encoding_concept_id,"'")
    }
  }

  if (!missing(language_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(language_concept_id)) {
      statement <- paste0(statement, ' language_concept_id IS NULL')
    } else if (is(language_concept_id, 'subQuery')){
      statement <- paste0(statement, ' language_concept_id = (', as.character(language_concept_id), ')')
    } else {
      statement <- paste0(statement, " language_concept_id = '", language_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(note_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_source_value)) {
      statement <- paste0(statement, ' note_source_value IS NULL')
    } else if (is(note_source_value, 'subQuery')){
      statement <- paste0(statement, ' note_source_value = (', as.character(note_source_value), ')')
    } else {
      statement <- paste0(statement, " note_source_value = '", note_source_value,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_fact_relationship <- function(domain_concept_id_1, fact_id_1, domain_concept_id_2, fact_id_2, relationship_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect fact_relationship' AS test, CASE WHEN(SELECT COUNT(*) FROM fact_relationship WHERE")
  first <- TRUE
  if (!missing(domain_concept_id_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(domain_concept_id_1)) {
      statement <- paste0(statement, ' domain_concept_id_1 IS NULL')
    } else if (is(domain_concept_id_1, 'subQuery')){
      statement <- paste0(statement, ' domain_concept_id_1 = (', as.character(domain_concept_id_1), ')')
    } else {
      statement <- paste0(statement, " domain_concept_id_1 = '", domain_concept_id_1,"'")
    }
  }

  if (!missing(fact_id_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(fact_id_1)) {
      statement <- paste0(statement, ' fact_id_1 IS NULL')
    } else if (is(fact_id_1, 'subQuery')){
      statement <- paste0(statement, ' fact_id_1 = (', as.character(fact_id_1), ')')
    } else {
      statement <- paste0(statement, " fact_id_1 = '", fact_id_1,"'")
    }
  }

  if (!missing(domain_concept_id_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(domain_concept_id_2)) {
      statement <- paste0(statement, ' domain_concept_id_2 IS NULL')
    } else if (is(domain_concept_id_2, 'subQuery')){
      statement <- paste0(statement, ' domain_concept_id_2 = (', as.character(domain_concept_id_2), ')')
    } else {
      statement <- paste0(statement, " domain_concept_id_2 = '", domain_concept_id_2,"'")
    }
  }

  if (!missing(fact_id_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(fact_id_2)) {
      statement <- paste0(statement, ' fact_id_2 IS NULL')
    } else if (is(fact_id_2, 'subQuery')){
      statement <- paste0(statement, ' fact_id_2 = (', as.character(fact_id_2), ')')
    } else {
      statement <- paste0(statement, " fact_id_2 = '", fact_id_2,"'")
    }
  }

  if (!missing(relationship_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(relationship_concept_id)) {
      statement <- paste0(statement, ' relationship_concept_id IS NULL')
    } else if (is(relationship_concept_id, 'subQuery')){
      statement <- paste0(statement, ' relationship_concept_id = (', as.character(relationship_concept_id), ')')
    } else {
      statement <- paste0(statement, " relationship_concept_id = '", relationship_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_note_nlp <- function(note_nlp_id, note_id, section_concept_id, snippet, offset, lexical_variant, note_nlp_concept_id, note_nlp_source_concept_id, nlp_system, nlp_date, nlp_date_time, term_exists, term_temporal, term_modifiers) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect note_nlp' AS test, CASE WHEN(SELECT COUNT(*) FROM note_nlp WHERE")
  first <- TRUE
  if (!missing(note_nlp_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_id)) {
      statement <- paste0(statement, ' note_nlp_id IS NULL')
    } else if (is(note_nlp_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_id = (', as.character(note_nlp_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_id = '", note_nlp_id,"'")
    }
  }

  if (!missing(note_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_id)) {
      statement <- paste0(statement, ' note_id IS NULL')
    } else if (is(note_id, 'subQuery')){
      statement <- paste0(statement, ' note_id = (', as.character(note_id), ')')
    } else {
      statement <- paste0(statement, " note_id = '", note_id,"'")
    }
  }

  if (!missing(section_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(section_concept_id)) {
      statement <- paste0(statement, ' section_concept_id IS NULL')
    } else if (is(section_concept_id, 'subQuery')){
      statement <- paste0(statement, ' section_concept_id = (', as.character(section_concept_id), ')')
    } else {
      statement <- paste0(statement, " section_concept_id = '", section_concept_id,"'")
    }
  }

  if (!missing(snippet)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(snippet)) {
      statement <- paste0(statement, ' snippet IS NULL')
    } else if (is(snippet, 'subQuery')){
      statement <- paste0(statement, ' snippet = (', as.character(snippet), ')')
    } else {
      statement <- paste0(statement, " snippet = '", snippet,"'")
    }
  }

  if (!missing(offset)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(offset)) {
      statement <- paste0(statement, ' offset IS NULL')
    } else if (is(offset, 'subQuery')){
      statement <- paste0(statement, ' offset = (', as.character(offset), ')')
    } else {
      statement <- paste0(statement, " offset = '", offset,"'")
    }
  }

  if (!missing(lexical_variant)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(lexical_variant)) {
      statement <- paste0(statement, ' lexical_variant IS NULL')
    } else if (is(lexical_variant, 'subQuery')){
      statement <- paste0(statement, ' lexical_variant = (', as.character(lexical_variant), ')')
    } else {
      statement <- paste0(statement, " lexical_variant = '", lexical_variant,"'")
    }
  }

  if (!missing(note_nlp_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_concept_id)) {
      statement <- paste0(statement, ' note_nlp_concept_id IS NULL')
    } else if (is(note_nlp_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_concept_id = (', as.character(note_nlp_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_concept_id = '", note_nlp_concept_id,"'")
    }
  }

  if (!missing(note_nlp_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_source_concept_id)) {
      statement <- paste0(statement, ' note_nlp_source_concept_id IS NULL')
    } else if (is(note_nlp_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_source_concept_id = (', as.character(note_nlp_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_source_concept_id = '", note_nlp_source_concept_id,"'")
    }
  }

  if (!missing(nlp_system)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_system)) {
      statement <- paste0(statement, ' nlp_system IS NULL')
    } else if (is(nlp_system, 'subQuery')){
      statement <- paste0(statement, ' nlp_system = (', as.character(nlp_system), ')')
    } else {
      statement <- paste0(statement, " nlp_system = '", nlp_system,"'")
    }
  }

  if (!missing(nlp_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_date)) {
      statement <- paste0(statement, ' nlp_date IS NULL')
    } else if (is(nlp_date, 'subQuery')){
      statement <- paste0(statement, ' nlp_date = (', as.character(nlp_date), ')')
    } else {
      statement <- paste0(statement, " nlp_date = '", nlp_date,"'")
    }
  }

  if (!missing(nlp_date_time)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_date_time)) {
      statement <- paste0(statement, ' nlp_date_time IS NULL')
    } else if (is(nlp_date_time, 'subQuery')){
      statement <- paste0(statement, ' nlp_date_time = (', as.character(nlp_date_time), ')')
    } else {
      statement <- paste0(statement, " nlp_date_time = '", nlp_date_time,"'")
    }
  }

  if (!missing(term_exists)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_exists)) {
      statement <- paste0(statement, ' term_exists IS NULL')
    } else if (is(term_exists, 'subQuery')){
      statement <- paste0(statement, ' term_exists = (', as.character(term_exists), ')')
    } else {
      statement <- paste0(statement, " term_exists = '", term_exists,"'")
    }
  }

  if (!missing(term_temporal)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_temporal)) {
      statement <- paste0(statement, ' term_temporal IS NULL')
    } else if (is(term_temporal, 'subQuery')){
      statement <- paste0(statement, ' term_temporal = (', as.character(term_temporal), ')')
    } else {
      statement <- paste0(statement, " term_temporal = '", term_temporal,"'")
    }
  }

  if (!missing(term_modifiers)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_modifiers)) {
      statement <- paste0(statement, ' term_modifiers IS NULL')
    } else if (is(term_modifiers, 'subQuery')){
      statement <- paste0(statement, ' term_modifiers = (', as.character(term_modifiers), ')')
    } else {
      statement <- paste0(statement, " term_modifiers = '", term_modifiers,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_specimen <- function(specimen_id, person_id, specimen_concept_id, specimen_type_concept_id, specimen_date, specimen_datetime, quantity, unit_concept_id, anatomic_site_concept_id, disease_status_concept_id, specimen_source_id, specimen_source_value, unit_source_value, anatomic_site_source_value, disease_status_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect specimen' AS test, CASE WHEN(SELECT COUNT(*) FROM specimen WHERE")
  first <- TRUE
  if (!missing(specimen_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_id)) {
      statement <- paste0(statement, ' specimen_id IS NULL')
    } else if (is(specimen_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_id = (', as.character(specimen_id), ')')
    } else {
      statement <- paste0(statement, " specimen_id = '", specimen_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(specimen_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_concept_id)) {
      statement <- paste0(statement, ' specimen_concept_id IS NULL')
    } else if (is(specimen_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_concept_id = (', as.character(specimen_concept_id), ')')
    } else {
      statement <- paste0(statement, " specimen_concept_id = '", specimen_concept_id,"'")
    }
  }

  if (!missing(specimen_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_type_concept_id)) {
      statement <- paste0(statement, ' specimen_type_concept_id IS NULL')
    } else if (is(specimen_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_type_concept_id = (', as.character(specimen_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " specimen_type_concept_id = '", specimen_type_concept_id,"'")
    }
  }

  if (!missing(specimen_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_date)) {
      statement <- paste0(statement, ' specimen_date IS NULL')
    } else if (is(specimen_date, 'subQuery')){
      statement <- paste0(statement, ' specimen_date = (', as.character(specimen_date), ')')
    } else {
      statement <- paste0(statement, " specimen_date = '", specimen_date,"'")
    }
  }

  if (!missing(specimen_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_datetime)) {
      statement <- paste0(statement, ' specimen_datetime IS NULL')
    } else if (is(specimen_datetime, 'subQuery')){
      statement <- paste0(statement, ' specimen_datetime = (', as.character(specimen_datetime), ')')
    } else {
      statement <- paste0(statement, " specimen_datetime = '", specimen_datetime,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(anatomic_site_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(anatomic_site_concept_id)) {
      statement <- paste0(statement, ' anatomic_site_concept_id IS NULL')
    } else if (is(anatomic_site_concept_id, 'subQuery')){
      statement <- paste0(statement, ' anatomic_site_concept_id = (', as.character(anatomic_site_concept_id), ')')
    } else {
      statement <- paste0(statement, " anatomic_site_concept_id = '", anatomic_site_concept_id,"'")
    }
  }

  if (!missing(disease_status_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(disease_status_concept_id)) {
      statement <- paste0(statement, ' disease_status_concept_id IS NULL')
    } else if (is(disease_status_concept_id, 'subQuery')){
      statement <- paste0(statement, ' disease_status_concept_id = (', as.character(disease_status_concept_id), ')')
    } else {
      statement <- paste0(statement, " disease_status_concept_id = '", disease_status_concept_id,"'")
    }
  }

  if (!missing(specimen_source_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_source_id)) {
      statement <- paste0(statement, ' specimen_source_id IS NULL')
    } else if (is(specimen_source_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_source_id = (', as.character(specimen_source_id), ')')
    } else {
      statement <- paste0(statement, " specimen_source_id = '", specimen_source_id,"'")
    }
  }

  if (!missing(specimen_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_source_value)) {
      statement <- paste0(statement, ' specimen_source_value IS NULL')
    } else if (is(specimen_source_value, 'subQuery')){
      statement <- paste0(statement, ' specimen_source_value = (', as.character(specimen_source_value), ')')
    } else {
      statement <- paste0(statement, " specimen_source_value = '", specimen_source_value,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(anatomic_site_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(anatomic_site_source_value)) {
      statement <- paste0(statement, ' anatomic_site_source_value IS NULL')
    } else if (is(anatomic_site_source_value, 'subQuery')){
      statement <- paste0(statement, ' anatomic_site_source_value = (', as.character(anatomic_site_source_value), ')')
    } else {
      statement <- paste0(statement, " anatomic_site_source_value = '", anatomic_site_source_value,"'")
    }
  }

  if (!missing(disease_status_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(disease_status_source_value)) {
      statement <- paste0(statement, ' disease_status_source_value IS NULL')
    } else if (is(disease_status_source_value, 'subQuery')){
      statement <- paste0(statement, ' disease_status_source_value = (', as.character(disease_status_source_value), ')')
    } else {
      statement <- paste0(statement, " disease_status_source_value = '", disease_status_source_value,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_cohort <- function(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cohort' AS test, CASE WHEN(SELECT COUNT(*) FROM cohort WHERE")
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(subject_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_id)) {
      statement <- paste0(statement, ' subject_id IS NULL')
    } else if (is(subject_id, 'subQuery')){
      statement <- paste0(statement, ' subject_id = (', as.character(subject_id), ')')
    } else {
      statement <- paste0(statement, " subject_id = '", subject_id,"'")
    }
  }

  if (!missing(cohort_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_start_date)) {
      statement <- paste0(statement, ' cohort_start_date IS NULL')
    } else if (is(cohort_start_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_start_date = (', as.character(cohort_start_date), ')')
    } else {
      statement <- paste0(statement, " cohort_start_date = '", cohort_start_date,"'")
    }
  }

  if (!missing(cohort_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_end_date)) {
      statement <- paste0(statement, ' cohort_end_date IS NULL')
    } else if (is(cohort_end_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_end_date = (', as.character(cohort_end_date), ')')
    } else {
      statement <- paste0(statement, " cohort_end_date = '", cohort_end_date,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_cohort_attribute <- function(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, attribute_definition_id, value_as_number, value_as_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cohort_attribute' AS test, CASE WHEN(SELECT COUNT(*) FROM cohort_attribute WHERE")
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(subject_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_id)) {
      statement <- paste0(statement, ' subject_id IS NULL')
    } else if (is(subject_id, 'subQuery')){
      statement <- paste0(statement, ' subject_id = (', as.character(subject_id), ')')
    } else {
      statement <- paste0(statement, " subject_id = '", subject_id,"'")
    }
  }

  if (!missing(cohort_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_start_date)) {
      statement <- paste0(statement, ' cohort_start_date IS NULL')
    } else if (is(cohort_start_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_start_date = (', as.character(cohort_start_date), ')')
    } else {
      statement <- paste0(statement, " cohort_start_date = '", cohort_start_date,"'")
    }
  }

  if (!missing(cohort_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_end_date)) {
      statement <- paste0(statement, ' cohort_end_date IS NULL')
    } else if (is(cohort_end_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_end_date = (', as.character(cohort_end_date), ')')
    } else {
      statement <- paste0(statement, " cohort_end_date = '", cohort_end_date,"'")
    }
  }

  if (!missing(attribute_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_definition_id)) {
      statement <- paste0(statement, ' attribute_definition_id IS NULL')
    } else if (is(attribute_definition_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_definition_id = (', as.character(attribute_definition_id), ')')
    } else {
      statement <- paste0(statement, " attribute_definition_id = '", attribute_definition_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_drug_era <- function(drug_era_id, person_id, drug_concept_id, drug_era_start_date, drug_era_end_date, drug_exposure_count, gap_days) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect drug_era' AS test, CASE WHEN(SELECT COUNT(*) FROM drug_era WHERE")
  first <- TRUE
  if (!missing(drug_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_id)) {
      statement <- paste0(statement, ' drug_era_id IS NULL')
    } else if (is(drug_era_id, 'subQuery')){
      statement <- paste0(statement, ' drug_era_id = (', as.character(drug_era_id), ')')
    } else {
      statement <- paste0(statement, " drug_era_id = '", drug_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(drug_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_start_date)) {
      statement <- paste0(statement, ' drug_era_start_date IS NULL')
    } else if (is(drug_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' drug_era_start_date = (', as.character(drug_era_start_date), ')')
    } else {
      statement <- paste0(statement, " drug_era_start_date = '", drug_era_start_date,"'")
    }
  }

  if (!missing(drug_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_end_date)) {
      statement <- paste0(statement, ' drug_era_end_date IS NULL')
    } else if (is(drug_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' drug_era_end_date = (', as.character(drug_era_end_date), ')')
    } else {
      statement <- paste0(statement, " drug_era_end_date = '", drug_era_end_date,"'")
    }
  }

  if (!missing(drug_exposure_count)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_count)) {
      statement <- paste0(statement, ' drug_exposure_count IS NULL')
    } else if (is(drug_exposure_count, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_count = (', as.character(drug_exposure_count), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_count = '", drug_exposure_count,"'")
    }
  }

  if (!missing(gap_days)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gap_days)) {
      statement <- paste0(statement, ' gap_days IS NULL')
    } else if (is(gap_days, 'subQuery')){
      statement <- paste0(statement, ' gap_days = (', as.character(gap_days), ')')
    } else {
      statement <- paste0(statement, " gap_days = '", gap_days,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_condition_era <- function(condition_era_id, person_id, condition_concept_id, condition_era_start_date, condition_era_end_date, condition_occurrence_count) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect condition_era' AS test, CASE WHEN(SELECT COUNT(*) FROM condition_era WHERE")
  first <- TRUE
  if (!missing(condition_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_id)) {
      statement <- paste0(statement, ' condition_era_id IS NULL')
    } else if (is(condition_era_id, 'subQuery')){
      statement <- paste0(statement, ' condition_era_id = (', as.character(condition_era_id), ')')
    } else {
      statement <- paste0(statement, " condition_era_id = '", condition_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(condition_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_concept_id)) {
      statement <- paste0(statement, ' condition_concept_id IS NULL')
    } else if (is(condition_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_concept_id = (', as.character(condition_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_concept_id = '", condition_concept_id,"'")
    }
  }

  if (!missing(condition_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_start_date)) {
      statement <- paste0(statement, ' condition_era_start_date IS NULL')
    } else if (is(condition_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' condition_era_start_date = (', as.character(condition_era_start_date), ')')
    } else {
      statement <- paste0(statement, " condition_era_start_date = '", condition_era_start_date,"'")
    }
  }

  if (!missing(condition_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_end_date)) {
      statement <- paste0(statement, ' condition_era_end_date IS NULL')
    } else if (is(condition_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' condition_era_end_date = (', as.character(condition_era_end_date), ')')
    } else {
      statement <- paste0(statement, " condition_era_end_date = '", condition_era_end_date,"'")
    }
  }

  if (!missing(condition_occurrence_count)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_occurrence_count)) {
      statement <- paste0(statement, ' condition_occurrence_count IS NULL')
    } else if (is(condition_occurrence_count, 'subQuery')){
      statement <- paste0(statement, ' condition_occurrence_count = (', as.character(condition_occurrence_count), ')')
    } else {
      statement <- paste0(statement, " condition_occurrence_count = '", condition_occurrence_count,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_dose_era <- function(dose_era_id, person_id, drug_concept_id, unit_concept_id, dose_value, dose_era_start_date, dose_era_end_date) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect dose_era' AS test, CASE WHEN(SELECT COUNT(*) FROM dose_era WHERE")
  first <- TRUE
  if (!missing(dose_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_id)) {
      statement <- paste0(statement, ' dose_era_id IS NULL')
    } else if (is(dose_era_id, 'subQuery')){
      statement <- paste0(statement, ' dose_era_id = (', as.character(dose_era_id), ')')
    } else {
      statement <- paste0(statement, " dose_era_id = '", dose_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(dose_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_value)) {
      statement <- paste0(statement, ' dose_value IS NULL')
    } else if (is(dose_value, 'subQuery')){
      statement <- paste0(statement, ' dose_value = (', as.character(dose_value), ')')
    } else {
      statement <- paste0(statement, " dose_value = '", dose_value,"'")
    }
  }

  if (!missing(dose_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_start_date)) {
      statement <- paste0(statement, ' dose_era_start_date IS NULL')
    } else if (is(dose_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' dose_era_start_date = (', as.character(dose_era_start_date), ')')
    } else {
      statement <- paste0(statement, " dose_era_start_date = '", dose_era_start_date,"'")
    }
  }

  if (!missing(dose_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_end_date)) {
      statement <- paste0(statement, ' dose_era_end_date IS NULL')
    } else if (is(dose_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' dose_era_end_date = (', as.character(dose_era_end_date), ')')
    } else {
      statement <- paste0(statement, " dose_era_end_date = '", dose_era_end_date,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_cost <- function(cost_id, cost_event_id, cost_domain_id, cost_type_concept_id, currency_concept_id, total_charge, total_cost, total_paid, paid_by_payer, paid_by_patient, paid_patient_copay, paid_patient_coinsurance, paid_patient_deductible, paid_by_primary, paid_ingredient_cost, paid_dispensing_fee, payer_plan_period_id, amount_allowed, revenue_code_concept_id, revenue_code_source_value, drg_concept_id, drg_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cost' AS test, CASE WHEN(SELECT COUNT(*) FROM cost WHERE")
  first <- TRUE
  if (!missing(cost_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_id)) {
      statement <- paste0(statement, ' cost_id IS NULL')
    } else if (is(cost_id, 'subQuery')){
      statement <- paste0(statement, ' cost_id = (', as.character(cost_id), ')')
    } else {
      statement <- paste0(statement, " cost_id = '", cost_id,"'")
    }
  }

  if (!missing(cost_event_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_event_id)) {
      statement <- paste0(statement, ' cost_event_id IS NULL')
    } else if (is(cost_event_id, 'subQuery')){
      statement <- paste0(statement, ' cost_event_id = (', as.character(cost_event_id), ')')
    } else {
      statement <- paste0(statement, " cost_event_id = '", cost_event_id,"'")
    }
  }

  if (!missing(cost_domain_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_domain_id)) {
      statement <- paste0(statement, ' cost_domain_id IS NULL')
    } else if (is(cost_domain_id, 'subQuery')){
      statement <- paste0(statement, ' cost_domain_id = (', as.character(cost_domain_id), ')')
    } else {
      statement <- paste0(statement, " cost_domain_id = '", cost_domain_id,"'")
    }
  }

  if (!missing(cost_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_type_concept_id)) {
      statement <- paste0(statement, ' cost_type_concept_id IS NULL')
    } else if (is(cost_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cost_type_concept_id = (', as.character(cost_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " cost_type_concept_id = '", cost_type_concept_id,"'")
    }
  }

  if (!missing(currency_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(currency_concept_id)) {
      statement <- paste0(statement, ' currency_concept_id IS NULL')
    } else if (is(currency_concept_id, 'subQuery')){
      statement <- paste0(statement, ' currency_concept_id = (', as.character(currency_concept_id), ')')
    } else {
      statement <- paste0(statement, " currency_concept_id = '", currency_concept_id,"'")
    }
  }

  if (!missing(total_charge)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_charge)) {
      statement <- paste0(statement, ' total_charge IS NULL')
    } else if (is(total_charge, 'subQuery')){
      statement <- paste0(statement, ' total_charge = (', as.character(total_charge), ')')
    } else {
      statement <- paste0(statement, " total_charge = '", total_charge,"'")
    }
  }

  if (!missing(total_cost)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_cost)) {
      statement <- paste0(statement, ' total_cost IS NULL')
    } else if (is(total_cost, 'subQuery')){
      statement <- paste0(statement, ' total_cost = (', as.character(total_cost), ')')
    } else {
      statement <- paste0(statement, " total_cost = '", total_cost,"'")
    }
  }

  if (!missing(total_paid)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_paid)) {
      statement <- paste0(statement, ' total_paid IS NULL')
    } else if (is(total_paid, 'subQuery')){
      statement <- paste0(statement, ' total_paid = (', as.character(total_paid), ')')
    } else {
      statement <- paste0(statement, " total_paid = '", total_paid,"'")
    }
  }

  if (!missing(paid_by_payer)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_payer)) {
      statement <- paste0(statement, ' paid_by_payer IS NULL')
    } else if (is(paid_by_payer, 'subQuery')){
      statement <- paste0(statement, ' paid_by_payer = (', as.character(paid_by_payer), ')')
    } else {
      statement <- paste0(statement, " paid_by_payer = '", paid_by_payer,"'")
    }
  }

  if (!missing(paid_by_patient)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_patient)) {
      statement <- paste0(statement, ' paid_by_patient IS NULL')
    } else if (is(paid_by_patient, 'subQuery')){
      statement <- paste0(statement, ' paid_by_patient = (', as.character(paid_by_patient), ')')
    } else {
      statement <- paste0(statement, " paid_by_patient = '", paid_by_patient,"'")
    }
  }

  if (!missing(paid_patient_copay)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_copay)) {
      statement <- paste0(statement, ' paid_patient_copay IS NULL')
    } else if (is(paid_patient_copay, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_copay = (', as.character(paid_patient_copay), ')')
    } else {
      statement <- paste0(statement, " paid_patient_copay = '", paid_patient_copay,"'")
    }
  }

  if (!missing(paid_patient_coinsurance)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_coinsurance)) {
      statement <- paste0(statement, ' paid_patient_coinsurance IS NULL')
    } else if (is(paid_patient_coinsurance, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_coinsurance = (', as.character(paid_patient_coinsurance), ')')
    } else {
      statement <- paste0(statement, " paid_patient_coinsurance = '", paid_patient_coinsurance,"'")
    }
  }

  if (!missing(paid_patient_deductible)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_deductible)) {
      statement <- paste0(statement, ' paid_patient_deductible IS NULL')
    } else if (is(paid_patient_deductible, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_deductible = (', as.character(paid_patient_deductible), ')')
    } else {
      statement <- paste0(statement, " paid_patient_deductible = '", paid_patient_deductible,"'")
    }
  }

  if (!missing(paid_by_primary)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_primary)) {
      statement <- paste0(statement, ' paid_by_primary IS NULL')
    } else if (is(paid_by_primary, 'subQuery')){
      statement <- paste0(statement, ' paid_by_primary = (', as.character(paid_by_primary), ')')
    } else {
      statement <- paste0(statement, " paid_by_primary = '", paid_by_primary,"'")
    }
  }

  if (!missing(paid_ingredient_cost)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_ingredient_cost)) {
      statement <- paste0(statement, ' paid_ingredient_cost IS NULL')
    } else if (is(paid_ingredient_cost, 'subQuery')){
      statement <- paste0(statement, ' paid_ingredient_cost = (', as.character(paid_ingredient_cost), ')')
    } else {
      statement <- paste0(statement, " paid_ingredient_cost = '", paid_ingredient_cost,"'")
    }
  }

  if (!missing(paid_dispensing_fee)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_dispensing_fee)) {
      statement <- paste0(statement, ' paid_dispensing_fee IS NULL')
    } else if (is(paid_dispensing_fee, 'subQuery')){
      statement <- paste0(statement, ' paid_dispensing_fee = (', as.character(paid_dispensing_fee), ')')
    } else {
      statement <- paste0(statement, " paid_dispensing_fee = '", paid_dispensing_fee,"'")
    }
  }

  if (!missing(payer_plan_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_id)) {
      statement <- paste0(statement, ' payer_plan_period_id IS NULL')
    } else if (is(payer_plan_period_id, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_id = (', as.character(payer_plan_period_id), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_id = '", payer_plan_period_id,"'")
    }
  }

  if (!missing(amount_allowed)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(amount_allowed)) {
      statement <- paste0(statement, ' amount_allowed IS NULL')
    } else if (is(amount_allowed, 'subQuery')){
      statement <- paste0(statement, ' amount_allowed = (', as.character(amount_allowed), ')')
    } else {
      statement <- paste0(statement, " amount_allowed = '", amount_allowed,"'")
    }
  }

  if (!missing(revenue_code_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(revenue_code_concept_id)) {
      statement <- paste0(statement, ' revenue_code_concept_id IS NULL')
    } else if (is(revenue_code_concept_id, 'subQuery')){
      statement <- paste0(statement, ' revenue_code_concept_id = (', as.character(revenue_code_concept_id), ')')
    } else {
      statement <- paste0(statement, " revenue_code_concept_id = '", revenue_code_concept_id,"'")
    }
  }

  if (!missing(revenue_code_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(revenue_code_source_value)) {
      statement <- paste0(statement, ' revenue_code_source_value IS NULL')
    } else if (is(revenue_code_source_value, 'subQuery')){
      statement <- paste0(statement, ' revenue_code_source_value = (', as.character(revenue_code_source_value), ')')
    } else {
      statement <- paste0(statement, " revenue_code_source_value = '", revenue_code_source_value,"'")
    }
  }

  if (!missing(drg_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drg_concept_id)) {
      statement <- paste0(statement, ' drg_concept_id IS NULL')
    } else if (is(drg_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drg_concept_id = (', as.character(drg_concept_id), ')')
    } else {
      statement <- paste0(statement, " drg_concept_id = '", drg_concept_id,"'")
    }
  }

  if (!missing(drg_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drg_source_value)) {
      statement <- paste0(statement, ' drg_source_value IS NULL')
    } else if (is(drg_source_value, 'subQuery')){
      statement <- paste0(statement, ' drg_source_value = (', as.character(drg_source_value), ')')
    } else {
      statement <- paste0(statement, " drg_source_value = '", drg_source_value,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_payer_plan_period <- function(payer_plan_period_id, person_id, payer_plan_period_start_date, payer_plan_period_end_date, payer_source_value, plan_source_value, family_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect payer_plan_period' AS test, CASE WHEN(SELECT COUNT(*) FROM payer_plan_period WHERE")
  first <- TRUE
  if (!missing(payer_plan_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_id)) {
      statement <- paste0(statement, ' payer_plan_period_id IS NULL')
    } else if (is(payer_plan_period_id, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_id = (', as.character(payer_plan_period_id), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_id = '", payer_plan_period_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(payer_plan_period_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_start_date)) {
      statement <- paste0(statement, ' payer_plan_period_start_date IS NULL')
    } else if (is(payer_plan_period_start_date, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_start_date = (', as.character(payer_plan_period_start_date), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_start_date = '", payer_plan_period_start_date,"'")
    }
  }

  if (!missing(payer_plan_period_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_end_date)) {
      statement <- paste0(statement, ' payer_plan_period_end_date IS NULL')
    } else if (is(payer_plan_period_end_date, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_end_date = (', as.character(payer_plan_period_end_date), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_end_date = '", payer_plan_period_end_date,"'")
    }
  }

  if (!missing(payer_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_source_value)) {
      statement <- paste0(statement, ' payer_source_value IS NULL')
    } else if (is(payer_source_value, 'subQuery')){
      statement <- paste0(statement, ' payer_source_value = (', as.character(payer_source_value), ')')
    } else {
      statement <- paste0(statement, " payer_source_value = '", payer_source_value,"'")
    }
  }

  if (!missing(plan_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(plan_source_value)) {
      statement <- paste0(statement, ' plan_source_value IS NULL')
    } else if (is(plan_source_value, 'subQuery')){
      statement <- paste0(statement, ' plan_source_value = (', as.character(plan_source_value), ')')
    } else {
      statement <- paste0(statement, " plan_source_value = '", plan_source_value,"'")
    }
  }

  if (!missing(family_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(family_source_value)) {
      statement <- paste0(statement, ' family_source_value IS NULL')
    } else if (is(family_source_value, 'subQuery')){
      statement <- paste0(statement, ' family_source_value = (', as.character(family_source_value), ')')
    } else {
      statement <- paste0(statement, " family_source_value = '", family_source_value,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_cdm_source <- function(cdm_source_name, cdm_source_abbreviation, cdm_holder, source_description, source_documentation_reference, cdm_etl__reference, source_release_date, cdm_release_date, cdm_version, vocabulary_version) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cdm_source' AS test, CASE WHEN(SELECT COUNT(*) FROM cdm_source WHERE")
  first <- TRUE
  if (!missing(cdm_source_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_source_name)) {
      statement <- paste0(statement, ' cdm_source_name IS NULL')
    } else if (is(cdm_source_name, 'subQuery')){
      statement <- paste0(statement, ' cdm_source_name = (', as.character(cdm_source_name), ')')
    } else {
      statement <- paste0(statement, " cdm_source_name = '", cdm_source_name,"'")
    }
  }

  if (!missing(cdm_source_abbreviation)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_source_abbreviation)) {
      statement <- paste0(statement, ' cdm_source_abbreviation IS NULL')
    } else if (is(cdm_source_abbreviation, 'subQuery')){
      statement <- paste0(statement, ' cdm_source_abbreviation = (', as.character(cdm_source_abbreviation), ')')
    } else {
      statement <- paste0(statement, " cdm_source_abbreviation = '", cdm_source_abbreviation,"'")
    }
  }

  if (!missing(cdm_holder)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_holder)) {
      statement <- paste0(statement, ' cdm_holder IS NULL')
    } else if (is(cdm_holder, 'subQuery')){
      statement <- paste0(statement, ' cdm_holder = (', as.character(cdm_holder), ')')
    } else {
      statement <- paste0(statement, " cdm_holder = '", cdm_holder,"'")
    }
  }

  if (!missing(source_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_description)) {
      statement <- paste0(statement, ' source_description IS NULL')
    } else if (is(source_description, 'subQuery')){
      statement <- paste0(statement, ' source_description = (', as.character(source_description), ')')
    } else {
      statement <- paste0(statement, " source_description = '", source_description,"'")
    }
  }

  if (!missing(source_documentation_reference)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_documentation_reference)) {
      statement <- paste0(statement, ' source_documentation_reference IS NULL')
    } else if (is(source_documentation_reference, 'subQuery')){
      statement <- paste0(statement, ' source_documentation_reference = (', as.character(source_documentation_reference), ')')
    } else {
      statement <- paste0(statement, " source_documentation_reference = '", source_documentation_reference,"'")
    }
  }

  if (!missing(cdm_etl__reference)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_etl__reference)) {
      statement <- paste0(statement, ' [cdm_etl _reference] IS NULL')
    } else if (is(cdm_etl__reference, 'subQuery')){
      statement <- paste0(statement, ' [cdm_etl _reference] = (', as.character(cdm_etl__reference), ')')
    } else {
      statement <- paste0(statement, " [cdm_etl _reference] = '", cdm_etl__reference,"'")
    }
  }

  if (!missing(source_release_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_release_date)) {
      statement <- paste0(statement, ' source_release_date IS NULL')
    } else if (is(source_release_date, 'subQuery')){
      statement <- paste0(statement, ' source_release_date = (', as.character(source_release_date), ')')
    } else {
      statement <- paste0(statement, " source_release_date = '", source_release_date,"'")
    }
  }

  if (!missing(cdm_release_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_release_date)) {
      statement <- paste0(statement, ' cdm_release_date IS NULL')
    } else if (is(cdm_release_date, 'subQuery')){
      statement <- paste0(statement, ' cdm_release_date = (', as.character(cdm_release_date), ')')
    } else {
      statement <- paste0(statement, " cdm_release_date = '", cdm_release_date,"'")
    }
  }

  if (!missing(cdm_version)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_version)) {
      statement <- paste0(statement, ' cdm_version IS NULL')
    } else if (is(cdm_version, 'subQuery')){
      statement <- paste0(statement, ' cdm_version = (', as.character(cdm_version), ')')
    } else {
      statement <- paste0(statement, " cdm_version = '", cdm_version,"'")
    }
  }

  if (!missing(vocabulary_version)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(vocabulary_version)) {
      statement <- paste0(statement, ' vocabulary_version IS NULL')
    } else if (is(vocabulary_version, 'subQuery')){
      statement <- paste0(statement, ' vocabulary_version = (', as.character(vocabulary_version), ')')
    } else {
      statement <- paste0(statement, " vocabulary_version = '", vocabulary_version,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_attribute_definition <- function(attribute_definition_id, attribute_name, attribute_description, attribute_type_concept_id, attribute_syntax) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect attribute_definition' AS test, CASE WHEN(SELECT COUNT(*) FROM attribute_definition WHERE")
  first <- TRUE
  if (!missing(attribute_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_definition_id)) {
      statement <- paste0(statement, ' attribute_definition_id IS NULL')
    } else if (is(attribute_definition_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_definition_id = (', as.character(attribute_definition_id), ')')
    } else {
      statement <- paste0(statement, " attribute_definition_id = '", attribute_definition_id,"'")
    }
  }

  if (!missing(attribute_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_name)) {
      statement <- paste0(statement, ' attribute_name IS NULL')
    } else if (is(attribute_name, 'subQuery')){
      statement <- paste0(statement, ' attribute_name = (', as.character(attribute_name), ')')
    } else {
      statement <- paste0(statement, " attribute_name = '", attribute_name,"'")
    }
  }

  if (!missing(attribute_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_description)) {
      statement <- paste0(statement, ' attribute_description IS NULL')
    } else if (is(attribute_description, 'subQuery')){
      statement <- paste0(statement, ' attribute_description = (', as.character(attribute_description), ')')
    } else {
      statement <- paste0(statement, " attribute_description = '", attribute_description,"'")
    }
  }

  if (!missing(attribute_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_type_concept_id)) {
      statement <- paste0(statement, ' attribute_type_concept_id IS NULL')
    } else if (is(attribute_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_type_concept_id = (', as.character(attribute_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " attribute_type_concept_id = '", attribute_type_concept_id,"'")
    }
  }

  if (!missing(attribute_syntax)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_syntax)) {
      statement <- paste0(statement, ' attribute_syntax IS NULL')
    } else if (is(attribute_syntax, 'subQuery')){
      statement <- paste0(statement, ' attribute_syntax = (', as.character(attribute_syntax), ')')
    } else {
      statement <- paste0(statement, " attribute_syntax = '", attribute_syntax,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_no_cohort_definition <- function(cohort_definition_id, cohort_definition_name, cohort_definition_description, definition_type_concept_id, cohort_definition_syntax, subject_concept_id, cohort_instantiation_date) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cohort_definition' AS test, CASE WHEN(SELECT COUNT(*) FROM cohort_definition WHERE")
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(cohort_definition_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_name)) {
      statement <- paste0(statement, ' cohort_definition_name IS NULL')
    } else if (is(cohort_definition_name, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_name = (', as.character(cohort_definition_name), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_name = '", cohort_definition_name,"'")
    }
  }

  if (!missing(cohort_definition_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_description)) {
      statement <- paste0(statement, ' cohort_definition_description IS NULL')
    } else if (is(cohort_definition_description, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_description = (', as.character(cohort_definition_description), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_description = '", cohort_definition_description,"'")
    }
  }

  if (!missing(definition_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(definition_type_concept_id)) {
      statement <- paste0(statement, ' definition_type_concept_id IS NULL')
    } else if (is(definition_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' definition_type_concept_id = (', as.character(definition_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " definition_type_concept_id = '", definition_type_concept_id,"'")
    }
  }

  if (!missing(cohort_definition_syntax)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_syntax)) {
      statement <- paste0(statement, ' cohort_definition_syntax IS NULL')
    } else if (is(cohort_definition_syntax, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_syntax = (', as.character(cohort_definition_syntax), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_syntax = '", cohort_definition_syntax,"'")
    }
  }

  if (!missing(subject_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_concept_id)) {
      statement <- paste0(statement, ' subject_concept_id IS NULL')
    } else if (is(subject_concept_id, 'subQuery')){
      statement <- paste0(statement, ' subject_concept_id = (', as.character(subject_concept_id), ')')
    } else {
      statement <- paste0(statement, " subject_concept_id = '", subject_concept_id,"'")
    }
  }

  if (!missing(cohort_instantiation_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_instantiation_date)) {
      statement <- paste0(statement, ' cohort_instantiation_date IS NULL')
    } else if (is(cohort_instantiation_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_instantiation_date = (', as.character(cohort_instantiation_date), ')')
    } else {
      statement <- paste0(statement, " cohort_instantiation_date = '", cohort_instantiation_date,"'")
    }
  }

  statement <- paste0(statement, ") != 0 THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_person <- function(rowCount, person_id, person_source_value, care_site_id, gender_concept_id, gender_source_value, year_of_birth, month_of_birth, day_of_birth, birth_datetime, race_concept_id, race_source_value, ethnicity_concept_id, ethnicity_source_value, location_id, provider_id, gender_source_concept_id, race_source_concept_id, ethnicity_source_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect person' AS test, CASE WHEN(SELECT COUNT(*) FROM person WHERE")
  first <- TRUE
  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(person_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_source_value)) {
      statement <- paste0(statement, ' person_source_value IS NULL')
    } else if (is(person_source_value, 'subQuery')){
      statement <- paste0(statement, ' person_source_value = (', as.character(person_source_value), ')')
    } else {
      statement <- paste0(statement, " person_source_value = '", person_source_value,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(gender_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_concept_id)) {
      statement <- paste0(statement, ' gender_concept_id IS NULL')
    } else if (is(gender_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_concept_id = (', as.character(gender_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_concept_id = '", gender_concept_id,"'")
    }
  }

  if (!missing(gender_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_value)) {
      statement <- paste0(statement, ' gender_source_value IS NULL')
    } else if (is(gender_source_value, 'subQuery')){
      statement <- paste0(statement, ' gender_source_value = (', as.character(gender_source_value), ')')
    } else {
      statement <- paste0(statement, " gender_source_value = '", gender_source_value,"'")
    }
  }

  if (!missing(year_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(year_of_birth)) {
      statement <- paste0(statement, ' year_of_birth IS NULL')
    } else if (is(year_of_birth, 'subQuery')){
      statement <- paste0(statement, ' year_of_birth = (', as.character(year_of_birth), ')')
    } else {
      statement <- paste0(statement, " year_of_birth = '", year_of_birth,"'")
    }
  }

  if (!missing(month_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(month_of_birth)) {
      statement <- paste0(statement, ' month_of_birth IS NULL')
    } else if (is(month_of_birth, 'subQuery')){
      statement <- paste0(statement, ' month_of_birth = (', as.character(month_of_birth), ')')
    } else {
      statement <- paste0(statement, " month_of_birth = '", month_of_birth,"'")
    }
  }

  if (!missing(day_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(day_of_birth)) {
      statement <- paste0(statement, ' day_of_birth IS NULL')
    } else if (is(day_of_birth, 'subQuery')){
      statement <- paste0(statement, ' day_of_birth = (', as.character(day_of_birth), ')')
    } else {
      statement <- paste0(statement, " day_of_birth = '", day_of_birth,"'")
    }
  }

  if (!missing(birth_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(birth_datetime)) {
      statement <- paste0(statement, ' birth_datetime IS NULL')
    } else if (is(birth_datetime, 'subQuery')){
      statement <- paste0(statement, ' birth_datetime = (', as.character(birth_datetime), ')')
    } else {
      statement <- paste0(statement, " birth_datetime = '", birth_datetime,"'")
    }
  }

  if (!missing(race_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_concept_id)) {
      statement <- paste0(statement, ' race_concept_id IS NULL')
    } else if (is(race_concept_id, 'subQuery')){
      statement <- paste0(statement, ' race_concept_id = (', as.character(race_concept_id), ')')
    } else {
      statement <- paste0(statement, " race_concept_id = '", race_concept_id,"'")
    }
  }

  if (!missing(race_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_source_value)) {
      statement <- paste0(statement, ' race_source_value IS NULL')
    } else if (is(race_source_value, 'subQuery')){
      statement <- paste0(statement, ' race_source_value = (', as.character(race_source_value), ')')
    } else {
      statement <- paste0(statement, " race_source_value = '", race_source_value,"'")
    }
  }

  if (!missing(ethnicity_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_concept_id)) {
      statement <- paste0(statement, ' ethnicity_concept_id IS NULL')
    } else if (is(ethnicity_concept_id, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_concept_id = (', as.character(ethnicity_concept_id), ')')
    } else {
      statement <- paste0(statement, " ethnicity_concept_id = '", ethnicity_concept_id,"'")
    }
  }

  if (!missing(ethnicity_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_source_value)) {
      statement <- paste0(statement, ' ethnicity_source_value IS NULL')
    } else if (is(ethnicity_source_value, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_source_value = (', as.character(ethnicity_source_value), ')')
    } else {
      statement <- paste0(statement, " ethnicity_source_value = '", ethnicity_source_value,"'")
    }
  }

  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(gender_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_concept_id)) {
      statement <- paste0(statement, ' gender_source_concept_id IS NULL')
    } else if (is(gender_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_source_concept_id = (', as.character(gender_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_source_concept_id = '", gender_source_concept_id,"'")
    }
  }

  if (!missing(race_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_source_concept_id)) {
      statement <- paste0(statement, ' race_source_concept_id IS NULL')
    } else if (is(race_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' race_source_concept_id = (', as.character(race_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " race_source_concept_id = '", race_source_concept_id,"'")
    }
  }

  if (!missing(ethnicity_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_source_concept_id)) {
      statement <- paste0(statement, ' ethnicity_source_concept_id IS NULL')
    } else if (is(ethnicity_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_source_concept_id = (', as.character(ethnicity_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " ethnicity_source_concept_id = '", ethnicity_source_concept_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_observation_period <- function(rowCount, observation_period_id, person_id, observation_period_start_date, observation_period_end_date, period_type_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect observation_period' AS test, CASE WHEN(SELECT COUNT(*) FROM observation_period WHERE")
  first <- TRUE
  if (!missing(observation_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_id)) {
      statement <- paste0(statement, ' observation_period_id IS NULL')
    } else if (is(observation_period_id, 'subQuery')){
      statement <- paste0(statement, ' observation_period_id = (', as.character(observation_period_id), ')')
    } else {
      statement <- paste0(statement, " observation_period_id = '", observation_period_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(observation_period_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_start_date)) {
      statement <- paste0(statement, ' observation_period_start_date IS NULL')
    } else if (is(observation_period_start_date, 'subQuery')){
      statement <- paste0(statement, ' observation_period_start_date = (', as.character(observation_period_start_date), ')')
    } else {
      statement <- paste0(statement, " observation_period_start_date = '", observation_period_start_date,"'")
    }
  }

  if (!missing(observation_period_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_end_date)) {
      statement <- paste0(statement, ' observation_period_end_date IS NULL')
    } else if (is(observation_period_end_date, 'subQuery')){
      statement <- paste0(statement, ' observation_period_end_date = (', as.character(observation_period_end_date), ')')
    } else {
      statement <- paste0(statement, " observation_period_end_date = '", observation_period_end_date,"'")
    }
  }

  if (!missing(period_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(period_type_concept_id)) {
      statement <- paste0(statement, ' period_type_concept_id IS NULL')
    } else if (is(period_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' period_type_concept_id = (', as.character(period_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " period_type_concept_id = '", period_type_concept_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_visit_occurrence <- function(rowCount, visit_occurrence_id, visit_source_value, person_id, care_site_id, visit_start_date, visit_end_date, visit_concept_id, provider_id, visit_type_concept_id, visit_source_concept_id, admitting_source_value, discharge_to_concept_id, discharge_to_source_value, preceding_visit_occurrence_id, visit_start_datetime, visit_end_datetime) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect visit_occurrence' AS test, CASE WHEN(SELECT COUNT(*) FROM visit_occurrence WHERE")
  first <- TRUE
  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_value)) {
      statement <- paste0(statement, ' visit_source_value IS NULL')
    } else if (is(visit_source_value, 'subQuery')){
      statement <- paste0(statement, ' visit_source_value = (', as.character(visit_source_value), ')')
    } else {
      statement <- paste0(statement, " visit_source_value = '", visit_source_value,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(visit_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_date)) {
      statement <- paste0(statement, ' visit_start_date IS NULL')
    } else if (is(visit_start_date, 'subQuery')){
      statement <- paste0(statement, ' visit_start_date = (', as.character(visit_start_date), ')')
    } else {
      statement <- paste0(statement, " visit_start_date = '", visit_start_date,"'")
    }
  }

  if (!missing(visit_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_date)) {
      statement <- paste0(statement, ' visit_end_date IS NULL')
    } else if (is(visit_end_date, 'subQuery')){
      statement <- paste0(statement, ' visit_end_date = (', as.character(visit_end_date), ')')
    } else {
      statement <- paste0(statement, " visit_end_date = '", visit_end_date,"'")
    }
  }

  if (!missing(visit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_concept_id)) {
      statement <- paste0(statement, ' visit_concept_id IS NULL')
    } else if (is(visit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_concept_id = (', as.character(visit_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_concept_id = '", visit_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_type_concept_id)) {
      statement <- paste0(statement, ' visit_type_concept_id IS NULL')
    } else if (is(visit_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_type_concept_id = (', as.character(visit_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_type_concept_id = '", visit_type_concept_id,"'")
    }
  }

  if (!missing(visit_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_concept_id)) {
      statement <- paste0(statement, ' visit_source_concept_id IS NULL')
    } else if (is(visit_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_source_concept_id = (', as.character(visit_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_source_concept_id = '", visit_source_concept_id,"'")
    }
  }

  if (!missing(admitting_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(admitting_source_value)) {
      statement <- paste0(statement, ' admitting_source_value IS NULL')
    } else if (is(admitting_source_value, 'subQuery')){
      statement <- paste0(statement, ' admitting_source_value = (', as.character(admitting_source_value), ')')
    } else {
      statement <- paste0(statement, " admitting_source_value = '", admitting_source_value,"'")
    }
  }

  if (!missing(discharge_to_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_concept_id)) {
      statement <- paste0(statement, ' discharge_to_concept_id IS NULL')
    } else if (is(discharge_to_concept_id, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_concept_id = (', as.character(discharge_to_concept_id), ')')
    } else {
      statement <- paste0(statement, " discharge_to_concept_id = '", discharge_to_concept_id,"'")
    }
  }

  if (!missing(discharge_to_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_source_value)) {
      statement <- paste0(statement, ' discharge_to_source_value IS NULL')
    } else if (is(discharge_to_source_value, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_source_value = (', as.character(discharge_to_source_value), ')')
    } else {
      statement <- paste0(statement, " discharge_to_source_value = '", discharge_to_source_value,"'")
    }
  }

  if (!missing(preceding_visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(preceding_visit_occurrence_id)) {
      statement <- paste0(statement, ' preceding_visit_occurrence_id IS NULL')
    } else if (is(preceding_visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' preceding_visit_occurrence_id = (', as.character(preceding_visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " preceding_visit_occurrence_id = '", preceding_visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_datetime)) {
      statement <- paste0(statement, ' visit_start_datetime IS NULL')
    } else if (is(visit_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_start_datetime = (', as.character(visit_start_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_start_datetime = '", visit_start_datetime,"'")
    }
  }

  if (!missing(visit_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_datetime)) {
      statement <- paste0(statement, ' visit_end_datetime IS NULL')
    } else if (is(visit_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_end_datetime = (', as.character(visit_end_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_end_datetime = '", visit_end_datetime,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_visit_detail <- function(rowCount, visit_detail_id, person_id, visit_detail_concept_id, visit_start_date, visit_start_datetime, visit_end_date, visit_end_datetime, visit_type_concept_id, provider_id, care_site_id, visit_source_value, visit_source_concept_id, admitting_source_value, discharge_to_source_value, discharge_to_concept_id, preceding_visit_detail_id, visit_detail_parent_id, visit_occurrence_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect visit_detail' AS test, CASE WHEN(SELECT COUNT(*) FROM visit_detail WHERE")
  first <- TRUE
  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(visit_detail_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_concept_id)) {
      statement <- paste0(statement, ' visit_detail_concept_id IS NULL')
    } else if (is(visit_detail_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_concept_id = (', as.character(visit_detail_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_concept_id = '", visit_detail_concept_id,"'")
    }
  }

  if (!missing(visit_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_date)) {
      statement <- paste0(statement, ' visit_start_date IS NULL')
    } else if (is(visit_start_date, 'subQuery')){
      statement <- paste0(statement, ' visit_start_date = (', as.character(visit_start_date), ')')
    } else {
      statement <- paste0(statement, " visit_start_date = '", visit_start_date,"'")
    }
  }

  if (!missing(visit_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_datetime)) {
      statement <- paste0(statement, ' visit_start_datetime IS NULL')
    } else if (is(visit_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_start_datetime = (', as.character(visit_start_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_start_datetime = '", visit_start_datetime,"'")
    }
  }

  if (!missing(visit_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_date)) {
      statement <- paste0(statement, ' visit_end_date IS NULL')
    } else if (is(visit_end_date, 'subQuery')){
      statement <- paste0(statement, ' visit_end_date = (', as.character(visit_end_date), ')')
    } else {
      statement <- paste0(statement, " visit_end_date = '", visit_end_date,"'")
    }
  }

  if (!missing(visit_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_datetime)) {
      statement <- paste0(statement, ' visit_end_datetime IS NULL')
    } else if (is(visit_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_end_datetime = (', as.character(visit_end_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_end_datetime = '", visit_end_datetime,"'")
    }
  }

  if (!missing(visit_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_type_concept_id)) {
      statement <- paste0(statement, ' visit_type_concept_id IS NULL')
    } else if (is(visit_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_type_concept_id = (', as.character(visit_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_type_concept_id = '", visit_type_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(visit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_value)) {
      statement <- paste0(statement, ' visit_source_value IS NULL')
    } else if (is(visit_source_value, 'subQuery')){
      statement <- paste0(statement, ' visit_source_value = (', as.character(visit_source_value), ')')
    } else {
      statement <- paste0(statement, " visit_source_value = '", visit_source_value,"'")
    }
  }

  if (!missing(visit_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_concept_id)) {
      statement <- paste0(statement, ' visit_source_concept_id IS NULL')
    } else if (is(visit_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_source_concept_id = (', as.character(visit_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_source_concept_id = '", visit_source_concept_id,"'")
    }
  }

  if (!missing(admitting_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(admitting_source_value)) {
      statement <- paste0(statement, ' admitting_source_value IS NULL')
    } else if (is(admitting_source_value, 'subQuery')){
      statement <- paste0(statement, ' admitting_source_value = (', as.character(admitting_source_value), ')')
    } else {
      statement <- paste0(statement, " admitting_source_value = '", admitting_source_value,"'")
    }
  }

  if (!missing(discharge_to_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_source_value)) {
      statement <- paste0(statement, ' discharge_to_source_value IS NULL')
    } else if (is(discharge_to_source_value, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_source_value = (', as.character(discharge_to_source_value), ')')
    } else {
      statement <- paste0(statement, " discharge_to_source_value = '", discharge_to_source_value,"'")
    }
  }

  if (!missing(discharge_to_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_concept_id)) {
      statement <- paste0(statement, ' discharge_to_concept_id IS NULL')
    } else if (is(discharge_to_concept_id, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_concept_id = (', as.character(discharge_to_concept_id), ')')
    } else {
      statement <- paste0(statement, " discharge_to_concept_id = '", discharge_to_concept_id,"'")
    }
  }

  if (!missing(preceding_visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(preceding_visit_detail_id)) {
      statement <- paste0(statement, ' preceding_visit_detail_id IS NULL')
    } else if (is(preceding_visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' preceding_visit_detail_id = (', as.character(preceding_visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " preceding_visit_detail_id = '", preceding_visit_detail_id,"'")
    }
  }

  if (!missing(visit_detail_parent_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_parent_id)) {
      statement <- paste0(statement, ' visit_detail_parent_id IS NULL')
    } else if (is(visit_detail_parent_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_parent_id = (', as.character(visit_detail_parent_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_parent_id = '", visit_detail_parent_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_condition_occurrence <- function(rowCount, condition_occurrence_id, person_id, condition_start_date, visit_occurrence_id, condition_concept_id, condition_start_datetime, condition_end_date, condition_end_datetime, condition_type_concept_id, stop_reason, provider_id, visit_detail_id, condition_source_concept_id, condition_source_value, condition_status_source_value, condition_status_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect condition_occurrence' AS test, CASE WHEN(SELECT COUNT(*) FROM condition_occurrence WHERE")
  first <- TRUE
  if (!missing(condition_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_occurrence_id)) {
      statement <- paste0(statement, ' condition_occurrence_id IS NULL')
    } else if (is(condition_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' condition_occurrence_id = (', as.character(condition_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " condition_occurrence_id = '", condition_occurrence_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(condition_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_start_date)) {
      statement <- paste0(statement, ' condition_start_date IS NULL')
    } else if (is(condition_start_date, 'subQuery')){
      statement <- paste0(statement, ' condition_start_date = (', as.character(condition_start_date), ')')
    } else {
      statement <- paste0(statement, " condition_start_date = '", condition_start_date,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(condition_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_concept_id)) {
      statement <- paste0(statement, ' condition_concept_id IS NULL')
    } else if (is(condition_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_concept_id = (', as.character(condition_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_concept_id = '", condition_concept_id,"'")
    }
  }

  if (!missing(condition_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_start_datetime)) {
      statement <- paste0(statement, ' condition_start_datetime IS NULL')
    } else if (is(condition_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' condition_start_datetime = (', as.character(condition_start_datetime), ')')
    } else {
      statement <- paste0(statement, " condition_start_datetime = '", condition_start_datetime,"'")
    }
  }

  if (!missing(condition_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_end_date)) {
      statement <- paste0(statement, ' condition_end_date IS NULL')
    } else if (is(condition_end_date, 'subQuery')){
      statement <- paste0(statement, ' condition_end_date = (', as.character(condition_end_date), ')')
    } else {
      statement <- paste0(statement, " condition_end_date = '", condition_end_date,"'")
    }
  }

  if (!missing(condition_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_end_datetime)) {
      statement <- paste0(statement, ' condition_end_datetime IS NULL')
    } else if (is(condition_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' condition_end_datetime = (', as.character(condition_end_datetime), ')')
    } else {
      statement <- paste0(statement, " condition_end_datetime = '", condition_end_datetime,"'")
    }
  }

  if (!missing(condition_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_type_concept_id)) {
      statement <- paste0(statement, ' condition_type_concept_id IS NULL')
    } else if (is(condition_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_type_concept_id = (', as.character(condition_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_type_concept_id = '", condition_type_concept_id,"'")
    }
  }

  if (!missing(stop_reason)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(stop_reason)) {
      statement <- paste0(statement, ' stop_reason IS NULL')
    } else if (is(stop_reason, 'subQuery')){
      statement <- paste0(statement, ' stop_reason = (', as.character(stop_reason), ')')
    } else {
      statement <- paste0(statement, " stop_reason = '", stop_reason,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(condition_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_source_concept_id)) {
      statement <- paste0(statement, ' condition_source_concept_id IS NULL')
    } else if (is(condition_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_source_concept_id = (', as.character(condition_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_source_concept_id = '", condition_source_concept_id,"'")
    }
  }

  if (!missing(condition_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_source_value)) {
      statement <- paste0(statement, ' condition_source_value IS NULL')
    } else if (is(condition_source_value, 'subQuery')){
      statement <- paste0(statement, ' condition_source_value = (', as.character(condition_source_value), ')')
    } else {
      statement <- paste0(statement, " condition_source_value = '", condition_source_value,"'")
    }
  }

  if (!missing(condition_status_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_status_source_value)) {
      statement <- paste0(statement, ' condition_status_source_value IS NULL')
    } else if (is(condition_status_source_value, 'subQuery')){
      statement <- paste0(statement, ' condition_status_source_value = (', as.character(condition_status_source_value), ')')
    } else {
      statement <- paste0(statement, " condition_status_source_value = '", condition_status_source_value,"'")
    }
  }

  if (!missing(condition_status_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_status_concept_id)) {
      statement <- paste0(statement, ' condition_status_concept_id IS NULL')
    } else if (is(condition_status_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_status_concept_id = (', as.character(condition_status_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_status_concept_id = '", condition_status_concept_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_procedure_occurrence <- function(rowCount, procedure_occurrence_id, visit_occurrence_id, person_id, procedure_concept_id, procedure_date, procedure_datetime, procedure_type_concept_id, modifier_concept_id, quantity, provider_id, procedure_source_value, procedure_source_concept_id, qualifier_source_value, visit_detail_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect procedure_occurrence' AS test, CASE WHEN(SELECT COUNT(*) FROM procedure_occurrence WHERE")
  first <- TRUE
  if (!missing(procedure_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_occurrence_id)) {
      statement <- paste0(statement, ' procedure_occurrence_id IS NULL')
    } else if (is(procedure_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_occurrence_id = (', as.character(procedure_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " procedure_occurrence_id = '", procedure_occurrence_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(procedure_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_concept_id)) {
      statement <- paste0(statement, ' procedure_concept_id IS NULL')
    } else if (is(procedure_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_concept_id = (', as.character(procedure_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_concept_id = '", procedure_concept_id,"'")
    }
  }

  if (!missing(procedure_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_date)) {
      statement <- paste0(statement, ' procedure_date IS NULL')
    } else if (is(procedure_date, 'subQuery')){
      statement <- paste0(statement, ' procedure_date = (', as.character(procedure_date), ')')
    } else {
      statement <- paste0(statement, " procedure_date = '", procedure_date,"'")
    }
  }

  if (!missing(procedure_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_datetime)) {
      statement <- paste0(statement, ' procedure_datetime IS NULL')
    } else if (is(procedure_datetime, 'subQuery')){
      statement <- paste0(statement, ' procedure_datetime = (', as.character(procedure_datetime), ')')
    } else {
      statement <- paste0(statement, " procedure_datetime = '", procedure_datetime,"'")
    }
  }

  if (!missing(procedure_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_type_concept_id)) {
      statement <- paste0(statement, ' procedure_type_concept_id IS NULL')
    } else if (is(procedure_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_type_concept_id = (', as.character(procedure_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_type_concept_id = '", procedure_type_concept_id,"'")
    }
  }

  if (!missing(modifier_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(modifier_concept_id)) {
      statement <- paste0(statement, ' modifier_concept_id IS NULL')
    } else if (is(modifier_concept_id, 'subQuery')){
      statement <- paste0(statement, ' modifier_concept_id = (', as.character(modifier_concept_id), ')')
    } else {
      statement <- paste0(statement, " modifier_concept_id = '", modifier_concept_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(procedure_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_source_value)) {
      statement <- paste0(statement, ' procedure_source_value IS NULL')
    } else if (is(procedure_source_value, 'subQuery')){
      statement <- paste0(statement, ' procedure_source_value = (', as.character(procedure_source_value), ')')
    } else {
      statement <- paste0(statement, " procedure_source_value = '", procedure_source_value,"'")
    }
  }

  if (!missing(procedure_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_source_concept_id)) {
      statement <- paste0(statement, ' procedure_source_concept_id IS NULL')
    } else if (is(procedure_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_source_concept_id = (', as.character(procedure_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_source_concept_id = '", procedure_source_concept_id,"'")
    }
  }

  if (!missing(qualifier_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_source_value)) {
      statement <- paste0(statement, ' qualifier_source_value IS NULL')
    } else if (is(qualifier_source_value, 'subQuery')){
      statement <- paste0(statement, ' qualifier_source_value = (', as.character(qualifier_source_value), ')')
    } else {
      statement <- paste0(statement, " qualifier_source_value = '", qualifier_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_drug_exposure <- function(rowCount, drug_exposure_id, person_id, drug_exposure_start_date, drug_exposure_end_date, days_supply, drug_concept_id, drug_source_value, drug_source_concept_id, provider_id, drug_type_concept_id, quantity, sig, refills, visit_occurrence_id, route_source_value, dose_unit_source_value, visit_detail_id, drug_exposure_start_datetime, drug_exposure_end_datetime, verbatim_end_date, route_concept_id, stop_reason, lot_number) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect drug_exposure' AS test, CASE WHEN(SELECT COUNT(*) FROM drug_exposure WHERE")
  first <- TRUE
  if (!missing(drug_exposure_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_id)) {
      statement <- paste0(statement, ' drug_exposure_id IS NULL')
    } else if (is(drug_exposure_id, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_id = (', as.character(drug_exposure_id), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_id = '", drug_exposure_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_exposure_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_start_date)) {
      statement <- paste0(statement, ' drug_exposure_start_date IS NULL')
    } else if (is(drug_exposure_start_date, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_start_date = (', as.character(drug_exposure_start_date), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_start_date = '", drug_exposure_start_date,"'")
    }
  }

  if (!missing(drug_exposure_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_end_date)) {
      statement <- paste0(statement, ' drug_exposure_end_date IS NULL')
    } else if (is(drug_exposure_end_date, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_end_date = (', as.character(drug_exposure_end_date), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_end_date = '", drug_exposure_end_date,"'")
    }
  }

  if (!missing(days_supply)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(days_supply)) {
      statement <- paste0(statement, ' days_supply IS NULL')
    } else if (is(days_supply, 'subQuery')){
      statement <- paste0(statement, ' days_supply = (', as.character(days_supply), ')')
    } else {
      statement <- paste0(statement, " days_supply = '", days_supply,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(drug_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_source_value)) {
      statement <- paste0(statement, ' drug_source_value IS NULL')
    } else if (is(drug_source_value, 'subQuery')){
      statement <- paste0(statement, ' drug_source_value = (', as.character(drug_source_value), ')')
    } else {
      statement <- paste0(statement, " drug_source_value = '", drug_source_value,"'")
    }
  }

  if (!missing(drug_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_source_concept_id)) {
      statement <- paste0(statement, ' drug_source_concept_id IS NULL')
    } else if (is(drug_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_source_concept_id = (', as.character(drug_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_source_concept_id = '", drug_source_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(drug_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_type_concept_id)) {
      statement <- paste0(statement, ' drug_type_concept_id IS NULL')
    } else if (is(drug_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_type_concept_id = (', as.character(drug_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_type_concept_id = '", drug_type_concept_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(sig)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(sig)) {
      statement <- paste0(statement, ' sig IS NULL')
    } else if (is(sig, 'subQuery')){
      statement <- paste0(statement, ' sig = (', as.character(sig), ')')
    } else {
      statement <- paste0(statement, " sig = '", sig,"'")
    }
  }

  if (!missing(refills)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(refills)) {
      statement <- paste0(statement, ' refills IS NULL')
    } else if (is(refills, 'subQuery')){
      statement <- paste0(statement, ' refills = (', as.character(refills), ')')
    } else {
      statement <- paste0(statement, " refills = '", refills,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(route_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(route_source_value)) {
      statement <- paste0(statement, ' route_source_value IS NULL')
    } else if (is(route_source_value, 'subQuery')){
      statement <- paste0(statement, ' route_source_value = (', as.character(route_source_value), ')')
    } else {
      statement <- paste0(statement, " route_source_value = '", route_source_value,"'")
    }
  }

  if (!missing(dose_unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_unit_source_value)) {
      statement <- paste0(statement, ' dose_unit_source_value IS NULL')
    } else if (is(dose_unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' dose_unit_source_value = (', as.character(dose_unit_source_value), ')')
    } else {
      statement <- paste0(statement, " dose_unit_source_value = '", dose_unit_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(drug_exposure_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_start_datetime)) {
      statement <- paste0(statement, ' drug_exposure_start_datetime IS NULL')
    } else if (is(drug_exposure_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_start_datetime = (', as.character(drug_exposure_start_datetime), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_start_datetime = '", drug_exposure_start_datetime,"'")
    }
  }

  if (!missing(drug_exposure_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_end_datetime)) {
      statement <- paste0(statement, ' drug_exposure_end_datetime IS NULL')
    } else if (is(drug_exposure_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_end_datetime = (', as.character(drug_exposure_end_datetime), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_end_datetime = '", drug_exposure_end_datetime,"'")
    }
  }

  if (!missing(verbatim_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(verbatim_end_date)) {
      statement <- paste0(statement, ' verbatim_end_date IS NULL')
    } else if (is(verbatim_end_date, 'subQuery')){
      statement <- paste0(statement, ' verbatim_end_date = (', as.character(verbatim_end_date), ')')
    } else {
      statement <- paste0(statement, " verbatim_end_date = '", verbatim_end_date,"'")
    }
  }

  if (!missing(route_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(route_concept_id)) {
      statement <- paste0(statement, ' route_concept_id IS NULL')
    } else if (is(route_concept_id, 'subQuery')){
      statement <- paste0(statement, ' route_concept_id = (', as.character(route_concept_id), ')')
    } else {
      statement <- paste0(statement, " route_concept_id = '", route_concept_id,"'")
    }
  }

  if (!missing(stop_reason)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(stop_reason)) {
      statement <- paste0(statement, ' stop_reason IS NULL')
    } else if (is(stop_reason, 'subQuery')){
      statement <- paste0(statement, ' stop_reason = (', as.character(stop_reason), ')')
    } else {
      statement <- paste0(statement, " stop_reason = '", stop_reason,"'")
    }
  }

  if (!missing(lot_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(lot_number)) {
      statement <- paste0(statement, ' lot_number IS NULL')
    } else if (is(lot_number, 'subQuery')){
      statement <- paste0(statement, ' lot_number = (', as.character(lot_number), ')')
    } else {
      statement <- paste0(statement, " lot_number = '", lot_number,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_measurement <- function(rowCount, measurement_id, person_id, measurement_concept_id, measurement_date, measurement_datetime, measurement_type_concept_id, operator_concept_id, value_as_number, value_as_concept_id, unit_concept_id, range_low, range_high, provider_id, visit_occurrence_id, visit_detail_id, measurement_source_value, measurement_source_concept_id, unit_source_value, value_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect measurement' AS test, CASE WHEN(SELECT COUNT(*) FROM measurement WHERE")
  first <- TRUE
  if (!missing(measurement_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_id)) {
      statement <- paste0(statement, ' measurement_id IS NULL')
    } else if (is(measurement_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_id = (', as.character(measurement_id), ')')
    } else {
      statement <- paste0(statement, " measurement_id = '", measurement_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(measurement_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_concept_id)) {
      statement <- paste0(statement, ' measurement_concept_id IS NULL')
    } else if (is(measurement_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_concept_id = (', as.character(measurement_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_concept_id = '", measurement_concept_id,"'")
    }
  }

  if (!missing(measurement_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_date)) {
      statement <- paste0(statement, ' measurement_date IS NULL')
    } else if (is(measurement_date, 'subQuery')){
      statement <- paste0(statement, ' measurement_date = (', as.character(measurement_date), ')')
    } else {
      statement <- paste0(statement, " measurement_date = '", measurement_date,"'")
    }
  }

  if (!missing(measurement_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_datetime)) {
      statement <- paste0(statement, ' measurement_datetime IS NULL')
    } else if (is(measurement_datetime, 'subQuery')){
      statement <- paste0(statement, ' measurement_datetime = (', as.character(measurement_datetime), ')')
    } else {
      statement <- paste0(statement, " measurement_datetime = '", measurement_datetime,"'")
    }
  }

  if (!missing(measurement_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_type_concept_id)) {
      statement <- paste0(statement, ' measurement_type_concept_id IS NULL')
    } else if (is(measurement_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_type_concept_id = (', as.character(measurement_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_type_concept_id = '", measurement_type_concept_id,"'")
    }
  }

  if (!missing(operator_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(operator_concept_id)) {
      statement <- paste0(statement, ' operator_concept_id IS NULL')
    } else if (is(operator_concept_id, 'subQuery')){
      statement <- paste0(statement, ' operator_concept_id = (', as.character(operator_concept_id), ')')
    } else {
      statement <- paste0(statement, " operator_concept_id = '", operator_concept_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(range_low)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(range_low)) {
      statement <- paste0(statement, ' range_low IS NULL')
    } else if (is(range_low, 'subQuery')){
      statement <- paste0(statement, ' range_low = (', as.character(range_low), ')')
    } else {
      statement <- paste0(statement, " range_low = '", range_low,"'")
    }
  }

  if (!missing(range_high)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(range_high)) {
      statement <- paste0(statement, ' range_high IS NULL')
    } else if (is(range_high, 'subQuery')){
      statement <- paste0(statement, ' range_high = (', as.character(range_high), ')')
    } else {
      statement <- paste0(statement, " range_high = '", range_high,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(measurement_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_source_value)) {
      statement <- paste0(statement, ' measurement_source_value IS NULL')
    } else if (is(measurement_source_value, 'subQuery')){
      statement <- paste0(statement, ' measurement_source_value = (', as.character(measurement_source_value), ')')
    } else {
      statement <- paste0(statement, " measurement_source_value = '", measurement_source_value,"'")
    }
  }

  if (!missing(measurement_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_source_concept_id)) {
      statement <- paste0(statement, ' measurement_source_concept_id IS NULL')
    } else if (is(measurement_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_source_concept_id = (', as.character(measurement_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_source_concept_id = '", measurement_source_concept_id,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(value_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_source_value)) {
      statement <- paste0(statement, ' value_source_value IS NULL')
    } else if (is(value_source_value, 'subQuery')){
      statement <- paste0(statement, ' value_source_value = (', as.character(value_source_value), ')')
    } else {
      statement <- paste0(statement, " value_source_value = '", value_source_value,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_observation <- function(rowCount, observation_id, person_id, visit_occurrence_id, observation_date, observation_concept_id, observation_source_value, provider_id, observation_type_concept_id, value_as_number, value_as_string, qualifier_concept_id, qualifier_source_value, unit_concept_id, unit_source_value, visit_detail_id, observation_source_concept_id, observation_datetime, value_as_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect observation' AS test, CASE WHEN(SELECT COUNT(*) FROM observation WHERE")
  first <- TRUE
  if (!missing(observation_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_id)) {
      statement <- paste0(statement, ' observation_id IS NULL')
    } else if (is(observation_id, 'subQuery')){
      statement <- paste0(statement, ' observation_id = (', as.character(observation_id), ')')
    } else {
      statement <- paste0(statement, " observation_id = '", observation_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(observation_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_date)) {
      statement <- paste0(statement, ' observation_date IS NULL')
    } else if (is(observation_date, 'subQuery')){
      statement <- paste0(statement, ' observation_date = (', as.character(observation_date), ')')
    } else {
      statement <- paste0(statement, " observation_date = '", observation_date,"'")
    }
  }

  if (!missing(observation_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_concept_id)) {
      statement <- paste0(statement, ' observation_concept_id IS NULL')
    } else if (is(observation_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_concept_id = (', as.character(observation_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_concept_id = '", observation_concept_id,"'")
    }
  }

  if (!missing(observation_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_source_value)) {
      statement <- paste0(statement, ' observation_source_value IS NULL')
    } else if (is(observation_source_value, 'subQuery')){
      statement <- paste0(statement, ' observation_source_value = (', as.character(observation_source_value), ')')
    } else {
      statement <- paste0(statement, " observation_source_value = '", observation_source_value,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(observation_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_type_concept_id)) {
      statement <- paste0(statement, ' observation_type_concept_id IS NULL')
    } else if (is(observation_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_type_concept_id = (', as.character(observation_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_type_concept_id = '", observation_type_concept_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_string)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_string)) {
      statement <- paste0(statement, ' value_as_string IS NULL')
    } else if (is(value_as_string, 'subQuery')){
      statement <- paste0(statement, ' value_as_string = (', as.character(value_as_string), ')')
    } else {
      statement <- paste0(statement, " value_as_string = '", value_as_string,"'")
    }
  }

  if (!missing(qualifier_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_concept_id)) {
      statement <- paste0(statement, ' qualifier_concept_id IS NULL')
    } else if (is(qualifier_concept_id, 'subQuery')){
      statement <- paste0(statement, ' qualifier_concept_id = (', as.character(qualifier_concept_id), ')')
    } else {
      statement <- paste0(statement, " qualifier_concept_id = '", qualifier_concept_id,"'")
    }
  }

  if (!missing(qualifier_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_source_value)) {
      statement <- paste0(statement, ' qualifier_source_value IS NULL')
    } else if (is(qualifier_source_value, 'subQuery')){
      statement <- paste0(statement, ' qualifier_source_value = (', as.character(qualifier_source_value), ')')
    } else {
      statement <- paste0(statement, " qualifier_source_value = '", qualifier_source_value,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(observation_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_source_concept_id)) {
      statement <- paste0(statement, ' observation_source_concept_id IS NULL')
    } else if (is(observation_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_source_concept_id = (', as.character(observation_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_source_concept_id = '", observation_source_concept_id,"'")
    }
  }

  if (!missing(observation_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_datetime)) {
      statement <- paste0(statement, ' observation_datetime IS NULL')
    } else if (is(observation_datetime, 'subQuery')){
      statement <- paste0(statement, ' observation_datetime = (', as.character(observation_datetime), ')')
    } else {
      statement <- paste0(statement, " observation_datetime = '", observation_datetime,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_device_exposure <- function(rowCount, device_exposure_id, person_id, device_exposure_start_date, device_concept_id, device_exposure_start_datetime, device_exposure_end_date, device_exposure_end_datetime, device_type_concept_id, unique_device_id, quantity, provider_id, visit_occurrence_id, device_source_value, device_source__concept_id, visit_detail_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect device_exposure' AS test, CASE WHEN(SELECT COUNT(*) FROM device_exposure WHERE")
  first <- TRUE
  if (!missing(device_exposure_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_id)) {
      statement <- paste0(statement, ' device_exposure_id IS NULL')
    } else if (is(device_exposure_id, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_id = (', as.character(device_exposure_id), ')')
    } else {
      statement <- paste0(statement, " device_exposure_id = '", device_exposure_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(device_exposure_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_start_date)) {
      statement <- paste0(statement, ' device_exposure_start_date IS NULL')
    } else if (is(device_exposure_start_date, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_start_date = (', as.character(device_exposure_start_date), ')')
    } else {
      statement <- paste0(statement, " device_exposure_start_date = '", device_exposure_start_date,"'")
    }
  }

  if (!missing(device_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_concept_id)) {
      statement <- paste0(statement, ' device_concept_id IS NULL')
    } else if (is(device_concept_id, 'subQuery')){
      statement <- paste0(statement, ' device_concept_id = (', as.character(device_concept_id), ')')
    } else {
      statement <- paste0(statement, " device_concept_id = '", device_concept_id,"'")
    }
  }

  if (!missing(device_exposure_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_start_datetime)) {
      statement <- paste0(statement, ' device_exposure_start_datetime IS NULL')
    } else if (is(device_exposure_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_start_datetime = (', as.character(device_exposure_start_datetime), ')')
    } else {
      statement <- paste0(statement, " device_exposure_start_datetime = '", device_exposure_start_datetime,"'")
    }
  }

  if (!missing(device_exposure_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_end_date)) {
      statement <- paste0(statement, ' device_exposure_end_date IS NULL')
    } else if (is(device_exposure_end_date, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_end_date = (', as.character(device_exposure_end_date), ')')
    } else {
      statement <- paste0(statement, " device_exposure_end_date = '", device_exposure_end_date,"'")
    }
  }

  if (!missing(device_exposure_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_end_datetime)) {
      statement <- paste0(statement, ' device_exposure_end_datetime IS NULL')
    } else if (is(device_exposure_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_end_datetime = (', as.character(device_exposure_end_datetime), ')')
    } else {
      statement <- paste0(statement, " device_exposure_end_datetime = '", device_exposure_end_datetime,"'")
    }
  }

  if (!missing(device_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_type_concept_id)) {
      statement <- paste0(statement, ' device_type_concept_id IS NULL')
    } else if (is(device_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' device_type_concept_id = (', as.character(device_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " device_type_concept_id = '", device_type_concept_id,"'")
    }
  }

  if (!missing(unique_device_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unique_device_id)) {
      statement <- paste0(statement, ' unique_device_id IS NULL')
    } else if (is(unique_device_id, 'subQuery')){
      statement <- paste0(statement, ' unique_device_id = (', as.character(unique_device_id), ')')
    } else {
      statement <- paste0(statement, " unique_device_id = '", unique_device_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(device_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_source_value)) {
      statement <- paste0(statement, ' device_source_value IS NULL')
    } else if (is(device_source_value, 'subQuery')){
      statement <- paste0(statement, ' device_source_value = (', as.character(device_source_value), ')')
    } else {
      statement <- paste0(statement, " device_source_value = '", device_source_value,"'")
    }
  }

  if (!missing(device_source__concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_source__concept_id)) {
      statement <- paste0(statement, ' [device_source_ concept_id] IS NULL')
    } else if (is(device_source__concept_id, 'subQuery')){
      statement <- paste0(statement, ' [device_source_ concept_id] = (', as.character(device_source__concept_id), ')')
    } else {
      statement <- paste0(statement, " [device_source_ concept_id] = '", device_source__concept_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_death <- function(rowCount, person_id, death_date, death_datetime, death_type_concept_id, cause_concept_id, cause_source_value, cause_source_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect death' AS test, CASE WHEN(SELECT COUNT(*) FROM death WHERE")
  first <- TRUE
  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(death_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_date)) {
      statement <- paste0(statement, ' death_date IS NULL')
    } else if (is(death_date, 'subQuery')){
      statement <- paste0(statement, ' death_date = (', as.character(death_date), ')')
    } else {
      statement <- paste0(statement, " death_date = '", death_date,"'")
    }
  }

  if (!missing(death_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_datetime)) {
      statement <- paste0(statement, ' death_datetime IS NULL')
    } else if (is(death_datetime, 'subQuery')){
      statement <- paste0(statement, ' death_datetime = (', as.character(death_datetime), ')')
    } else {
      statement <- paste0(statement, " death_datetime = '", death_datetime,"'")
    }
  }

  if (!missing(death_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_type_concept_id)) {
      statement <- paste0(statement, ' death_type_concept_id IS NULL')
    } else if (is(death_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' death_type_concept_id = (', as.character(death_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " death_type_concept_id = '", death_type_concept_id,"'")
    }
  }

  if (!missing(cause_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_concept_id)) {
      statement <- paste0(statement, ' cause_concept_id IS NULL')
    } else if (is(cause_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cause_concept_id = (', as.character(cause_concept_id), ')')
    } else {
      statement <- paste0(statement, " cause_concept_id = '", cause_concept_id,"'")
    }
  }

  if (!missing(cause_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_source_value)) {
      statement <- paste0(statement, ' cause_source_value IS NULL')
    } else if (is(cause_source_value, 'subQuery')){
      statement <- paste0(statement, ' cause_source_value = (', as.character(cause_source_value), ')')
    } else {
      statement <- paste0(statement, " cause_source_value = '", cause_source_value,"'")
    }
  }

  if (!missing(cause_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_source_concept_id)) {
      statement <- paste0(statement, ' cause_source_concept_id IS NULL')
    } else if (is(cause_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cause_source_concept_id = (', as.character(cause_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " cause_source_concept_id = '", cause_source_concept_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_location <- function(rowCount, location_id, address_1, address_2, city, state, zip, county, location_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect location' AS test, CASE WHEN(SELECT COUNT(*) FROM location WHERE")
  first <- TRUE
  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(address_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(address_1)) {
      statement <- paste0(statement, ' address_1 IS NULL')
    } else if (is(address_1, 'subQuery')){
      statement <- paste0(statement, ' address_1 = (', as.character(address_1), ')')
    } else {
      statement <- paste0(statement, " address_1 = '", address_1,"'")
    }
  }

  if (!missing(address_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(address_2)) {
      statement <- paste0(statement, ' address_2 IS NULL')
    } else if (is(address_2, 'subQuery')){
      statement <- paste0(statement, ' address_2 = (', as.character(address_2), ')')
    } else {
      statement <- paste0(statement, " address_2 = '", address_2,"'")
    }
  }

  if (!missing(city)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(city)) {
      statement <- paste0(statement, ' city IS NULL')
    } else if (is(city, 'subQuery')){
      statement <- paste0(statement, ' city = (', as.character(city), ')')
    } else {
      statement <- paste0(statement, " city = '", city,"'")
    }
  }

  if (!missing(state)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(state)) {
      statement <- paste0(statement, ' state IS NULL')
    } else if (is(state, 'subQuery')){
      statement <- paste0(statement, ' state = (', as.character(state), ')')
    } else {
      statement <- paste0(statement, " state = '", state,"'")
    }
  }

  if (!missing(zip)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(zip)) {
      statement <- paste0(statement, ' zip IS NULL')
    } else if (is(zip, 'subQuery')){
      statement <- paste0(statement, ' zip = (', as.character(zip), ')')
    } else {
      statement <- paste0(statement, " zip = '", zip,"'")
    }
  }

  if (!missing(county)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(county)) {
      statement <- paste0(statement, ' county IS NULL')
    } else if (is(county, 'subQuery')){
      statement <- paste0(statement, ' county = (', as.character(county), ')')
    } else {
      statement <- paste0(statement, " county = '", county,"'")
    }
  }

  if (!missing(location_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_source_value)) {
      statement <- paste0(statement, ' location_source_value IS NULL')
    } else if (is(location_source_value, 'subQuery')){
      statement <- paste0(statement, ' location_source_value = (', as.character(location_source_value), ')')
    } else {
      statement <- paste0(statement, " location_source_value = '", location_source_value,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_care_site <- function(rowCount, care_site_id, care_site_source_value, location_id, care_site_name, place_of_service_concept_id, place_of_service_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect care_site' AS test, CASE WHEN(SELECT COUNT(*) FROM care_site WHERE")
  first <- TRUE
  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(care_site_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_source_value)) {
      statement <- paste0(statement, ' care_site_source_value IS NULL')
    } else if (is(care_site_source_value, 'subQuery')){
      statement <- paste0(statement, ' care_site_source_value = (', as.character(care_site_source_value), ')')
    } else {
      statement <- paste0(statement, " care_site_source_value = '", care_site_source_value,"'")
    }
  }

  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(care_site_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_name)) {
      statement <- paste0(statement, ' care_site_name IS NULL')
    } else if (is(care_site_name, 'subQuery')){
      statement <- paste0(statement, ' care_site_name = (', as.character(care_site_name), ')')
    } else {
      statement <- paste0(statement, " care_site_name = '", care_site_name,"'")
    }
  }

  if (!missing(place_of_service_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(place_of_service_concept_id)) {
      statement <- paste0(statement, ' place_of_service_concept_id IS NULL')
    } else if (is(place_of_service_concept_id, 'subQuery')){
      statement <- paste0(statement, ' place_of_service_concept_id = (', as.character(place_of_service_concept_id), ')')
    } else {
      statement <- paste0(statement, " place_of_service_concept_id = '", place_of_service_concept_id,"'")
    }
  }

  if (!missing(place_of_service_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(place_of_service_source_value)) {
      statement <- paste0(statement, ' place_of_service_source_value IS NULL')
    } else if (is(place_of_service_source_value, 'subQuery')){
      statement <- paste0(statement, ' place_of_service_source_value = (', as.character(place_of_service_source_value), ')')
    } else {
      statement <- paste0(statement, " place_of_service_source_value = '", place_of_service_source_value,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_provider <- function(rowCount, provider_id, specialty_concept_id, specialty_source_value, provider_name, npi, dea, care_site_id, year_of_birth, gender_concept_id, provider_source_value, specialty_source_concept_id, gender_source_value, gender_source_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect provider' AS test, CASE WHEN(SELECT COUNT(*) FROM provider WHERE")
  first <- TRUE
  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(specialty_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_concept_id)) {
      statement <- paste0(statement, ' specialty_concept_id IS NULL')
    } else if (is(specialty_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specialty_concept_id = (', as.character(specialty_concept_id), ')')
    } else {
      statement <- paste0(statement, " specialty_concept_id = '", specialty_concept_id,"'")
    }
  }

  if (!missing(specialty_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_source_value)) {
      statement <- paste0(statement, ' specialty_source_value IS NULL')
    } else if (is(specialty_source_value, 'subQuery')){
      statement <- paste0(statement, ' specialty_source_value = (', as.character(specialty_source_value), ')')
    } else {
      statement <- paste0(statement, " specialty_source_value = '", specialty_source_value,"'")
    }
  }

  if (!missing(provider_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_name)) {
      statement <- paste0(statement, ' provider_name IS NULL')
    } else if (is(provider_name, 'subQuery')){
      statement <- paste0(statement, ' provider_name = (', as.character(provider_name), ')')
    } else {
      statement <- paste0(statement, " provider_name = '", provider_name,"'")
    }
  }

  if (!missing(npi)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(npi)) {
      statement <- paste0(statement, ' npi IS NULL')
    } else if (is(npi, 'subQuery')){
      statement <- paste0(statement, ' npi = (', as.character(npi), ')')
    } else {
      statement <- paste0(statement, " npi = '", npi,"'")
    }
  }

  if (!missing(dea)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dea)) {
      statement <- paste0(statement, ' dea IS NULL')
    } else if (is(dea, 'subQuery')){
      statement <- paste0(statement, ' dea = (', as.character(dea), ')')
    } else {
      statement <- paste0(statement, " dea = '", dea,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(year_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(year_of_birth)) {
      statement <- paste0(statement, ' year_of_birth IS NULL')
    } else if (is(year_of_birth, 'subQuery')){
      statement <- paste0(statement, ' year_of_birth = (', as.character(year_of_birth), ')')
    } else {
      statement <- paste0(statement, " year_of_birth = '", year_of_birth,"'")
    }
  }

  if (!missing(gender_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_concept_id)) {
      statement <- paste0(statement, ' gender_concept_id IS NULL')
    } else if (is(gender_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_concept_id = (', as.character(gender_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_concept_id = '", gender_concept_id,"'")
    }
  }

  if (!missing(provider_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_source_value)) {
      statement <- paste0(statement, ' provider_source_value IS NULL')
    } else if (is(provider_source_value, 'subQuery')){
      statement <- paste0(statement, ' provider_source_value = (', as.character(provider_source_value), ')')
    } else {
      statement <- paste0(statement, " provider_source_value = '", provider_source_value,"'")
    }
  }

  if (!missing(specialty_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_source_concept_id)) {
      statement <- paste0(statement, ' specialty_source_concept_id IS NULL')
    } else if (is(specialty_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specialty_source_concept_id = (', as.character(specialty_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " specialty_source_concept_id = '", specialty_source_concept_id,"'")
    }
  }

  if (!missing(gender_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_value)) {
      statement <- paste0(statement, ' gender_source_value IS NULL')
    } else if (is(gender_source_value, 'subQuery')){
      statement <- paste0(statement, ' gender_source_value = (', as.character(gender_source_value), ')')
    } else {
      statement <- paste0(statement, " gender_source_value = '", gender_source_value,"'")
    }
  }

  if (!missing(gender_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_concept_id)) {
      statement <- paste0(statement, ' gender_source_concept_id IS NULL')
    } else if (is(gender_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_source_concept_id = (', as.character(gender_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_source_concept_id = '", gender_source_concept_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_note <- function(rowCount, note_id, person_id, note_date, note_datetime, note_type_concept_id, note_class_concept_id, note_title, note_text, encoding_concept_id, language_concept_id, provider_id, note_source_value, visit_occurrence_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect note' AS test, CASE WHEN(SELECT COUNT(*) FROM note WHERE")
  first <- TRUE
  if (!missing(note_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_id)) {
      statement <- paste0(statement, ' note_id IS NULL')
    } else if (is(note_id, 'subQuery')){
      statement <- paste0(statement, ' note_id = (', as.character(note_id), ')')
    } else {
      statement <- paste0(statement, " note_id = '", note_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(note_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_date)) {
      statement <- paste0(statement, ' note_date IS NULL')
    } else if (is(note_date, 'subQuery')){
      statement <- paste0(statement, ' note_date = (', as.character(note_date), ')')
    } else {
      statement <- paste0(statement, " note_date = '", note_date,"'")
    }
  }

  if (!missing(note_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_datetime)) {
      statement <- paste0(statement, ' note_datetime IS NULL')
    } else if (is(note_datetime, 'subQuery')){
      statement <- paste0(statement, ' note_datetime = (', as.character(note_datetime), ')')
    } else {
      statement <- paste0(statement, " note_datetime = '", note_datetime,"'")
    }
  }

  if (!missing(note_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_type_concept_id)) {
      statement <- paste0(statement, ' note_type_concept_id IS NULL')
    } else if (is(note_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_type_concept_id = (', as.character(note_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_type_concept_id = '", note_type_concept_id,"'")
    }
  }

  if (!missing(note_class_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_class_concept_id)) {
      statement <- paste0(statement, ' note_class_concept_id IS NULL')
    } else if (is(note_class_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_class_concept_id = (', as.character(note_class_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_class_concept_id = '", note_class_concept_id,"'")
    }
  }

  if (!missing(note_title)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_title)) {
      statement <- paste0(statement, ' note_title IS NULL')
    } else if (is(note_title, 'subQuery')){
      statement <- paste0(statement, ' note_title = (', as.character(note_title), ')')
    } else {
      statement <- paste0(statement, " note_title = '", note_title,"'")
    }
  }

  if (!missing(note_text)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_text)) {
      statement <- paste0(statement, ' note_text IS NULL')
    } else if (is(note_text, 'subQuery')){
      statement <- paste0(statement, ' note_text = (', as.character(note_text), ')')
    } else {
      statement <- paste0(statement, " note_text = '", note_text,"'")
    }
  }

  if (!missing(encoding_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(encoding_concept_id)) {
      statement <- paste0(statement, ' encoding_concept_id IS NULL')
    } else if (is(encoding_concept_id, 'subQuery')){
      statement <- paste0(statement, ' encoding_concept_id = (', as.character(encoding_concept_id), ')')
    } else {
      statement <- paste0(statement, " encoding_concept_id = '", encoding_concept_id,"'")
    }
  }

  if (!missing(language_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(language_concept_id)) {
      statement <- paste0(statement, ' language_concept_id IS NULL')
    } else if (is(language_concept_id, 'subQuery')){
      statement <- paste0(statement, ' language_concept_id = (', as.character(language_concept_id), ')')
    } else {
      statement <- paste0(statement, " language_concept_id = '", language_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(note_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_source_value)) {
      statement <- paste0(statement, ' note_source_value IS NULL')
    } else if (is(note_source_value, 'subQuery')){
      statement <- paste0(statement, ' note_source_value = (', as.character(note_source_value), ')')
    } else {
      statement <- paste0(statement, " note_source_value = '", note_source_value,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_fact_relationship <- function(rowCount, domain_concept_id_1, fact_id_1, domain_concept_id_2, fact_id_2, relationship_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect fact_relationship' AS test, CASE WHEN(SELECT COUNT(*) FROM fact_relationship WHERE")
  first <- TRUE
  if (!missing(domain_concept_id_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(domain_concept_id_1)) {
      statement <- paste0(statement, ' domain_concept_id_1 IS NULL')
    } else if (is(domain_concept_id_1, 'subQuery')){
      statement <- paste0(statement, ' domain_concept_id_1 = (', as.character(domain_concept_id_1), ')')
    } else {
      statement <- paste0(statement, " domain_concept_id_1 = '", domain_concept_id_1,"'")
    }
  }

  if (!missing(fact_id_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(fact_id_1)) {
      statement <- paste0(statement, ' fact_id_1 IS NULL')
    } else if (is(fact_id_1, 'subQuery')){
      statement <- paste0(statement, ' fact_id_1 = (', as.character(fact_id_1), ')')
    } else {
      statement <- paste0(statement, " fact_id_1 = '", fact_id_1,"'")
    }
  }

  if (!missing(domain_concept_id_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(domain_concept_id_2)) {
      statement <- paste0(statement, ' domain_concept_id_2 IS NULL')
    } else if (is(domain_concept_id_2, 'subQuery')){
      statement <- paste0(statement, ' domain_concept_id_2 = (', as.character(domain_concept_id_2), ')')
    } else {
      statement <- paste0(statement, " domain_concept_id_2 = '", domain_concept_id_2,"'")
    }
  }

  if (!missing(fact_id_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(fact_id_2)) {
      statement <- paste0(statement, ' fact_id_2 IS NULL')
    } else if (is(fact_id_2, 'subQuery')){
      statement <- paste0(statement, ' fact_id_2 = (', as.character(fact_id_2), ')')
    } else {
      statement <- paste0(statement, " fact_id_2 = '", fact_id_2,"'")
    }
  }

  if (!missing(relationship_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(relationship_concept_id)) {
      statement <- paste0(statement, ' relationship_concept_id IS NULL')
    } else if (is(relationship_concept_id, 'subQuery')){
      statement <- paste0(statement, ' relationship_concept_id = (', as.character(relationship_concept_id), ')')
    } else {
      statement <- paste0(statement, " relationship_concept_id = '", relationship_concept_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_note_nlp <- function(rowCount, note_nlp_id, note_id, section_concept_id, snippet, offset, lexical_variant, note_nlp_concept_id, note_nlp_source_concept_id, nlp_system, nlp_date, nlp_date_time, term_exists, term_temporal, term_modifiers) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect note_nlp' AS test, CASE WHEN(SELECT COUNT(*) FROM note_nlp WHERE")
  first <- TRUE
  if (!missing(note_nlp_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_id)) {
      statement <- paste0(statement, ' note_nlp_id IS NULL')
    } else if (is(note_nlp_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_id = (', as.character(note_nlp_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_id = '", note_nlp_id,"'")
    }
  }

  if (!missing(note_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_id)) {
      statement <- paste0(statement, ' note_id IS NULL')
    } else if (is(note_id, 'subQuery')){
      statement <- paste0(statement, ' note_id = (', as.character(note_id), ')')
    } else {
      statement <- paste0(statement, " note_id = '", note_id,"'")
    }
  }

  if (!missing(section_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(section_concept_id)) {
      statement <- paste0(statement, ' section_concept_id IS NULL')
    } else if (is(section_concept_id, 'subQuery')){
      statement <- paste0(statement, ' section_concept_id = (', as.character(section_concept_id), ')')
    } else {
      statement <- paste0(statement, " section_concept_id = '", section_concept_id,"'")
    }
  }

  if (!missing(snippet)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(snippet)) {
      statement <- paste0(statement, ' snippet IS NULL')
    } else if (is(snippet, 'subQuery')){
      statement <- paste0(statement, ' snippet = (', as.character(snippet), ')')
    } else {
      statement <- paste0(statement, " snippet = '", snippet,"'")
    }
  }

  if (!missing(offset)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(offset)) {
      statement <- paste0(statement, ' offset IS NULL')
    } else if (is(offset, 'subQuery')){
      statement <- paste0(statement, ' offset = (', as.character(offset), ')')
    } else {
      statement <- paste0(statement, " offset = '", offset,"'")
    }
  }

  if (!missing(lexical_variant)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(lexical_variant)) {
      statement <- paste0(statement, ' lexical_variant IS NULL')
    } else if (is(lexical_variant, 'subQuery')){
      statement <- paste0(statement, ' lexical_variant = (', as.character(lexical_variant), ')')
    } else {
      statement <- paste0(statement, " lexical_variant = '", lexical_variant,"'")
    }
  }

  if (!missing(note_nlp_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_concept_id)) {
      statement <- paste0(statement, ' note_nlp_concept_id IS NULL')
    } else if (is(note_nlp_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_concept_id = (', as.character(note_nlp_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_concept_id = '", note_nlp_concept_id,"'")
    }
  }

  if (!missing(note_nlp_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_source_concept_id)) {
      statement <- paste0(statement, ' note_nlp_source_concept_id IS NULL')
    } else if (is(note_nlp_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_source_concept_id = (', as.character(note_nlp_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_source_concept_id = '", note_nlp_source_concept_id,"'")
    }
  }

  if (!missing(nlp_system)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_system)) {
      statement <- paste0(statement, ' nlp_system IS NULL')
    } else if (is(nlp_system, 'subQuery')){
      statement <- paste0(statement, ' nlp_system = (', as.character(nlp_system), ')')
    } else {
      statement <- paste0(statement, " nlp_system = '", nlp_system,"'")
    }
  }

  if (!missing(nlp_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_date)) {
      statement <- paste0(statement, ' nlp_date IS NULL')
    } else if (is(nlp_date, 'subQuery')){
      statement <- paste0(statement, ' nlp_date = (', as.character(nlp_date), ')')
    } else {
      statement <- paste0(statement, " nlp_date = '", nlp_date,"'")
    }
  }

  if (!missing(nlp_date_time)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_date_time)) {
      statement <- paste0(statement, ' nlp_date_time IS NULL')
    } else if (is(nlp_date_time, 'subQuery')){
      statement <- paste0(statement, ' nlp_date_time = (', as.character(nlp_date_time), ')')
    } else {
      statement <- paste0(statement, " nlp_date_time = '", nlp_date_time,"'")
    }
  }

  if (!missing(term_exists)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_exists)) {
      statement <- paste0(statement, ' term_exists IS NULL')
    } else if (is(term_exists, 'subQuery')){
      statement <- paste0(statement, ' term_exists = (', as.character(term_exists), ')')
    } else {
      statement <- paste0(statement, " term_exists = '", term_exists,"'")
    }
  }

  if (!missing(term_temporal)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_temporal)) {
      statement <- paste0(statement, ' term_temporal IS NULL')
    } else if (is(term_temporal, 'subQuery')){
      statement <- paste0(statement, ' term_temporal = (', as.character(term_temporal), ')')
    } else {
      statement <- paste0(statement, " term_temporal = '", term_temporal,"'")
    }
  }

  if (!missing(term_modifiers)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_modifiers)) {
      statement <- paste0(statement, ' term_modifiers IS NULL')
    } else if (is(term_modifiers, 'subQuery')){
      statement <- paste0(statement, ' term_modifiers = (', as.character(term_modifiers), ')')
    } else {
      statement <- paste0(statement, " term_modifiers = '", term_modifiers,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_specimen <- function(rowCount, specimen_id, person_id, specimen_concept_id, specimen_type_concept_id, specimen_date, specimen_datetime, quantity, unit_concept_id, anatomic_site_concept_id, disease_status_concept_id, specimen_source_id, specimen_source_value, unit_source_value, anatomic_site_source_value, disease_status_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect specimen' AS test, CASE WHEN(SELECT COUNT(*) FROM specimen WHERE")
  first <- TRUE
  if (!missing(specimen_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_id)) {
      statement <- paste0(statement, ' specimen_id IS NULL')
    } else if (is(specimen_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_id = (', as.character(specimen_id), ')')
    } else {
      statement <- paste0(statement, " specimen_id = '", specimen_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(specimen_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_concept_id)) {
      statement <- paste0(statement, ' specimen_concept_id IS NULL')
    } else if (is(specimen_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_concept_id = (', as.character(specimen_concept_id), ')')
    } else {
      statement <- paste0(statement, " specimen_concept_id = '", specimen_concept_id,"'")
    }
  }

  if (!missing(specimen_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_type_concept_id)) {
      statement <- paste0(statement, ' specimen_type_concept_id IS NULL')
    } else if (is(specimen_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_type_concept_id = (', as.character(specimen_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " specimen_type_concept_id = '", specimen_type_concept_id,"'")
    }
  }

  if (!missing(specimen_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_date)) {
      statement <- paste0(statement, ' specimen_date IS NULL')
    } else if (is(specimen_date, 'subQuery')){
      statement <- paste0(statement, ' specimen_date = (', as.character(specimen_date), ')')
    } else {
      statement <- paste0(statement, " specimen_date = '", specimen_date,"'")
    }
  }

  if (!missing(specimen_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_datetime)) {
      statement <- paste0(statement, ' specimen_datetime IS NULL')
    } else if (is(specimen_datetime, 'subQuery')){
      statement <- paste0(statement, ' specimen_datetime = (', as.character(specimen_datetime), ')')
    } else {
      statement <- paste0(statement, " specimen_datetime = '", specimen_datetime,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(anatomic_site_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(anatomic_site_concept_id)) {
      statement <- paste0(statement, ' anatomic_site_concept_id IS NULL')
    } else if (is(anatomic_site_concept_id, 'subQuery')){
      statement <- paste0(statement, ' anatomic_site_concept_id = (', as.character(anatomic_site_concept_id), ')')
    } else {
      statement <- paste0(statement, " anatomic_site_concept_id = '", anatomic_site_concept_id,"'")
    }
  }

  if (!missing(disease_status_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(disease_status_concept_id)) {
      statement <- paste0(statement, ' disease_status_concept_id IS NULL')
    } else if (is(disease_status_concept_id, 'subQuery')){
      statement <- paste0(statement, ' disease_status_concept_id = (', as.character(disease_status_concept_id), ')')
    } else {
      statement <- paste0(statement, " disease_status_concept_id = '", disease_status_concept_id,"'")
    }
  }

  if (!missing(specimen_source_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_source_id)) {
      statement <- paste0(statement, ' specimen_source_id IS NULL')
    } else if (is(specimen_source_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_source_id = (', as.character(specimen_source_id), ')')
    } else {
      statement <- paste0(statement, " specimen_source_id = '", specimen_source_id,"'")
    }
  }

  if (!missing(specimen_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_source_value)) {
      statement <- paste0(statement, ' specimen_source_value IS NULL')
    } else if (is(specimen_source_value, 'subQuery')){
      statement <- paste0(statement, ' specimen_source_value = (', as.character(specimen_source_value), ')')
    } else {
      statement <- paste0(statement, " specimen_source_value = '", specimen_source_value,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(anatomic_site_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(anatomic_site_source_value)) {
      statement <- paste0(statement, ' anatomic_site_source_value IS NULL')
    } else if (is(anatomic_site_source_value, 'subQuery')){
      statement <- paste0(statement, ' anatomic_site_source_value = (', as.character(anatomic_site_source_value), ')')
    } else {
      statement <- paste0(statement, " anatomic_site_source_value = '", anatomic_site_source_value,"'")
    }
  }

  if (!missing(disease_status_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(disease_status_source_value)) {
      statement <- paste0(statement, ' disease_status_source_value IS NULL')
    } else if (is(disease_status_source_value, 'subQuery')){
      statement <- paste0(statement, ' disease_status_source_value = (', as.character(disease_status_source_value), ')')
    } else {
      statement <- paste0(statement, " disease_status_source_value = '", disease_status_source_value,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_cohort <- function(rowCount, cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cohort' AS test, CASE WHEN(SELECT COUNT(*) FROM cohort WHERE")
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(subject_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_id)) {
      statement <- paste0(statement, ' subject_id IS NULL')
    } else if (is(subject_id, 'subQuery')){
      statement <- paste0(statement, ' subject_id = (', as.character(subject_id), ')')
    } else {
      statement <- paste0(statement, " subject_id = '", subject_id,"'")
    }
  }

  if (!missing(cohort_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_start_date)) {
      statement <- paste0(statement, ' cohort_start_date IS NULL')
    } else if (is(cohort_start_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_start_date = (', as.character(cohort_start_date), ')')
    } else {
      statement <- paste0(statement, " cohort_start_date = '", cohort_start_date,"'")
    }
  }

  if (!missing(cohort_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_end_date)) {
      statement <- paste0(statement, ' cohort_end_date IS NULL')
    } else if (is(cohort_end_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_end_date = (', as.character(cohort_end_date), ')')
    } else {
      statement <- paste0(statement, " cohort_end_date = '", cohort_end_date,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_cohort_attribute <- function(rowCount, cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, attribute_definition_id, value_as_number, value_as_concept_id) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cohort_attribute' AS test, CASE WHEN(SELECT COUNT(*) FROM cohort_attribute WHERE")
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(subject_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_id)) {
      statement <- paste0(statement, ' subject_id IS NULL')
    } else if (is(subject_id, 'subQuery')){
      statement <- paste0(statement, ' subject_id = (', as.character(subject_id), ')')
    } else {
      statement <- paste0(statement, " subject_id = '", subject_id,"'")
    }
  }

  if (!missing(cohort_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_start_date)) {
      statement <- paste0(statement, ' cohort_start_date IS NULL')
    } else if (is(cohort_start_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_start_date = (', as.character(cohort_start_date), ')')
    } else {
      statement <- paste0(statement, " cohort_start_date = '", cohort_start_date,"'")
    }
  }

  if (!missing(cohort_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_end_date)) {
      statement <- paste0(statement, ' cohort_end_date IS NULL')
    } else if (is(cohort_end_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_end_date = (', as.character(cohort_end_date), ')')
    } else {
      statement <- paste0(statement, " cohort_end_date = '", cohort_end_date,"'")
    }
  }

  if (!missing(attribute_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_definition_id)) {
      statement <- paste0(statement, ' attribute_definition_id IS NULL')
    } else if (is(attribute_definition_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_definition_id = (', as.character(attribute_definition_id), ')')
    } else {
      statement <- paste0(statement, " attribute_definition_id = '", attribute_definition_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_drug_era <- function(rowCount, drug_era_id, person_id, drug_concept_id, drug_era_start_date, drug_era_end_date, drug_exposure_count, gap_days) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect drug_era' AS test, CASE WHEN(SELECT COUNT(*) FROM drug_era WHERE")
  first <- TRUE
  if (!missing(drug_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_id)) {
      statement <- paste0(statement, ' drug_era_id IS NULL')
    } else if (is(drug_era_id, 'subQuery')){
      statement <- paste0(statement, ' drug_era_id = (', as.character(drug_era_id), ')')
    } else {
      statement <- paste0(statement, " drug_era_id = '", drug_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(drug_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_start_date)) {
      statement <- paste0(statement, ' drug_era_start_date IS NULL')
    } else if (is(drug_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' drug_era_start_date = (', as.character(drug_era_start_date), ')')
    } else {
      statement <- paste0(statement, " drug_era_start_date = '", drug_era_start_date,"'")
    }
  }

  if (!missing(drug_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_end_date)) {
      statement <- paste0(statement, ' drug_era_end_date IS NULL')
    } else if (is(drug_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' drug_era_end_date = (', as.character(drug_era_end_date), ')')
    } else {
      statement <- paste0(statement, " drug_era_end_date = '", drug_era_end_date,"'")
    }
  }

  if (!missing(drug_exposure_count)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_count)) {
      statement <- paste0(statement, ' drug_exposure_count IS NULL')
    } else if (is(drug_exposure_count, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_count = (', as.character(drug_exposure_count), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_count = '", drug_exposure_count,"'")
    }
  }

  if (!missing(gap_days)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gap_days)) {
      statement <- paste0(statement, ' gap_days IS NULL')
    } else if (is(gap_days, 'subQuery')){
      statement <- paste0(statement, ' gap_days = (', as.character(gap_days), ')')
    } else {
      statement <- paste0(statement, " gap_days = '", gap_days,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_condition_era <- function(rowCount, condition_era_id, person_id, condition_concept_id, condition_era_start_date, condition_era_end_date, condition_occurrence_count) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect condition_era' AS test, CASE WHEN(SELECT COUNT(*) FROM condition_era WHERE")
  first <- TRUE
  if (!missing(condition_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_id)) {
      statement <- paste0(statement, ' condition_era_id IS NULL')
    } else if (is(condition_era_id, 'subQuery')){
      statement <- paste0(statement, ' condition_era_id = (', as.character(condition_era_id), ')')
    } else {
      statement <- paste0(statement, " condition_era_id = '", condition_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(condition_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_concept_id)) {
      statement <- paste0(statement, ' condition_concept_id IS NULL')
    } else if (is(condition_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_concept_id = (', as.character(condition_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_concept_id = '", condition_concept_id,"'")
    }
  }

  if (!missing(condition_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_start_date)) {
      statement <- paste0(statement, ' condition_era_start_date IS NULL')
    } else if (is(condition_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' condition_era_start_date = (', as.character(condition_era_start_date), ')')
    } else {
      statement <- paste0(statement, " condition_era_start_date = '", condition_era_start_date,"'")
    }
  }

  if (!missing(condition_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_end_date)) {
      statement <- paste0(statement, ' condition_era_end_date IS NULL')
    } else if (is(condition_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' condition_era_end_date = (', as.character(condition_era_end_date), ')')
    } else {
      statement <- paste0(statement, " condition_era_end_date = '", condition_era_end_date,"'")
    }
  }

  if (!missing(condition_occurrence_count)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_occurrence_count)) {
      statement <- paste0(statement, ' condition_occurrence_count IS NULL')
    } else if (is(condition_occurrence_count, 'subQuery')){
      statement <- paste0(statement, ' condition_occurrence_count = (', as.character(condition_occurrence_count), ')')
    } else {
      statement <- paste0(statement, " condition_occurrence_count = '", condition_occurrence_count,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_dose_era <- function(rowCount, dose_era_id, person_id, drug_concept_id, unit_concept_id, dose_value, dose_era_start_date, dose_era_end_date) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect dose_era' AS test, CASE WHEN(SELECT COUNT(*) FROM dose_era WHERE")
  first <- TRUE
  if (!missing(dose_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_id)) {
      statement <- paste0(statement, ' dose_era_id IS NULL')
    } else if (is(dose_era_id, 'subQuery')){
      statement <- paste0(statement, ' dose_era_id = (', as.character(dose_era_id), ')')
    } else {
      statement <- paste0(statement, " dose_era_id = '", dose_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(dose_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_value)) {
      statement <- paste0(statement, ' dose_value IS NULL')
    } else if (is(dose_value, 'subQuery')){
      statement <- paste0(statement, ' dose_value = (', as.character(dose_value), ')')
    } else {
      statement <- paste0(statement, " dose_value = '", dose_value,"'")
    }
  }

  if (!missing(dose_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_start_date)) {
      statement <- paste0(statement, ' dose_era_start_date IS NULL')
    } else if (is(dose_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' dose_era_start_date = (', as.character(dose_era_start_date), ')')
    } else {
      statement <- paste0(statement, " dose_era_start_date = '", dose_era_start_date,"'")
    }
  }

  if (!missing(dose_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_end_date)) {
      statement <- paste0(statement, ' dose_era_end_date IS NULL')
    } else if (is(dose_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' dose_era_end_date = (', as.character(dose_era_end_date), ')')
    } else {
      statement <- paste0(statement, " dose_era_end_date = '", dose_era_end_date,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_cost <- function(rowCount, cost_id, cost_event_id, cost_domain_id, cost_type_concept_id, currency_concept_id, total_charge, total_cost, total_paid, paid_by_payer, paid_by_patient, paid_patient_copay, paid_patient_coinsurance, paid_patient_deductible, paid_by_primary, paid_ingredient_cost, paid_dispensing_fee, payer_plan_period_id, amount_allowed, revenue_code_concept_id, revenue_code_source_value, drg_concept_id, drg_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cost' AS test, CASE WHEN(SELECT COUNT(*) FROM cost WHERE")
  first <- TRUE
  if (!missing(cost_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_id)) {
      statement <- paste0(statement, ' cost_id IS NULL')
    } else if (is(cost_id, 'subQuery')){
      statement <- paste0(statement, ' cost_id = (', as.character(cost_id), ')')
    } else {
      statement <- paste0(statement, " cost_id = '", cost_id,"'")
    }
  }

  if (!missing(cost_event_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_event_id)) {
      statement <- paste0(statement, ' cost_event_id IS NULL')
    } else if (is(cost_event_id, 'subQuery')){
      statement <- paste0(statement, ' cost_event_id = (', as.character(cost_event_id), ')')
    } else {
      statement <- paste0(statement, " cost_event_id = '", cost_event_id,"'")
    }
  }

  if (!missing(cost_domain_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_domain_id)) {
      statement <- paste0(statement, ' cost_domain_id IS NULL')
    } else if (is(cost_domain_id, 'subQuery')){
      statement <- paste0(statement, ' cost_domain_id = (', as.character(cost_domain_id), ')')
    } else {
      statement <- paste0(statement, " cost_domain_id = '", cost_domain_id,"'")
    }
  }

  if (!missing(cost_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_type_concept_id)) {
      statement <- paste0(statement, ' cost_type_concept_id IS NULL')
    } else if (is(cost_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cost_type_concept_id = (', as.character(cost_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " cost_type_concept_id = '", cost_type_concept_id,"'")
    }
  }

  if (!missing(currency_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(currency_concept_id)) {
      statement <- paste0(statement, ' currency_concept_id IS NULL')
    } else if (is(currency_concept_id, 'subQuery')){
      statement <- paste0(statement, ' currency_concept_id = (', as.character(currency_concept_id), ')')
    } else {
      statement <- paste0(statement, " currency_concept_id = '", currency_concept_id,"'")
    }
  }

  if (!missing(total_charge)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_charge)) {
      statement <- paste0(statement, ' total_charge IS NULL')
    } else if (is(total_charge, 'subQuery')){
      statement <- paste0(statement, ' total_charge = (', as.character(total_charge), ')')
    } else {
      statement <- paste0(statement, " total_charge = '", total_charge,"'")
    }
  }

  if (!missing(total_cost)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_cost)) {
      statement <- paste0(statement, ' total_cost IS NULL')
    } else if (is(total_cost, 'subQuery')){
      statement <- paste0(statement, ' total_cost = (', as.character(total_cost), ')')
    } else {
      statement <- paste0(statement, " total_cost = '", total_cost,"'")
    }
  }

  if (!missing(total_paid)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_paid)) {
      statement <- paste0(statement, ' total_paid IS NULL')
    } else if (is(total_paid, 'subQuery')){
      statement <- paste0(statement, ' total_paid = (', as.character(total_paid), ')')
    } else {
      statement <- paste0(statement, " total_paid = '", total_paid,"'")
    }
  }

  if (!missing(paid_by_payer)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_payer)) {
      statement <- paste0(statement, ' paid_by_payer IS NULL')
    } else if (is(paid_by_payer, 'subQuery')){
      statement <- paste0(statement, ' paid_by_payer = (', as.character(paid_by_payer), ')')
    } else {
      statement <- paste0(statement, " paid_by_payer = '", paid_by_payer,"'")
    }
  }

  if (!missing(paid_by_patient)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_patient)) {
      statement <- paste0(statement, ' paid_by_patient IS NULL')
    } else if (is(paid_by_patient, 'subQuery')){
      statement <- paste0(statement, ' paid_by_patient = (', as.character(paid_by_patient), ')')
    } else {
      statement <- paste0(statement, " paid_by_patient = '", paid_by_patient,"'")
    }
  }

  if (!missing(paid_patient_copay)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_copay)) {
      statement <- paste0(statement, ' paid_patient_copay IS NULL')
    } else if (is(paid_patient_copay, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_copay = (', as.character(paid_patient_copay), ')')
    } else {
      statement <- paste0(statement, " paid_patient_copay = '", paid_patient_copay,"'")
    }
  }

  if (!missing(paid_patient_coinsurance)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_coinsurance)) {
      statement <- paste0(statement, ' paid_patient_coinsurance IS NULL')
    } else if (is(paid_patient_coinsurance, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_coinsurance = (', as.character(paid_patient_coinsurance), ')')
    } else {
      statement <- paste0(statement, " paid_patient_coinsurance = '", paid_patient_coinsurance,"'")
    }
  }

  if (!missing(paid_patient_deductible)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_deductible)) {
      statement <- paste0(statement, ' paid_patient_deductible IS NULL')
    } else if (is(paid_patient_deductible, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_deductible = (', as.character(paid_patient_deductible), ')')
    } else {
      statement <- paste0(statement, " paid_patient_deductible = '", paid_patient_deductible,"'")
    }
  }

  if (!missing(paid_by_primary)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_primary)) {
      statement <- paste0(statement, ' paid_by_primary IS NULL')
    } else if (is(paid_by_primary, 'subQuery')){
      statement <- paste0(statement, ' paid_by_primary = (', as.character(paid_by_primary), ')')
    } else {
      statement <- paste0(statement, " paid_by_primary = '", paid_by_primary,"'")
    }
  }

  if (!missing(paid_ingredient_cost)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_ingredient_cost)) {
      statement <- paste0(statement, ' paid_ingredient_cost IS NULL')
    } else if (is(paid_ingredient_cost, 'subQuery')){
      statement <- paste0(statement, ' paid_ingredient_cost = (', as.character(paid_ingredient_cost), ')')
    } else {
      statement <- paste0(statement, " paid_ingredient_cost = '", paid_ingredient_cost,"'")
    }
  }

  if (!missing(paid_dispensing_fee)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_dispensing_fee)) {
      statement <- paste0(statement, ' paid_dispensing_fee IS NULL')
    } else if (is(paid_dispensing_fee, 'subQuery')){
      statement <- paste0(statement, ' paid_dispensing_fee = (', as.character(paid_dispensing_fee), ')')
    } else {
      statement <- paste0(statement, " paid_dispensing_fee = '", paid_dispensing_fee,"'")
    }
  }

  if (!missing(payer_plan_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_id)) {
      statement <- paste0(statement, ' payer_plan_period_id IS NULL')
    } else if (is(payer_plan_period_id, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_id = (', as.character(payer_plan_period_id), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_id = '", payer_plan_period_id,"'")
    }
  }

  if (!missing(amount_allowed)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(amount_allowed)) {
      statement <- paste0(statement, ' amount_allowed IS NULL')
    } else if (is(amount_allowed, 'subQuery')){
      statement <- paste0(statement, ' amount_allowed = (', as.character(amount_allowed), ')')
    } else {
      statement <- paste0(statement, " amount_allowed = '", amount_allowed,"'")
    }
  }

  if (!missing(revenue_code_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(revenue_code_concept_id)) {
      statement <- paste0(statement, ' revenue_code_concept_id IS NULL')
    } else if (is(revenue_code_concept_id, 'subQuery')){
      statement <- paste0(statement, ' revenue_code_concept_id = (', as.character(revenue_code_concept_id), ')')
    } else {
      statement <- paste0(statement, " revenue_code_concept_id = '", revenue_code_concept_id,"'")
    }
  }

  if (!missing(revenue_code_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(revenue_code_source_value)) {
      statement <- paste0(statement, ' revenue_code_source_value IS NULL')
    } else if (is(revenue_code_source_value, 'subQuery')){
      statement <- paste0(statement, ' revenue_code_source_value = (', as.character(revenue_code_source_value), ')')
    } else {
      statement <- paste0(statement, " revenue_code_source_value = '", revenue_code_source_value,"'")
    }
  }

  if (!missing(drg_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drg_concept_id)) {
      statement <- paste0(statement, ' drg_concept_id IS NULL')
    } else if (is(drg_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drg_concept_id = (', as.character(drg_concept_id), ')')
    } else {
      statement <- paste0(statement, " drg_concept_id = '", drg_concept_id,"'")
    }
  }

  if (!missing(drg_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drg_source_value)) {
      statement <- paste0(statement, ' drg_source_value IS NULL')
    } else if (is(drg_source_value, 'subQuery')){
      statement <- paste0(statement, ' drg_source_value = (', as.character(drg_source_value), ')')
    } else {
      statement <- paste0(statement, " drg_source_value = '", drg_source_value,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_payer_plan_period <- function(rowCount, payer_plan_period_id, person_id, payer_plan_period_start_date, payer_plan_period_end_date, payer_source_value, plan_source_value, family_source_value) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect payer_plan_period' AS test, CASE WHEN(SELECT COUNT(*) FROM payer_plan_period WHERE")
  first <- TRUE
  if (!missing(payer_plan_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_id)) {
      statement <- paste0(statement, ' payer_plan_period_id IS NULL')
    } else if (is(payer_plan_period_id, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_id = (', as.character(payer_plan_period_id), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_id = '", payer_plan_period_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(payer_plan_period_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_start_date)) {
      statement <- paste0(statement, ' payer_plan_period_start_date IS NULL')
    } else if (is(payer_plan_period_start_date, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_start_date = (', as.character(payer_plan_period_start_date), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_start_date = '", payer_plan_period_start_date,"'")
    }
  }

  if (!missing(payer_plan_period_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_end_date)) {
      statement <- paste0(statement, ' payer_plan_period_end_date IS NULL')
    } else if (is(payer_plan_period_end_date, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_end_date = (', as.character(payer_plan_period_end_date), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_end_date = '", payer_plan_period_end_date,"'")
    }
  }

  if (!missing(payer_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_source_value)) {
      statement <- paste0(statement, ' payer_source_value IS NULL')
    } else if (is(payer_source_value, 'subQuery')){
      statement <- paste0(statement, ' payer_source_value = (', as.character(payer_source_value), ')')
    } else {
      statement <- paste0(statement, " payer_source_value = '", payer_source_value,"'")
    }
  }

  if (!missing(plan_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(plan_source_value)) {
      statement <- paste0(statement, ' plan_source_value IS NULL')
    } else if (is(plan_source_value, 'subQuery')){
      statement <- paste0(statement, ' plan_source_value = (', as.character(plan_source_value), ')')
    } else {
      statement <- paste0(statement, " plan_source_value = '", plan_source_value,"'")
    }
  }

  if (!missing(family_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(family_source_value)) {
      statement <- paste0(statement, ' family_source_value IS NULL')
    } else if (is(family_source_value, 'subQuery')){
      statement <- paste0(statement, ' family_source_value = (', as.character(family_source_value), ')')
    } else {
      statement <- paste0(statement, " family_source_value = '", family_source_value,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_cdm_source <- function(rowCount, cdm_source_name, cdm_source_abbreviation, cdm_holder, source_description, source_documentation_reference, cdm_etl__reference, source_release_date, cdm_release_date, cdm_version, vocabulary_version) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cdm_source' AS test, CASE WHEN(SELECT COUNT(*) FROM cdm_source WHERE")
  first <- TRUE
  if (!missing(cdm_source_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_source_name)) {
      statement <- paste0(statement, ' cdm_source_name IS NULL')
    } else if (is(cdm_source_name, 'subQuery')){
      statement <- paste0(statement, ' cdm_source_name = (', as.character(cdm_source_name), ')')
    } else {
      statement <- paste0(statement, " cdm_source_name = '", cdm_source_name,"'")
    }
  }

  if (!missing(cdm_source_abbreviation)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_source_abbreviation)) {
      statement <- paste0(statement, ' cdm_source_abbreviation IS NULL')
    } else if (is(cdm_source_abbreviation, 'subQuery')){
      statement <- paste0(statement, ' cdm_source_abbreviation = (', as.character(cdm_source_abbreviation), ')')
    } else {
      statement <- paste0(statement, " cdm_source_abbreviation = '", cdm_source_abbreviation,"'")
    }
  }

  if (!missing(cdm_holder)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_holder)) {
      statement <- paste0(statement, ' cdm_holder IS NULL')
    } else if (is(cdm_holder, 'subQuery')){
      statement <- paste0(statement, ' cdm_holder = (', as.character(cdm_holder), ')')
    } else {
      statement <- paste0(statement, " cdm_holder = '", cdm_holder,"'")
    }
  }

  if (!missing(source_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_description)) {
      statement <- paste0(statement, ' source_description IS NULL')
    } else if (is(source_description, 'subQuery')){
      statement <- paste0(statement, ' source_description = (', as.character(source_description), ')')
    } else {
      statement <- paste0(statement, " source_description = '", source_description,"'")
    }
  }

  if (!missing(source_documentation_reference)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_documentation_reference)) {
      statement <- paste0(statement, ' source_documentation_reference IS NULL')
    } else if (is(source_documentation_reference, 'subQuery')){
      statement <- paste0(statement, ' source_documentation_reference = (', as.character(source_documentation_reference), ')')
    } else {
      statement <- paste0(statement, " source_documentation_reference = '", source_documentation_reference,"'")
    }
  }

  if (!missing(cdm_etl__reference)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_etl__reference)) {
      statement <- paste0(statement, ' [cdm_etl _reference] IS NULL')
    } else if (is(cdm_etl__reference, 'subQuery')){
      statement <- paste0(statement, ' [cdm_etl _reference] = (', as.character(cdm_etl__reference), ')')
    } else {
      statement <- paste0(statement, " [cdm_etl _reference] = '", cdm_etl__reference,"'")
    }
  }

  if (!missing(source_release_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_release_date)) {
      statement <- paste0(statement, ' source_release_date IS NULL')
    } else if (is(source_release_date, 'subQuery')){
      statement <- paste0(statement, ' source_release_date = (', as.character(source_release_date), ')')
    } else {
      statement <- paste0(statement, " source_release_date = '", source_release_date,"'")
    }
  }

  if (!missing(cdm_release_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_release_date)) {
      statement <- paste0(statement, ' cdm_release_date IS NULL')
    } else if (is(cdm_release_date, 'subQuery')){
      statement <- paste0(statement, ' cdm_release_date = (', as.character(cdm_release_date), ')')
    } else {
      statement <- paste0(statement, " cdm_release_date = '", cdm_release_date,"'")
    }
  }

  if (!missing(cdm_version)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_version)) {
      statement <- paste0(statement, ' cdm_version IS NULL')
    } else if (is(cdm_version, 'subQuery')){
      statement <- paste0(statement, ' cdm_version = (', as.character(cdm_version), ')')
    } else {
      statement <- paste0(statement, " cdm_version = '", cdm_version,"'")
    }
  }

  if (!missing(vocabulary_version)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(vocabulary_version)) {
      statement <- paste0(statement, ' vocabulary_version IS NULL')
    } else if (is(vocabulary_version, 'subQuery')){
      statement <- paste0(statement, ' vocabulary_version = (', as.character(vocabulary_version), ')')
    } else {
      statement <- paste0(statement, " vocabulary_version = '", vocabulary_version,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_attribute_definition <- function(rowCount, attribute_definition_id, attribute_name, attribute_description, attribute_type_concept_id, attribute_syntax) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect attribute_definition' AS test, CASE WHEN(SELECT COUNT(*) FROM attribute_definition WHERE")
  first <- TRUE
  if (!missing(attribute_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_definition_id)) {
      statement <- paste0(statement, ' attribute_definition_id IS NULL')
    } else if (is(attribute_definition_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_definition_id = (', as.character(attribute_definition_id), ')')
    } else {
      statement <- paste0(statement, " attribute_definition_id = '", attribute_definition_id,"'")
    }
  }

  if (!missing(attribute_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_name)) {
      statement <- paste0(statement, ' attribute_name IS NULL')
    } else if (is(attribute_name, 'subQuery')){
      statement <- paste0(statement, ' attribute_name = (', as.character(attribute_name), ')')
    } else {
      statement <- paste0(statement, " attribute_name = '", attribute_name,"'")
    }
  }

  if (!missing(attribute_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_description)) {
      statement <- paste0(statement, ' attribute_description IS NULL')
    } else if (is(attribute_description, 'subQuery')){
      statement <- paste0(statement, ' attribute_description = (', as.character(attribute_description), ')')
    } else {
      statement <- paste0(statement, " attribute_description = '", attribute_description,"'")
    }
  }

  if (!missing(attribute_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_type_concept_id)) {
      statement <- paste0(statement, ' attribute_type_concept_id IS NULL')
    } else if (is(attribute_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_type_concept_id = (', as.character(attribute_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " attribute_type_concept_id = '", attribute_type_concept_id,"'")
    }
  }

  if (!missing(attribute_syntax)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_syntax)) {
      statement <- paste0(statement, ' attribute_syntax IS NULL')
    } else if (is(attribute_syntax, 'subQuery')){
      statement <- paste0(statement, ' attribute_syntax = (', as.character(attribute_syntax), ')')
    } else {
      statement <- paste0(statement, " attribute_syntax = '", attribute_syntax,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

expect_count_cohort_definition <- function(rowCount, cohort_definition_id, cohort_definition_name, cohort_definition_description, definition_type_concept_id, cohort_definition_syntax, subject_concept_id, cohort_instantiation_date) {
  statement <- paste0("INSERT INTO test_results SELECT ", get("testId", envir = globalenv()), " AS id, '", get("testDescription", envir = globalenv()), "' AS description, 'Expect cohort_definition' AS test, CASE WHEN(SELECT COUNT(*) FROM cohort_definition WHERE")
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(cohort_definition_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_name)) {
      statement <- paste0(statement, ' cohort_definition_name IS NULL')
    } else if (is(cohort_definition_name, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_name = (', as.character(cohort_definition_name), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_name = '", cohort_definition_name,"'")
    }
  }

  if (!missing(cohort_definition_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_description)) {
      statement <- paste0(statement, ' cohort_definition_description IS NULL')
    } else if (is(cohort_definition_description, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_description = (', as.character(cohort_definition_description), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_description = '", cohort_definition_description,"'")
    }
  }

  if (!missing(definition_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(definition_type_concept_id)) {
      statement <- paste0(statement, ' definition_type_concept_id IS NULL')
    } else if (is(definition_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' definition_type_concept_id = (', as.character(definition_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " definition_type_concept_id = '", definition_type_concept_id,"'")
    }
  }

  if (!missing(cohort_definition_syntax)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_syntax)) {
      statement <- paste0(statement, ' cohort_definition_syntax IS NULL')
    } else if (is(cohort_definition_syntax, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_syntax = (', as.character(cohort_definition_syntax), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_syntax = '", cohort_definition_syntax,"'")
    }
  }

  if (!missing(subject_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_concept_id)) {
      statement <- paste0(statement, ' subject_concept_id IS NULL')
    } else if (is(subject_concept_id, 'subQuery')){
      statement <- paste0(statement, ' subject_concept_id = (', as.character(subject_concept_id), ')')
    } else {
      statement <- paste0(statement, " subject_concept_id = '", subject_concept_id,"'")
    }
  }

  if (!missing(cohort_instantiation_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_instantiation_date)) {
      statement <- paste0(statement, ' cohort_instantiation_date IS NULL')
    } else if (is(cohort_instantiation_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_instantiation_date = (', as.character(cohort_instantiation_date), ')')
    } else {
      statement <- paste0(statement, " cohort_instantiation_date = '", cohort_instantiation_date,"'")
    }
  }

  statement <- paste0(statement, ') != ', rowCount , " THEN 'FAIL' ELSE 'PASS' END AS status")
  if (exists('testNewExpected', where = globalenv()) && get('testNewExpected'))
  {
    assign('testNewExpected', FALSE, envir = globalenv())
    id <- get('testId', envir = globalenv())
    description <- get('testDescription', envir = globalenv())
    comment <- paste0('-- ', id, ': ', description)
    testSql <<- c(testSql, comment)
  }
  testSql <<- c(testSql, statement)
  invisible(statement)
}

lookup_person <- function(fetchField, person_id, person_source_value, care_site_id, gender_concept_id, gender_source_value, year_of_birth, month_of_birth, day_of_birth, birth_datetime, race_concept_id, race_source_value, ethnicity_concept_id, ethnicity_source_value, location_id, provider_id, gender_source_concept_id, race_source_concept_id, ethnicity_source_concept_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM person WHERE')
  first <- TRUE
  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(person_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_source_value)) {
      statement <- paste0(statement, ' person_source_value IS NULL')
    } else if (is(person_source_value, 'subQuery')){
      statement <- paste0(statement, ' person_source_value = (', as.character(person_source_value), ')')
    } else {
      statement <- paste0(statement, " person_source_value = '", person_source_value,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(gender_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_concept_id)) {
      statement <- paste0(statement, ' gender_concept_id IS NULL')
    } else if (is(gender_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_concept_id = (', as.character(gender_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_concept_id = '", gender_concept_id,"'")
    }
  }

  if (!missing(gender_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_value)) {
      statement <- paste0(statement, ' gender_source_value IS NULL')
    } else if (is(gender_source_value, 'subQuery')){
      statement <- paste0(statement, ' gender_source_value = (', as.character(gender_source_value), ')')
    } else {
      statement <- paste0(statement, " gender_source_value = '", gender_source_value,"'")
    }
  }

  if (!missing(year_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(year_of_birth)) {
      statement <- paste0(statement, ' year_of_birth IS NULL')
    } else if (is(year_of_birth, 'subQuery')){
      statement <- paste0(statement, ' year_of_birth = (', as.character(year_of_birth), ')')
    } else {
      statement <- paste0(statement, " year_of_birth = '", year_of_birth,"'")
    }
  }

  if (!missing(month_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(month_of_birth)) {
      statement <- paste0(statement, ' month_of_birth IS NULL')
    } else if (is(month_of_birth, 'subQuery')){
      statement <- paste0(statement, ' month_of_birth = (', as.character(month_of_birth), ')')
    } else {
      statement <- paste0(statement, " month_of_birth = '", month_of_birth,"'")
    }
  }

  if (!missing(day_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(day_of_birth)) {
      statement <- paste0(statement, ' day_of_birth IS NULL')
    } else if (is(day_of_birth, 'subQuery')){
      statement <- paste0(statement, ' day_of_birth = (', as.character(day_of_birth), ')')
    } else {
      statement <- paste0(statement, " day_of_birth = '", day_of_birth,"'")
    }
  }

  if (!missing(birth_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(birth_datetime)) {
      statement <- paste0(statement, ' birth_datetime IS NULL')
    } else if (is(birth_datetime, 'subQuery')){
      statement <- paste0(statement, ' birth_datetime = (', as.character(birth_datetime), ')')
    } else {
      statement <- paste0(statement, " birth_datetime = '", birth_datetime,"'")
    }
  }

  if (!missing(race_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_concept_id)) {
      statement <- paste0(statement, ' race_concept_id IS NULL')
    } else if (is(race_concept_id, 'subQuery')){
      statement <- paste0(statement, ' race_concept_id = (', as.character(race_concept_id), ')')
    } else {
      statement <- paste0(statement, " race_concept_id = '", race_concept_id,"'")
    }
  }

  if (!missing(race_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_source_value)) {
      statement <- paste0(statement, ' race_source_value IS NULL')
    } else if (is(race_source_value, 'subQuery')){
      statement <- paste0(statement, ' race_source_value = (', as.character(race_source_value), ')')
    } else {
      statement <- paste0(statement, " race_source_value = '", race_source_value,"'")
    }
  }

  if (!missing(ethnicity_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_concept_id)) {
      statement <- paste0(statement, ' ethnicity_concept_id IS NULL')
    } else if (is(ethnicity_concept_id, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_concept_id = (', as.character(ethnicity_concept_id), ')')
    } else {
      statement <- paste0(statement, " ethnicity_concept_id = '", ethnicity_concept_id,"'")
    }
  }

  if (!missing(ethnicity_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_source_value)) {
      statement <- paste0(statement, ' ethnicity_source_value IS NULL')
    } else if (is(ethnicity_source_value, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_source_value = (', as.character(ethnicity_source_value), ')')
    } else {
      statement <- paste0(statement, " ethnicity_source_value = '", ethnicity_source_value,"'")
    }
  }

  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(gender_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_concept_id)) {
      statement <- paste0(statement, ' gender_source_concept_id IS NULL')
    } else if (is(gender_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_source_concept_id = (', as.character(gender_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_source_concept_id = '", gender_source_concept_id,"'")
    }
  }

  if (!missing(race_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(race_source_concept_id)) {
      statement <- paste0(statement, ' race_source_concept_id IS NULL')
    } else if (is(race_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' race_source_concept_id = (', as.character(race_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " race_source_concept_id = '", race_source_concept_id,"'")
    }
  }

  if (!missing(ethnicity_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(ethnicity_source_concept_id)) {
      statement <- paste0(statement, ' ethnicity_source_concept_id IS NULL')
    } else if (is(ethnicity_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' ethnicity_source_concept_id = (', as.character(ethnicity_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " ethnicity_source_concept_id = '", ethnicity_source_concept_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_observation_period <- function(fetchField, observation_period_id, person_id, observation_period_start_date, observation_period_end_date, period_type_concept_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM observation_period WHERE')
  first <- TRUE
  if (!missing(observation_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_id)) {
      statement <- paste0(statement, ' observation_period_id IS NULL')
    } else if (is(observation_period_id, 'subQuery')){
      statement <- paste0(statement, ' observation_period_id = (', as.character(observation_period_id), ')')
    } else {
      statement <- paste0(statement, " observation_period_id = '", observation_period_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(observation_period_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_start_date)) {
      statement <- paste0(statement, ' observation_period_start_date IS NULL')
    } else if (is(observation_period_start_date, 'subQuery')){
      statement <- paste0(statement, ' observation_period_start_date = (', as.character(observation_period_start_date), ')')
    } else {
      statement <- paste0(statement, " observation_period_start_date = '", observation_period_start_date,"'")
    }
  }

  if (!missing(observation_period_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_period_end_date)) {
      statement <- paste0(statement, ' observation_period_end_date IS NULL')
    } else if (is(observation_period_end_date, 'subQuery')){
      statement <- paste0(statement, ' observation_period_end_date = (', as.character(observation_period_end_date), ')')
    } else {
      statement <- paste0(statement, " observation_period_end_date = '", observation_period_end_date,"'")
    }
  }

  if (!missing(period_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(period_type_concept_id)) {
      statement <- paste0(statement, ' period_type_concept_id IS NULL')
    } else if (is(period_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' period_type_concept_id = (', as.character(period_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " period_type_concept_id = '", period_type_concept_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_visit_occurrence <- function(fetchField, visit_occurrence_id, visit_source_value, person_id, care_site_id, visit_start_date, visit_end_date, visit_concept_id, provider_id, visit_type_concept_id, visit_source_concept_id, admitting_source_value, discharge_to_concept_id, discharge_to_source_value, preceding_visit_occurrence_id, visit_start_datetime, visit_end_datetime) {
  statement <- paste0('SELECT ', fetchField , ' FROM visit_occurrence WHERE')
  first <- TRUE
  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_value)) {
      statement <- paste0(statement, ' visit_source_value IS NULL')
    } else if (is(visit_source_value, 'subQuery')){
      statement <- paste0(statement, ' visit_source_value = (', as.character(visit_source_value), ')')
    } else {
      statement <- paste0(statement, " visit_source_value = '", visit_source_value,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(visit_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_date)) {
      statement <- paste0(statement, ' visit_start_date IS NULL')
    } else if (is(visit_start_date, 'subQuery')){
      statement <- paste0(statement, ' visit_start_date = (', as.character(visit_start_date), ')')
    } else {
      statement <- paste0(statement, " visit_start_date = '", visit_start_date,"'")
    }
  }

  if (!missing(visit_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_date)) {
      statement <- paste0(statement, ' visit_end_date IS NULL')
    } else if (is(visit_end_date, 'subQuery')){
      statement <- paste0(statement, ' visit_end_date = (', as.character(visit_end_date), ')')
    } else {
      statement <- paste0(statement, " visit_end_date = '", visit_end_date,"'")
    }
  }

  if (!missing(visit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_concept_id)) {
      statement <- paste0(statement, ' visit_concept_id IS NULL')
    } else if (is(visit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_concept_id = (', as.character(visit_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_concept_id = '", visit_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_type_concept_id)) {
      statement <- paste0(statement, ' visit_type_concept_id IS NULL')
    } else if (is(visit_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_type_concept_id = (', as.character(visit_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_type_concept_id = '", visit_type_concept_id,"'")
    }
  }

  if (!missing(visit_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_concept_id)) {
      statement <- paste0(statement, ' visit_source_concept_id IS NULL')
    } else if (is(visit_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_source_concept_id = (', as.character(visit_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_source_concept_id = '", visit_source_concept_id,"'")
    }
  }

  if (!missing(admitting_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(admitting_source_value)) {
      statement <- paste0(statement, ' admitting_source_value IS NULL')
    } else if (is(admitting_source_value, 'subQuery')){
      statement <- paste0(statement, ' admitting_source_value = (', as.character(admitting_source_value), ')')
    } else {
      statement <- paste0(statement, " admitting_source_value = '", admitting_source_value,"'")
    }
  }

  if (!missing(discharge_to_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_concept_id)) {
      statement <- paste0(statement, ' discharge_to_concept_id IS NULL')
    } else if (is(discharge_to_concept_id, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_concept_id = (', as.character(discharge_to_concept_id), ')')
    } else {
      statement <- paste0(statement, " discharge_to_concept_id = '", discharge_to_concept_id,"'")
    }
  }

  if (!missing(discharge_to_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_source_value)) {
      statement <- paste0(statement, ' discharge_to_source_value IS NULL')
    } else if (is(discharge_to_source_value, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_source_value = (', as.character(discharge_to_source_value), ')')
    } else {
      statement <- paste0(statement, " discharge_to_source_value = '", discharge_to_source_value,"'")
    }
  }

  if (!missing(preceding_visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(preceding_visit_occurrence_id)) {
      statement <- paste0(statement, ' preceding_visit_occurrence_id IS NULL')
    } else if (is(preceding_visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' preceding_visit_occurrence_id = (', as.character(preceding_visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " preceding_visit_occurrence_id = '", preceding_visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_datetime)) {
      statement <- paste0(statement, ' visit_start_datetime IS NULL')
    } else if (is(visit_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_start_datetime = (', as.character(visit_start_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_start_datetime = '", visit_start_datetime,"'")
    }
  }

  if (!missing(visit_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_datetime)) {
      statement <- paste0(statement, ' visit_end_datetime IS NULL')
    } else if (is(visit_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_end_datetime = (', as.character(visit_end_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_end_datetime = '", visit_end_datetime,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_visit_detail <- function(fetchField, visit_detail_id, person_id, visit_detail_concept_id, visit_start_date, visit_start_datetime, visit_end_date, visit_end_datetime, visit_type_concept_id, provider_id, care_site_id, visit_source_value, visit_source_concept_id, admitting_source_value, discharge_to_source_value, discharge_to_concept_id, preceding_visit_detail_id, visit_detail_parent_id, visit_occurrence_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM visit_detail WHERE')
  first <- TRUE
  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(visit_detail_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_concept_id)) {
      statement <- paste0(statement, ' visit_detail_concept_id IS NULL')
    } else if (is(visit_detail_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_concept_id = (', as.character(visit_detail_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_concept_id = '", visit_detail_concept_id,"'")
    }
  }

  if (!missing(visit_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_date)) {
      statement <- paste0(statement, ' visit_start_date IS NULL')
    } else if (is(visit_start_date, 'subQuery')){
      statement <- paste0(statement, ' visit_start_date = (', as.character(visit_start_date), ')')
    } else {
      statement <- paste0(statement, " visit_start_date = '", visit_start_date,"'")
    }
  }

  if (!missing(visit_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_start_datetime)) {
      statement <- paste0(statement, ' visit_start_datetime IS NULL')
    } else if (is(visit_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_start_datetime = (', as.character(visit_start_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_start_datetime = '", visit_start_datetime,"'")
    }
  }

  if (!missing(visit_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_date)) {
      statement <- paste0(statement, ' visit_end_date IS NULL')
    } else if (is(visit_end_date, 'subQuery')){
      statement <- paste0(statement, ' visit_end_date = (', as.character(visit_end_date), ')')
    } else {
      statement <- paste0(statement, " visit_end_date = '", visit_end_date,"'")
    }
  }

  if (!missing(visit_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_end_datetime)) {
      statement <- paste0(statement, ' visit_end_datetime IS NULL')
    } else if (is(visit_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' visit_end_datetime = (', as.character(visit_end_datetime), ')')
    } else {
      statement <- paste0(statement, " visit_end_datetime = '", visit_end_datetime,"'")
    }
  }

  if (!missing(visit_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_type_concept_id)) {
      statement <- paste0(statement, ' visit_type_concept_id IS NULL')
    } else if (is(visit_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_type_concept_id = (', as.character(visit_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_type_concept_id = '", visit_type_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(visit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_value)) {
      statement <- paste0(statement, ' visit_source_value IS NULL')
    } else if (is(visit_source_value, 'subQuery')){
      statement <- paste0(statement, ' visit_source_value = (', as.character(visit_source_value), ')')
    } else {
      statement <- paste0(statement, " visit_source_value = '", visit_source_value,"'")
    }
  }

  if (!missing(visit_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_source_concept_id)) {
      statement <- paste0(statement, ' visit_source_concept_id IS NULL')
    } else if (is(visit_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' visit_source_concept_id = (', as.character(visit_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " visit_source_concept_id = '", visit_source_concept_id,"'")
    }
  }

  if (!missing(admitting_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(admitting_source_value)) {
      statement <- paste0(statement, ' admitting_source_value IS NULL')
    } else if (is(admitting_source_value, 'subQuery')){
      statement <- paste0(statement, ' admitting_source_value = (', as.character(admitting_source_value), ')')
    } else {
      statement <- paste0(statement, " admitting_source_value = '", admitting_source_value,"'")
    }
  }

  if (!missing(discharge_to_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_source_value)) {
      statement <- paste0(statement, ' discharge_to_source_value IS NULL')
    } else if (is(discharge_to_source_value, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_source_value = (', as.character(discharge_to_source_value), ')')
    } else {
      statement <- paste0(statement, " discharge_to_source_value = '", discharge_to_source_value,"'")
    }
  }

  if (!missing(discharge_to_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(discharge_to_concept_id)) {
      statement <- paste0(statement, ' discharge_to_concept_id IS NULL')
    } else if (is(discharge_to_concept_id, 'subQuery')){
      statement <- paste0(statement, ' discharge_to_concept_id = (', as.character(discharge_to_concept_id), ')')
    } else {
      statement <- paste0(statement, " discharge_to_concept_id = '", discharge_to_concept_id,"'")
    }
  }

  if (!missing(preceding_visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(preceding_visit_detail_id)) {
      statement <- paste0(statement, ' preceding_visit_detail_id IS NULL')
    } else if (is(preceding_visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' preceding_visit_detail_id = (', as.character(preceding_visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " preceding_visit_detail_id = '", preceding_visit_detail_id,"'")
    }
  }

  if (!missing(visit_detail_parent_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_parent_id)) {
      statement <- paste0(statement, ' visit_detail_parent_id IS NULL')
    } else if (is(visit_detail_parent_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_parent_id = (', as.character(visit_detail_parent_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_parent_id = '", visit_detail_parent_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_condition_occurrence <- function(fetchField, condition_occurrence_id, person_id, condition_start_date, visit_occurrence_id, condition_concept_id, condition_start_datetime, condition_end_date, condition_end_datetime, condition_type_concept_id, stop_reason, provider_id, visit_detail_id, condition_source_concept_id, condition_source_value, condition_status_source_value, condition_status_concept_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM condition_occurrence WHERE')
  first <- TRUE
  if (!missing(condition_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_occurrence_id)) {
      statement <- paste0(statement, ' condition_occurrence_id IS NULL')
    } else if (is(condition_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' condition_occurrence_id = (', as.character(condition_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " condition_occurrence_id = '", condition_occurrence_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(condition_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_start_date)) {
      statement <- paste0(statement, ' condition_start_date IS NULL')
    } else if (is(condition_start_date, 'subQuery')){
      statement <- paste0(statement, ' condition_start_date = (', as.character(condition_start_date), ')')
    } else {
      statement <- paste0(statement, " condition_start_date = '", condition_start_date,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(condition_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_concept_id)) {
      statement <- paste0(statement, ' condition_concept_id IS NULL')
    } else if (is(condition_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_concept_id = (', as.character(condition_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_concept_id = '", condition_concept_id,"'")
    }
  }

  if (!missing(condition_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_start_datetime)) {
      statement <- paste0(statement, ' condition_start_datetime IS NULL')
    } else if (is(condition_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' condition_start_datetime = (', as.character(condition_start_datetime), ')')
    } else {
      statement <- paste0(statement, " condition_start_datetime = '", condition_start_datetime,"'")
    }
  }

  if (!missing(condition_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_end_date)) {
      statement <- paste0(statement, ' condition_end_date IS NULL')
    } else if (is(condition_end_date, 'subQuery')){
      statement <- paste0(statement, ' condition_end_date = (', as.character(condition_end_date), ')')
    } else {
      statement <- paste0(statement, " condition_end_date = '", condition_end_date,"'")
    }
  }

  if (!missing(condition_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_end_datetime)) {
      statement <- paste0(statement, ' condition_end_datetime IS NULL')
    } else if (is(condition_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' condition_end_datetime = (', as.character(condition_end_datetime), ')')
    } else {
      statement <- paste0(statement, " condition_end_datetime = '", condition_end_datetime,"'")
    }
  }

  if (!missing(condition_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_type_concept_id)) {
      statement <- paste0(statement, ' condition_type_concept_id IS NULL')
    } else if (is(condition_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_type_concept_id = (', as.character(condition_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_type_concept_id = '", condition_type_concept_id,"'")
    }
  }

  if (!missing(stop_reason)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(stop_reason)) {
      statement <- paste0(statement, ' stop_reason IS NULL')
    } else if (is(stop_reason, 'subQuery')){
      statement <- paste0(statement, ' stop_reason = (', as.character(stop_reason), ')')
    } else {
      statement <- paste0(statement, " stop_reason = '", stop_reason,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(condition_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_source_concept_id)) {
      statement <- paste0(statement, ' condition_source_concept_id IS NULL')
    } else if (is(condition_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_source_concept_id = (', as.character(condition_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_source_concept_id = '", condition_source_concept_id,"'")
    }
  }

  if (!missing(condition_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_source_value)) {
      statement <- paste0(statement, ' condition_source_value IS NULL')
    } else if (is(condition_source_value, 'subQuery')){
      statement <- paste0(statement, ' condition_source_value = (', as.character(condition_source_value), ')')
    } else {
      statement <- paste0(statement, " condition_source_value = '", condition_source_value,"'")
    }
  }

  if (!missing(condition_status_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_status_source_value)) {
      statement <- paste0(statement, ' condition_status_source_value IS NULL')
    } else if (is(condition_status_source_value, 'subQuery')){
      statement <- paste0(statement, ' condition_status_source_value = (', as.character(condition_status_source_value), ')')
    } else {
      statement <- paste0(statement, " condition_status_source_value = '", condition_status_source_value,"'")
    }
  }

  if (!missing(condition_status_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_status_concept_id)) {
      statement <- paste0(statement, ' condition_status_concept_id IS NULL')
    } else if (is(condition_status_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_status_concept_id = (', as.character(condition_status_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_status_concept_id = '", condition_status_concept_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_procedure_occurrence <- function(fetchField, procedure_occurrence_id, visit_occurrence_id, person_id, procedure_concept_id, procedure_date, procedure_datetime, procedure_type_concept_id, modifier_concept_id, quantity, provider_id, procedure_source_value, procedure_source_concept_id, qualifier_source_value, visit_detail_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM procedure_occurrence WHERE')
  first <- TRUE
  if (!missing(procedure_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_occurrence_id)) {
      statement <- paste0(statement, ' procedure_occurrence_id IS NULL')
    } else if (is(procedure_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_occurrence_id = (', as.character(procedure_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " procedure_occurrence_id = '", procedure_occurrence_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(procedure_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_concept_id)) {
      statement <- paste0(statement, ' procedure_concept_id IS NULL')
    } else if (is(procedure_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_concept_id = (', as.character(procedure_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_concept_id = '", procedure_concept_id,"'")
    }
  }

  if (!missing(procedure_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_date)) {
      statement <- paste0(statement, ' procedure_date IS NULL')
    } else if (is(procedure_date, 'subQuery')){
      statement <- paste0(statement, ' procedure_date = (', as.character(procedure_date), ')')
    } else {
      statement <- paste0(statement, " procedure_date = '", procedure_date,"'")
    }
  }

  if (!missing(procedure_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_datetime)) {
      statement <- paste0(statement, ' procedure_datetime IS NULL')
    } else if (is(procedure_datetime, 'subQuery')){
      statement <- paste0(statement, ' procedure_datetime = (', as.character(procedure_datetime), ')')
    } else {
      statement <- paste0(statement, " procedure_datetime = '", procedure_datetime,"'")
    }
  }

  if (!missing(procedure_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_type_concept_id)) {
      statement <- paste0(statement, ' procedure_type_concept_id IS NULL')
    } else if (is(procedure_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_type_concept_id = (', as.character(procedure_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_type_concept_id = '", procedure_type_concept_id,"'")
    }
  }

  if (!missing(modifier_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(modifier_concept_id)) {
      statement <- paste0(statement, ' modifier_concept_id IS NULL')
    } else if (is(modifier_concept_id, 'subQuery')){
      statement <- paste0(statement, ' modifier_concept_id = (', as.character(modifier_concept_id), ')')
    } else {
      statement <- paste0(statement, " modifier_concept_id = '", modifier_concept_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(procedure_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_source_value)) {
      statement <- paste0(statement, ' procedure_source_value IS NULL')
    } else if (is(procedure_source_value, 'subQuery')){
      statement <- paste0(statement, ' procedure_source_value = (', as.character(procedure_source_value), ')')
    } else {
      statement <- paste0(statement, " procedure_source_value = '", procedure_source_value,"'")
    }
  }

  if (!missing(procedure_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(procedure_source_concept_id)) {
      statement <- paste0(statement, ' procedure_source_concept_id IS NULL')
    } else if (is(procedure_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' procedure_source_concept_id = (', as.character(procedure_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " procedure_source_concept_id = '", procedure_source_concept_id,"'")
    }
  }

  if (!missing(qualifier_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_source_value)) {
      statement <- paste0(statement, ' qualifier_source_value IS NULL')
    } else if (is(qualifier_source_value, 'subQuery')){
      statement <- paste0(statement, ' qualifier_source_value = (', as.character(qualifier_source_value), ')')
    } else {
      statement <- paste0(statement, " qualifier_source_value = '", qualifier_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_drug_exposure <- function(fetchField, drug_exposure_id, person_id, drug_exposure_start_date, drug_exposure_end_date, days_supply, drug_concept_id, drug_source_value, drug_source_concept_id, provider_id, drug_type_concept_id, quantity, sig, refills, visit_occurrence_id, route_source_value, dose_unit_source_value, visit_detail_id, drug_exposure_start_datetime, drug_exposure_end_datetime, verbatim_end_date, route_concept_id, stop_reason, lot_number) {
  statement <- paste0('SELECT ', fetchField , ' FROM drug_exposure WHERE')
  first <- TRUE
  if (!missing(drug_exposure_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_id)) {
      statement <- paste0(statement, ' drug_exposure_id IS NULL')
    } else if (is(drug_exposure_id, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_id = (', as.character(drug_exposure_id), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_id = '", drug_exposure_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_exposure_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_start_date)) {
      statement <- paste0(statement, ' drug_exposure_start_date IS NULL')
    } else if (is(drug_exposure_start_date, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_start_date = (', as.character(drug_exposure_start_date), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_start_date = '", drug_exposure_start_date,"'")
    }
  }

  if (!missing(drug_exposure_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_end_date)) {
      statement <- paste0(statement, ' drug_exposure_end_date IS NULL')
    } else if (is(drug_exposure_end_date, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_end_date = (', as.character(drug_exposure_end_date), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_end_date = '", drug_exposure_end_date,"'")
    }
  }

  if (!missing(days_supply)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(days_supply)) {
      statement <- paste0(statement, ' days_supply IS NULL')
    } else if (is(days_supply, 'subQuery')){
      statement <- paste0(statement, ' days_supply = (', as.character(days_supply), ')')
    } else {
      statement <- paste0(statement, " days_supply = '", days_supply,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(drug_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_source_value)) {
      statement <- paste0(statement, ' drug_source_value IS NULL')
    } else if (is(drug_source_value, 'subQuery')){
      statement <- paste0(statement, ' drug_source_value = (', as.character(drug_source_value), ')')
    } else {
      statement <- paste0(statement, " drug_source_value = '", drug_source_value,"'")
    }
  }

  if (!missing(drug_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_source_concept_id)) {
      statement <- paste0(statement, ' drug_source_concept_id IS NULL')
    } else if (is(drug_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_source_concept_id = (', as.character(drug_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_source_concept_id = '", drug_source_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(drug_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_type_concept_id)) {
      statement <- paste0(statement, ' drug_type_concept_id IS NULL')
    } else if (is(drug_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_type_concept_id = (', as.character(drug_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_type_concept_id = '", drug_type_concept_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(sig)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(sig)) {
      statement <- paste0(statement, ' sig IS NULL')
    } else if (is(sig, 'subQuery')){
      statement <- paste0(statement, ' sig = (', as.character(sig), ')')
    } else {
      statement <- paste0(statement, " sig = '", sig,"'")
    }
  }

  if (!missing(refills)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(refills)) {
      statement <- paste0(statement, ' refills IS NULL')
    } else if (is(refills, 'subQuery')){
      statement <- paste0(statement, ' refills = (', as.character(refills), ')')
    } else {
      statement <- paste0(statement, " refills = '", refills,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(route_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(route_source_value)) {
      statement <- paste0(statement, ' route_source_value IS NULL')
    } else if (is(route_source_value, 'subQuery')){
      statement <- paste0(statement, ' route_source_value = (', as.character(route_source_value), ')')
    } else {
      statement <- paste0(statement, " route_source_value = '", route_source_value,"'")
    }
  }

  if (!missing(dose_unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_unit_source_value)) {
      statement <- paste0(statement, ' dose_unit_source_value IS NULL')
    } else if (is(dose_unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' dose_unit_source_value = (', as.character(dose_unit_source_value), ')')
    } else {
      statement <- paste0(statement, " dose_unit_source_value = '", dose_unit_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(drug_exposure_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_start_datetime)) {
      statement <- paste0(statement, ' drug_exposure_start_datetime IS NULL')
    } else if (is(drug_exposure_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_start_datetime = (', as.character(drug_exposure_start_datetime), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_start_datetime = '", drug_exposure_start_datetime,"'")
    }
  }

  if (!missing(drug_exposure_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_end_datetime)) {
      statement <- paste0(statement, ' drug_exposure_end_datetime IS NULL')
    } else if (is(drug_exposure_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_end_datetime = (', as.character(drug_exposure_end_datetime), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_end_datetime = '", drug_exposure_end_datetime,"'")
    }
  }

  if (!missing(verbatim_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(verbatim_end_date)) {
      statement <- paste0(statement, ' verbatim_end_date IS NULL')
    } else if (is(verbatim_end_date, 'subQuery')){
      statement <- paste0(statement, ' verbatim_end_date = (', as.character(verbatim_end_date), ')')
    } else {
      statement <- paste0(statement, " verbatim_end_date = '", verbatim_end_date,"'")
    }
  }

  if (!missing(route_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(route_concept_id)) {
      statement <- paste0(statement, ' route_concept_id IS NULL')
    } else if (is(route_concept_id, 'subQuery')){
      statement <- paste0(statement, ' route_concept_id = (', as.character(route_concept_id), ')')
    } else {
      statement <- paste0(statement, " route_concept_id = '", route_concept_id,"'")
    }
  }

  if (!missing(stop_reason)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(stop_reason)) {
      statement <- paste0(statement, ' stop_reason IS NULL')
    } else if (is(stop_reason, 'subQuery')){
      statement <- paste0(statement, ' stop_reason = (', as.character(stop_reason), ')')
    } else {
      statement <- paste0(statement, " stop_reason = '", stop_reason,"'")
    }
  }

  if (!missing(lot_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(lot_number)) {
      statement <- paste0(statement, ' lot_number IS NULL')
    } else if (is(lot_number, 'subQuery')){
      statement <- paste0(statement, ' lot_number = (', as.character(lot_number), ')')
    } else {
      statement <- paste0(statement, " lot_number = '", lot_number,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_measurement <- function(fetchField, measurement_id, person_id, measurement_concept_id, measurement_date, measurement_datetime, measurement_type_concept_id, operator_concept_id, value_as_number, value_as_concept_id, unit_concept_id, range_low, range_high, provider_id, visit_occurrence_id, visit_detail_id, measurement_source_value, measurement_source_concept_id, unit_source_value, value_source_value) {
  statement <- paste0('SELECT ', fetchField , ' FROM measurement WHERE')
  first <- TRUE
  if (!missing(measurement_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_id)) {
      statement <- paste0(statement, ' measurement_id IS NULL')
    } else if (is(measurement_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_id = (', as.character(measurement_id), ')')
    } else {
      statement <- paste0(statement, " measurement_id = '", measurement_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(measurement_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_concept_id)) {
      statement <- paste0(statement, ' measurement_concept_id IS NULL')
    } else if (is(measurement_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_concept_id = (', as.character(measurement_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_concept_id = '", measurement_concept_id,"'")
    }
  }

  if (!missing(measurement_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_date)) {
      statement <- paste0(statement, ' measurement_date IS NULL')
    } else if (is(measurement_date, 'subQuery')){
      statement <- paste0(statement, ' measurement_date = (', as.character(measurement_date), ')')
    } else {
      statement <- paste0(statement, " measurement_date = '", measurement_date,"'")
    }
  }

  if (!missing(measurement_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_datetime)) {
      statement <- paste0(statement, ' measurement_datetime IS NULL')
    } else if (is(measurement_datetime, 'subQuery')){
      statement <- paste0(statement, ' measurement_datetime = (', as.character(measurement_datetime), ')')
    } else {
      statement <- paste0(statement, " measurement_datetime = '", measurement_datetime,"'")
    }
  }

  if (!missing(measurement_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_type_concept_id)) {
      statement <- paste0(statement, ' measurement_type_concept_id IS NULL')
    } else if (is(measurement_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_type_concept_id = (', as.character(measurement_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_type_concept_id = '", measurement_type_concept_id,"'")
    }
  }

  if (!missing(operator_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(operator_concept_id)) {
      statement <- paste0(statement, ' operator_concept_id IS NULL')
    } else if (is(operator_concept_id, 'subQuery')){
      statement <- paste0(statement, ' operator_concept_id = (', as.character(operator_concept_id), ')')
    } else {
      statement <- paste0(statement, " operator_concept_id = '", operator_concept_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(range_low)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(range_low)) {
      statement <- paste0(statement, ' range_low IS NULL')
    } else if (is(range_low, 'subQuery')){
      statement <- paste0(statement, ' range_low = (', as.character(range_low), ')')
    } else {
      statement <- paste0(statement, " range_low = '", range_low,"'")
    }
  }

  if (!missing(range_high)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(range_high)) {
      statement <- paste0(statement, ' range_high IS NULL')
    } else if (is(range_high, 'subQuery')){
      statement <- paste0(statement, ' range_high = (', as.character(range_high), ')')
    } else {
      statement <- paste0(statement, " range_high = '", range_high,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(measurement_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_source_value)) {
      statement <- paste0(statement, ' measurement_source_value IS NULL')
    } else if (is(measurement_source_value, 'subQuery')){
      statement <- paste0(statement, ' measurement_source_value = (', as.character(measurement_source_value), ')')
    } else {
      statement <- paste0(statement, " measurement_source_value = '", measurement_source_value,"'")
    }
  }

  if (!missing(measurement_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(measurement_source_concept_id)) {
      statement <- paste0(statement, ' measurement_source_concept_id IS NULL')
    } else if (is(measurement_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' measurement_source_concept_id = (', as.character(measurement_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " measurement_source_concept_id = '", measurement_source_concept_id,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(value_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_source_value)) {
      statement <- paste0(statement, ' value_source_value IS NULL')
    } else if (is(value_source_value, 'subQuery')){
      statement <- paste0(statement, ' value_source_value = (', as.character(value_source_value), ')')
    } else {
      statement <- paste0(statement, " value_source_value = '", value_source_value,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_observation <- function(fetchField, observation_id, person_id, visit_occurrence_id, observation_date, observation_concept_id, observation_source_value, provider_id, observation_type_concept_id, value_as_number, value_as_string, qualifier_concept_id, qualifier_source_value, unit_concept_id, unit_source_value, visit_detail_id, observation_source_concept_id, observation_datetime, value_as_concept_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM observation WHERE')
  first <- TRUE
  if (!missing(observation_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_id)) {
      statement <- paste0(statement, ' observation_id IS NULL')
    } else if (is(observation_id, 'subQuery')){
      statement <- paste0(statement, ' observation_id = (', as.character(observation_id), ')')
    } else {
      statement <- paste0(statement, " observation_id = '", observation_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(observation_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_date)) {
      statement <- paste0(statement, ' observation_date IS NULL')
    } else if (is(observation_date, 'subQuery')){
      statement <- paste0(statement, ' observation_date = (', as.character(observation_date), ')')
    } else {
      statement <- paste0(statement, " observation_date = '", observation_date,"'")
    }
  }

  if (!missing(observation_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_concept_id)) {
      statement <- paste0(statement, ' observation_concept_id IS NULL')
    } else if (is(observation_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_concept_id = (', as.character(observation_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_concept_id = '", observation_concept_id,"'")
    }
  }

  if (!missing(observation_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_source_value)) {
      statement <- paste0(statement, ' observation_source_value IS NULL')
    } else if (is(observation_source_value, 'subQuery')){
      statement <- paste0(statement, ' observation_source_value = (', as.character(observation_source_value), ')')
    } else {
      statement <- paste0(statement, " observation_source_value = '", observation_source_value,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(observation_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_type_concept_id)) {
      statement <- paste0(statement, ' observation_type_concept_id IS NULL')
    } else if (is(observation_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_type_concept_id = (', as.character(observation_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_type_concept_id = '", observation_type_concept_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_string)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_string)) {
      statement <- paste0(statement, ' value_as_string IS NULL')
    } else if (is(value_as_string, 'subQuery')){
      statement <- paste0(statement, ' value_as_string = (', as.character(value_as_string), ')')
    } else {
      statement <- paste0(statement, " value_as_string = '", value_as_string,"'")
    }
  }

  if (!missing(qualifier_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_concept_id)) {
      statement <- paste0(statement, ' qualifier_concept_id IS NULL')
    } else if (is(qualifier_concept_id, 'subQuery')){
      statement <- paste0(statement, ' qualifier_concept_id = (', as.character(qualifier_concept_id), ')')
    } else {
      statement <- paste0(statement, " qualifier_concept_id = '", qualifier_concept_id,"'")
    }
  }

  if (!missing(qualifier_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(qualifier_source_value)) {
      statement <- paste0(statement, ' qualifier_source_value IS NULL')
    } else if (is(qualifier_source_value, 'subQuery')){
      statement <- paste0(statement, ' qualifier_source_value = (', as.character(qualifier_source_value), ')')
    } else {
      statement <- paste0(statement, " qualifier_source_value = '", qualifier_source_value,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  if (!missing(observation_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_source_concept_id)) {
      statement <- paste0(statement, ' observation_source_concept_id IS NULL')
    } else if (is(observation_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' observation_source_concept_id = (', as.character(observation_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " observation_source_concept_id = '", observation_source_concept_id,"'")
    }
  }

  if (!missing(observation_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(observation_datetime)) {
      statement <- paste0(statement, ' observation_datetime IS NULL')
    } else if (is(observation_datetime, 'subQuery')){
      statement <- paste0(statement, ' observation_datetime = (', as.character(observation_datetime), ')')
    } else {
      statement <- paste0(statement, " observation_datetime = '", observation_datetime,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_device_exposure <- function(fetchField, device_exposure_id, person_id, device_exposure_start_date, device_concept_id, device_exposure_start_datetime, device_exposure_end_date, device_exposure_end_datetime, device_type_concept_id, unique_device_id, quantity, provider_id, visit_occurrence_id, device_source_value, device_source__concept_id, visit_detail_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM device_exposure WHERE')
  first <- TRUE
  if (!missing(device_exposure_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_id)) {
      statement <- paste0(statement, ' device_exposure_id IS NULL')
    } else if (is(device_exposure_id, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_id = (', as.character(device_exposure_id), ')')
    } else {
      statement <- paste0(statement, " device_exposure_id = '", device_exposure_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(device_exposure_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_start_date)) {
      statement <- paste0(statement, ' device_exposure_start_date IS NULL')
    } else if (is(device_exposure_start_date, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_start_date = (', as.character(device_exposure_start_date), ')')
    } else {
      statement <- paste0(statement, " device_exposure_start_date = '", device_exposure_start_date,"'")
    }
  }

  if (!missing(device_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_concept_id)) {
      statement <- paste0(statement, ' device_concept_id IS NULL')
    } else if (is(device_concept_id, 'subQuery')){
      statement <- paste0(statement, ' device_concept_id = (', as.character(device_concept_id), ')')
    } else {
      statement <- paste0(statement, " device_concept_id = '", device_concept_id,"'")
    }
  }

  if (!missing(device_exposure_start_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_start_datetime)) {
      statement <- paste0(statement, ' device_exposure_start_datetime IS NULL')
    } else if (is(device_exposure_start_datetime, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_start_datetime = (', as.character(device_exposure_start_datetime), ')')
    } else {
      statement <- paste0(statement, " device_exposure_start_datetime = '", device_exposure_start_datetime,"'")
    }
  }

  if (!missing(device_exposure_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_end_date)) {
      statement <- paste0(statement, ' device_exposure_end_date IS NULL')
    } else if (is(device_exposure_end_date, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_end_date = (', as.character(device_exposure_end_date), ')')
    } else {
      statement <- paste0(statement, " device_exposure_end_date = '", device_exposure_end_date,"'")
    }
  }

  if (!missing(device_exposure_end_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_exposure_end_datetime)) {
      statement <- paste0(statement, ' device_exposure_end_datetime IS NULL')
    } else if (is(device_exposure_end_datetime, 'subQuery')){
      statement <- paste0(statement, ' device_exposure_end_datetime = (', as.character(device_exposure_end_datetime), ')')
    } else {
      statement <- paste0(statement, " device_exposure_end_datetime = '", device_exposure_end_datetime,"'")
    }
  }

  if (!missing(device_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_type_concept_id)) {
      statement <- paste0(statement, ' device_type_concept_id IS NULL')
    } else if (is(device_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' device_type_concept_id = (', as.character(device_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " device_type_concept_id = '", device_type_concept_id,"'")
    }
  }

  if (!missing(unique_device_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unique_device_id)) {
      statement <- paste0(statement, ' unique_device_id IS NULL')
    } else if (is(unique_device_id, 'subQuery')){
      statement <- paste0(statement, ' unique_device_id = (', as.character(unique_device_id), ')')
    } else {
      statement <- paste0(statement, " unique_device_id = '", unique_device_id,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  if (!missing(device_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_source_value)) {
      statement <- paste0(statement, ' device_source_value IS NULL')
    } else if (is(device_source_value, 'subQuery')){
      statement <- paste0(statement, ' device_source_value = (', as.character(device_source_value), ')')
    } else {
      statement <- paste0(statement, " device_source_value = '", device_source_value,"'")
    }
  }

  if (!missing(device_source__concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(device_source__concept_id)) {
      statement <- paste0(statement, ' [device_source_ concept_id] IS NULL')
    } else if (is(device_source__concept_id, 'subQuery')){
      statement <- paste0(statement, ' [device_source_ concept_id] = (', as.character(device_source__concept_id), ')')
    } else {
      statement <- paste0(statement, " [device_source_ concept_id] = '", device_source__concept_id,"'")
    }
  }

  if (!missing(visit_detail_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_detail_id)) {
      statement <- paste0(statement, ' visit_detail_id IS NULL')
    } else if (is(visit_detail_id, 'subQuery')){
      statement <- paste0(statement, ' visit_detail_id = (', as.character(visit_detail_id), ')')
    } else {
      statement <- paste0(statement, " visit_detail_id = '", visit_detail_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_death <- function(fetchField, person_id, death_date, death_datetime, death_type_concept_id, cause_concept_id, cause_source_value, cause_source_concept_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM death WHERE')
  first <- TRUE
  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(death_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_date)) {
      statement <- paste0(statement, ' death_date IS NULL')
    } else if (is(death_date, 'subQuery')){
      statement <- paste0(statement, ' death_date = (', as.character(death_date), ')')
    } else {
      statement <- paste0(statement, " death_date = '", death_date,"'")
    }
  }

  if (!missing(death_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_datetime)) {
      statement <- paste0(statement, ' death_datetime IS NULL')
    } else if (is(death_datetime, 'subQuery')){
      statement <- paste0(statement, ' death_datetime = (', as.character(death_datetime), ')')
    } else {
      statement <- paste0(statement, " death_datetime = '", death_datetime,"'")
    }
  }

  if (!missing(death_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(death_type_concept_id)) {
      statement <- paste0(statement, ' death_type_concept_id IS NULL')
    } else if (is(death_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' death_type_concept_id = (', as.character(death_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " death_type_concept_id = '", death_type_concept_id,"'")
    }
  }

  if (!missing(cause_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_concept_id)) {
      statement <- paste0(statement, ' cause_concept_id IS NULL')
    } else if (is(cause_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cause_concept_id = (', as.character(cause_concept_id), ')')
    } else {
      statement <- paste0(statement, " cause_concept_id = '", cause_concept_id,"'")
    }
  }

  if (!missing(cause_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_source_value)) {
      statement <- paste0(statement, ' cause_source_value IS NULL')
    } else if (is(cause_source_value, 'subQuery')){
      statement <- paste0(statement, ' cause_source_value = (', as.character(cause_source_value), ')')
    } else {
      statement <- paste0(statement, " cause_source_value = '", cause_source_value,"'")
    }
  }

  if (!missing(cause_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cause_source_concept_id)) {
      statement <- paste0(statement, ' cause_source_concept_id IS NULL')
    } else if (is(cause_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cause_source_concept_id = (', as.character(cause_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " cause_source_concept_id = '", cause_source_concept_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_location <- function(fetchField, location_id, address_1, address_2, city, state, zip, county, location_source_value) {
  statement <- paste0('SELECT ', fetchField , ' FROM location WHERE')
  first <- TRUE
  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(address_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(address_1)) {
      statement <- paste0(statement, ' address_1 IS NULL')
    } else if (is(address_1, 'subQuery')){
      statement <- paste0(statement, ' address_1 = (', as.character(address_1), ')')
    } else {
      statement <- paste0(statement, " address_1 = '", address_1,"'")
    }
  }

  if (!missing(address_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(address_2)) {
      statement <- paste0(statement, ' address_2 IS NULL')
    } else if (is(address_2, 'subQuery')){
      statement <- paste0(statement, ' address_2 = (', as.character(address_2), ')')
    } else {
      statement <- paste0(statement, " address_2 = '", address_2,"'")
    }
  }

  if (!missing(city)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(city)) {
      statement <- paste0(statement, ' city IS NULL')
    } else if (is(city, 'subQuery')){
      statement <- paste0(statement, ' city = (', as.character(city), ')')
    } else {
      statement <- paste0(statement, " city = '", city,"'")
    }
  }

  if (!missing(state)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(state)) {
      statement <- paste0(statement, ' state IS NULL')
    } else if (is(state, 'subQuery')){
      statement <- paste0(statement, ' state = (', as.character(state), ')')
    } else {
      statement <- paste0(statement, " state = '", state,"'")
    }
  }

  if (!missing(zip)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(zip)) {
      statement <- paste0(statement, ' zip IS NULL')
    } else if (is(zip, 'subQuery')){
      statement <- paste0(statement, ' zip = (', as.character(zip), ')')
    } else {
      statement <- paste0(statement, " zip = '", zip,"'")
    }
  }

  if (!missing(county)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(county)) {
      statement <- paste0(statement, ' county IS NULL')
    } else if (is(county, 'subQuery')){
      statement <- paste0(statement, ' county = (', as.character(county), ')')
    } else {
      statement <- paste0(statement, " county = '", county,"'")
    }
  }

  if (!missing(location_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_source_value)) {
      statement <- paste0(statement, ' location_source_value IS NULL')
    } else if (is(location_source_value, 'subQuery')){
      statement <- paste0(statement, ' location_source_value = (', as.character(location_source_value), ')')
    } else {
      statement <- paste0(statement, " location_source_value = '", location_source_value,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_care_site <- function(fetchField, care_site_id, care_site_source_value, location_id, care_site_name, place_of_service_concept_id, place_of_service_source_value) {
  statement <- paste0('SELECT ', fetchField , ' FROM care_site WHERE')
  first <- TRUE
  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(care_site_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_source_value)) {
      statement <- paste0(statement, ' care_site_source_value IS NULL')
    } else if (is(care_site_source_value, 'subQuery')){
      statement <- paste0(statement, ' care_site_source_value = (', as.character(care_site_source_value), ')')
    } else {
      statement <- paste0(statement, " care_site_source_value = '", care_site_source_value,"'")
    }
  }

  if (!missing(location_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(location_id)) {
      statement <- paste0(statement, ' location_id IS NULL')
    } else if (is(location_id, 'subQuery')){
      statement <- paste0(statement, ' location_id = (', as.character(location_id), ')')
    } else {
      statement <- paste0(statement, " location_id = '", location_id,"'")
    }
  }

  if (!missing(care_site_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_name)) {
      statement <- paste0(statement, ' care_site_name IS NULL')
    } else if (is(care_site_name, 'subQuery')){
      statement <- paste0(statement, ' care_site_name = (', as.character(care_site_name), ')')
    } else {
      statement <- paste0(statement, " care_site_name = '", care_site_name,"'")
    }
  }

  if (!missing(place_of_service_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(place_of_service_concept_id)) {
      statement <- paste0(statement, ' place_of_service_concept_id IS NULL')
    } else if (is(place_of_service_concept_id, 'subQuery')){
      statement <- paste0(statement, ' place_of_service_concept_id = (', as.character(place_of_service_concept_id), ')')
    } else {
      statement <- paste0(statement, " place_of_service_concept_id = '", place_of_service_concept_id,"'")
    }
  }

  if (!missing(place_of_service_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(place_of_service_source_value)) {
      statement <- paste0(statement, ' place_of_service_source_value IS NULL')
    } else if (is(place_of_service_source_value, 'subQuery')){
      statement <- paste0(statement, ' place_of_service_source_value = (', as.character(place_of_service_source_value), ')')
    } else {
      statement <- paste0(statement, " place_of_service_source_value = '", place_of_service_source_value,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_provider <- function(fetchField, provider_id, specialty_concept_id, specialty_source_value, provider_name, npi, dea, care_site_id, year_of_birth, gender_concept_id, provider_source_value, specialty_source_concept_id, gender_source_value, gender_source_concept_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM provider WHERE')
  first <- TRUE
  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(specialty_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_concept_id)) {
      statement <- paste0(statement, ' specialty_concept_id IS NULL')
    } else if (is(specialty_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specialty_concept_id = (', as.character(specialty_concept_id), ')')
    } else {
      statement <- paste0(statement, " specialty_concept_id = '", specialty_concept_id,"'")
    }
  }

  if (!missing(specialty_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_source_value)) {
      statement <- paste0(statement, ' specialty_source_value IS NULL')
    } else if (is(specialty_source_value, 'subQuery')){
      statement <- paste0(statement, ' specialty_source_value = (', as.character(specialty_source_value), ')')
    } else {
      statement <- paste0(statement, " specialty_source_value = '", specialty_source_value,"'")
    }
  }

  if (!missing(provider_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_name)) {
      statement <- paste0(statement, ' provider_name IS NULL')
    } else if (is(provider_name, 'subQuery')){
      statement <- paste0(statement, ' provider_name = (', as.character(provider_name), ')')
    } else {
      statement <- paste0(statement, " provider_name = '", provider_name,"'")
    }
  }

  if (!missing(npi)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(npi)) {
      statement <- paste0(statement, ' npi IS NULL')
    } else if (is(npi, 'subQuery')){
      statement <- paste0(statement, ' npi = (', as.character(npi), ')')
    } else {
      statement <- paste0(statement, " npi = '", npi,"'")
    }
  }

  if (!missing(dea)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dea)) {
      statement <- paste0(statement, ' dea IS NULL')
    } else if (is(dea, 'subQuery')){
      statement <- paste0(statement, ' dea = (', as.character(dea), ')')
    } else {
      statement <- paste0(statement, " dea = '", dea,"'")
    }
  }

  if (!missing(care_site_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(care_site_id)) {
      statement <- paste0(statement, ' care_site_id IS NULL')
    } else if (is(care_site_id, 'subQuery')){
      statement <- paste0(statement, ' care_site_id = (', as.character(care_site_id), ')')
    } else {
      statement <- paste0(statement, " care_site_id = '", care_site_id,"'")
    }
  }

  if (!missing(year_of_birth)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(year_of_birth)) {
      statement <- paste0(statement, ' year_of_birth IS NULL')
    } else if (is(year_of_birth, 'subQuery')){
      statement <- paste0(statement, ' year_of_birth = (', as.character(year_of_birth), ')')
    } else {
      statement <- paste0(statement, " year_of_birth = '", year_of_birth,"'")
    }
  }

  if (!missing(gender_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_concept_id)) {
      statement <- paste0(statement, ' gender_concept_id IS NULL')
    } else if (is(gender_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_concept_id = (', as.character(gender_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_concept_id = '", gender_concept_id,"'")
    }
  }

  if (!missing(provider_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_source_value)) {
      statement <- paste0(statement, ' provider_source_value IS NULL')
    } else if (is(provider_source_value, 'subQuery')){
      statement <- paste0(statement, ' provider_source_value = (', as.character(provider_source_value), ')')
    } else {
      statement <- paste0(statement, " provider_source_value = '", provider_source_value,"'")
    }
  }

  if (!missing(specialty_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specialty_source_concept_id)) {
      statement <- paste0(statement, ' specialty_source_concept_id IS NULL')
    } else if (is(specialty_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specialty_source_concept_id = (', as.character(specialty_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " specialty_source_concept_id = '", specialty_source_concept_id,"'")
    }
  }

  if (!missing(gender_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_value)) {
      statement <- paste0(statement, ' gender_source_value IS NULL')
    } else if (is(gender_source_value, 'subQuery')){
      statement <- paste0(statement, ' gender_source_value = (', as.character(gender_source_value), ')')
    } else {
      statement <- paste0(statement, " gender_source_value = '", gender_source_value,"'")
    }
  }

  if (!missing(gender_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gender_source_concept_id)) {
      statement <- paste0(statement, ' gender_source_concept_id IS NULL')
    } else if (is(gender_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' gender_source_concept_id = (', as.character(gender_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " gender_source_concept_id = '", gender_source_concept_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_note <- function(fetchField, note_id, person_id, note_date, note_datetime, note_type_concept_id, note_class_concept_id, note_title, note_text, encoding_concept_id, language_concept_id, provider_id, note_source_value, visit_occurrence_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM note WHERE')
  first <- TRUE
  if (!missing(note_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_id)) {
      statement <- paste0(statement, ' note_id IS NULL')
    } else if (is(note_id, 'subQuery')){
      statement <- paste0(statement, ' note_id = (', as.character(note_id), ')')
    } else {
      statement <- paste0(statement, " note_id = '", note_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(note_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_date)) {
      statement <- paste0(statement, ' note_date IS NULL')
    } else if (is(note_date, 'subQuery')){
      statement <- paste0(statement, ' note_date = (', as.character(note_date), ')')
    } else {
      statement <- paste0(statement, " note_date = '", note_date,"'")
    }
  }

  if (!missing(note_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_datetime)) {
      statement <- paste0(statement, ' note_datetime IS NULL')
    } else if (is(note_datetime, 'subQuery')){
      statement <- paste0(statement, ' note_datetime = (', as.character(note_datetime), ')')
    } else {
      statement <- paste0(statement, " note_datetime = '", note_datetime,"'")
    }
  }

  if (!missing(note_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_type_concept_id)) {
      statement <- paste0(statement, ' note_type_concept_id IS NULL')
    } else if (is(note_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_type_concept_id = (', as.character(note_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_type_concept_id = '", note_type_concept_id,"'")
    }
  }

  if (!missing(note_class_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_class_concept_id)) {
      statement <- paste0(statement, ' note_class_concept_id IS NULL')
    } else if (is(note_class_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_class_concept_id = (', as.character(note_class_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_class_concept_id = '", note_class_concept_id,"'")
    }
  }

  if (!missing(note_title)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_title)) {
      statement <- paste0(statement, ' note_title IS NULL')
    } else if (is(note_title, 'subQuery')){
      statement <- paste0(statement, ' note_title = (', as.character(note_title), ')')
    } else {
      statement <- paste0(statement, " note_title = '", note_title,"'")
    }
  }

  if (!missing(note_text)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_text)) {
      statement <- paste0(statement, ' note_text IS NULL')
    } else if (is(note_text, 'subQuery')){
      statement <- paste0(statement, ' note_text = (', as.character(note_text), ')')
    } else {
      statement <- paste0(statement, " note_text = '", note_text,"'")
    }
  }

  if (!missing(encoding_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(encoding_concept_id)) {
      statement <- paste0(statement, ' encoding_concept_id IS NULL')
    } else if (is(encoding_concept_id, 'subQuery')){
      statement <- paste0(statement, ' encoding_concept_id = (', as.character(encoding_concept_id), ')')
    } else {
      statement <- paste0(statement, " encoding_concept_id = '", encoding_concept_id,"'")
    }
  }

  if (!missing(language_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(language_concept_id)) {
      statement <- paste0(statement, ' language_concept_id IS NULL')
    } else if (is(language_concept_id, 'subQuery')){
      statement <- paste0(statement, ' language_concept_id = (', as.character(language_concept_id), ')')
    } else {
      statement <- paste0(statement, " language_concept_id = '", language_concept_id,"'")
    }
  }

  if (!missing(provider_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(provider_id)) {
      statement <- paste0(statement, ' provider_id IS NULL')
    } else if (is(provider_id, 'subQuery')){
      statement <- paste0(statement, ' provider_id = (', as.character(provider_id), ')')
    } else {
      statement <- paste0(statement, " provider_id = '", provider_id,"'")
    }
  }

  if (!missing(note_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_source_value)) {
      statement <- paste0(statement, ' note_source_value IS NULL')
    } else if (is(note_source_value, 'subQuery')){
      statement <- paste0(statement, ' note_source_value = (', as.character(note_source_value), ')')
    } else {
      statement <- paste0(statement, " note_source_value = '", note_source_value,"'")
    }
  }

  if (!missing(visit_occurrence_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(visit_occurrence_id)) {
      statement <- paste0(statement, ' visit_occurrence_id IS NULL')
    } else if (is(visit_occurrence_id, 'subQuery')){
      statement <- paste0(statement, ' visit_occurrence_id = (', as.character(visit_occurrence_id), ')')
    } else {
      statement <- paste0(statement, " visit_occurrence_id = '", visit_occurrence_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_fact_relationship <- function(fetchField, domain_concept_id_1, fact_id_1, domain_concept_id_2, fact_id_2, relationship_concept_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM fact_relationship WHERE')
  first <- TRUE
  if (!missing(domain_concept_id_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(domain_concept_id_1)) {
      statement <- paste0(statement, ' domain_concept_id_1 IS NULL')
    } else if (is(domain_concept_id_1, 'subQuery')){
      statement <- paste0(statement, ' domain_concept_id_1 = (', as.character(domain_concept_id_1), ')')
    } else {
      statement <- paste0(statement, " domain_concept_id_1 = '", domain_concept_id_1,"'")
    }
  }

  if (!missing(fact_id_1)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(fact_id_1)) {
      statement <- paste0(statement, ' fact_id_1 IS NULL')
    } else if (is(fact_id_1, 'subQuery')){
      statement <- paste0(statement, ' fact_id_1 = (', as.character(fact_id_1), ')')
    } else {
      statement <- paste0(statement, " fact_id_1 = '", fact_id_1,"'")
    }
  }

  if (!missing(domain_concept_id_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(domain_concept_id_2)) {
      statement <- paste0(statement, ' domain_concept_id_2 IS NULL')
    } else if (is(domain_concept_id_2, 'subQuery')){
      statement <- paste0(statement, ' domain_concept_id_2 = (', as.character(domain_concept_id_2), ')')
    } else {
      statement <- paste0(statement, " domain_concept_id_2 = '", domain_concept_id_2,"'")
    }
  }

  if (!missing(fact_id_2)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(fact_id_2)) {
      statement <- paste0(statement, ' fact_id_2 IS NULL')
    } else if (is(fact_id_2, 'subQuery')){
      statement <- paste0(statement, ' fact_id_2 = (', as.character(fact_id_2), ')')
    } else {
      statement <- paste0(statement, " fact_id_2 = '", fact_id_2,"'")
    }
  }

  if (!missing(relationship_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(relationship_concept_id)) {
      statement <- paste0(statement, ' relationship_concept_id IS NULL')
    } else if (is(relationship_concept_id, 'subQuery')){
      statement <- paste0(statement, ' relationship_concept_id = (', as.character(relationship_concept_id), ')')
    } else {
      statement <- paste0(statement, " relationship_concept_id = '", relationship_concept_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_note_nlp <- function(fetchField, note_nlp_id, note_id, section_concept_id, snippet, offset, lexical_variant, note_nlp_concept_id, note_nlp_source_concept_id, nlp_system, nlp_date, nlp_date_time, term_exists, term_temporal, term_modifiers) {
  statement <- paste0('SELECT ', fetchField , ' FROM note_nlp WHERE')
  first <- TRUE
  if (!missing(note_nlp_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_id)) {
      statement <- paste0(statement, ' note_nlp_id IS NULL')
    } else if (is(note_nlp_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_id = (', as.character(note_nlp_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_id = '", note_nlp_id,"'")
    }
  }

  if (!missing(note_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_id)) {
      statement <- paste0(statement, ' note_id IS NULL')
    } else if (is(note_id, 'subQuery')){
      statement <- paste0(statement, ' note_id = (', as.character(note_id), ')')
    } else {
      statement <- paste0(statement, " note_id = '", note_id,"'")
    }
  }

  if (!missing(section_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(section_concept_id)) {
      statement <- paste0(statement, ' section_concept_id IS NULL')
    } else if (is(section_concept_id, 'subQuery')){
      statement <- paste0(statement, ' section_concept_id = (', as.character(section_concept_id), ')')
    } else {
      statement <- paste0(statement, " section_concept_id = '", section_concept_id,"'")
    }
  }

  if (!missing(snippet)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(snippet)) {
      statement <- paste0(statement, ' snippet IS NULL')
    } else if (is(snippet, 'subQuery')){
      statement <- paste0(statement, ' snippet = (', as.character(snippet), ')')
    } else {
      statement <- paste0(statement, " snippet = '", snippet,"'")
    }
  }

  if (!missing(offset)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(offset)) {
      statement <- paste0(statement, ' offset IS NULL')
    } else if (is(offset, 'subQuery')){
      statement <- paste0(statement, ' offset = (', as.character(offset), ')')
    } else {
      statement <- paste0(statement, " offset = '", offset,"'")
    }
  }

  if (!missing(lexical_variant)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(lexical_variant)) {
      statement <- paste0(statement, ' lexical_variant IS NULL')
    } else if (is(lexical_variant, 'subQuery')){
      statement <- paste0(statement, ' lexical_variant = (', as.character(lexical_variant), ')')
    } else {
      statement <- paste0(statement, " lexical_variant = '", lexical_variant,"'")
    }
  }

  if (!missing(note_nlp_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_concept_id)) {
      statement <- paste0(statement, ' note_nlp_concept_id IS NULL')
    } else if (is(note_nlp_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_concept_id = (', as.character(note_nlp_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_concept_id = '", note_nlp_concept_id,"'")
    }
  }

  if (!missing(note_nlp_source_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(note_nlp_source_concept_id)) {
      statement <- paste0(statement, ' note_nlp_source_concept_id IS NULL')
    } else if (is(note_nlp_source_concept_id, 'subQuery')){
      statement <- paste0(statement, ' note_nlp_source_concept_id = (', as.character(note_nlp_source_concept_id), ')')
    } else {
      statement <- paste0(statement, " note_nlp_source_concept_id = '", note_nlp_source_concept_id,"'")
    }
  }

  if (!missing(nlp_system)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_system)) {
      statement <- paste0(statement, ' nlp_system IS NULL')
    } else if (is(nlp_system, 'subQuery')){
      statement <- paste0(statement, ' nlp_system = (', as.character(nlp_system), ')')
    } else {
      statement <- paste0(statement, " nlp_system = '", nlp_system,"'")
    }
  }

  if (!missing(nlp_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_date)) {
      statement <- paste0(statement, ' nlp_date IS NULL')
    } else if (is(nlp_date, 'subQuery')){
      statement <- paste0(statement, ' nlp_date = (', as.character(nlp_date), ')')
    } else {
      statement <- paste0(statement, " nlp_date = '", nlp_date,"'")
    }
  }

  if (!missing(nlp_date_time)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(nlp_date_time)) {
      statement <- paste0(statement, ' nlp_date_time IS NULL')
    } else if (is(nlp_date_time, 'subQuery')){
      statement <- paste0(statement, ' nlp_date_time = (', as.character(nlp_date_time), ')')
    } else {
      statement <- paste0(statement, " nlp_date_time = '", nlp_date_time,"'")
    }
  }

  if (!missing(term_exists)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_exists)) {
      statement <- paste0(statement, ' term_exists IS NULL')
    } else if (is(term_exists, 'subQuery')){
      statement <- paste0(statement, ' term_exists = (', as.character(term_exists), ')')
    } else {
      statement <- paste0(statement, " term_exists = '", term_exists,"'")
    }
  }

  if (!missing(term_temporal)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_temporal)) {
      statement <- paste0(statement, ' term_temporal IS NULL')
    } else if (is(term_temporal, 'subQuery')){
      statement <- paste0(statement, ' term_temporal = (', as.character(term_temporal), ')')
    } else {
      statement <- paste0(statement, " term_temporal = '", term_temporal,"'")
    }
  }

  if (!missing(term_modifiers)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(term_modifiers)) {
      statement <- paste0(statement, ' term_modifiers IS NULL')
    } else if (is(term_modifiers, 'subQuery')){
      statement <- paste0(statement, ' term_modifiers = (', as.character(term_modifiers), ')')
    } else {
      statement <- paste0(statement, " term_modifiers = '", term_modifiers,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_specimen <- function(fetchField, specimen_id, person_id, specimen_concept_id, specimen_type_concept_id, specimen_date, specimen_datetime, quantity, unit_concept_id, anatomic_site_concept_id, disease_status_concept_id, specimen_source_id, specimen_source_value, unit_source_value, anatomic_site_source_value, disease_status_source_value) {
  statement <- paste0('SELECT ', fetchField , ' FROM specimen WHERE')
  first <- TRUE
  if (!missing(specimen_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_id)) {
      statement <- paste0(statement, ' specimen_id IS NULL')
    } else if (is(specimen_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_id = (', as.character(specimen_id), ')')
    } else {
      statement <- paste0(statement, " specimen_id = '", specimen_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(specimen_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_concept_id)) {
      statement <- paste0(statement, ' specimen_concept_id IS NULL')
    } else if (is(specimen_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_concept_id = (', as.character(specimen_concept_id), ')')
    } else {
      statement <- paste0(statement, " specimen_concept_id = '", specimen_concept_id,"'")
    }
  }

  if (!missing(specimen_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_type_concept_id)) {
      statement <- paste0(statement, ' specimen_type_concept_id IS NULL')
    } else if (is(specimen_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_type_concept_id = (', as.character(specimen_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " specimen_type_concept_id = '", specimen_type_concept_id,"'")
    }
  }

  if (!missing(specimen_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_date)) {
      statement <- paste0(statement, ' specimen_date IS NULL')
    } else if (is(specimen_date, 'subQuery')){
      statement <- paste0(statement, ' specimen_date = (', as.character(specimen_date), ')')
    } else {
      statement <- paste0(statement, " specimen_date = '", specimen_date,"'")
    }
  }

  if (!missing(specimen_datetime)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_datetime)) {
      statement <- paste0(statement, ' specimen_datetime IS NULL')
    } else if (is(specimen_datetime, 'subQuery')){
      statement <- paste0(statement, ' specimen_datetime = (', as.character(specimen_datetime), ')')
    } else {
      statement <- paste0(statement, " specimen_datetime = '", specimen_datetime,"'")
    }
  }

  if (!missing(quantity)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(quantity)) {
      statement <- paste0(statement, ' quantity IS NULL')
    } else if (is(quantity, 'subQuery')){
      statement <- paste0(statement, ' quantity = (', as.character(quantity), ')')
    } else {
      statement <- paste0(statement, " quantity = '", quantity,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(anatomic_site_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(anatomic_site_concept_id)) {
      statement <- paste0(statement, ' anatomic_site_concept_id IS NULL')
    } else if (is(anatomic_site_concept_id, 'subQuery')){
      statement <- paste0(statement, ' anatomic_site_concept_id = (', as.character(anatomic_site_concept_id), ')')
    } else {
      statement <- paste0(statement, " anatomic_site_concept_id = '", anatomic_site_concept_id,"'")
    }
  }

  if (!missing(disease_status_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(disease_status_concept_id)) {
      statement <- paste0(statement, ' disease_status_concept_id IS NULL')
    } else if (is(disease_status_concept_id, 'subQuery')){
      statement <- paste0(statement, ' disease_status_concept_id = (', as.character(disease_status_concept_id), ')')
    } else {
      statement <- paste0(statement, " disease_status_concept_id = '", disease_status_concept_id,"'")
    }
  }

  if (!missing(specimen_source_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_source_id)) {
      statement <- paste0(statement, ' specimen_source_id IS NULL')
    } else if (is(specimen_source_id, 'subQuery')){
      statement <- paste0(statement, ' specimen_source_id = (', as.character(specimen_source_id), ')')
    } else {
      statement <- paste0(statement, " specimen_source_id = '", specimen_source_id,"'")
    }
  }

  if (!missing(specimen_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(specimen_source_value)) {
      statement <- paste0(statement, ' specimen_source_value IS NULL')
    } else if (is(specimen_source_value, 'subQuery')){
      statement <- paste0(statement, ' specimen_source_value = (', as.character(specimen_source_value), ')')
    } else {
      statement <- paste0(statement, " specimen_source_value = '", specimen_source_value,"'")
    }
  }

  if (!missing(unit_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_source_value)) {
      statement <- paste0(statement, ' unit_source_value IS NULL')
    } else if (is(unit_source_value, 'subQuery')){
      statement <- paste0(statement, ' unit_source_value = (', as.character(unit_source_value), ')')
    } else {
      statement <- paste0(statement, " unit_source_value = '", unit_source_value,"'")
    }
  }

  if (!missing(anatomic_site_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(anatomic_site_source_value)) {
      statement <- paste0(statement, ' anatomic_site_source_value IS NULL')
    } else if (is(anatomic_site_source_value, 'subQuery')){
      statement <- paste0(statement, ' anatomic_site_source_value = (', as.character(anatomic_site_source_value), ')')
    } else {
      statement <- paste0(statement, " anatomic_site_source_value = '", anatomic_site_source_value,"'")
    }
  }

  if (!missing(disease_status_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(disease_status_source_value)) {
      statement <- paste0(statement, ' disease_status_source_value IS NULL')
    } else if (is(disease_status_source_value, 'subQuery')){
      statement <- paste0(statement, ' disease_status_source_value = (', as.character(disease_status_source_value), ')')
    } else {
      statement <- paste0(statement, " disease_status_source_value = '", disease_status_source_value,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_cohort <- function(fetchField, cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) {
  statement <- paste0('SELECT ', fetchField , ' FROM cohort WHERE')
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(subject_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_id)) {
      statement <- paste0(statement, ' subject_id IS NULL')
    } else if (is(subject_id, 'subQuery')){
      statement <- paste0(statement, ' subject_id = (', as.character(subject_id), ')')
    } else {
      statement <- paste0(statement, " subject_id = '", subject_id,"'")
    }
  }

  if (!missing(cohort_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_start_date)) {
      statement <- paste0(statement, ' cohort_start_date IS NULL')
    } else if (is(cohort_start_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_start_date = (', as.character(cohort_start_date), ')')
    } else {
      statement <- paste0(statement, " cohort_start_date = '", cohort_start_date,"'")
    }
  }

  if (!missing(cohort_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_end_date)) {
      statement <- paste0(statement, ' cohort_end_date IS NULL')
    } else if (is(cohort_end_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_end_date = (', as.character(cohort_end_date), ')')
    } else {
      statement <- paste0(statement, " cohort_end_date = '", cohort_end_date,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_cohort_attribute <- function(fetchField, cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, attribute_definition_id, value_as_number, value_as_concept_id) {
  statement <- paste0('SELECT ', fetchField , ' FROM cohort_attribute WHERE')
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(subject_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_id)) {
      statement <- paste0(statement, ' subject_id IS NULL')
    } else if (is(subject_id, 'subQuery')){
      statement <- paste0(statement, ' subject_id = (', as.character(subject_id), ')')
    } else {
      statement <- paste0(statement, " subject_id = '", subject_id,"'")
    }
  }

  if (!missing(cohort_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_start_date)) {
      statement <- paste0(statement, ' cohort_start_date IS NULL')
    } else if (is(cohort_start_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_start_date = (', as.character(cohort_start_date), ')')
    } else {
      statement <- paste0(statement, " cohort_start_date = '", cohort_start_date,"'")
    }
  }

  if (!missing(cohort_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_end_date)) {
      statement <- paste0(statement, ' cohort_end_date IS NULL')
    } else if (is(cohort_end_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_end_date = (', as.character(cohort_end_date), ')')
    } else {
      statement <- paste0(statement, " cohort_end_date = '", cohort_end_date,"'")
    }
  }

  if (!missing(attribute_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_definition_id)) {
      statement <- paste0(statement, ' attribute_definition_id IS NULL')
    } else if (is(attribute_definition_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_definition_id = (', as.character(attribute_definition_id), ')')
    } else {
      statement <- paste0(statement, " attribute_definition_id = '", attribute_definition_id,"'")
    }
  }

  if (!missing(value_as_number)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_number)) {
      statement <- paste0(statement, ' value_as_number IS NULL')
    } else if (is(value_as_number, 'subQuery')){
      statement <- paste0(statement, ' value_as_number = (', as.character(value_as_number), ')')
    } else {
      statement <- paste0(statement, " value_as_number = '", value_as_number,"'")
    }
  }

  if (!missing(value_as_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(value_as_concept_id)) {
      statement <- paste0(statement, ' value_as_concept_id IS NULL')
    } else if (is(value_as_concept_id, 'subQuery')){
      statement <- paste0(statement, ' value_as_concept_id = (', as.character(value_as_concept_id), ')')
    } else {
      statement <- paste0(statement, " value_as_concept_id = '", value_as_concept_id,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_drug_era <- function(fetchField, drug_era_id, person_id, drug_concept_id, drug_era_start_date, drug_era_end_date, drug_exposure_count, gap_days) {
  statement <- paste0('SELECT ', fetchField , ' FROM drug_era WHERE')
  first <- TRUE
  if (!missing(drug_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_id)) {
      statement <- paste0(statement, ' drug_era_id IS NULL')
    } else if (is(drug_era_id, 'subQuery')){
      statement <- paste0(statement, ' drug_era_id = (', as.character(drug_era_id), ')')
    } else {
      statement <- paste0(statement, " drug_era_id = '", drug_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(drug_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_start_date)) {
      statement <- paste0(statement, ' drug_era_start_date IS NULL')
    } else if (is(drug_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' drug_era_start_date = (', as.character(drug_era_start_date), ')')
    } else {
      statement <- paste0(statement, " drug_era_start_date = '", drug_era_start_date,"'")
    }
  }

  if (!missing(drug_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_era_end_date)) {
      statement <- paste0(statement, ' drug_era_end_date IS NULL')
    } else if (is(drug_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' drug_era_end_date = (', as.character(drug_era_end_date), ')')
    } else {
      statement <- paste0(statement, " drug_era_end_date = '", drug_era_end_date,"'")
    }
  }

  if (!missing(drug_exposure_count)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_exposure_count)) {
      statement <- paste0(statement, ' drug_exposure_count IS NULL')
    } else if (is(drug_exposure_count, 'subQuery')){
      statement <- paste0(statement, ' drug_exposure_count = (', as.character(drug_exposure_count), ')')
    } else {
      statement <- paste0(statement, " drug_exposure_count = '", drug_exposure_count,"'")
    }
  }

  if (!missing(gap_days)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(gap_days)) {
      statement <- paste0(statement, ' gap_days IS NULL')
    } else if (is(gap_days, 'subQuery')){
      statement <- paste0(statement, ' gap_days = (', as.character(gap_days), ')')
    } else {
      statement <- paste0(statement, " gap_days = '", gap_days,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_condition_era <- function(fetchField, condition_era_id, person_id, condition_concept_id, condition_era_start_date, condition_era_end_date, condition_occurrence_count) {
  statement <- paste0('SELECT ', fetchField , ' FROM condition_era WHERE')
  first <- TRUE
  if (!missing(condition_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_id)) {
      statement <- paste0(statement, ' condition_era_id IS NULL')
    } else if (is(condition_era_id, 'subQuery')){
      statement <- paste0(statement, ' condition_era_id = (', as.character(condition_era_id), ')')
    } else {
      statement <- paste0(statement, " condition_era_id = '", condition_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(condition_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_concept_id)) {
      statement <- paste0(statement, ' condition_concept_id IS NULL')
    } else if (is(condition_concept_id, 'subQuery')){
      statement <- paste0(statement, ' condition_concept_id = (', as.character(condition_concept_id), ')')
    } else {
      statement <- paste0(statement, " condition_concept_id = '", condition_concept_id,"'")
    }
  }

  if (!missing(condition_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_start_date)) {
      statement <- paste0(statement, ' condition_era_start_date IS NULL')
    } else if (is(condition_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' condition_era_start_date = (', as.character(condition_era_start_date), ')')
    } else {
      statement <- paste0(statement, " condition_era_start_date = '", condition_era_start_date,"'")
    }
  }

  if (!missing(condition_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_era_end_date)) {
      statement <- paste0(statement, ' condition_era_end_date IS NULL')
    } else if (is(condition_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' condition_era_end_date = (', as.character(condition_era_end_date), ')')
    } else {
      statement <- paste0(statement, " condition_era_end_date = '", condition_era_end_date,"'")
    }
  }

  if (!missing(condition_occurrence_count)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(condition_occurrence_count)) {
      statement <- paste0(statement, ' condition_occurrence_count IS NULL')
    } else if (is(condition_occurrence_count, 'subQuery')){
      statement <- paste0(statement, ' condition_occurrence_count = (', as.character(condition_occurrence_count), ')')
    } else {
      statement <- paste0(statement, " condition_occurrence_count = '", condition_occurrence_count,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_dose_era <- function(fetchField, dose_era_id, person_id, drug_concept_id, unit_concept_id, dose_value, dose_era_start_date, dose_era_end_date) {
  statement <- paste0('SELECT ', fetchField , ' FROM dose_era WHERE')
  first <- TRUE
  if (!missing(dose_era_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_id)) {
      statement <- paste0(statement, ' dose_era_id IS NULL')
    } else if (is(dose_era_id, 'subQuery')){
      statement <- paste0(statement, ' dose_era_id = (', as.character(dose_era_id), ')')
    } else {
      statement <- paste0(statement, " dose_era_id = '", dose_era_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(drug_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drug_concept_id)) {
      statement <- paste0(statement, ' drug_concept_id IS NULL')
    } else if (is(drug_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drug_concept_id = (', as.character(drug_concept_id), ')')
    } else {
      statement <- paste0(statement, " drug_concept_id = '", drug_concept_id,"'")
    }
  }

  if (!missing(unit_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(unit_concept_id)) {
      statement <- paste0(statement, ' unit_concept_id IS NULL')
    } else if (is(unit_concept_id, 'subQuery')){
      statement <- paste0(statement, ' unit_concept_id = (', as.character(unit_concept_id), ')')
    } else {
      statement <- paste0(statement, " unit_concept_id = '", unit_concept_id,"'")
    }
  }

  if (!missing(dose_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_value)) {
      statement <- paste0(statement, ' dose_value IS NULL')
    } else if (is(dose_value, 'subQuery')){
      statement <- paste0(statement, ' dose_value = (', as.character(dose_value), ')')
    } else {
      statement <- paste0(statement, " dose_value = '", dose_value,"'")
    }
  }

  if (!missing(dose_era_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_start_date)) {
      statement <- paste0(statement, ' dose_era_start_date IS NULL')
    } else if (is(dose_era_start_date, 'subQuery')){
      statement <- paste0(statement, ' dose_era_start_date = (', as.character(dose_era_start_date), ')')
    } else {
      statement <- paste0(statement, " dose_era_start_date = '", dose_era_start_date,"'")
    }
  }

  if (!missing(dose_era_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(dose_era_end_date)) {
      statement <- paste0(statement, ' dose_era_end_date IS NULL')
    } else if (is(dose_era_end_date, 'subQuery')){
      statement <- paste0(statement, ' dose_era_end_date = (', as.character(dose_era_end_date), ')')
    } else {
      statement <- paste0(statement, " dose_era_end_date = '", dose_era_end_date,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_cost <- function(fetchField, cost_id, cost_event_id, cost_domain_id, cost_type_concept_id, currency_concept_id, total_charge, total_cost, total_paid, paid_by_payer, paid_by_patient, paid_patient_copay, paid_patient_coinsurance, paid_patient_deductible, paid_by_primary, paid_ingredient_cost, paid_dispensing_fee, payer_plan_period_id, amount_allowed, revenue_code_concept_id, revenue_code_source_value, drg_concept_id, drg_source_value) {
  statement <- paste0('SELECT ', fetchField , ' FROM cost WHERE')
  first <- TRUE
  if (!missing(cost_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_id)) {
      statement <- paste0(statement, ' cost_id IS NULL')
    } else if (is(cost_id, 'subQuery')){
      statement <- paste0(statement, ' cost_id = (', as.character(cost_id), ')')
    } else {
      statement <- paste0(statement, " cost_id = '", cost_id,"'")
    }
  }

  if (!missing(cost_event_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_event_id)) {
      statement <- paste0(statement, ' cost_event_id IS NULL')
    } else if (is(cost_event_id, 'subQuery')){
      statement <- paste0(statement, ' cost_event_id = (', as.character(cost_event_id), ')')
    } else {
      statement <- paste0(statement, " cost_event_id = '", cost_event_id,"'")
    }
  }

  if (!missing(cost_domain_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_domain_id)) {
      statement <- paste0(statement, ' cost_domain_id IS NULL')
    } else if (is(cost_domain_id, 'subQuery')){
      statement <- paste0(statement, ' cost_domain_id = (', as.character(cost_domain_id), ')')
    } else {
      statement <- paste0(statement, " cost_domain_id = '", cost_domain_id,"'")
    }
  }

  if (!missing(cost_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cost_type_concept_id)) {
      statement <- paste0(statement, ' cost_type_concept_id IS NULL')
    } else if (is(cost_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' cost_type_concept_id = (', as.character(cost_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " cost_type_concept_id = '", cost_type_concept_id,"'")
    }
  }

  if (!missing(currency_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(currency_concept_id)) {
      statement <- paste0(statement, ' currency_concept_id IS NULL')
    } else if (is(currency_concept_id, 'subQuery')){
      statement <- paste0(statement, ' currency_concept_id = (', as.character(currency_concept_id), ')')
    } else {
      statement <- paste0(statement, " currency_concept_id = '", currency_concept_id,"'")
    }
  }

  if (!missing(total_charge)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_charge)) {
      statement <- paste0(statement, ' total_charge IS NULL')
    } else if (is(total_charge, 'subQuery')){
      statement <- paste0(statement, ' total_charge = (', as.character(total_charge), ')')
    } else {
      statement <- paste0(statement, " total_charge = '", total_charge,"'")
    }
  }

  if (!missing(total_cost)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_cost)) {
      statement <- paste0(statement, ' total_cost IS NULL')
    } else if (is(total_cost, 'subQuery')){
      statement <- paste0(statement, ' total_cost = (', as.character(total_cost), ')')
    } else {
      statement <- paste0(statement, " total_cost = '", total_cost,"'")
    }
  }

  if (!missing(total_paid)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(total_paid)) {
      statement <- paste0(statement, ' total_paid IS NULL')
    } else if (is(total_paid, 'subQuery')){
      statement <- paste0(statement, ' total_paid = (', as.character(total_paid), ')')
    } else {
      statement <- paste0(statement, " total_paid = '", total_paid,"'")
    }
  }

  if (!missing(paid_by_payer)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_payer)) {
      statement <- paste0(statement, ' paid_by_payer IS NULL')
    } else if (is(paid_by_payer, 'subQuery')){
      statement <- paste0(statement, ' paid_by_payer = (', as.character(paid_by_payer), ')')
    } else {
      statement <- paste0(statement, " paid_by_payer = '", paid_by_payer,"'")
    }
  }

  if (!missing(paid_by_patient)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_patient)) {
      statement <- paste0(statement, ' paid_by_patient IS NULL')
    } else if (is(paid_by_patient, 'subQuery')){
      statement <- paste0(statement, ' paid_by_patient = (', as.character(paid_by_patient), ')')
    } else {
      statement <- paste0(statement, " paid_by_patient = '", paid_by_patient,"'")
    }
  }

  if (!missing(paid_patient_copay)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_copay)) {
      statement <- paste0(statement, ' paid_patient_copay IS NULL')
    } else if (is(paid_patient_copay, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_copay = (', as.character(paid_patient_copay), ')')
    } else {
      statement <- paste0(statement, " paid_patient_copay = '", paid_patient_copay,"'")
    }
  }

  if (!missing(paid_patient_coinsurance)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_coinsurance)) {
      statement <- paste0(statement, ' paid_patient_coinsurance IS NULL')
    } else if (is(paid_patient_coinsurance, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_coinsurance = (', as.character(paid_patient_coinsurance), ')')
    } else {
      statement <- paste0(statement, " paid_patient_coinsurance = '", paid_patient_coinsurance,"'")
    }
  }

  if (!missing(paid_patient_deductible)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_patient_deductible)) {
      statement <- paste0(statement, ' paid_patient_deductible IS NULL')
    } else if (is(paid_patient_deductible, 'subQuery')){
      statement <- paste0(statement, ' paid_patient_deductible = (', as.character(paid_patient_deductible), ')')
    } else {
      statement <- paste0(statement, " paid_patient_deductible = '", paid_patient_deductible,"'")
    }
  }

  if (!missing(paid_by_primary)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_by_primary)) {
      statement <- paste0(statement, ' paid_by_primary IS NULL')
    } else if (is(paid_by_primary, 'subQuery')){
      statement <- paste0(statement, ' paid_by_primary = (', as.character(paid_by_primary), ')')
    } else {
      statement <- paste0(statement, " paid_by_primary = '", paid_by_primary,"'")
    }
  }

  if (!missing(paid_ingredient_cost)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_ingredient_cost)) {
      statement <- paste0(statement, ' paid_ingredient_cost IS NULL')
    } else if (is(paid_ingredient_cost, 'subQuery')){
      statement <- paste0(statement, ' paid_ingredient_cost = (', as.character(paid_ingredient_cost), ')')
    } else {
      statement <- paste0(statement, " paid_ingredient_cost = '", paid_ingredient_cost,"'")
    }
  }

  if (!missing(paid_dispensing_fee)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(paid_dispensing_fee)) {
      statement <- paste0(statement, ' paid_dispensing_fee IS NULL')
    } else if (is(paid_dispensing_fee, 'subQuery')){
      statement <- paste0(statement, ' paid_dispensing_fee = (', as.character(paid_dispensing_fee), ')')
    } else {
      statement <- paste0(statement, " paid_dispensing_fee = '", paid_dispensing_fee,"'")
    }
  }

  if (!missing(payer_plan_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_id)) {
      statement <- paste0(statement, ' payer_plan_period_id IS NULL')
    } else if (is(payer_plan_period_id, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_id = (', as.character(payer_plan_period_id), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_id = '", payer_plan_period_id,"'")
    }
  }

  if (!missing(amount_allowed)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(amount_allowed)) {
      statement <- paste0(statement, ' amount_allowed IS NULL')
    } else if (is(amount_allowed, 'subQuery')){
      statement <- paste0(statement, ' amount_allowed = (', as.character(amount_allowed), ')')
    } else {
      statement <- paste0(statement, " amount_allowed = '", amount_allowed,"'")
    }
  }

  if (!missing(revenue_code_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(revenue_code_concept_id)) {
      statement <- paste0(statement, ' revenue_code_concept_id IS NULL')
    } else if (is(revenue_code_concept_id, 'subQuery')){
      statement <- paste0(statement, ' revenue_code_concept_id = (', as.character(revenue_code_concept_id), ')')
    } else {
      statement <- paste0(statement, " revenue_code_concept_id = '", revenue_code_concept_id,"'")
    }
  }

  if (!missing(revenue_code_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(revenue_code_source_value)) {
      statement <- paste0(statement, ' revenue_code_source_value IS NULL')
    } else if (is(revenue_code_source_value, 'subQuery')){
      statement <- paste0(statement, ' revenue_code_source_value = (', as.character(revenue_code_source_value), ')')
    } else {
      statement <- paste0(statement, " revenue_code_source_value = '", revenue_code_source_value,"'")
    }
  }

  if (!missing(drg_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drg_concept_id)) {
      statement <- paste0(statement, ' drg_concept_id IS NULL')
    } else if (is(drg_concept_id, 'subQuery')){
      statement <- paste0(statement, ' drg_concept_id = (', as.character(drg_concept_id), ')')
    } else {
      statement <- paste0(statement, " drg_concept_id = '", drg_concept_id,"'")
    }
  }

  if (!missing(drg_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(drg_source_value)) {
      statement <- paste0(statement, ' drg_source_value IS NULL')
    } else if (is(drg_source_value, 'subQuery')){
      statement <- paste0(statement, ' drg_source_value = (', as.character(drg_source_value), ')')
    } else {
      statement <- paste0(statement, " drg_source_value = '", drg_source_value,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_payer_plan_period <- function(fetchField, payer_plan_period_id, person_id, payer_plan_period_start_date, payer_plan_period_end_date, payer_source_value, plan_source_value, family_source_value) {
  statement <- paste0('SELECT ', fetchField , ' FROM payer_plan_period WHERE')
  first <- TRUE
  if (!missing(payer_plan_period_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_id)) {
      statement <- paste0(statement, ' payer_plan_period_id IS NULL')
    } else if (is(payer_plan_period_id, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_id = (', as.character(payer_plan_period_id), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_id = '", payer_plan_period_id,"'")
    }
  }

  if (!missing(person_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(person_id)) {
      statement <- paste0(statement, ' person_id IS NULL')
    } else if (is(person_id, 'subQuery')){
      statement <- paste0(statement, ' person_id = (', as.character(person_id), ')')
    } else {
      statement <- paste0(statement, " person_id = '", person_id,"'")
    }
  }

  if (!missing(payer_plan_period_start_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_start_date)) {
      statement <- paste0(statement, ' payer_plan_period_start_date IS NULL')
    } else if (is(payer_plan_period_start_date, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_start_date = (', as.character(payer_plan_period_start_date), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_start_date = '", payer_plan_period_start_date,"'")
    }
  }

  if (!missing(payer_plan_period_end_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_plan_period_end_date)) {
      statement <- paste0(statement, ' payer_plan_period_end_date IS NULL')
    } else if (is(payer_plan_period_end_date, 'subQuery')){
      statement <- paste0(statement, ' payer_plan_period_end_date = (', as.character(payer_plan_period_end_date), ')')
    } else {
      statement <- paste0(statement, " payer_plan_period_end_date = '", payer_plan_period_end_date,"'")
    }
  }

  if (!missing(payer_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(payer_source_value)) {
      statement <- paste0(statement, ' payer_source_value IS NULL')
    } else if (is(payer_source_value, 'subQuery')){
      statement <- paste0(statement, ' payer_source_value = (', as.character(payer_source_value), ')')
    } else {
      statement <- paste0(statement, " payer_source_value = '", payer_source_value,"'")
    }
  }

  if (!missing(plan_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(plan_source_value)) {
      statement <- paste0(statement, ' plan_source_value IS NULL')
    } else if (is(plan_source_value, 'subQuery')){
      statement <- paste0(statement, ' plan_source_value = (', as.character(plan_source_value), ')')
    } else {
      statement <- paste0(statement, " plan_source_value = '", plan_source_value,"'")
    }
  }

  if (!missing(family_source_value)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(family_source_value)) {
      statement <- paste0(statement, ' family_source_value IS NULL')
    } else if (is(family_source_value, 'subQuery')){
      statement <- paste0(statement, ' family_source_value = (', as.character(family_source_value), ')')
    } else {
      statement <- paste0(statement, " family_source_value = '", family_source_value,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_cdm_source <- function(fetchField, cdm_source_name, cdm_source_abbreviation, cdm_holder, source_description, source_documentation_reference, cdm_etl__reference, source_release_date, cdm_release_date, cdm_version, vocabulary_version) {
  statement <- paste0('SELECT ', fetchField , ' FROM cdm_source WHERE')
  first <- TRUE
  if (!missing(cdm_source_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_source_name)) {
      statement <- paste0(statement, ' cdm_source_name IS NULL')
    } else if (is(cdm_source_name, 'subQuery')){
      statement <- paste0(statement, ' cdm_source_name = (', as.character(cdm_source_name), ')')
    } else {
      statement <- paste0(statement, " cdm_source_name = '", cdm_source_name,"'")
    }
  }

  if (!missing(cdm_source_abbreviation)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_source_abbreviation)) {
      statement <- paste0(statement, ' cdm_source_abbreviation IS NULL')
    } else if (is(cdm_source_abbreviation, 'subQuery')){
      statement <- paste0(statement, ' cdm_source_abbreviation = (', as.character(cdm_source_abbreviation), ')')
    } else {
      statement <- paste0(statement, " cdm_source_abbreviation = '", cdm_source_abbreviation,"'")
    }
  }

  if (!missing(cdm_holder)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_holder)) {
      statement <- paste0(statement, ' cdm_holder IS NULL')
    } else if (is(cdm_holder, 'subQuery')){
      statement <- paste0(statement, ' cdm_holder = (', as.character(cdm_holder), ')')
    } else {
      statement <- paste0(statement, " cdm_holder = '", cdm_holder,"'")
    }
  }

  if (!missing(source_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_description)) {
      statement <- paste0(statement, ' source_description IS NULL')
    } else if (is(source_description, 'subQuery')){
      statement <- paste0(statement, ' source_description = (', as.character(source_description), ')')
    } else {
      statement <- paste0(statement, " source_description = '", source_description,"'")
    }
  }

  if (!missing(source_documentation_reference)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_documentation_reference)) {
      statement <- paste0(statement, ' source_documentation_reference IS NULL')
    } else if (is(source_documentation_reference, 'subQuery')){
      statement <- paste0(statement, ' source_documentation_reference = (', as.character(source_documentation_reference), ')')
    } else {
      statement <- paste0(statement, " source_documentation_reference = '", source_documentation_reference,"'")
    }
  }

  if (!missing(cdm_etl__reference)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_etl__reference)) {
      statement <- paste0(statement, ' [cdm_etl _reference] IS NULL')
    } else if (is(cdm_etl__reference, 'subQuery')){
      statement <- paste0(statement, ' [cdm_etl _reference] = (', as.character(cdm_etl__reference), ')')
    } else {
      statement <- paste0(statement, " [cdm_etl _reference] = '", cdm_etl__reference,"'")
    }
  }

  if (!missing(source_release_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(source_release_date)) {
      statement <- paste0(statement, ' source_release_date IS NULL')
    } else if (is(source_release_date, 'subQuery')){
      statement <- paste0(statement, ' source_release_date = (', as.character(source_release_date), ')')
    } else {
      statement <- paste0(statement, " source_release_date = '", source_release_date,"'")
    }
  }

  if (!missing(cdm_release_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_release_date)) {
      statement <- paste0(statement, ' cdm_release_date IS NULL')
    } else if (is(cdm_release_date, 'subQuery')){
      statement <- paste0(statement, ' cdm_release_date = (', as.character(cdm_release_date), ')')
    } else {
      statement <- paste0(statement, " cdm_release_date = '", cdm_release_date,"'")
    }
  }

  if (!missing(cdm_version)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cdm_version)) {
      statement <- paste0(statement, ' cdm_version IS NULL')
    } else if (is(cdm_version, 'subQuery')){
      statement <- paste0(statement, ' cdm_version = (', as.character(cdm_version), ')')
    } else {
      statement <- paste0(statement, " cdm_version = '", cdm_version,"'")
    }
  }

  if (!missing(vocabulary_version)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(vocabulary_version)) {
      statement <- paste0(statement, ' vocabulary_version IS NULL')
    } else if (is(vocabulary_version, 'subQuery')){
      statement <- paste0(statement, ' vocabulary_version = (', as.character(vocabulary_version), ')')
    } else {
      statement <- paste0(statement, " vocabulary_version = '", vocabulary_version,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_attribute_definition <- function(fetchField, attribute_definition_id, attribute_name, attribute_description, attribute_type_concept_id, attribute_syntax) {
  statement <- paste0('SELECT ', fetchField , ' FROM attribute_definition WHERE')
  first <- TRUE
  if (!missing(attribute_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_definition_id)) {
      statement <- paste0(statement, ' attribute_definition_id IS NULL')
    } else if (is(attribute_definition_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_definition_id = (', as.character(attribute_definition_id), ')')
    } else {
      statement <- paste0(statement, " attribute_definition_id = '", attribute_definition_id,"'")
    }
  }

  if (!missing(attribute_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_name)) {
      statement <- paste0(statement, ' attribute_name IS NULL')
    } else if (is(attribute_name, 'subQuery')){
      statement <- paste0(statement, ' attribute_name = (', as.character(attribute_name), ')')
    } else {
      statement <- paste0(statement, " attribute_name = '", attribute_name,"'")
    }
  }

  if (!missing(attribute_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_description)) {
      statement <- paste0(statement, ' attribute_description IS NULL')
    } else if (is(attribute_description, 'subQuery')){
      statement <- paste0(statement, ' attribute_description = (', as.character(attribute_description), ')')
    } else {
      statement <- paste0(statement, " attribute_description = '", attribute_description,"'")
    }
  }

  if (!missing(attribute_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_type_concept_id)) {
      statement <- paste0(statement, ' attribute_type_concept_id IS NULL')
    } else if (is(attribute_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' attribute_type_concept_id = (', as.character(attribute_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " attribute_type_concept_id = '", attribute_type_concept_id,"'")
    }
  }

  if (!missing(attribute_syntax)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(attribute_syntax)) {
      statement <- paste0(statement, ' attribute_syntax IS NULL')
    } else if (is(attribute_syntax, 'subQuery')){
      statement <- paste0(statement, ' attribute_syntax = (', as.character(attribute_syntax), ')')
    } else {
      statement <- paste0(statement, " attribute_syntax = '", attribute_syntax,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}

lookup_cohort_definition <- function(fetchField, cohort_definition_id, cohort_definition_name, cohort_definition_description, definition_type_concept_id, cohort_definition_syntax, subject_concept_id, cohort_instantiation_date) {
  statement <- paste0('SELECT ', fetchField , ' FROM cohort_definition WHERE')
  first <- TRUE
  if (!missing(cohort_definition_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_id)) {
      statement <- paste0(statement, ' cohort_definition_id IS NULL')
    } else if (is(cohort_definition_id, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_id = (', as.character(cohort_definition_id), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_id = '", cohort_definition_id,"'")
    }
  }

  if (!missing(cohort_definition_name)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_name)) {
      statement <- paste0(statement, ' cohort_definition_name IS NULL')
    } else if (is(cohort_definition_name, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_name = (', as.character(cohort_definition_name), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_name = '", cohort_definition_name,"'")
    }
  }

  if (!missing(cohort_definition_description)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_description)) {
      statement <- paste0(statement, ' cohort_definition_description IS NULL')
    } else if (is(cohort_definition_description, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_description = (', as.character(cohort_definition_description), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_description = '", cohort_definition_description,"'")
    }
  }

  if (!missing(definition_type_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(definition_type_concept_id)) {
      statement <- paste0(statement, ' definition_type_concept_id IS NULL')
    } else if (is(definition_type_concept_id, 'subQuery')){
      statement <- paste0(statement, ' definition_type_concept_id = (', as.character(definition_type_concept_id), ')')
    } else {
      statement <- paste0(statement, " definition_type_concept_id = '", definition_type_concept_id,"'")
    }
  }

  if (!missing(cohort_definition_syntax)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_definition_syntax)) {
      statement <- paste0(statement, ' cohort_definition_syntax IS NULL')
    } else if (is(cohort_definition_syntax, 'subQuery')){
      statement <- paste0(statement, ' cohort_definition_syntax = (', as.character(cohort_definition_syntax), ')')
    } else {
      statement <- paste0(statement, " cohort_definition_syntax = '", cohort_definition_syntax,"'")
    }
  }

  if (!missing(subject_concept_id)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(subject_concept_id)) {
      statement <- paste0(statement, ' subject_concept_id IS NULL')
    } else if (is(subject_concept_id, 'subQuery')){
      statement <- paste0(statement, ' subject_concept_id = (', as.character(subject_concept_id), ')')
    } else {
      statement <- paste0(statement, " subject_concept_id = '", subject_concept_id,"'")
    }
  }

  if (!missing(cohort_instantiation_date)) {
    if (first) {
      first <- FALSE
    } else {
      statement <- paste0(statement, ' AND')
    }
    if (is.null(cohort_instantiation_date)) {
      statement <- paste0(statement, ' cohort_instantiation_date IS NULL')
    } else if (is(cohort_instantiation_date, 'subQuery')){
      statement <- paste0(statement, ' cohort_instantiation_date = (', as.character(cohort_instantiation_date), ')')
    } else {
      statement <- paste0(statement, " cohort_instantiation_date = '", cohort_instantiation_date,"'")
    }
  }

  class(statement) <- 'subQuery'
  return(statement)
}


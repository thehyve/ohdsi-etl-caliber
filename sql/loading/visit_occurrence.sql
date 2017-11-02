
INSERT INTO cdm5.visit_occurrence
(
  visit_occurrence_id,
  person_id,
  care_site_id,
  visit_start_date,
  visit_end_date,
  visit_concept_id,
  visit_source_value,
  provider_id,
  visit_type_concept_id
)
  SELECT
    -- Only one visit per patient and date
    DISTINCT ON (consultation.patid, consultation.eventdate)

    -- Patid * 10000000000  + (year(eventdate)*10000)  + (month(eventdate)* 100)  + day(eventdate)
    createVisitId(consultation.patid, consultation.eventdate) AS visit_occurrence_id,

    consultation.patid                                        AS person_id,

    patient.pracid                                            AS care_site_id,

    consultation.eventdate                                    AS visit_start_date,

    consultation.eventdate                                    AS visit_end_date,

    -- 'Outpatient Visit'
    9202                                                      AS visit_concept_id,

    'consultation'                                            AS visit_source_value,

    consultation.staffid                                      AS provider_id,

    44818518                                                  AS visit_type_concept_id -- 'Visit derived from EHR record'

  FROM caliber.consultation AS consultation
    JOIN caliber.patient AS patient
      ON consultation.patid = patient.patid
  WHERE consultation.eventdate IS NOT NULL

  UNION ALL

  SELECT DISTINCT ON (hes_diag_hosp.patid)
    hes_diag_hosp.spno                                         AS visit_occurrence_id,

    hes_diag_hosp.patid                                        AS person_id,

    patient.pracid                                             AS care_site_id,

    hes_diag_hosp.admidate                                     AS visit_start_date,

    -- Use start date if no end date available
    coalesce(hes_diag_hosp.discharged, hes_diag_hosp.admidate) AS visit_end_date,

    9201                                                       AS visit_concept_id,

    -- Inpatient visit
    'hes_diag_hosp'                                            AS visit_source_value,

    NULL                                                       AS provider_id,

    44818518                                                   AS visit_type_concept_id -- Visit derived from EHR record

  FROM caliber.hes_diag_hosp AS hes_diag_hosp
    JOIN caliber.patient AS patient
      ON hes_diag_hosp.patid = patient.patid
  WHERE hes_diag_hosp.admidate IS NOT NULL;

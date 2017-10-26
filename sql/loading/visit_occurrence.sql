
INSERT INTO cdm5.visit_occurrence
(
 visit_occurrence_id,
 person_id,
 visit_start_date,
 visit_end_date,
 visit_concept_id, -- When from CPRD.patient: ‘9202’ - Outpatient Visit When from HES_diag_hosp: ‘9201’ - Inpatient Visit
 provider_id,
 visit_type_concept_id, -- ‘44818518’ ‘Visit derived from EHR record’
 visit_source_concept_id,
 admitting_source_value,
 discharge_to_concept_id,
 discharge_to_source_value,
 preceding_visit_occurrence_id,
 visit_start_datetime,
 visit_end_datetime,
 care_site_id,
 visit_source_value
);

SELECT
  cons.patid,
  cons.voi,
  cons.eventdate AS start_date,
  cons.eventdate AS end_date,
  cons.visit_concept_id,
  44818518       AS visit_type_concept_id,
  -- Visit derived from EHR record
  cons.provider_id,
  cons.source_val
FROM (
       SELECT
         patid,
         patid * 10000000000 + (EXTRACT(YEAR FROM eventdate) * 10000) +
         (EXTRACT(MONTH FROM eventdate) * 100) + EXTRACT(DAY FROM eventdate) AS voi,
         eventdate,
         9202                                                                AS visit_concept_id,
         CASE
         WHEN staffid = 0
           THEN NULL
         ELSE staffid
         END                                                                 AS provider_id,
         patid :: TEXT || ':' || eventdate                                   AS source_val
       FROM caliber.consultation
     ) cons
UNION
SELECT
  hdh.patid,
  hdh.voi,
  hdh.admidate   AS start_date,
  hdh.discharged AS end_date,
  hdh.visit_concept_id,
  44818518       AS visit_type_concept_id,
  -- Visit derived from EHR record
  NULL           AS provider_id,
  hdh.source_val
FROM (
       SELECT
         patid,
         spno         AS voi,
         admidate,
         discharged,
         9201         AS visit_concept_id,
         spno :: TEXT AS source_val
       FROM caliber.hes_diag_hosp
     ) hdh;


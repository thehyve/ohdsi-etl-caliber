/*
Merge and process all tables with medical codes.
Every row is loaded into the condition, procedure, drug, device, observation or measurement table,
depending on the domain of the target standard concept and source (READ) concept.
This script assigns a visit id and does the concept mapping.

It is possible that one row maps to multiple tables, e.g. vaccinations map both to a procedure and a drug.
Note: for 30k records, this query takes ~3 seconds in the dev environment
*/
DROP TABLE IF EXISTS public.medcode_merge;
WITH medcode_merge (patid, eventdate, constype, consid, medcode, staffid, source_table) AS (
  SELECT patid, eventdate, constype, consid, medcode, staffid, 'clinical'
  FROM caliber.clinical

  UNION ALL

  SELECT patid, eventdate, constype, consid, medcode, staffid, 'referral'
  FROM caliber.referral

  UNION ALL

  SELECT patid, eventdate, constype, consid, medcode, staffid, 'test'
  FROM caliber.test

  UNION ALL

  SELECT patid, eventdate, constype, consid, medcode, staffid, 'immunisation'
  FROM caliber.immunisation
)
SELECT
  medcode_merge.patid AS person_id,

  medcode_merge.eventdate AS _start_date,

  medcode_merge.eventdate AS _start_datetime,

  CASE
    WHEN createvisitid(medcode_merge.patid, medcode_merge.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
    THEN createvisitid(medcode_merge.patid, medcode_merge.eventdate)
    ELSE NULL
  END AS visit_occurrence_id,

  -- Use medcode lookup to convert to read codes
  -- Join medcode onto ‘Medical’ file to get read source codes (field name ‘read_code’).   Use target_concept_id from  SOURCE to STANDARD vocab query with: Source_vocabulary_id=’Read’ Target_domain_id=’Condition’ Target_invalid_reason=NULL  For HES tables: Use target_concept_id from SOURCE to STANDARD vocab query with: Source_vocabulary_id=’ICD10’ Target_domain_id=’Condition’ Target_invalid_reason=NULL
  coalesce(target_concept.concept_id,0)	AS _concept_id,

  medcode_merge.staffid AS provider_id,

  -- From join of medical table on cdm5 concept table
  source_concept.concept_id AS _source_concept_id,

  medcode_merge.medcode AS _source_value,

  /** Following tables have different logic depending on the domain **/
  medcode_merge.source_table AS source_table,

  source_concept.domain_id AS source_domain_id,

  target_concept.domain_id AS target_domain_id

INTO public.medcode_merge

FROM medcode_merge
LEFT JOIN caliber.medical AS medical
  ON medcode_merge.medcode = medical.medcode
LEFT JOIN cdm5.concept AS source_concept
  ON medical.readcode = source_concept.concept_code
  AND source_concept.vocabulary_id = 'Read'
LEFT JOIN cdm5.concept_relationship AS mapping
  ON source_concept.concept_id = mapping.concept_id_1
  AND mapping.relationship_id = 'Maps to'
LEFT JOIN cdm5.concept as target_concept
  ON mapping.concept_id_2 = target_concept.concept_id
  AND target_concept.invalid_reason IS NULL
WHERE eventdate IS NOT NULL AND medcode_merge.medcode > 0
;



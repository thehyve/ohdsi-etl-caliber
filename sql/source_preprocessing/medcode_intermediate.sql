/*
Merge and process all tables with medical codes.
Every row is loaded into the condition, procedure, drug, device, observation or measurement table,
depending on the domain of the target standard concept and source (READ) concept.
This script assigns a visit id and does the concept mapping.

It is possible that one row maps to multiple tables, e.g. vaccinations map both to a procedure and a drug.
Note: for 30k records, this query takes ~3 seconds in the dev environment
TODO: create indices on this table (source_table, target_domain_id, source_domain_id)
*/
DROP TABLE IF EXISTS public.medcode_intermediate;

WITH medcode_union (patid, eventdate, constype, consid, medcode, staffid, status, source_table) AS (
  SELECT patid, eventdate, constype, consid, medcode, staffid, NULL, 'clinical'
  FROM @source_schema.clinical

  UNION ALL

  SELECT patid, eventdate, constype, consid, medcode, staffid, NULL, 'referral'
  FROM @source_schema.referral

  UNION ALL

  SELECT patid, eventdate, constype, consid, medcode, staffid, NULL, 'test'
  FROM @source_schema.test

  UNION ALL

  SELECT patid, eventdate, constype, consid, medcode, staffid, status::varchar, 'immunisation'
  FROM @source_schema.immunisation
)
SELECT
  medcode_union.patid AS person_id,

  medcode_union.eventdate AS _start_date,

  medcode_union.eventdate :: TIMESTAMP AS _start_datetime,

  createvisitid(medcode_union.patid, medcode_union.eventdate) AS visit_occurrence_id,

  -- Use medcode lookup to convert to read codes
  -- Join medcode onto ‘Medical’ file to get read source codes (field name ‘read_code’).   Use target_concept_id from  SOURCE to STANDARD vocab query with: Source_vocabulary_id=’Read’ Target_domain_id=’Condition’ Target_invalid_reason=NULL  For HES tables: Use target_concept_id from SOURCE to STANDARD vocab query with: Source_vocabulary_id=’ICD10’ Target_domain_id=’Condition’ Target_invalid_reason=NULL
  coalesce(target_concept_id,0)	AS _concept_id,

  medcode_union.staffid AS provider_id,

  -- From join of medical table on cdm5 concept table
  read_map.source_concept_id AS _source_concept_id,

  medcode_union.medcode AS _source_value,

  medcode_union.source_table AS source_table,

  -- domain_id from standard concept or, if unavailable, from source READ concept
  coalesce(target_domain_id, source_domain_id) AS target_domain_id,

  -- null if not from immunisation file
  medcode_union.status AS immunisation_status

INTO public.medcode_intermediate

FROM medcode_union
  LEFT JOIN @source_schema.medical AS medical
    ON medcode_union.medcode = medical.medcode
  LEFT JOIN cdm5.source_to_target AS read_map
    ON medical.readcode = read_map.source_code
    AND read_map.source_vocabulary_id = 'Read'
-- Only include rows with date, and valid medcode
WHERE medcode_union.eventdate IS NOT NULL AND medcode_union.medcode > 0
;


INSERT INTO cdm5.observation
(
  person_id,
  observation_date,
  observation_datetime,
  provider_id,
  visit_occurrence_id,
  observation_type_concept_id,
  observation_concept_id,
  observation_source_value,
  value_as_number,
  value_as_concept_id,
  value_as_string,
  unit_concept_id,
  unit_source_value
)
  SELECT
    clinical.patid AS person_id,

    clinical.eventdate AS observation_date,

    clinical.eventdate :: TIMESTAMP AS observation_datetime,

    clinical.staffid AS provider_id,

    CASE
    WHEN createvisitid(additional_int.patid, clinical.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
      THEN createvisitid(additional_int.patid, clinical.eventdate)
    ELSE NULL
    END AS visit_occurrence_id,

    -- Patient reported value
    44818704 AS observation_type_concept_id,

    coalesce(enttype_map.target_concept_id, 0) AS observation_concept_id,

    additional_int.enttype_string AS observation_source_value,

    -- Numeric value
    additional_int.data_value as value_as_string,

    -- TODO: expand e.g. with yes/no
    CASE cprd_lookup.description
      WHEN 'Not examined'       THEN 4301433
      WHEN 'Potential Abnormal' THEN 40567538
      WHEN 'Present'            THEN 4181412
      WHEN 'Unknown'            THEN 4129922
      WHEN 'Normal'             THEN 4069590
      WHEN 'Absent'             THEN 4132135
      WHEN 'Abnormal'           THEN 4135493
      WHEN 'A'                  THEN 4008253
      WHEN 'A+'                 THEN 4082948
      WHEN 'A-'                 THEN 4080397
      WHEN 'B'                  THEN 4009006
      WHEN 'B+'                 THEN 4175555
      WHEN 'B-'                 THEN 4080398
      WHEN 'O'                  THEN 4237761
      WHEN 'O+'                 THEN 4080395
      WHEN 'O-'                 THEN 4082947
      WHEN 'AB'                 THEN 4013993
      WHEN 'AB+'                THEN 4080396
      WHEN 'AB-'                THEN 4082949
      WHEN 'Rhesus +'           THEN 4013995
      WHEN 'Rhesus -'           THEN 4013540
    END AS value_as_concept_id,

    -- Description of the code belonging to that lookup type
    cprd_lookup.description AS value_source_value,

    -- TODO: unit in rare cases also given
    NULL AS unit_concept_id,
    NULL AS unit_source_value

  FROM public.additional_intermediate as additional_int
  LEFT JOIN caliber_real.clinical AS clinical
    ON clinical.adid =  additional_int.adid
  LEFT JOIN cdm5.source_to_concept_map AS enttype_map
      ON enttype_map.source_code = additional_int.enttype_string AND
         enttype_map.source_vocabulary_id = 'JNJ_CPRD_ET_LOINC'
  LEFT JOIN cdm5.concept AS target_concept
      ON target_concept.concept_id = enttype_map.target_concept_id
  LEFT JOIN public.cprd_lookup AS cprd_lookup
      ON cprd_lookup.lookup_type = additional_int.lookup_type AND
         cprd_lookup.code = additional_int.data_code
  -- TODO: medcode and prodcode
  WHERE clinical.eventdate IS NOT NULL AND (target_concept.domain_id = 'Observation' OR target_concept_id IS NULL)
;
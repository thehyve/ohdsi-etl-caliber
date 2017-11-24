
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

    CASE WHEN additional_int.enttype_string LIKE '372%'
      -- Patient reported
      THEN 44814721
      -- Observation recorded from EHR
      ELSE 38000280
    END AS observation_type_concept_id,

    coalesce(enttype_map.target_concept_id, 0) AS observation_concept_id,

    additional_int.enttype_string AS observation_source_value,

    -- Numeric value
    additional_int.data_value as value_as_number,

    -- Mapping of the description
    mapCprdLookup(cprd_lookup.description) AS value_as_concept_id,

    -- Description of the code belonging to that lookup type, or if no data_code present, the date or lookup_type
    coalesce(
        cprd_lookup.description,
        additional_int.data_code :: TEXT,
        additional_int.data_date :: TEXT,
        additional_int.lookup_type
    ) AS value_as_string,

    unit_map.target_concept_id AS unit_concept_id,

    additional_int.unit_code AS unit_source_value

  FROM public.additional_intermediate as additional_int
  LEFT JOIN caliber_real.clinical AS clinical
    ON clinical.adid =  additional_int.adid
  LEFT JOIN cdm5.source_to_concept_map AS enttype_map
      ON enttype_map.source_code = additional_int.enttype_string AND
         enttype_map.source_vocabulary_id IN ('JNJ_CPRD_ET_LOINC','JNJ_CPRD_SCORE_LOINC')
  LEFT JOIN cdm5.concept AS target_concept
      ON target_concept.concept_id = enttype_map.target_concept_id
  LEFT JOIN public.cprd_lookup AS cprd_lookup
      ON cprd_lookup.lookup_type = additional_int.lookup_type AND
         cprd_lookup.code = additional_int.data_code
  LEFT JOIN cdm5.source_to_concept_map AS unit_map
      ON unit_map.source_code = additional_int.unit_code AND
         unit_map.source_vocabulary_id = 'CPRD_UNIT'
  WHERE clinical.eventdate IS NOT NULL AND
        (target_concept.domain_id = 'Observation'
         OR enttype_map.target_concept_id IS NULL
         OR enttype_map.target_concept_id = 0)
;
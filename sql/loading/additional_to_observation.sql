
INSERT INTO cdm5.observation
(
  person_id,
  observation_date,
  observation_datetime,
  provider_id,
  visit_occurrence_id,
  observation_type_concept_id,
  observation_concept_id,
  observation_source_concept_id,
  observation_source_value,
  value_as_number,
  value_as_concept_id,
  value_as_string,
  unit_concept_id,
  unit_source_value
)
  SELECT
    additional.patid AS person_id,

    clinical.eventdate AS observation_date,

    clinical.eventdate :: TIMESTAMP AS observation_datetime,

    provider.provider_id AS provider_id,

    createvisitid(additional.patid, clinical.eventdate),
--     CASE
--     WHEN createvisitid(additional.patid, clinical.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
--       THEN createvisitid(additional.patid, clinical.eventdate)
--     ELSE NULL
--     END AS visit_occurrence_id,

    -- Score entity is a patient reported value, others regular observations
    CASE left(additional.enttype_string,3)
      -- Patient reported
      WHEN '372' THEN 44814721
      -- Observation recorded from EHR
      ELSE 38000280
    END AS observation_type_concept_id,

    coalesce(enttype_map.target_concept_id, 0) AS observation_concept_id,

    enttype_map.source_concept_id AS observation_source_concept_id,

    additional.enttype_string AS observation_source_value,

    -- Numeric value
    additional.data_value as value_as_number,

    -- Mapping of the description. NULL if no mapping available
    mapCprdLookup(cprd_lookup.description) AS value_as_concept_id,

    -- Description of the code belonging to that lookup type, product code or medical code.
    -- If no description available, insert date. If date field empty, then insert datafield name.
    substr(
      coalesce(
          cprd_lookup.description,
          product.productname,
          medical.readterm,
          additional.data_date :: TEXT,
          additional.datafield_name
      ),
      0,
      60
    ) AS value_as_string,

    unit_map.target_concept_id AS unit_concept_id,

    additional.unit_code AS unit_source_value

  FROM public.additional_intermediate AS additional
    JOIN @source_schema.clinical AS clinical USING(adid, patid)
    LEFT JOIN cdm5.provider ON clinical.staffid = provider_id
    LEFT JOIN cdm5.source_to_target AS enttype_map
        ON enttype_map.source_code = additional.enttype_string AND
           enttype_map.source_vocabulary_id IN ('JNJ_CPRD_ET_LOINC','JNJ_CPRD_SCORE_LOINC')
    LEFT JOIN @source_schema.auxiliary_lookups AS cprd_lookup
        ON cprd_lookup.lookup_type = additional.lookup_type AND
           cprd_lookup.code = additional.data_code
    LEFT JOIN cdm5.source_to_target AS unit_map
        ON unit_map.source_code = additional.unit_code AND
           unit_map.source_vocabulary_id = 'CPRD_UNIT'
    LEFT JOIN @source_schema.product AS product
        ON additional.lookup_type = 'Product Dictionary' AND
           additional.data_code = product.prodcode :: TEXT
    LEFT JOIN @source_schema.medical AS medical
        ON additional.lookup_type = 'Medical Dictionary' AND
           additional.data_code = medical.medcode :: TEXT
  WHERE clinical.eventdate IS NOT NULL AND
        -- Rows not loaded in other tables
        (enttype_map.target_domain_id != 'Measurement' OR enttype_map.target_domain_id IS NULL)
;
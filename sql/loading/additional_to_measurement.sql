
INSERT INTO cdm5.measurement
(
  person_id,
  measurement_date,
  measurement_datetime,
  provider_id,
  visit_occurrence_id,
  measurement_type_concept_id,
  measurement_concept_id,
  measurement_source_concept_id,
  measurement_source_value,
  value_as_number,
  value_as_concept_id,
  value_source_value,
  unit_concept_id,
  unit_source_value
)
  SELECT
    additional.patid AS person_id,

    clinical.eventdate AS measurement_date,

    clinical.eventdate :: TIMESTAMP AS measurement_datetime,

    provider.provider_id AS provider_id,

    createvisitid(additional.patid, clinical.eventdate),
--     CASE
--     WHEN createvisitid(additional.patid, clinical.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
--       THEN createvisitid(additional.patid, clinical.eventdate)
--     ELSE NULL
--     END AS visit_occurrence_id,

    -- Score entity is a patient reported value, others a lab result
    CASE left(additional.enttype_string,3)
      -- Patient Reported Value
      WHEN '372' THEN 44818704
      -- Lab Result
      ELSE 44818702
    END AS measurement_type_concept_id,

    coalesce(enttype_map.target_concept_id, 0) AS measurement_concept_id,

    enttype_map.source_concept_id AS measurement_source_concept_id,

    additional.enttype_string AS measurement_source_value,

    -- Numeric value
    additional.data_value as value_as_number,

    -- Mapping of the description. NULL if no mapping available
    mapCprdLookup(cprd_lookup.description) AS value_as_concept_id,

    -- Description of the code belonging to that lookup type, product code or medical code.
    -- If no description available, insert date. If date field empty, then insert datafield name.
    -- Trim to 50 characters
    substr(
      coalesce(
          cprd_lookup.description,
          product.productname,
          medical.readterm,
          additional.data_date :: TEXT,
          additional.datafield_name
      ),
      0,
      50
    ) AS value_source_value,

    -- Hard coded units for specific entities
    CASE additional.enttype_string
        WHEN '1-1' THEN 8876 -- diastolic pressure in mmHG
        WHEN '1-2' THEN 8876 -- systolic pressure in mmHG
        WHEN '13-1' THEN 9529 -- weight in kg
        WHEN '13-3' THEN 9531 -- BMI in kg/m2
        WHEN '14-1' THEN 9546 -- height in m
        ELSE unit_map.target_concept_id
    END AS unit_concept_id,

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
  WHERE clinical.eventdate IS NOT NULL AND enttype_map.target_domain_id = 'Measurement'
;
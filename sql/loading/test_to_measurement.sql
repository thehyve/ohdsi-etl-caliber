/*
Contains mostly lab test result values
*/
INSERT INTO cdm5.measurement
(
  person_id,
  provider_id,
  visit_occurrence_id,
  measurement_date,
  measurement_datetime,
  measurement_type_concept_id,
  measurement_concept_id,
  measurement_source_concept_id,
  measurement_source_value,
  operator_concept_id,
  value_as_number,
  value_as_concept_id,
  value_source_value,
  unit_concept_id,
  unit_source_value,
  range_low,
  range_high
)
  SELECT
    test_intermediate.patid AS person_id,

    provider.provider_id AS provider_id,

    createvisitid(test_intermediate.patid, test_intermediate.eventdate),
--     CASE
--       WHEN createvisitid(test_intermediate.patid, test_intermediate.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
--       THEN createvisitid(test_intermediate.patid, test_intermediate.eventdate)
--       ELSE NULL
--     END AS visit_occurrence_id,

    test_intermediate.eventdate AS measurement_date,

    test_intermediate.eventdate :: TIMESTAMP  AS measurement_datetime,

    -- 'Lab Result'
    44818702 AS measurement_type_concept_id,

    coalesce(test_intermediate._concept_id,0) AS measurement_concept_id,

    test_intermediate._source_concept_id AS measurement_source_concept_id,

    test_intermediate.enttype_string AS measurement_source_value,

    -- NOTE: no _source_value field of the operator
    test_intermediate.operator_concept_id AS operator_concept_id,

    test_intermediate.value AS value_as_number,

    CASE
      WHEN test_intermediate.qualifier_source_value IS NULL THEN NULL
      ELSE coalesce(test_intermediate.qualifier_concept_id, 0)
    END AS value_as_concept_id,

    -- Alternative source value is either the data7 or data8 of entity type 311 or 154 or 285
    coalesce(test_intermediate.qualifier_source_value, test_intermediate.alternative_source_value) AS value_source_value,

    CASE
      WHEN test_intermediate.unit_source_value IS NULL THEN NULL
      ELSE coalesce(test_intermediate.unit_concept_id, 0)
    END AS unit_concept_id,

    test_intermediate.unit_source_value AS unit_source_value,

    test_intermediate.range_from AS range_low,

    test_intermediate.range_to AS range_high

  FROM public.test_intermediate AS test_intermediate
    LEFT JOIN cdm5.provider ON test_intermediate.staffid = provider_id
  -- All rows not captured in other tables
  WHERE target_domain_id != 'Observation' OR target_domain_id IS NULL
;
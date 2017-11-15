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

    coalesce(test_intermediate.staffid, 0) AS provider_id,

    CASE
      WHEN createvisitid(test_intermediate.patid, test_intermediate.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
      THEN createvisitid(test_intermediate.patid, test_intermediate.eventdate)
      ELSE NULL
    END AS visit_occurrence_id,

    test_intermediate.eventdate AS measurement_date,

    test_intermediate.eventdate :: TIMESTAMP  AS measurement_datetime,

    -- 'Lab Result'
    44818702 AS measurement_type_concept_id,

    coalesce(test_intermediate._concept_id,0) AS measurement_concept_id,

    test_intermediate.enttype_string AS measurement_source_value,

    -- NOTE: no _source_value of the operator
    test_intermediate.operator_concept_id AS operator_concept_id,

    test_intermediate.value AS value_as_number,

    coalesce(test_intermediate.qualifier_concept_id, 0) AS value_as_concept_id,

    coalesce(test_intermediate.qualifier_source_value, test_intermediate.alternative_source_value) AS value_source_value,

    test_intermediate.unit_concept_id AS unit_concept_id,

    test_intermediate.unit_source_value AS unit_source_value,

    test_intermediate.range_from AS range_low,

    test_intermediate.range_to AS range_high

  FROM public.test_intermediate AS test_intermediate
  WHERE test_intermediate.eventdate IS NOT NULL AND
        (test_intermediate.target_domain_id = 'Measurement' OR
         test_intermediate.target_domain_id = 'None' OR
         test_intermediate.target_domain_id IS NULL)
;
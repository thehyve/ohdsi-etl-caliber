/*
Contains mostly lab test result values
*/
INSERT INTO cdm5.observation
(
  person_id,
  provider_id,
  visit_occurrence_id,
  observation_date,
  observation_datetime,
  observation_type_concept_id,
  observation_concept_id,
  observation_source_value,
  value_as_number,
  qualifier_concept_id,
  qualifier_source_value,
  unit_concept_id,
  unit_source_value
)
  SELECT
    test_intermediate.patid AS person_id,

    coalesce(test_intermediate.staffid, 0) AS provider_id,

    CASE
      WHEN createvisitid(test_intermediate.patid, test_intermediate.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
      THEN createvisitid(test_intermediate.patid, test_intermediate.eventdate)
      ELSE NULL
    END AS visit_occurrence_id,

    test_intermediate.eventdate AS observation_date,

    test_intermediate.eventdate :: TIMESTAMP  AS observation_datetime,

    -- 'Lab observation text'
    38000278 AS observation_type_concept_id,

    coalesce(test_intermediate._concept_id, 0) AS observation_concept_id,

    test_intermediate.enttype_string AS observation_source_value,

    test_intermediate.value AS value_as_number,

    CASE
      WHEN test_intermediate.qualifier_source_value IS NULL THEN NULL
      ELSE coalesce(test_intermediate.qualifier_concept_id, 0)
    END AS qualifier_concept_id,

    test_intermediate.qualifier_source_value AS qualifier_source_value,

    CASE
      WHEN test_intermediate.unit_source_value IS NULL THEN NULL
      ELSE coalesce(test_intermediate.unit_concept_id, 0)
    END AS unit_concept_id,

    test_intermediate.unit_source_value AS unit_source_value

  FROM public.test_intermediate AS test_intermediate
  WHERE target_domain_id = 'Observation'
;
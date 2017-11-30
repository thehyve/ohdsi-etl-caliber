/*
Family identifier
 */
INSERT INTO cdm5.measurement
(
  person_id,
  measurement_date,
  measurement_datetime,
  measurement_concept_id,
  measurement_source_value,
  measurement_type_concept_id,
  value_as_number
)
  SELECT
    patient.patid AS person_id,

    -- Date the patient’s current period of registration with the practice began (date of the first ‘permanent’ record after the latest transferred out period). If there are no ‘transferred out periods’, the date is equal to ‘frd’
    patient.crd AS measurement_date,

    patient.crd :: TIMESTAMP AS measurement_datetime,

    -- Family member identifier
    44786681 AS measurement_concept_id,

    'famnum' AS measurement_source_value,

    -- Derived value
    45754907 AS measurement_type_concept_id,

    patient.famnum AS value_as_number

  FROM @source_schema.patient
;

INSERT INTO cdm5.observation
(
  person_id,
  observation_date,
  observation_datetime,
  observation_concept_id,
  observation_source_value,
  observation_type_concept_id,
  value_as_concept_id
)
  SELECT
    patient.patid AS person_id,

    -- Date the patient’s current period of registration with the practice began (date of the first ‘permanent’ record after the latest transferred out period). If there are no ‘transferred out periods’, the date is equal to ‘frd’
    patient.crd AS observation_date,

    patient.crd :: TIMESTAMP AS observation_datetime,

    -- Marital status
    CASE patient.marital
      WHEN 1 THEN 4053842 -- Single Person
      WHEN 2 THEN 4338692 -- Married
      WHEN 3 THEN 4143188 -- Widowed
      WHEN 4 THEN 4069297 -- Divorced
      WHEN 5 THEN 4027529 -- Separated
      WHEN 6 THEN 4052929 -- Martial state unknown
      WHEN 7 THEN 4204399 -- Engaged to be married
      WHEN 8 THEN 4242253 -- Cohabiting
      WHEN 9 THEN 4150598 -- Remarried
      WHEN 10 THEN 4135263 -- Partner in relationship
      WHEN 11 THEN 4135263 -- Partner in relationship
      ELSE 0
    END AS observation_concept_id,

    CASE patient.marital
      WHEN 1 THEN 'Single'
      WHEN 2 THEN 'Married'
      WHEN 3 THEN 'Widowed'
      WHEN 4 THEN 'Divorced'
      WHEN 5 THEN 'Separated'
      WHEN 6 THEN 'Unknown'
      WHEN 7 THEN 'Engaged'
      WHEN 8 THEN 'Co-habitating'
      WHEN 9 THEN 'Remarried'
      WHEN 10 THEN 'Stable Relationship'
      WHEN 11 THEN 'Civil Partnership'
    END AS observation_source_value,

    -- Patient reported value
    44818704 AS observation_type_concept_id,

    -- Yes
    4188539 AS value_as_concept_id

  FROM @source_schema.patient AS patient
  -- Exclude 'Data Not Entered'
  WHERE patient.marital != 0 AND patient.crd IS NOT NULL
;
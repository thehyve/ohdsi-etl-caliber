/*
Index of Multiple Deprivation (IMD) quintile or decile (where 1 is most deprived)
*/
INSERT INTO cdm5.measurement
(
  person_id,
  measurement_date,
  measurement_datetime,
  measurement_concept_id,
  measurement_source_value,
  measurement_type_concept_id,
  value_as_number,
  range_low,
  range_high
)
  SELECT
    ons_imd.patid AS person_id,

    -- Current registration date as measurement date
    patient.crd AS measurement_date,

    patient.crd :: TIMESTAMP AS measurement_date,

    -- Economic status (SNOMED)
    4249447 AS measurement_concept_id,

    'index_of_multiple_deprivation' AS measurement_source_value,

    -- Derived value
    45754907 AS measurement_type_concept_id,

    ons_imd.imd AS value_as_number,

    -- Ranges of this dataset
    (SELECT min(ons_imd.imd) FROM @source_schema.ons_imd) AS range_low,

    (SELECT max(ons_imd.imd) FROM @source_schema.ons_imd) AS range_high

  FROM @source_schema.ons_imd
    JOIN @source_schema.patient AS patient USING (patid)
;
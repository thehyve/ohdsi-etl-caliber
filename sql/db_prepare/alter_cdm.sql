-- Change to bigint to allow for large numbers created by createVisitId()
ALTER TABLE cdm5.visit_occurrence ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;
ALTER TABLE cdm5.drug_exposure ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;
ALTER TABLE cdm5.device_exposure ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;

/*
Assign auto increment to measurement and observation tables.
Needed to quarentee an unique primary key.
*/
SET client_min_messages TO WARNING; -- turns off cascading warnings

DROP SEQUENCE IF EXISTS measurement_seq CASCADE;
CREATE SEQUENCE measurement_seq;
ALTER TABLE cdm5.measurement ALTER COLUMN measurement_id SET DEFAULT nextval('measurement_seq');

DROP SEQUENCE IF EXISTS observation_seq CASCADE;
CREATE SEQUENCE observation_seq;
ALTER TABLE cdm5.observation ALTER COLUMN observation_id SET DEFAULT nextval('observation_seq');

DROP SEQUENCE IF EXISTS drug_exposure_seq CASCADE;
CREATE SEQUENCE drug_exposure_seq;
ALTER TABLE cdm5.drug_exposure ALTER COLUMN drug_exposure_id SET DEFAULT nextval('drug_exposure_seq');

DROP SEQUENCE IF EXISTS device_exposure_seq CASCADE;
CREATE SEQUENCE device_exposure_seq;
ALTER TABLE cdm5.device_exposure ALTER COLUMN device_exposure_id SET DEFAULT nextval('device_exposure_seq');

DROP SEQUENCE IF EXISTS condition_occurrence_seq CASCADE;
CREATE SEQUENCE condition_occurrence_seq;
ALTER TABLE cdm5.condition_occurrence ALTER COLUMN condition_occurrence_id SET DEFAULT nextval('condition_occurrence_seq');

DROP SEQUENCE IF EXISTS procedure_occurrence_seq CASCADE;
CREATE SEQUENCE procedure_occurrence_seq;
ALTER TABLE cdm5.procedure_occurrence ALTER COLUMN procedure_occurrence_id SET DEFAULT nextval('procedure_occurrence_seq');

DROP SEQUENCE IF EXISTS observation_period_seq CASCADE;
CREATE SEQUENCE observation_period_seq;
ALTER TABLE cdm5.observation_period ALTER COLUMN observation_period_id SET DEFAULT nextval('observation_period_seq');
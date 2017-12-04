/*
Assign auto increment to measurement and observation tables.
Needed to guarantee an unique primary key.
*/
DROP SEQUENCE IF EXISTS cdm5.measurement_seq CASCADE;
CREATE SEQUENCE cdm5.measurement_seq;
ALTER TABLE cdm5.measurement ALTER COLUMN measurement_id SET DEFAULT nextval('cdm5.measurement_seq');

DROP SEQUENCE IF EXISTS cdm5.observation_seq CASCADE;
CREATE SEQUENCE cdm5.observation_seq;
ALTER TABLE cdm5.observation ALTER COLUMN observation_id SET DEFAULT nextval('cdm5.observation_seq');

DROP SEQUENCE IF EXISTS cdm5.drug_exposure_seq CASCADE;
CREATE SEQUENCE cdm5.drug_exposure_seq;
ALTER TABLE cdm5.drug_exposure ALTER COLUMN drug_exposure_id SET DEFAULT nextval('cdm5.drug_exposure_seq');

DROP SEQUENCE IF EXISTS cdm5.device_exposure_seq CASCADE;
CREATE SEQUENCE cdm5.device_exposure_seq;
ALTER TABLE cdm5.device_exposure ALTER COLUMN device_exposure_id SET DEFAULT nextval('cdm5.device_exposure_seq');

DROP SEQUENCE IF EXISTS cdm5.condition_occurrence_seq CASCADE;
CREATE SEQUENCE cdm5.condition_occurrence_seq;
ALTER TABLE cdm5.condition_occurrence ALTER COLUMN condition_occurrence_id SET DEFAULT nextval('cdm5.condition_occurrence_seq');

DROP SEQUENCE IF EXISTS cdm5.procedure_occurrence_seq CASCADE;
CREATE SEQUENCE cdm5.procedure_occurrence_seq;
ALTER TABLE cdm5.procedure_occurrence ALTER COLUMN procedure_occurrence_id SET DEFAULT nextval('cdm5.procedure_occurrence_seq');

DROP SEQUENCE IF EXISTS cdm5.observation_period_seq CASCADE;
CREATE SEQUENCE cdm5.observation_period_seq;
ALTER TABLE cdm5.observation_period ALTER COLUMN observation_period_id SET DEFAULT nextval('cdm5.observation_period_seq');

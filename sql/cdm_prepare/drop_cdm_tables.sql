/*
Drop all, except the tables for the standard vocabulary.
Has to be rebuild again in next step. Without constraints
Has two goals: empty the table and remove constraints
*/
DROP TABLE IF EXISTS cdm5.care_site CASCADE;
DROP TABLE IF EXISTS cdm5.cohort CASCADE;
DROP TABLE IF EXISTS cdm5.cohort_attribute CASCADE;
DROP TABLE IF EXISTS cdm5.condition_era CASCADE;
DROP TABLE IF EXISTS cdm5.condition_occurrence CASCADE;
DROP TABLE IF EXISTS cdm5.death CASCADE;
DROP TABLE IF EXISTS cdm5.device_exposure CASCADE;
DROP TABLE IF EXISTS cdm5.dose_era CASCADE;
DROP TABLE IF EXISTS cdm5.drug_era CASCADE;
DROP TABLE IF EXISTS cdm5.drug_exposure CASCADE;
DROP TABLE IF EXISTS cdm5.fact_relationship CASCADE;
DROP TABLE IF EXISTS cdm5.location CASCADE;
DROP TABLE IF EXISTS cdm5.measurement CASCADE;
DROP TABLE IF EXISTS cdm5.note CASCADE;
DROP TABLE IF EXISTS cdm5.note_nlp CASCADE;
DROP TABLE IF EXISTS cdm5.observation CASCADE;
DROP TABLE IF EXISTS cdm5.observation_period CASCADE;
DROP TABLE IF EXISTS cdm5.payer_plan_period CASCADE;
DROP TABLE IF EXISTS cdm5.cost CASCADE;
DROP TABLE IF EXISTS cdm5.person CASCADE;
DROP TABLE IF EXISTS cdm5.procedure_occurrence CASCADE;
DROP TABLE IF EXISTS cdm5.provider CASCADE;
DROP TABLE IF EXISTS cdm5.specimen CASCADE;
DROP TABLE IF EXISTS cdm5.visit_occurrence CASCADE;
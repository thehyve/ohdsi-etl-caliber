-- Change to bigint to allow for large numbers created by createVisitId()
ALTER TABLE cdm5.visit_occurrence ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;
ALTER TABLE cdm5.condition_occurrence ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;
ALTER TABLE cdm5.procedure_occurrence ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;
ALTER TABLE cdm5.drug_exposure ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;
ALTER TABLE cdm5.device_exposure ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;
ALTER TABLE cdm5.observation ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;
ALTER TABLE cdm5.measurement ALTER COLUMN visit_occurrence_id TYPE BIGINT USING visit_occurrence_id::BIGINT;

/* Remove existing source_to_concept_map and custom vocabularies */
TRUNCATE cdm5.source_to_concept_map;
DELETE FROM cdm5.vocabulary WHERE vocabulary_concept_id = 0;

/* Add the 7 vocabulary ids */
COPY cdm5.vocabulary FROM '@absPath/resources/mapping_tables/VOCABULARY.csv' WITH CSV HEADER;

/* Load new source to concept maps */

-- Additional Entity Types to LOINC
COPY cdm5.source_to_concept_map FROM '@absPath/resources/mapping_tables/JNJ_CPRD_ET_LOINC.txt';

-- HES Observations to LOINC
COPY cdm5.source_to_concept_map FROM '@absPath/resources/mapping_tables/JNJ_CPRD_HES_LOINC.txt';

-- Provider to CMS2
COPY cdm5.source_to_concept_map FROM '@absPath/resources/mapping_tables/JNJ_CPRD_PROV_CMS2.txt';

-- Provider roles to CMS speciality
COPY cdm5.source_to_concept_map FROM '@absPath/resources/mapping_tables/JNJ_CPRD_PROV_SPEC.txt';

-- Scoring method and read to LOINC
COPY cdm5.source_to_concept_map FROM '@absPath/resources/mapping_tables/JNJ_CPRD_SCORE_LOINC.txt';

-- Test Entity Types to LOINC
COPY cdm5.source_to_concept_map FROM '@absPath/resources/mapping_tables/JNJ_CPRD_T_ET_LOINC.txt';

-- Product codes to RxNorm
COPY cdm5.source_to_concept_map FROM '@absPath/resources/mapping_tables/CPRD_PRODUCT_TO_RXNORM.csv' WITH CSV HEADER;

-- CPRD qualifiers (TQU)
COPY cdm5.source_to_concept_map FROM '@absPath/resources/mapping_tables/CPRD_QUALIFIER_TO_MEAS_VALUE.csv' WITH CSV HEADER;

-- CPRD units (SUM)
COPY cdm5.source_to_concept_map FROM '@absPath/resources/mapping_tables/CPRD_UNIT_TO_UCUM.csv' WITH CSV HEADER;

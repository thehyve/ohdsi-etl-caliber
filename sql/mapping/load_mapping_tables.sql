/* Add the 7 vocabulary ids */
\copy cdm5.vocabulary FROM 'resources/mapping_tables/VOCABULARY.csv' CSV;

/* Load source to concept maps */
-- Additional Entity Types to LOINC
\copy cdm5.source_to_concept_map FROM 'resources/mapping_tables/JNJ_CPRD_ET_LOINC.txt';

-- Gemscript to RxNorm (csv format)
\copy cdm5.source_to_concept_map FROM 'resources/mapping_tables/JNJ_CPRD_GS_RXN.csv' CSV;

-- HES Observations to LOINC
\copy cdm5.source_to_concept_map FROM 'resources/mapping_tables/JNJ_CPRD_HES_LOINC.txt';

-- Provider to CMS2
\copy cdm5.source_to_concept_map FROM 'resources/mapping_tables/JNJ_CPRD_PROV_CMS2.txt';

-- Provider roles to CMS speciality
\copy cdm5.source_to_concept_map FROM 'resources/mapping_tables/JNJ_CPRD_PROV_SPEC.txt';

-- Scoring method and read to LOINC
\copy cdm5.source_to_concept_map FROM 'resources/mapping_tables/JNJ_CPRD_SCORE_LOINC.txt';

-- Test Entity Types to LOINC
\copy cdm5.source_to_concept_map FROM 'resources/mapping_tables/JNJ_CPRD_T_ET_LOINC.txt';

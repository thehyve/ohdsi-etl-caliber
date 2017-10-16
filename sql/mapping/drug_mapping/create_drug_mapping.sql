/*
 * Creates chain of mappings: prodcode -> gemscript -> dm+d code -> dm+d concept -> standard concept (RxNorm [Extension])
 * Includes frequency of prodcode in caliber.therapy, which is proportional to frequencies in actual data.
 * Result stored in temp table, because this query is costly
 */
DROP TABLE IF EXISTS public.temp_product_map;
WITH prod_occurrence AS (
    SELECT prodcode, count(*)
    FROM caliber.therapy
    GROUP BY prodcode
)
SELECT prod_occurrence.*, product.gemscriptcode, temp_gemscript_to_dmd.dmd_code, concept.concept_id AS dmd_concept_id, concept_relationship.concept_id_2 AS standard_concept_id
INTO public.temp_product_map
FROM prod_occurrence
  LEFT JOIN caliber.product ON prod_occurrence.prodcode = product.prodcode
  LEFT JOIN public.temp_gemscript_to_dmd ON product.gemscriptcode = CAST(temp_gemscript_to_dmd.gemscript_code AS varchar) -- gemscript_code varchar due to 'ZZZZZZZ' code
  LEFT JOIN cdm5.concept ON (concept.vocabulary_id = 'dm+d' AND dmd_code = concept.concept_code)
  LEFT JOIN cdm5.concept_relationship ON (relationship_id = 'Maps to' AND concept.concept_id = concept_id_1)
ORDER BY prod_occurrence.count DESC
;

-- Write mapping to file
COPY(
  SELECT
    prodcode AS source_code,
    0 AS source_concept_id,
    'CPRD_PRODUCT' AS source_vocabulary_id,
    productname AS source_code_description,
    COALESCE(concept_id, 0) AS target_concept_id,
    COALESCE(vocabulary_id, 'None') AS target_vocabulary_id,
    '2017-01-01' AS valid_start_date,
    '2099-12-31' AS valid_end_date,
    NULL AS invalid_reason
  FROM
    temp_product_map
    LEFT JOIN caliber.product USING(prodcode)
    LEFT JOIN cdm5.concept ON concept_id = standard_concept_id
) TO '/Users/Maxim/Develop/OHDSI/ohdsi-etl-caliber/resources/mapping_tables/CPRD_PRODUCT_TO_RXNORM.csv' WITH CSV HEADER;
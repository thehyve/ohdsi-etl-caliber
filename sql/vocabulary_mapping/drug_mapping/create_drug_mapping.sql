/*
 * Creates multi step mapping: prodcode -> gemscript -> dm+d code -> dm+d concept -> standard concept (RxNorm [Extension])
 */
COPY(
	SELECT DISTINCT ON (product.prodcode) -- some concepts map to both RxNorm and RxNorm Extension
		product.prodcode 						AS source_code,
		0 										AS source_concept_id,
	    'CPRD_PRODUCT' 							AS source_vocabulary_id,
	    product.productname 					AS source_code_description,
	    COALESCE(target.concept_id, 0) 			AS target_concept_id,
	    COALESCE(target.vocabulary_id, 'None') 	AS target_vocabulary_id,
	    '2017-01-01' 							AS valid_start_date,
	    '2099-12-31' 							AS valid_end_date,
	    NULL 									AS invalid_reason
	FROM my_caliber.product
	  LEFT JOIN public.temp_gemscript_to_dmd 
	  	ON product.gemscriptcode = CAST(temp_gemscript_to_dmd.gemscript_code AS varchar) -- gemscript_code is a varchar due to 'ZZZZZZZ' code
	  LEFT JOIN cdm5.concept AS dmd
	  	ON temp_gemscript_to_dmd.dmd_code = dmd.concept_code 
	  	AND dmd.vocabulary_id = 'dm+d'
	  LEFT JOIN cdm5.concept_relationship 
	  	ON dmd.concept_id = concept_id_1 
	  	AND relationship_id = 'Maps to'
	  LEFT JOIN cdm5.concept AS target
	  	ON concept_id_2 = target.concept_id
	WHERE product.prodcode > 0
) TO 'CPRD_PRODUCT_TO_RXNORM.csv' WITH CSV HEADER;
;
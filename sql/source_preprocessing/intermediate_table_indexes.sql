-- Medcode intermediate
CREATE INDEX medcode_intermediate_target_domain_id_index ON public.medcode_intermediate(target_domain_id);

-- HES diagnoses intermediate
CREATE INDEX hes_diagnoses_intermediate_target_domain_id_index ON public.hes_diagnoses_intermediate(target_domain_id);

-- Additional intermediate
CREATE INDEX additional_intermediate_lookup_type_index ON public.additional_intermediate(lookup_type);
CREATE INDEX additional_intermediate_enttype_string_index ON public.additional_intermediate(enttype_string);
CREATE INDEX additional_intermediate_data_code_index ON public.additional_intermediate(data_code);
CREATE INDEX additional_intermediate_unit_code_index ON public.additional_intermediate(unit_code);

-- Test intermediate
CREATE INDEX test_intermediate_target_domain_id_index ON public.test_intermediate(target_domain_id);

-- Numdays aggregates
CREATE INDEX numdays_aggregate_prodcode_index ON public.numdays_aggregate_prodcode (prodcode);
CREATE INDEX numdays_aggregate_full_index ON public.numdays_aggregate_full (prodcode, ndd, qty, numpacks);


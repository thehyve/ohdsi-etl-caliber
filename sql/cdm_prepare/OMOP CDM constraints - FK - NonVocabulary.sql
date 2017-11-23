/*********************************************************************************
# Copyright 2014 Observational Health Data Sciences and Informatics
#
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
********************************************************************************/

/************************

 ####### #     # ####### ######      #####  ######  #     #           #######      #####      #####                                                                 
 #     # ##   ## #     # #     #    #     # #     # ##   ##    #    # #           #     #    #     #  ####  #    #  ####  ##### #####    ##   # #    # #####  ####  
 #     # # # # # #     # #     #    #       #     # # # # #    #    # #                 #    #       #    # ##   # #        #   #    #  #  #  # ##   #   #   #      
 #     # #  #  # #     # ######     #       #     # #  #  #    #    # ######       #####     #       #    # # #  #  ####    #   #    # #    # # # #  #   #    ####  
 #     # #     # #     # #          #       #     # #     #    #    #       # ### #          #       #    # #  # #      #   #   #####  ###### # #  # #   #        # 
 #     # #     # #     # #          #     # #     # #     #     #  #  #     # ### #          #     # #    # #   ## #    #   #   #   #  #    # # #   ##   #   #    # 
 ####### #     # ####### #           #####  ######  #     #      ##    #####  ### #######     #####   ####  #    #  ####    #   #    # #    # # #    #   #    ####  
                                                                              

script to create constraints within OMOP common data model, version 5.2 for PostgreSQL database

last revised: 14 July 2017

author:  Patrick Ryan


*************************/


/************************
*************************
*************************
*************************

Foreign key constraints

*************************
*************************
*************************
************************/


/************************

Standardized clinical data

************************/

ALTER TABLE cdm5.person ADD CONSTRAINT fpk_person_gender_concept FOREIGN KEY (gender_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.person ADD CONSTRAINT fpk_person_race_concept FOREIGN KEY (race_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.person ADD CONSTRAINT fpk_person_ethnicity_concept FOREIGN KEY (ethnicity_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.person ADD CONSTRAINT fpk_person_gender_concept_s FOREIGN KEY (gender_source_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.person ADD CONSTRAINT fpk_person_race_concept_s FOREIGN KEY (race_source_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.person ADD CONSTRAINT fpk_person_ethnicity_concept_s FOREIGN KEY (ethnicity_source_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.person ADD CONSTRAINT fpk_person_location FOREIGN KEY (location_id)  REFERENCES cdm5.location (location_id);

ALTER TABLE cdm5.person ADD CONSTRAINT fpk_person_provider FOREIGN KEY (provider_id)  REFERENCES cdm5.provider (provider_id);

ALTER TABLE cdm5.person ADD CONSTRAINT fpk_person_care_site FOREIGN KEY (care_site_id)  REFERENCES cdm5.care_site (care_site_id);


ALTER TABLE cdm5.observation_period ADD CONSTRAINT fpk_observation_period_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.observation_period ADD CONSTRAINT fpk_observation_period_concept FOREIGN KEY (period_type_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.specimen ADD CONSTRAINT fpk_specimen_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.specimen ADD CONSTRAINT fpk_specimen_concept FOREIGN KEY (specimen_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.specimen ADD CONSTRAINT fpk_specimen_type_concept FOREIGN KEY (specimen_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.specimen ADD CONSTRAINT fpk_specimen_unit_concept FOREIGN KEY (unit_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.specimen ADD CONSTRAINT fpk_specimen_site_concept FOREIGN KEY (anatomic_site_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.specimen ADD CONSTRAINT fpk_specimen_status_concept FOREIGN KEY (disease_status_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.death ADD CONSTRAINT fpk_death_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.death ADD CONSTRAINT fpk_death_type_concept FOREIGN KEY (death_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.death ADD CONSTRAINT fpk_death_cause_concept FOREIGN KEY (cause_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.death ADD CONSTRAINT fpk_death_cause_concept_s FOREIGN KEY (cause_source_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.visit_occurrence ADD CONSTRAINT fpk_visit_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.visit_occurrence ADD CONSTRAINT fpk_visit_concept FOREIGN KEY (visit_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.visit_occurrence ADD CONSTRAINT fpk_visit_type_concept FOREIGN KEY (visit_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.visit_occurrence ADD CONSTRAINT fpk_visit_provider FOREIGN KEY (provider_id)  REFERENCES cdm5.provider (provider_id);

ALTER TABLE cdm5.visit_occurrence ADD CONSTRAINT fpk_visit_care_site FOREIGN KEY (care_site_id)  REFERENCES cdm5.care_site (care_site_id);

ALTER TABLE cdm5.visit_occurrence ADD CONSTRAINT fpk_visit_concept_s FOREIGN KEY (visit_source_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.visit_occurrence ADD CONSTRAINT fpk_visit_admitting_s FOREIGN KEY (admitting_source_concept_id) REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.visit_occurrence ADD CONSTRAINT fpk_visit_discharge FOREIGN KEY (discharge_to_concept_id) REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.procedure_occurrence ADD CONSTRAINT fpk_procedure_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.procedure_occurrence ADD CONSTRAINT fpk_procedure_concept FOREIGN KEY (procedure_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.procedure_occurrence ADD CONSTRAINT fpk_procedure_type_concept FOREIGN KEY (procedure_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.procedure_occurrence ADD CONSTRAINT fpk_procedure_modifier FOREIGN KEY (modifier_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.procedure_occurrence ADD CONSTRAINT fpk_procedure_provider FOREIGN KEY (provider_id)  REFERENCES cdm5.provider (provider_id);

ALTER TABLE cdm5.procedure_occurrence ADD CONSTRAINT fpk_procedure_visit FOREIGN KEY (visit_occurrence_id)  REFERENCES cdm5.visit_occurrence (visit_occurrence_id);

ALTER TABLE cdm5.procedure_occurrence ADD CONSTRAINT fpk_procedure_concept_s FOREIGN KEY (procedure_source_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.drug_exposure ADD CONSTRAINT fpk_drug_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.drug_exposure ADD CONSTRAINT fpk_drug_concept FOREIGN KEY (drug_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.drug_exposure ADD CONSTRAINT fpk_drug_type_concept FOREIGN KEY (drug_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.drug_exposure ADD CONSTRAINT fpk_drug_route_concept FOREIGN KEY (route_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.drug_exposure ADD CONSTRAINT fpk_drug_provider FOREIGN KEY (provider_id)  REFERENCES cdm5.provider (provider_id);

ALTER TABLE cdm5.drug_exposure ADD CONSTRAINT fpk_drug_visit FOREIGN KEY (visit_occurrence_id)  REFERENCES cdm5.visit_occurrence (visit_occurrence_id);

ALTER TABLE cdm5.drug_exposure ADD CONSTRAINT fpk_drug_concept_s FOREIGN KEY (drug_source_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.device_exposure ADD CONSTRAINT fpk_device_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.device_exposure ADD CONSTRAINT fpk_device_concept FOREIGN KEY (device_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.device_exposure ADD CONSTRAINT fpk_device_type_concept FOREIGN KEY (device_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.device_exposure ADD CONSTRAINT fpk_device_provider FOREIGN KEY (provider_id)  REFERENCES cdm5.provider (provider_id);

ALTER TABLE cdm5.device_exposure ADD CONSTRAINT fpk_device_visit FOREIGN KEY (visit_occurrence_id)  REFERENCES cdm5.visit_occurrence (visit_occurrence_id);

ALTER TABLE cdm5.device_exposure ADD CONSTRAINT fpk_device_concept_s FOREIGN KEY (device_source_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.condition_occurrence ADD CONSTRAINT fpk_condition_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.condition_occurrence ADD CONSTRAINT fpk_condition_concept FOREIGN KEY (condition_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.condition_occurrence ADD CONSTRAINT fpk_condition_type_concept FOREIGN KEY (condition_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.condition_occurrence ADD CONSTRAINT fpk_condition_provider FOREIGN KEY (provider_id)  REFERENCES cdm5.provider (provider_id);

ALTER TABLE cdm5.condition_occurrence ADD CONSTRAINT fpk_condition_visit FOREIGN KEY (visit_occurrence_id)  REFERENCES cdm5.visit_occurrence (visit_occurrence_id);

ALTER TABLE cdm5.condition_occurrence ADD CONSTRAINT fpk_condition_concept_s FOREIGN KEY (condition_source_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.condition_occurrence ADD CONSTRAINT fpk_condition_status_concept FOREIGN KEY (condition_status_concept_id) REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.measurement ADD CONSTRAINT fpk_measurement_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.measurement ADD CONSTRAINT fpk_measurement_concept FOREIGN KEY (measurement_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.measurement ADD CONSTRAINT fpk_measurement_type_concept FOREIGN KEY (measurement_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.measurement ADD CONSTRAINT fpk_measurement_operator FOREIGN KEY (operator_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.measurement ADD CONSTRAINT fpk_measurement_value FOREIGN KEY (value_as_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.measurement ADD CONSTRAINT fpk_measurement_unit FOREIGN KEY (unit_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.measurement ADD CONSTRAINT fpk_measurement_provider FOREIGN KEY (provider_id)  REFERENCES cdm5.provider (provider_id);

ALTER TABLE cdm5.measurement ADD CONSTRAINT fpk_measurement_visit FOREIGN KEY (visit_occurrence_id)  REFERENCES cdm5.visit_occurrence (visit_occurrence_id);

ALTER TABLE cdm5.measurement ADD CONSTRAINT fpk_measurement_concept_s FOREIGN KEY (measurement_source_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.note ADD CONSTRAINT fpk_note_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.note ADD CONSTRAINT fpk_note_type_concept FOREIGN KEY (note_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.note ADD CONSTRAINT fpk_note_class_concept FOREIGN KEY (note_class_concept_id) REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.note ADD CONSTRAINT fpk_note_encoding_concept FOREIGN KEY (encoding_concept_id) REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.note ADD CONSTRAINT fpk_language_concept FOREIGN KEY (language_concept_id) REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.note ADD CONSTRAINT fpk_note_provider FOREIGN KEY (provider_id)  REFERENCES cdm5.provider (provider_id);

ALTER TABLE cdm5.note ADD CONSTRAINT fpk_note_visit FOREIGN KEY (visit_occurrence_id)  REFERENCES cdm5.visit_occurrence (visit_occurrence_id);


ALTER TABLE cdm5.note_nlp ADD CONSTRAINT fpk_note_nlp_note FOREIGN KEY (note_id) REFERENCES cdm5.note (note_id);

ALTER TABLE cdm5.note_nlp ADD CONSTRAINT fpk_note_nlp_section_concept FOREIGN KEY (section_concept_id) REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.note_nlp ADD CONSTRAINT fpk_note_nlp_concept FOREIGN KEY (note_nlp_concept_id) REFERENCES cdm5.concept (concept_id); 


ALTER TABLE cdm5.observation ADD CONSTRAINT fpk_observation_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.observation ADD CONSTRAINT fpk_observation_concept FOREIGN KEY (observation_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.observation ADD CONSTRAINT fpk_observation_type_concept FOREIGN KEY (observation_type_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.observation ADD CONSTRAINT fpk_observation_value FOREIGN KEY (value_as_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.observation ADD CONSTRAINT fpk_observation_qualifier FOREIGN KEY (qualifier_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.observation ADD CONSTRAINT fpk_observation_unit FOREIGN KEY (unit_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.observation ADD CONSTRAINT fpk_observation_provider FOREIGN KEY (provider_id)  REFERENCES cdm5.provider (provider_id);

ALTER TABLE cdm5.observation ADD CONSTRAINT fpk_observation_visit FOREIGN KEY (visit_occurrence_id)  REFERENCES cdm5.visit_occurrence (visit_occurrence_id);

ALTER TABLE cdm5.observation ADD CONSTRAINT fpk_observation_concept_s FOREIGN KEY (observation_source_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.fact_relationship ADD CONSTRAINT fpk_fact_domain_1 FOREIGN KEY (domain_concept_id_1)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.fact_relationship ADD CONSTRAINT fpk_fact_domain_2 FOREIGN KEY (domain_concept_id_2)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.fact_relationship ADD CONSTRAINT fpk_fact_relationship FOREIGN KEY (relationship_concept_id)  REFERENCES cdm5.concept (concept_id);



/************************

Standardized health system data

************************/

ALTER TABLE cdm5.care_site ADD CONSTRAINT fpk_care_site_location FOREIGN KEY (location_id)  REFERENCES cdm5.location (location_id);

ALTER TABLE cdm5.care_site ADD CONSTRAINT fpk_care_site_place FOREIGN KEY (place_of_service_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.provider ADD CONSTRAINT fpk_provider_specialty FOREIGN KEY (specialty_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.provider ADD CONSTRAINT fpk_provider_care_site FOREIGN KEY (care_site_id)  REFERENCES cdm5.care_site (care_site_id);

ALTER TABLE cdm5.provider ADD CONSTRAINT fpk_provider_gender FOREIGN KEY (gender_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.provider ADD CONSTRAINT fpk_provider_specialty_s FOREIGN KEY (specialty_source_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.provider ADD CONSTRAINT fpk_provider_gender_s FOREIGN KEY (gender_source_concept_id)  REFERENCES cdm5.concept (concept_id);




/************************

Standardized health economics

************************/

ALTER TABLE cdm5.payer_plan_period ADD CONSTRAINT fpk_payer_plan_period FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.cost ADD CONSTRAINT fpk_visit_cost_currency FOREIGN KEY (currency_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.cost ADD CONSTRAINT fpk_visit_cost_period FOREIGN KEY (payer_plan_period_id)  REFERENCES cdm5.payer_plan_period (payer_plan_period_id);

ALTER TABLE cdm5.cost ADD CONSTRAINT fpk_drg_concept FOREIGN KEY (drg_concept_id) REFERENCES cdm5.concept (concept_id);


/************************

Standardized derived elements

************************/


ALTER TABLE cdm5.cohort ADD CONSTRAINT fpk_cohort_definition FOREIGN KEY (cohort_definition_id)  REFERENCES cdm5.cohort_definition (cohort_definition_id);


ALTER TABLE cdm5.cohort_attribute ADD CONSTRAINT fpk_ca_cohort_definition FOREIGN KEY (cohort_definition_id)  REFERENCES cdm5.cohort_definition (cohort_definition_id);

ALTER TABLE cdm5.cohort_attribute ADD CONSTRAINT fpk_ca_attribute_definition FOREIGN KEY (attribute_definition_id)  REFERENCES cdm5.attribute_definition (attribute_definition_id);

ALTER TABLE cdm5.cohort_attribute ADD CONSTRAINT fpk_ca_value FOREIGN KEY (value_as_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.drug_era ADD CONSTRAINT fpk_drug_era_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.drug_era ADD CONSTRAINT fpk_drug_era_concept FOREIGN KEY (drug_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.dose_era ADD CONSTRAINT fpk_dose_era_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.dose_era ADD CONSTRAINT fpk_dose_era_concept FOREIGN KEY (drug_concept_id)  REFERENCES cdm5.concept (concept_id);

ALTER TABLE cdm5.dose_era ADD CONSTRAINT fpk_dose_era_unit_concept FOREIGN KEY (unit_concept_id)  REFERENCES cdm5.concept (concept_id);


ALTER TABLE cdm5.condition_era ADD CONSTRAINT fpk_condition_era_person FOREIGN KEY (person_id)  REFERENCES cdm5.person (person_id);

ALTER TABLE cdm5.condition_era ADD CONSTRAINT fpk_condition_era_concept FOREIGN KEY (condition_concept_id)  REFERENCES cdm5.concept (concept_id);


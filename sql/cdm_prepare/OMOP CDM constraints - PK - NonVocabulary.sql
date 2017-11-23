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

Primary key constraints

*************************
*************************
*************************
************************/



/************************

Standardized clinical data

************************/


/**PRIMARY KEY NONCLUSTERED constraints**/
ALTER TABLE cdm5.person ADD CONSTRAINT xpk_person PRIMARY KEY ( person_id ) ;

ALTER TABLE cdm5.observation_period ADD CONSTRAINT xpk_observation_period PRIMARY KEY ( observation_period_id ) ;

ALTER TABLE cdm5.specimen ADD CONSTRAINT xpk_specimen PRIMARY KEY ( specimen_id ) ;

ALTER TABLE cdm5.death ADD CONSTRAINT xpk_death PRIMARY KEY ( person_id ) ;

ALTER TABLE cdm5.visit_occurrence ADD CONSTRAINT xpk_visit_occurrence PRIMARY KEY ( visit_occurrence_id ) ;

ALTER TABLE cdm5.procedure_occurrence ADD CONSTRAINT xpk_procedure_occurrence PRIMARY KEY ( procedure_occurrence_id ) ;

ALTER TABLE cdm5.drug_exposure ADD CONSTRAINT xpk_drug_exposure PRIMARY KEY ( drug_exposure_id ) ;

ALTER TABLE cdm5.device_exposure ADD CONSTRAINT xpk_device_exposure PRIMARY KEY ( device_exposure_id ) ;

ALTER TABLE cdm5.condition_occurrence ADD CONSTRAINT xpk_condition_occurrence PRIMARY KEY ( condition_occurrence_id ) ;

ALTER TABLE cdm5.measurement ADD CONSTRAINT xpk_measurement PRIMARY KEY ( measurement_id ) ;

ALTER TABLE cdm5.note ADD CONSTRAINT xpk_note PRIMARY KEY ( note_id ) ;

ALTER TABLE cdm5.note_nlp ADD CONSTRAINT xpk_note_nlp PRIMARY KEY ( note_nlp_id ) ;

ALTER TABLE cdm5.observation  ADD CONSTRAINT xpk_observation PRIMARY KEY ( observation_id ) ;




/************************

Standardized health system data

************************/


ALTER TABLE cdm5.location ADD CONSTRAINT xpk_location PRIMARY KEY ( location_id ) ;

ALTER TABLE cdm5.care_site ADD CONSTRAINT xpk_care_site PRIMARY KEY ( care_site_id ) ;

ALTER TABLE cdm5.provider ADD CONSTRAINT xpk_provider PRIMARY KEY ( provider_id ) ;



/************************

Standardized health economics

************************/


ALTER TABLE cdm5.payer_plan_period ADD CONSTRAINT xpk_payer_plan_period PRIMARY KEY ( payer_plan_period_id ) ;

ALTER TABLE cdm5.cost ADD CONSTRAINT xpk_visit_cost PRIMARY KEY ( cost_id ) ;


/************************

Standardized derived elements

************************/

ALTER TABLE cdm5.cohort ADD CONSTRAINT xpk_cohort PRIMARY KEY ( cohort_definition_id, subject_id, cohort_start_date, cohort_end_date  ) ;

ALTER TABLE cdm5.cohort_attribute ADD CONSTRAINT xpk_cohort_attribute PRIMARY KEY ( cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, attribute_definition_id ) ;

ALTER TABLE cdm5.drug_era ADD CONSTRAINT xpk_drug_era PRIMARY KEY ( drug_era_id ) ;

ALTER TABLE cdm5.dose_era  ADD CONSTRAINT xpk_dose_era PRIMARY KEY ( dose_era_id ) ;

ALTER TABLE cdm5.condition_era ADD CONSTRAINT xpk_condition_era PRIMARY KEY ( condition_era_id ) ;




CREATE INDEX IF NOT EXISTS idx_additional_patid
  ON @source_schema.additional (patid);
CREATE INDEX IF NOT EXISTS idx_additional_enttype
  ON @source_schema.additional (enttype);
CREATE INDEX IF NOT EXISTS idx_additional_adid
  ON @source_schema.additional (adid);

CREATE INDEX IF NOT EXISTS idx_clinical_patid
  ON @source_schema.clinical (patid);
CREATE INDEX IF NOT EXISTS idx_clinical_enttype
  ON @source_schema.clinical (enttype);
CREATE INDEX IF NOT EXISTS idx_clinical_adid
  ON @source_schema.clinical (adid);
CREATE INDEX IF NOT EXISTS idx_clinical_medcode
  ON @source_schema.clinical (medcode);

CREATE INDEX IF NOT EXISTS idx_consultation_patid
  ON @source_schema.consultation (patid);
CREATE INDEX IF NOT EXISTS idx_consultation_consid
  ON @source_schema.consultation (consid);
CREATE INDEX IF NOT EXISTS idx_consultation_eventdate
  ON @source_schema.consultation (eventdate);

CREATE INDEX IF NOT EXISTS idx_hes_diag_epi_patid
  ON @source_schema.hes_diag_epi (patid);
CREATE INDEX IF NOT EXISTS idx_hes_diag_hosp_patid
  ON @source_schema.hes_diag_hosp (patid);
CREATE INDEX IF NOT EXISTS idx_hes_diag_hosp_icd
  ON @source_schema.hes_diag_hosp (icd);

CREATE INDEX IF NOT EXISTS idx_hes_op_appt_patid
  ON @source_schema.hes_op_appt (patid);
CREATE INDEX IF NOT EXISTS idx_hes_op_appt_attendkey
  ON @source_schema.hes_op_appt (attendkey);

CREATE INDEX IF NOT EXISTS idx_hes_op_clinical_patid
  ON @source_schema.hes_op_clinical (patid);
CREATE INDEX IF NOT EXISTS idx_hes_op_clinical_attendkey
  ON @source_schema.hes_op_clinical (attendkey);

CREATE INDEX IF NOT EXISTS idx_hes_op_clinical_diag_patid
  ON @source_schema.hes_op_clinical_diag (patid);
CREATE INDEX IF NOT EXISTS idx_hes_op_clinical_diag_attendkey
  ON @source_schema.hes_op_clinical_diag (attendkey);
CREATE INDEX IF NOT EXISTS idx_hes_op_clinical_diag_icd
  ON @source_schema.hes_op_clinical_diag (icd);
CREATE INDEX IF NOT EXISTS idx_hes_op_clinical_diag_newicd
  ON @source_schema.hes_op_clinical_diag (newicd);

CREATE INDEX IF NOT EXISTS idx_hes_op_clinical_proc_patid
  ON @source_schema.hes_op_clinical_proc (patid);
CREATE INDEX IF NOT EXISTS idx_hes_op_clinical_proc_attendkey
  ON @source_schema.hes_op_clinical_proc (attendkey);
CREATE INDEX IF NOT EXISTS idx_hes_op_clinical_proc_opcs
  ON @source_schema.hes_op_clinical_proc (opcs);

CREATE INDEX IF NOT EXISTS idx_hes_op_patient_patid
  ON @source_schema.hes_op_patient (patid);
CREATE INDEX IF NOT EXISTS idx_hes_patient_patid
  ON @source_schema.hes_patient (patid);

CREATE INDEX IF NOT EXISTS idx_hes_proc_epi_patid
  ON @source_schema.hes_proc_epi (patid);

CREATE INDEX IF NOT EXISTS idx_immunisation_patid
  ON @source_schema.immunisation (patid);
CREATE INDEX IF NOT EXISTS idx_immunisation_medcode
  ON @source_schema.immunisation (medcode);

CREATE INDEX IF NOT EXISTS idx_lookup_entity_enttype
  ON @source_schema.entity (enttype); -- originally called lookup_entity
CREATE INDEX IF NOT EXISTS idx_lookup_medical_medcode
  ON @source_schema.medical (medcode); -- originally called lookup_medical
CREATE INDEX IF NOT EXISTS idx_lookup_product_prodcode
  ON @source_schema.product (prodcode); -- originally called lookup_product
CREATE INDEX IF NOT EXISTS idx_lookup_linkage_eligibility_patid
  ON @source_schema.lookup_linkage_eligibility (patid);

CREATE INDEX IF NOT EXISTS idx_ons_death_patid
  ON @source_schema.ons_death (patid);
CREATE INDEX IF NOT EXISTS idx_ons_imd_patid
  ON @source_schema.ons_imd (patid);

CREATE INDEX IF NOT EXISTS idx_patient_patid
  ON @source_schema.patient (patid);
CREATE INDEX IF NOT EXISTS idx_patient_accept
  ON @source_schema.patient (accept);
CREATE INDEX IF NOT EXISTS idx_patient_gender
  ON @source_schema.patient (gender);

CREATE INDEX IF NOT EXISTS idx_practice_pracid
  ON @source_schema.practice (pracid);

CREATE INDEX IF NOT EXISTS idx_referral_patid
  ON @source_schema.referral (patid);
CREATE INDEX IF NOT EXISTS idx_referral_medcode
  ON @source_schema.referral (medcode);

CREATE INDEX IF NOT EXISTS idx_staff_staffid
  ON @source_schema.staff (staffid);

CREATE INDEX IF NOT EXISTS idx_test_patid
  ON @source_schema.test (patid);
CREATE INDEX IF NOT EXISTS idx_test_medcode
  ON @source_schema.test (medcode);
CREATE INDEX IF NOT EXISTS idx_test_enttype
  ON @source_schema.test (enttype);

CREATE INDEX IF NOT EXISTS idx_therapy_patid
  ON @source_schema.therapy (patid);
CREATE INDEX IF NOT EXISTS idx_therapy_prodcode
  ON @source_schema.therapy (prodcode);
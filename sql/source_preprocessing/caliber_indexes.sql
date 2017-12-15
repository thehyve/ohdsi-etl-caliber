-- Indexes
CREATE INDEX idx_additional_patid
  ON caliber.additional (patid);
CREATE INDEX idx_additional_enttype
  ON caliber.additional (enttype);
CREATE INDEX idx_additional_adid
  ON caliber.additional (adid);

CREATE INDEX idx_clinical_patid
  ON caliber.clinical (patid);
CREATE INDEX idx_clinical_enttype
  ON caliber.clinical (enttype);
CREATE INDEX idx_clinical_adid
  ON caliber.clinical (adid);
CREATE INDEX idx_clinical_medcode
  ON caliber.clinical (medcode);

CREATE INDEX idx_consultation_patid
  ON caliber.consultation (patid);
CREATE INDEX idx_consultation_consid
  ON caliber.consultation (consid);
CREATE INDEX idx_consultation_eventdate
  ON caliber.consultation (eventdate);

CREATE INDEX idx_hes_diag_epi_patid
  ON caliber.hes_diag_epi (patid);
CREATE INDEX idx_hes_diag_hosp_patid
  ON caliber.hes_diag_hosp (patid);
CREATE INDEX idx_hes_diag_hosp_icd
  ON caliber.hes_diag_hosp (icd);

CREATE INDEX idx_hes_op_appt_patid
  ON caliber.hes_op_appt (patid);
CREATE INDEX idx_hes_op_appt_attendkey
  ON caliber.hes_op_appt (attendkey);

CREATE INDEX idx_hes_op_clinical_patid
  ON caliber.hes_op_clinical (patid);
CREATE INDEX idx_hes_op_clinical_attendkey
  ON caliber.hes_op_clinical (attendkey);

CREATE INDEX idx_hes_op_clinical_diag_patid
  ON caliber.hes_op_clinical_diag (patid);
CREATE INDEX idx_hes_op_clinical_diag_attendkey
  ON caliber.hes_op_clinical_diag (attendkey);
CREATE INDEX idx_hes_op_clinical_diag_icd
  ON caliber.hes_op_clinical_diag (icd);
CREATE INDEX idx_hes_op_clinical_diag_newicd
  ON caliber.hes_op_clinical_diag (newicd);

CREATE INDEX idx_hes_op_clinical_proc_patid
  ON caliber.hes_op_clinical_proc (patid);
CREATE INDEX idx_hes_op_clinical_proc_attendkey
  ON caliber.hes_op_clinical_proc (attendkey);
CREATE INDEX idx_hes_op_clinical_proc_opcs
  ON caliber.hes_op_clinical_proc (opcs);

CREATE INDEX idx_hes_op_patient_patid
  ON caliber.hes_op_patient (patid);
CREATE INDEX idx_hes_patient_patid
  ON caliber.hes_patient (patid);

CREATE INDEX idx_hes_proc_epi_patid
  ON caliber.hes_proc_epi (patid);

CREATE INDEX idx_immunisation_patid
  ON caliber.immunisation (patid);
CREATE INDEX idx_immunisation_medcode
  ON caliber.immunisation (medcode);

-- TODO: rename lookup tables to lookup_***
-- CREATE INDEX idx_lookup_ANYTHING_code
--   ON caliber.lookup_ANYTHING (code);
-- CREATE INDEX idx_lookup_entity_enttype
--   ON caliber.lookup_entity (enttype);
CREATE INDEX idx_lookup_linkage_eligibility_patid
  ON caliber.lookup_linkage_eligibility (patid);
-- CREATE INDEX idx_lookup_medical_medcode
--   ON caliber.lookup_medical (medcode);
-- CREATE INDEX idx_lookup_product_prodcode
--   ON caliber.lookup_product (prodcode);

CREATE INDEX idx_ons_death_patid
  ON caliber.ons_death (patid);
CREATE INDEX idx_ons_imd_patid
  ON caliber.ons_imd (patid);

CREATE INDEX idx_patient_patid
  ON caliber.patient (patid);
CREATE INDEX idx_patient_accept
  ON caliber.patient (accept);
CREATE INDEX idx_patient_gender
  ON caliber.patient (gender);

CREATE INDEX idx_practice_pracid
  ON caliber.practice (pracid);

CREATE INDEX idx_referral_patid
  ON caliber.referral (patid);
CREATE INDEX idx_referral_medcode
  ON caliber.referral (medcode);

CREATE INDEX idx_staff_staffid
  ON caliber.staff (staffid);

CREATE INDEX idx_test_patid
  ON caliber.test (patid);
CREATE INDEX idx_test_medcode
  ON caliber.test (medcode);
CREATE INDEX idx_test_enttype
  ON caliber.test (enttype);

CREATE INDEX idx_therapy_patid
  ON caliber.therapy (patid);
CREATE INDEX idx_therapy_prodcode
  ON caliber.therapy (prodcode);
-- Find the OMOP concept_id for a given ICD9/10 code if no direct match can be found
-- In each iteration the last character will be truncated from the code to search for a (more general) match
-- If no match can be found for the first three characters, NULL will be returned

CREATE OR REPLACE FUNCTION mapIcdCode(code CHAR)
  RETURNS INTEGER
AS $$
  if not code:
    return None
  icd_code = code
  icd10 = False if (icd_code and icd_code[0].isnumeric()) else True

  query = "SELECT concept_id FROM cdm5.concept WHERE concept_code = '{0}' AND vocabulary_id = '{1}';"
  while len(icd_code) >= 3:
    if not icd_code.endswith('.'):
        if icd10:
            match = plpy.execute(query.format(icd_code, 'ICD10'))
            if match:
                return match[0]['concept_id']
        match = plpy.execute(query.format(icd_code, 'ICD9CM'))
        if match:
            return match[0]['concept_id']
    icd_code = icd_code[:-1]
  return None
$$ LANGUAGE plpython3u;

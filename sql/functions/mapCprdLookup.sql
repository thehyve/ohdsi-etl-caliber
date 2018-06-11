/*
Mapping of small CPRD lookups. Maps the description to a standard concept.
If null given, then null returned (STRICT evaluation)
TODO: create this as a source_to_concept_map for better maintainability
*/
CREATE OR REPLACE FUNCTION mapCprdLookup(lookupDescription text)
  RETURNS INTEGER AS
$$
BEGIN
  RETURN CASE lookupDescription
           WHEN 'Not examined'       THEN 4301433
           WHEN 'Potential Abnormal' THEN 4214712
           WHEN 'Present'            THEN 4181412
           WHEN 'Unknown'            THEN 4129922
           WHEN 'Normal'             THEN 4069590
           WHEN 'Absent'             THEN 4132135
           WHEN 'Abnormal'           THEN 4135493
           WHEN 'A'                  THEN 4008253
           WHEN 'A+'                 THEN 4082948
           WHEN 'A-'                 THEN 4080397
           WHEN 'B'                  THEN 4009006
           WHEN 'B+'                 THEN 4175555
           WHEN 'B-'                 THEN 4080398
           WHEN 'O'                  THEN 4237761
           WHEN 'O+'                 THEN 4080395
           WHEN 'O-'                 THEN 4082947
           WHEN 'AB'                 THEN 4013993
           WHEN 'AB+'                THEN 4080396
           WHEN 'AB-'                THEN 4082949
           WHEN 'Rhesus +'           THEN 4013995
           WHEN 'Rhesus -'           THEN 4013540
           WHEN 'Yes'                THEN 4188539
           WHEN 'No'                 THEN 4188540
           WHEN 'Ex'                 THEN 4188540
           WHEN 'Low'                THEN 4267416
           WHEN 'High'               THEN 4328749
           WHEN 'Negative'           THEN 9189
           WHEN 'Positive'           THEN 9191
           WHEN 'Inactive'           THEN 9173 -- Inactive
           WHEN 'Moderate'           THEN 4285732 -- Moderate
           WHEN 'Vigorous'           THEN 4171868 -- Vigorously
           WHEN 'Gentle'             THEN 4302792 -- Gently
         ELSE 0 -- 'Not mapped'
       END;
END;
$$ LANGUAGE plpgsql STRICT;

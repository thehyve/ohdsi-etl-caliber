/*
 * Test creation of HES Appointment visit occurrence id
 *
 * Copyright 2018 The Hyve
 *
 * Licensed under the GNU General Public License, version 3,
 * or (at your option) any later version (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.gnu.org/licenses/
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

WITH cases (expected, actual) AS (
  VALUES
     (503923813416656503, createHesApptVisitId('503923813416',6565030))
    ,(503923813416006196, createHesApptVisitId('503923813416',6196))
    ,(502981580400110971, createHesApptVisitId('502981580400',11097168))
    ,(502981580400743198, createHesApptVisitId('502981580400',743198))
    ,(309757565441178413, createHesApptVisitId('309757565441',17841337))
    -- clashes with the above because last two patid digits not used
    ,(309757565441178413, createHesApptVisitId('309757565441',17841389))
    ,(309757565458841389, createHesApptVisitId('309757565458',841389))
)
SELECT
  CASE
    WHEN expected = actual THEN 'passed'
    WHEN expected ISNULL AND actual ISNULL THEN 'passed'
    ELSE 'FAILED'
  END,
  expected,
  actual
FROM cases
;
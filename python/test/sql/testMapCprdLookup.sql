/*
 * Test mapCprdLookup
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
     (4069590, mapCprdLookup('Normal'))
    ,(4082948, mapCprdLookup('A+'))
    ,(4328749, mapCprdLookup('High'))
    ,(0, mapCprdLookup('not a match'))
    ,(0, mapCprdLookup(''))
    ,(NULL, mapCprdLookup(NULL))
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
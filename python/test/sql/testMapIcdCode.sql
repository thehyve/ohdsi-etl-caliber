/*
 * Test mapIcdCode
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
    (45601158, mapicdcode('K25.5x')), -- full ICD10 map
    (44833450, mapicdcode('349x')), -- full ICD9 map
    (45756398, mapicdcode('X42.0x')), -- Maps to X42
    (42616305, mapicdcode('M00.0000000')), -- Maps to M00.00
    (45591453, mapicdcode('I10')), -- full ICD10 map on complete given code
    (45568120, mapicdcode('R20.2')),
    (NULL, mapicdcode('QWERTY')), -- No map
    (NULL, mapicdcode('')), -- No map
    (NULL, mapicdcode(NULL)) -- No map
)
SELECT
  CASE
  WHEN expected :: INT = actual
    THEN 'passed'
  WHEN expected ISNULL AND actual ISNULL
    THEN 'passed'
  ELSE 'FAILED'
  END,
  expected,
  actual
FROM cases;

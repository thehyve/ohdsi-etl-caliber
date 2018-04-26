/*
 * Test createEndDate
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
    (to_date('20120409', 'yyyymmdd'), createEndDate(to_date('20120409', 'yyyymmdd'), 1))
    ,(to_date('20120410', 'yyyymmdd'), createEndDate(to_date('20120409', 'yyyymmdd'), 2))
    ,(to_date('20120418', 'yyyymmdd'), createEndDate(to_date('20120409', 'yyyymmdd'), 10))
    ,(to_date('20160407', 'yyyymmdd'), createEndDate(to_date('20150409', 'yyyymmdd'), 365))
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
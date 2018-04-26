/*
 * Test createCareSiteId
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
     (1, createCareSiteId(1001))
    ,(20, createCareSiteId(2020))
    ,(19, createCareSiteId(250019))
    ,(15, createCareSiteId(242015))
    ,(456, createCareSiteId(123456))
    ,(456, createCareSiteId(CAST(123456 AS BIGINT)))
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
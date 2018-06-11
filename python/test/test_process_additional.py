# Copyright 2018 The Hyve
#
# Licensed under the GNU General Public License, version 3,
# or (at your option) any later version (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# https://www.gnu.org/licenses/
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

import unittest
from python.process_additional import create_value, create_score_value


class TestCreateValue(unittest.TestCase):
    """
    Input for create_value:
    patid, adid, enttype_string, data_value, data_name, data_lookup, unit_code
    Output is a sql value string with the following columns:
    patid, adid, enttype-string, datafield_name, data_value, data_code, data_date, lookup_type, unit_code
    """
    def test_create_value_lookup(self):
        # Lookup code
        result = create_value(1, 371198, '30-1', '2', 'Type of Exercise', 'EXE', None)
        self.assertEqual(result, "(1,371198,'30-1','Type of Exercise',NULL,'2',NULL,'EXE',NULL)")

    def test_create_value(self):
        # Value
        result = create_value(1, 2, '1-1', '64.3', 'Diastolic', None, None)
        self.assertEqual(result, "(1,2,'1-1','Diastolic',64.3,NULL,NULL,NULL,NULL)")

        # Value with unit
        result = create_value(205006, 2, '120-1', '5', 'Weeks', None, '52')
        self.assertEqual(result, "(205006,2,'120-1','Weeks',5,NULL,NULL,NULL,'52')")

    def test_empty_string(self):
        # Value with empty string instead of None
        result = create_value(1, 2, '1-1', '64.3', 'Diastolic', '', '')
        self.assertEqual(result, "(1,2,'1-1','Diastolic',64.3,NULL,NULL,NULL,NULL)")

    def test_unit(self):
        # Do not include unit code '0'
        result = create_value(205006, 2, '120-1', '5', 'Weeks', None, '0')
        self.assertEqual(result, "(205006,2,'120-1','Weeks',5,NULL,NULL,NULL,NULL)")

    def test_dates(self):
        # Dates
        result = create_value(9013, 529850, '20-4', '15/01/1996', 'Last claim date', 'dd/mm/yyyy', None)
        self.assertEqual(result, "(9013,529850,'20-4','Last claim date',NULL,NULL,'1996-01-15','dd/mm/yyyy',NULL)")

    def test_empty_value(self):
        result = create_value(1, 2, '1-1', None, 'Diastolic', None, None)
        self.assertEqual(result, "(1,2,'1-1','Diastolic',NULL,NULL,NULL,NULL,NULL)")

    def test_skip(self):
        # Skip if lookup and code is 0 ('data not entered')
        result = create_value(1, 371198, '30-1', '0', 'Type of Exercise', 'EXE', None)
        self.assertEqual(result, None)

        # Skip if lookup is SUM
        result = create_value(205006, 2, '120-2', '5', 'Unit of measure', 'SUM', None)
        self.assertEqual(result, None)

    def test_create_score_value(self):
        """
        Input of create_score_value is:
        patid, adid, enttype, data_values, data_names
        Output is a sql value string with the following columns:
        patid, adid, enttype-string, datafield_name, data_value, data_code, data_date, lookup_type, unit_code
        """
        result = create_score_value(1,
                                    2,
                                    372,
                                    ['30.185','13259','17','0'],
                                    ['Result of test','Condition','Scoring Methodology','Qualifier']
                                    )
        self.assertEqual(result, "(1,2,'372-17-13259','Result of test',30.185,NULL,NULL,NULL,NULL)")


if __name__ == '__main__':
    unittest.main()

import unittest
from python.process_additional import create_value, create_score_value


class TestStringMethods(unittest.TestCase):

    def test_create_value(self):
        """
        Output is a sql value string with the following columns:
        patid, adid, enttype-string, datafield_name, data_value, data_code, data_date, lookup_type, unit_code
        """
        # Lookup code
        result = create_value(1, 371198, '30-1', '2', 'Type of Exercise', 'EXE', None)
        self.assertEqual(result, "(1,371198,'30-1','Type of Exercise',NULL,'2',NULL,'EXE',NULL)")

        # Value
        result = create_value(1, 2, '1-1', '64.3', 'Diastolic', None, None)
        self.assertEqual(result, "(1,2,'1-1','Diastolic',64.3,NULL,NULL,NULL,NULL)")

        # Value with unit
        result = create_value(205006, 2, '120-1', '5', 'Weeks', None, '52')
        self.assertEqual(result, "(205006,2,'120-1','Weeks',5,NULL,NULL,NULL,'52')")

        # But do not include unit code '0'
        result = create_value(205006, 2, '120-1', '5', 'Weeks', None, '0')
        self.assertEqual(result, "(205006,2,'120-1','Weeks',5,NULL,NULL,NULL,NULL)")

        # Dates
        result = create_value(9013, 529850, '20-4', '15/01/1996', 'Last claim date', 'dd/mm/yyyy', None)
        self.assertEqual(result, "(9013,529850,'20-4','Last claim date',NULL,NULL,'1996-01-15','dd/mm/yyyy',NULL)")

        # Skip if no value
        result = create_value(1, 2, '1-1', None, 'Diastolic', None, None)
        self.assertEqual(result, None)

        # Skip if lookup and code is 0
        result = create_value(1, 371198, '30-1', '0', 'Type of Exercise', 'EXE', None)
        self.assertEqual(result, None)

        # Skip lookup is SUM
        result = create_value(205006, 2, '120-2', '5', 'Unit of measure', 'SUM', None)
        self.assertEqual(result, None)

    def test_create_score_value(self):
        """
        Output is a sql value string with the following columns:
        patid, adid, enttype-string, datafield_name, data_value, data_code, data_date, lookup_type, unit_code
        """
        result = create_score_value(1, 2, 372,
                                    ['30.185','13259','17','0'],
                                    ['Result of test','Condition','Scoring Methodology','Qualifier']
                                    )
        self.assertEqual(result, "(1,2,'372-17-13259','Result of test',30.185,NULL,NULL,NULL,NULL)")


if __name__ == '__main__':
    unittest.main()
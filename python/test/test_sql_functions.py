import unittest
import os
from sqlalchemy import create_engine


class TestSqlFunctions(unittest.TestCase):
    SQL_FUNCTIONS_DIRECTORY = "sql/functions"
    SQL_TESTS_DIRECTORY = "python/test/sql"

    @classmethod
    def setUpClass(cls):
        engine = create_engine('postgresql://%s:%s@%s:%s/%s' % ('postgres', '', '127.0.0.1', 5432, 'postgres'))
        cls._connection = engine.connect()

    def testCreateCareSiteId(self):
        self._testSqlFunction('createCareSiteId', 'testCreateCareSiteId')

    def testCreateEndDate(self):
        self._testSqlFunction('createEndDate', 'testCreateEndDate')

    def testCreateHesApptVisitId(self):
        self._testSqlFunction('createHesApptVisitId', 'testCreateHesApptVisitId')

    def testCreateVisitId(self):
        self._testSqlFunction('createVisitId', 'testCreateVisitId')

    def testMapCprdLookup(self):
        self._testSqlFunction('mapCprdLookup', 'testMapCprdLookup')

    def testMapIcdCode(self):
        raise NotImplementedError("Test of mapIcdCode is disabled due to dependency on plpython3u")
        # self._testSqlFunction('mapIcdCode', 'testMapIcdCode')

    def _testSqlFunction(self, function_name, test_name):
        """
        Creates the given function and runs the test.
        :param function_name: name of the function, should equal the name of the file
        :param test_name: name of the test file without extension
        """
        # TODO: also drop function afterwards (need argument types for that)
        with open(os.path.join(self.SQL_FUNCTIONS_DIRECTORY, function_name + '.sql')) as f_function,\
             open(os.path.join(self.SQL_TESTS_DIRECTORY, test_name + '.sql')) as f_test:
            self.__class__._connection.execute(f_function.read())
            test_result = self.__class__._connection.execute(f_test.read())

        tests = test_result.fetchall()

        for test_result in tests:
            self.assertEqual(test_result[0], 'passed', "This test has not passed: %s" % test_result)

        # self.__class__._connection.execute("DROP FUNCTION IF EXISTS %s;" % function_name)

    @classmethod
    def tearDownClass(cls):
        cls._connection.close()


if __name__ == '__main__':
    unittest.main()
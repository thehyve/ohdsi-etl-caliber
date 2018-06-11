source('TestingFramework.R')

# get_defaults_patient()
# get_defaults_practice()

# Define
initFramework()
declareTest(1, "Patient valid dates")
add_practice(pracid = 1, uts = "2016-01-01", lcd = "2017-01-01")
add_patient(patid = 1001)
add_patient(patid = 2001, tod = "2016-03-03", deathdate = "2016-06-06")
add_patient(patid = 3002, crd = "2000-01-01", tod = "2010-10-10")

# expect_person(person_id = 111001)
expect_observation_period(observation_period_id = 1, 
                          person_id = 1001, 
                          observation_period_start_date = "2016-01-01", 
                          observation_period_end_date = "2017-01-01")
expect_observation_period(observation_period_id = 2, 
                          person_id = 2001, 
                          observation_period_start_date = "2016-01-01", 
                          observation_period_end_date = "2016-03-03")
expect_observation_period(observation_period_id = 3, 
                          person_id = 3002, 
                          observation_period_start_date = "2000-01-01", 
                          observation_period_end_date = "2010-10-10")

###
# Enttype 30 (type of exercise) has 1 data field
###
declareTest(2, "Additional - One observation (enttype 30)")
add_clinical(patid = 1001, adid=101, eventdate = '01-01-2000')
add_additional(patid = 1001, adid=101, enttype = 30, data1 = 1, data2 = NULL) # data3 t/m 8 are empty by default

expect_observation(person_id = 1001, observation_source_value = '30-1', observation_concept_id = 40758539, value_as_string = 'Inactive')
expect_no_observation(person_id = 1001, observation_source_value = '30-2')

###
# Enttype 1 (blood pressure) has 7 data fields. First two are measurements, last five observations
###
declareTest(3, "Additional - Measurements and observations (enttype 1)")
add_clinical(patid = 1001, adid=102, eventdate = '01-01-2000')
add_additional(patid = 1001, adid=102, enttype = 1, data1 = 70, data2 = 120)

expect_measurement(person_id = 1001, measurement_source_value = '1-1', measurement_concept_id = 3004249, value_as_number = 70, unit_concept_id = 8876)
expect_measurement(person_id = 1001, measurement_source_value = '1-2', measurement_concept_id = 3012888, value_as_number = 120, unit_concept_id = 8876)
expect_observation(person_id = 1001, observation_source_value = '1-3')
expect_observation(person_id = 1001, observation_source_value = '1-4')
expect_observation(person_id = 1001, observation_source_value = '1-5')
expect_observation(person_id = 1001, observation_source_value = '1-6')
expect_observation(person_id = 1001, observation_source_value = '1-7')

###
# If value 0 (data not entered), do not include.
###
declareTest(2, "Additional - No data entered")
add_clinical(patid = 2001, adid=101, eventdate = '01-01-2000')
add_additional(patid = 2001, adid=101, enttype = 30, data1 = 0, data2 = NULL) # data3 t/m 8 are empty by default

expect_no_observation(person_id = 2001, observation_source_value = '30-1')


###
# Execute
# If caliberTest does not exist:
#  - Create caliberTest schema
#  - Run WhiteRabbit to create (empty) source tables 
###

library(DatabaseConnector)
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             user = "postgres",
                                             password = "heartdatabig",
                                             server = "localhost/caliber_dev",
                                             port = 6000)
connection <- connect(connectionDetails)

insert_sql <- generateInsertSql()
executeSql(connection, paste(c("SET search_path TO caliberTest;", insert_sql), collapse = "\n"))

### 
# Test
# Run ETL on source caliberTest (to cdm5)
# python main.py -w heartdatabig -s caliberTest --skipvocab
###

test_sql <- testSql
test_sql[1] <- "DROP TABLE IF EXISTS cdm5.test_results;"
executeSql(connection, paste(c("SET search_path TO cdm5;", test_sql), collapse = ";\n"))
querySql(connection, "SELECT * FROM cdm5.test_results")
# querySql(connection, "SELECT * FROM cdm5.observation_period")

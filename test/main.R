library("yaml")
source('TestingFramework.R')

config <- yaml.load_file("config.yml")

get_defaults_patient()
get_defaults_practice()

# Define
initFramework()
declareTest(1, "Observation Period Derivation")
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
                          observation_period_end_date = "2016-06-06")
expect_observation_period(observation_period_id = 3, 
                          person_id = 3002, 
                          observation_period_start_date = "2000-01-01", 
                          observation_period_end_date = "2010-10-10")

# Execute
library(DatabaseConnector)
connectionConfig <- config$connectionDetails
connectionDetails <- createConnectionDetails(dbms = 'postgresql',
                                             user = connectionConfig$user,
                                             password = connectionConfig$password,
                                             server = connectionConfig$server,
                                             port = connectionConfig$port)
connection <- connect(connectionDetails)

#executeSql(connection, "CREATE SCHEMA caliberTest;")

### 
# Run WhiteRabbit to create (empty) source tables 
###
insert_sql <- generateInsertSql()
executeSql(connection, sprintf("SET search_path TO %s;", config$sourceSchema))
executeSql(connection, paste(insert_sql, collapse = "\n"))

### 
# Run ETL on source caliberTest (to cdm5)
###

test_sql <- testSql
executeSql(connection, sprintf("SET search_path TO %s;", config$cdmSchema))
test_sql[1] <- "DROP TABLE IF EXISTS test_results;" # Replace existing SQL server specific table drop
executeSql(connection, paste(test_sql, collapse = ";\n"))


querySql(connection, "SELECT * FROM test_results")
querySql(connection, "SELECT * FROM observation_period")

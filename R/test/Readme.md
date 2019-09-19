# Unit tests for the CALIBER ETL
## Setup
Copy `config.example.yml` and rename to `config.yml`. Enter your connection details.

The `sourceSchema` is the schema where the mock data will be inserted. 
Create this schema and the (empty) source tables separately (e.g. with the White Rabbit Fake data generator).

## Usage
Run `main.R` in RStudio up to the test data insertion.
Then, run your ETL on the test data.
Finally, run the last part of `main.R` to execute the tests.

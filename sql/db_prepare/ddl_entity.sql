DROP TABLE IF EXISTS caliber.entity;
CREATE TABLE caliber.entity (
	enttype integer NOT NULL,
	description varchar(255) NOT NULL,
	filetype varchar(10) NOT NULL,
	category varchar(50) NULL,
	data_fields integer NOT NULL,
	data1 varchar(50) NULL,
	data1_lkup varchar(50) NULL,
	data2 varchar(50) NULL,
	data2_lkup varchar(50) NULL,
	data3 varchar(50) NULL,
	data3_lkup varchar(50) NULL,
	data4 varchar(50) NULL,
	data4_lkup varchar(50) NULL,
	data5 varchar(50) NULL,
	data5_lkup varchar(50) NULL,
	data6 varchar(50) NULL,
	data6_lkup varchar(50) NULL,
	data7 varchar(50) NULL,
	data7_lkup varchar(50) NULL,
	data8 varchar(50) NULL,
	data8_lkup varchar(50) NULL
)

DROP TABLE IF EXISTS caliber.entity;
CREATE TABLE caliber.entity (
	enttype 	integer NOT NULL,
	description text NOT NULL,
	filetype 	varchar(10) NOT NULL,
	category 	text,
	data_fields integer NOT NULL,
	data1 		text,
	data1_lkup 	text,
	data2 		text,
	data2_lkup 	text,
	data3 		text,
	data3_lkup 	text,
	data4 		text,
	data4_lkup 	text,
	data5 		text,
	data5_lkup 	text,
	data6 		text,
	data6_lkup 	text,
	data7 		text,
	data7_lkup 	text,
	data8 		text,
	data8_lkup 	text
);

DROP TABLE IF EXISTS caliber.medical;
CREATE TABLE caliber.medical (
	medcode 	integer NOT NULL,
	readcode 	varchar(8),
	readterm	text
);

DROP TABLE IF EXISTS caliber.product;
CREATE TABLE caliber.product (
	prodcode 		integer NOT NULL, 
	gemscriptcode 	varchar(8),
	productname		text,
	drugsubstance 	text,	
	strength 		text,
	formulation		text,
	route 			text,
	bnfcode			text,
	bnfchapter 		text
);
-- Strategus Tables
CREATE TABLE public.database_meta_data (
  	 cdm_source_name VARCHAR,
	 cdm_source_abbreviation VARCHAR,
	 cdm_holder VARCHAR,
	 source_description VARCHAR,
	 source_documentation_reference VARCHAR,
	 cdm_etl_reference VARCHAR,
	 source_release_date DATE,
	 cdm_release_date DATE,
	 cdm_version VARCHAR,
	 cdm_version_concept_id INT,
	 vocabulary_version VARCHAR,
	 database_id VARCHAR NOT NULL,
	 max_obs_period_end_date DATE,
	PRIMARY KEY(database_id)
);

-- Strategus Tables
{DEFAULT @table_prefix = ''}
{DEFAULT @database_meta_data = database_meta_data}
  
CREATE TABLE @database_schema.@table_prefix@database_meta_data (
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
-- CharacterizationModule Tables
{DEFAULT @table_prefix = ''}
{DEFAULT @c_time_to_event = c_time_to_event}
{DEFAULT @c_rechallenge_fail_case_series = c_rechallenge_fail_case_series}
{DEFAULT @c_dechallenge_rechallenge = c_dechallenge_rechallenge}
{DEFAULT @c_analysis_ref = c_analysis_ref}
{DEFAULT @c_covariate_ref = c_covariate_ref}
{DEFAULT @c_covariates = c_covariates}
{DEFAULT @c_covariates_continuous = c_covariates_continuous}
{DEFAULT @c_settings = c_settings}
{DEFAULT @c_cohort_details = c_cohort_details}
{DEFAULT @c_cohort_counts = c_cohort_counts}
  
CREATE TABLE @database_schema.@table_prefix@c_time_to_event (
  	 database_id VARCHAR(100) NOT NULL,
	 target_cohort_definition_id BIGINT NOT NULL,
	 outcome_cohort_definition_id BIGINT NOT NULL,
	 outcome_type VARCHAR(100) NOT NULL,
	 target_outcome_type VARCHAR(40) NOT NULL,
	 time_to_event INT NOT NULL,
	 num_events INT,
	 time_scale VARCHAR(20) NOT NULL,
	PRIMARY KEY(database_id,target_cohort_definition_id,outcome_cohort_definition_id,outcome_type,target_outcome_type,time_to_event,time_scale)
);
 
CREATE TABLE @database_schema.@table_prefix@c_rechallenge_fail_case_series (
  	 database_id VARCHAR(100) NOT NULL,
	 dechallenge_stop_interval INT NOT NULL,
	 dechallenge_evaluation_window INT NOT NULL,
	 target_cohort_definition_id BIGINT NOT NULL,
	 outcome_cohort_definition_id BIGINT NOT NULL,
	 person_key INT NOT NULL,
	 subject_id BIGINT,
	 dechallenge_exposure_number INT,
	 dechallenge_exposure_start_date_offset INT,
	 dechallenge_exposure_end_date_offset INT,
	 dechallenge_outcome_number INT,
	 dechallenge_outcome_start_date_offset INT,
	 rechallenge_exposure_number INT,
	 rechallenge_exposure_start_date_offset INT,
	 rechallenge_exposure_end_date_offset INT,
	 rechallenge_outcome_number INT,
	 rechallenge_outcome_start_date_offset INT,
	PRIMARY KEY(database_id,dechallenge_stop_interval,dechallenge_evaluation_window,target_cohort_definition_id,outcome_cohort_definition_id,person_key)
);
 
CREATE TABLE @database_schema.@table_prefix@c_dechallenge_rechallenge (
  	 database_id VARCHAR(100) NOT NULL,
	 dechallenge_stop_interval INT NOT NULL,
	 dechallenge_evaluation_window INT NOT NULL,
	 target_cohort_definition_id BIGINT NOT NULL,
	 outcome_cohort_definition_id BIGINT NOT NULL,
	 num_exposure_eras INT,
	 num_persons_exposed INT,
	 num_cases INT,
	 dechallenge_attempt INT,
	 dechallenge_fail INT,
	 dechallenge_success INT,
	 rechallenge_attempt INT,
	 rechallenge_fail INT,
	 rechallenge_success INT,
	 pct_dechallenge_attempt FLOAT,
	 pct_dechallenge_success FLOAT,
	 pct_dechallenge_fail FLOAT,
	 pct_rechallenge_attempt FLOAT,
	 pct_rechallenge_success FLOAT,
	 pct_rechallenge_fail FLOAT,
	PRIMARY KEY(database_id,dechallenge_stop_interval,dechallenge_evaluation_window,target_cohort_definition_id,outcome_cohort_definition_id)
);
 
CREATE TABLE @database_schema.@table_prefix@c_analysis_ref (
  	 database_id VARCHAR(100) NOT NULL,
	 setting_id VARCHAR(30) NOT NULL,
	 analysis_id INT NOT NULL,
	 analysis_name VARCHAR,
	 domain_id VARCHAR,
	 start_day INT,
	 end_day INT,
	 is_binary VARCHAR(1),
	 missing_means_zero VARCHAR(1),
	PRIMARY KEY(database_id,setting_id,analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@c_covariate_ref (
  	 database_id VARCHAR(100) NOT NULL,
	 setting_id VARCHAR(30) NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 covariate_name VARCHAR,
	 analysis_id INT,
	 concept_id BIGINT,
	 value_as_concept_id INT,
	 collisions INT,
	PRIMARY KEY(database_id,setting_id,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefix@c_covariates (
  	 database_id VARCHAR(100) NOT NULL,
	 setting_id VARCHAR(30) NOT NULL,
	 cohort_type VARCHAR(12) NOT NULL,
	 target_cohort_id INT NOT NULL,
	 outcome_cohort_id INT NOT NULL,
	 min_characterization_mean FLOAT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 sum_value INT,
	 average_value FLOAT,
	PRIMARY KEY(database_id,setting_id,cohort_type,target_cohort_id,outcome_cohort_id,min_characterization_mean,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefix@c_covariates_continuous (
  	 database_id VARCHAR(100) NOT NULL,
	 setting_id VARCHAR(30) NOT NULL,
	 cohort_type VARCHAR(12) NOT NULL,
	 target_cohort_id INT NOT NULL,
	 outcome_cohort_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 count_value INT,
	 min_value FLOAT,
	 max_value FLOAT,
	 average_value FLOAT,
	 standard_deviation FLOAT,
	 median_value FLOAT,
	 p_10_value FLOAT,
	 p_25_value FLOAT,
	 p_75_value FLOAT,
	 p_90_value FLOAT,
	PRIMARY KEY(database_id,setting_id,cohort_type,target_cohort_id,outcome_cohort_id,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefix@c_settings (
  	 setting_id VARCHAR(30) NOT NULL,
	 database_id VARCHAR(100) NOT NULL,
	 covariate_setting_json VARCHAR,
	 case_covariate_setting_json VARCHAR,
	 min_prior_observation INT,
	 outcome_washout_days INT,
	 risk_window_start INT,
	 risk_window_end INT,
	 start_anchor VARCHAR(15),
	 end_anchor VARCHAR(15),
	 case_pre_target_duration INT,
	 case_post_outcome_duration INT,
	PRIMARY KEY(setting_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@c_cohort_details (
  	 database_id VARCHAR(100) NOT NULL,
	 setting_id VARCHAR(30) NOT NULL,
	 cohort_type VARCHAR(12) NOT NULL,
	 target_cohort_id INT NOT NULL,
	 outcome_cohort_id INT NOT NULL,
	PRIMARY KEY(database_id,setting_id,cohort_type,target_cohort_id,outcome_cohort_id)
);
 
CREATE TABLE @database_schema.@table_prefix@c_cohort_counts (
  	 database_id VARCHAR(100),
	 cohort_type VARCHAR(12),
	 target_cohort_id INT,
	 outcome_cohort_id INT,
	 risk_window_start INT,
	 risk_window_end INT,
	 start_anchor VARCHAR(15),
	 end_anchor VARCHAR(15),
	 min_prior_observation INT,
	 outcome_washout_days INT,
	 row_count INT,
	 person_count INT,
	 min_exposure_time BIGINT,
	 mean_exposure_time BIGINT,
	 max_exposure_time BIGINT
);
-- CohortDiagnosticsModule Tables
{DEFAULT @table_prefix = ''}
{DEFAULT @cd_cohort = cd_cohort}
{DEFAULT @cd_subset_definition = cd_subset_definition}
{DEFAULT @cd_cohort_count = cd_cohort_count}
{DEFAULT @cd_cohort_inclusion = cd_cohort_inclusion}
{DEFAULT @cd_cohort_inc_result = cd_cohort_inc_result}
{DEFAULT @cd_cohort_inc_stats = cd_cohort_inc_stats}
{DEFAULT @cd_cohort_overlap = cd_cohort_overlap}
{DEFAULT @cd_cohort_relationships = cd_cohort_relationships}
{DEFAULT @cd_cohort_summary_stats = cd_cohort_summary_stats}
{DEFAULT @cd_concept = cd_concept}
{DEFAULT @cd_concept_ancestor = cd_concept_ancestor}
{DEFAULT @cd_concept_relationship = cd_concept_relationship}
{DEFAULT @cd_concept_sets = cd_concept_sets}
{DEFAULT @cd_concept_synonym = cd_concept_synonym}
{DEFAULT @cd_database = cd_database}
{DEFAULT @cd_domain = cd_domain}
{DEFAULT @cd_incidence_rate = cd_incidence_rate}
{DEFAULT @cd_included_source_concept = cd_included_source_concept}
{DEFAULT @cd_index_event_breakdown = cd_index_event_breakdown}
{DEFAULT @cd_metadata = cd_metadata}
{DEFAULT @cd_orphan_concept = cd_orphan_concept}
{DEFAULT @cd_relationship = cd_relationship}
{DEFAULT @cd_resolved_concepts = cd_resolved_concepts}
{DEFAULT @cd_temporal_analysis_ref = cd_temporal_analysis_ref}
{DEFAULT @cd_temporal_covariate_ref = cd_temporal_covariate_ref}
{DEFAULT @cd_temporal_covariate_value = cd_temporal_covariate_value}
{DEFAULT @cd_temporal_covariate_value_dist = cd_temporal_covariate_value_dist}
{DEFAULT @cd_temporal_time_ref = cd_temporal_time_ref}
{DEFAULT @cd_time_series = cd_time_series}
{DEFAULT @cd_visit_context = cd_visit_context}
{DEFAULT @cd_vocabulary = cd_vocabulary}
  
CREATE TABLE @database_schema.@table_prefix@cd_cohort (
  	 cohort_id BIGINT NOT NULL,
	 cohort_name VARCHAR,
	 metadata VARCHAR,
	 json VARCHAR,
	 sql VARCHAR,
	 subset_parent BIGINT,
	 subset_definition_id BIGINT,
	 is_subset INT,
	PRIMARY KEY(cohort_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_subset_definition (
  	 subset_definition_id BIGINT NOT NULL,
	 json VARCHAR NOT NULL,
	PRIMARY KEY(subset_definition_id,json)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_cohort_count (
  	 cohort_id BIGINT NOT NULL,
	 cohort_entries FLOAT,
	 cohort_subjects FLOAT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_cohort_inclusion (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 rule_sequence BIGINT NOT NULL,
	 name VARCHAR,
	 description VARCHAR,
	PRIMARY KEY(database_id,cohort_id,rule_sequence)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_cohort_inc_result (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 mode_id BIGINT NOT NULL,
	 inclusion_rule_mask BIGINT NOT NULL,
	 person_count FLOAT,
	PRIMARY KEY(database_id,cohort_id,mode_id,inclusion_rule_mask)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_cohort_inc_stats (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 rule_sequence BIGINT NOT NULL,
	 mode_id BIGINT NOT NULL,
	 person_count FLOAT,
	 gain_count FLOAT,
	 person_total FLOAT,
	PRIMARY KEY(database_id,cohort_id,rule_sequence,mode_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_cohort_overlap (
  	 either_subjects FLOAT,
	 both_subjects FLOAT,
	 t_only_subjects FLOAT,
	 c_only_subjects FLOAT,
	 t_before_c_subjects FLOAT,
	 c_before_t_subjects FLOAT,
	 same_day_subjects FLOAT,
	 t_in_c_subjects FLOAT,
	 c_in_t_subjects FLOAT,
	 target_cohort_id BIGINT NOT NULL,
	 comparator_cohort_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(target_cohort_id,comparator_cohort_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_cohort_relationships (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 comparator_cohort_id BIGINT NOT NULL,
	 start_day BIGINT NOT NULL,
	 end_day FLOAT NOT NULL,
	 subjects BIGINT,
	 sub_cs_before_ts BIGINT,
	 rec_cs_before_ts BIGINT,
	 sub_cs_on_ts BIGINT,
	 rec_cs_on_ts BIGINT,
	 sub_cs_after_ts BIGINT,
	 rec_cs_after_ts BIGINT,
	 sub_cs_before_te BIGINT,
	 rec_cs_before_te BIGINT,
	 sub_cs_on_te BIGINT,
	 rec_cs_on_te BIGINT,
	 sub_cs_after_te BIGINT,
	 rec_cs_after_te BIGINT,
	 sub_cs_window_t BIGINT,
	 rec_cs_window_t BIGINT,
	 sub_ce_window_t BIGINT,
	 rec_ce_window_t BIGINT,
	 sub_cs_window_ts BIGINT,
	 rec_cs_window_ts BIGINT,
	 sub_cs_window_te BIGINT,
	 rec_cs_window_te BIGINT,
	 sub_ce_window_ts BIGINT,
	 rec_ce_window_ts BIGINT,
	 sub_ce_window_te BIGINT,
	 rec_ce_window_te BIGINT,
	 sub_c_within_t BIGINT,
	 rec_c_within_t BIGINT,
	 c_days_before_ts BIGINT,
	 c_days_before_te BIGINT,
	 c_days_within_t_days BIGINT,
	 c_days_after_ts BIGINT,
	 c_days_after_te BIGINT,
	 t_days BIGINT,
	 c_days BIGINT,
	PRIMARY KEY(database_id,cohort_id,comparator_cohort_id,start_day,end_day)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_cohort_summary_stats (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 mode_id BIGINT NOT NULL,
	 base_count FLOAT,
	 final_count FLOAT,
	PRIMARY KEY(database_id,cohort_id,mode_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_concept (
  	 concept_id BIGINT NOT NULL,
	 concept_name VARCHAR(255),
	 domain_id VARCHAR(20),
	 vocabulary_id VARCHAR(50),
	 concept_class_id VARCHAR(20),
	 standard_concept VARCHAR(1),
	 concept_code VARCHAR(255),
	 valid_start_date DATE,
	 valid_end_date DATE,
	 invalid_reason VARCHAR,
	PRIMARY KEY(concept_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_concept_ancestor (
  	 ancestor_concept_id BIGINT NOT NULL,
	 descendant_concept_id BIGINT NOT NULL,
	 min_levels_of_separation INT,
	 max_levels_of_separation INT,
	PRIMARY KEY(ancestor_concept_id,descendant_concept_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_concept_relationship (
  	 concept_id_1 BIGINT NOT NULL,
	 concept_id_2 BIGINT NOT NULL,
	 relationship_id VARCHAR(20) NOT NULL,
	 valid_start_date DATE,
	 valid_end_date DATE,
	 invalid_reason VARCHAR(1),
	PRIMARY KEY(concept_id_1,concept_id_2,relationship_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_concept_sets (
  	 cohort_id BIGINT NOT NULL,
	 concept_set_id INT NOT NULL,
	 concept_set_sql VARCHAR,
	 concept_set_name VARCHAR(255),
	 concept_set_expression VARCHAR,
	PRIMARY KEY(cohort_id,concept_set_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_concept_synonym (
  	 concept_id BIGINT NOT NULL,
	 concept_synonym_name VARCHAR NOT NULL,
	 language_concept_id BIGINT NOT NULL,
	PRIMARY KEY(concept_id,concept_synonym_name,language_concept_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_database (
  	 database_id VARCHAR NOT NULL,
	 database_name VARCHAR,
	 description VARCHAR,
	 is_meta_analysis VARCHAR(1),
	 vocabulary_version VARCHAR,
	 vocabulary_version_cdm VARCHAR,
	PRIMARY KEY(database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_domain (
  	 domain_id VARCHAR(20) NOT NULL,
	 domain_name VARCHAR(255),
	 domain_concept_id BIGINT,
	PRIMARY KEY(domain_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_incidence_rate (
  	 cohort_count FLOAT,
	 person_years FLOAT,
	 gender VARCHAR NOT NULL,
	 age_group VARCHAR NOT NULL,
	 calendar_year VARCHAR(4) NOT NULL,
	 incidence_rate FLOAT,
	 cohort_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(gender,age_group,calendar_year,cohort_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_included_source_concept (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 concept_set_id INT NOT NULL,
	 concept_id BIGINT NOT NULL,
	 source_concept_id BIGINT NOT NULL,
	 concept_subjects FLOAT,
	 concept_count FLOAT,
	PRIMARY KEY(database_id,cohort_id,concept_set_id,concept_id,source_concept_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_index_event_breakdown (
  	 concept_id BIGINT NOT NULL,
	 concept_count FLOAT,
	 subject_count FLOAT,
	 cohort_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 domain_field VARCHAR NOT NULL,
	 domain_table VARCHAR NOT NULL,
	PRIMARY KEY(concept_id,cohort_id,database_id,domain_field,domain_table)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_metadata (
  	 database_id VARCHAR NOT NULL,
	 start_time VARCHAR NOT NULL,
	 variable_field VARCHAR NOT NULL,
	 value_field VARCHAR,
	PRIMARY KEY(database_id,start_time,variable_field)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_orphan_concept (
  	 cohort_id BIGINT NOT NULL,
	 concept_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 concept_id BIGINT NOT NULL,
	 concept_count FLOAT,
	 concept_subjects FLOAT,
	PRIMARY KEY(cohort_id,concept_set_id,database_id,concept_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_relationship (
  	 relationship_id VARCHAR(20) NOT NULL,
	 relationship_name VARCHAR(255),
	 is_hierarchical VARCHAR(1),
	 defines_ancestry VARCHAR(1),
	 reverse_relationship_id VARCHAR(20) NOT NULL,
	 relationship_concept_id BIGINT NOT NULL,
	PRIMARY KEY(relationship_id,reverse_relationship_id,relationship_concept_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_resolved_concepts (
  	 cohort_id BIGINT NOT NULL,
	 concept_set_id INT NOT NULL,
	 concept_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,concept_set_id,concept_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_temporal_analysis_ref (
  	 analysis_id INT NOT NULL,
	 analysis_name VARCHAR,
	 domain_id VARCHAR(20) NOT NULL,
	 is_binary VARCHAR(1),
	 missing_means_zero VARCHAR(1),
	PRIMARY KEY(analysis_id,domain_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_temporal_covariate_ref (
  	 covariate_id BIGINT NOT NULL,
	 covariate_name VARCHAR,
	 analysis_id INT,
	 concept_id BIGINT,
	PRIMARY KEY(covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_temporal_covariate_value (
  	 cohort_id BIGINT NOT NULL,
	 time_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 sum_value FLOAT,
	 mean FLOAT,
	 sd FLOAT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,time_id,covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_temporal_covariate_value_dist (
  	 cohort_id BIGINT NOT NULL,
	 time_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 count_value FLOAT,
	 min_value FLOAT,
	 max_value FLOAT,
	 mean FLOAT,
	 sd FLOAT,
	 median_value FLOAT,
	 p_10_value FLOAT,
	 p_25_value FLOAT,
	 p_75_value FLOAT,
	 p_90_value FLOAT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,time_id,covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_temporal_time_ref (
  	 time_id INT NOT NULL,
	 start_day FLOAT,
	 end_day FLOAT,
	PRIMARY KEY(time_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_time_series (
  	 cohort_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 period_begin DATE NOT NULL,
	 period_end DATE NOT NULL,
	 series_type VARCHAR NOT NULL,
	 calendar_interval VARCHAR NOT NULL,
	 gender VARCHAR NOT NULL,
	 age_group VARCHAR NOT NULL,
	 records BIGINT,
	 subjects BIGINT,
	 person_days BIGINT,
	 person_days_in BIGINT,
	 records_start BIGINT,
	 subjects_start BIGINT,
	 subjects_start_in BIGINT,
	 records_end BIGINT,
	 subjects_end BIGINT,
	 subjects_end_in BIGINT,
	PRIMARY KEY(cohort_id,database_id,period_begin,period_end,series_type,calendar_interval,gender,age_group)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_visit_context (
  	 cohort_id BIGINT NOT NULL,
	 visit_concept_id BIGINT NOT NULL,
	 visit_context VARCHAR NOT NULL,
	 subjects FLOAT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,visit_concept_id,visit_context,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cd_vocabulary (
  	 vocabulary_id VARCHAR(50),
	 vocabulary_name VARCHAR(255),
	 vocabulary_reference VARCHAR,
	 vocabulary_version VARCHAR,
	 vocabulary_concept_id BIGINT
);
-- CohortGeneratorModule Tables
{DEFAULT @table_prefix = ''}
{DEFAULT @cg_cohort_definition = cg_cohort_definition}
{DEFAULT @cg_cohort_generation = cg_cohort_generation}
{DEFAULT @cg_cohort_inclusion = cg_cohort_inclusion}
{DEFAULT @cg_cohort_inc_result = cg_cohort_inc_result}
{DEFAULT @cg_cohort_inc_stats = cg_cohort_inc_stats}
{DEFAULT @cg_cohort_summary_stats = cg_cohort_summary_stats}
{DEFAULT @cg_cohort_censor_stats = cg_cohort_censor_stats}
{DEFAULT @cg_cohort_count = cg_cohort_count}
{DEFAULT @cg_cohort_count_neg_ctrl = cg_cohort_count_neg_ctrl}
{DEFAULT @cg_cohort_subset_definition = cg_cohort_subset_definition}
{DEFAULT @cg_cohort_definition_neg_ctrl = cg_cohort_definition_neg_ctrl}
  
CREATE TABLE @database_schema.@table_prefix@cg_cohort_definition (
  	 cohort_definition_id BIGINT NOT NULL,
	 cohort_name VARCHAR,
	 description VARCHAR,
	 json TEXT,
	 sql_command TEXT,
	 subset_parent BIGINT,
	 is_subset INT,
	 subset_definition_id BIGINT,
	PRIMARY KEY(cohort_definition_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_generation (
  	 cohort_id BIGINT NOT NULL,
	 cohort_name VARCHAR,
	 generation_status VARCHAR,
	 start_time TIMESTAMP,
	 end_time TIMESTAMP,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_inclusion (
  	 cohort_definition_id BIGINT NOT NULL,
	 rule_sequence INT NOT NULL,
	 name VARCHAR NOT NULL,
	 description VARCHAR,
	PRIMARY KEY(cohort_definition_id,rule_sequence,name)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_inc_result (
  	 database_id VARCHAR NOT NULL,
	 cohort_definition_id BIGINT NOT NULL,
	 inclusion_rule_mask INT NOT NULL,
	 person_count BIGINT NOT NULL,
	 mode_id INT NOT NULL,
	PRIMARY KEY(database_id,cohort_definition_id,inclusion_rule_mask,person_count,mode_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_inc_stats (
  	 database_id VARCHAR NOT NULL,
	 cohort_definition_id BIGINT NOT NULL,
	 rule_sequence INT NOT NULL,
	 person_count BIGINT NOT NULL,
	 gain_count BIGINT NOT NULL,
	 person_total BIGINT NOT NULL,
	 mode_id INT NOT NULL,
	PRIMARY KEY(database_id,cohort_definition_id,rule_sequence,person_count,gain_count,person_total,mode_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_summary_stats (
  	 database_id VARCHAR NOT NULL,
	 cohort_definition_id BIGINT NOT NULL,
	 base_count BIGINT NOT NULL,
	 final_count BIGINT NOT NULL,
	 mode_id INT NOT NULL,
	PRIMARY KEY(database_id,cohort_definition_id,base_count,final_count,mode_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_censor_stats (
  	 cohort_definition_id BIGINT NOT NULL,
	 lost_count BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_definition_id,lost_count,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_count (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 cohort_entries BIGINT NOT NULL,
	 cohort_subjects BIGINT NOT NULL,
	PRIMARY KEY(database_id,cohort_id,cohort_entries,cohort_subjects)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_count_neg_ctrl (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 cohort_entries BIGINT NOT NULL,
	 cohort_subjects BIGINT NOT NULL,
	PRIMARY KEY(database_id,cohort_id,cohort_entries,cohort_subjects)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_subset_definition (
  	 subset_definition_id BIGINT NOT NULL,
	 json TEXT,
	PRIMARY KEY(subset_definition_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cg_cohort_definition_neg_ctrl (
  	 cohort_id BIGINT NOT NULL,
	 outcome_concept_id BIGINT,
	 cohort_name VARCHAR,
	 occurrence_type VARCHAR,
	 detect_on_descendants INT,
	PRIMARY KEY(cohort_id)
);
-- CohortIncidenceModule Tables
{DEFAULT @table_prefix = ''}
{DEFAULT @ci_incidence_summary = ci_incidence_summary}
{DEFAULT @ci_target_def = ci_target_def}
{DEFAULT @ci_outcome_def = ci_outcome_def}
{DEFAULT @ci_tar_def = ci_tar_def}
{DEFAULT @ci_age_group_def = ci_age_group_def}
{DEFAULT @ci_subgroup_def = ci_subgroup_def}
{DEFAULT @ci_target_outcome_ref = ci_target_outcome_ref}
  
CREATE TABLE @database_schema.@table_prefix@ci_incidence_summary (
  	 ref_id INT,
	 database_id VARCHAR(255),
	 source_name VARCHAR(255),
	 target_cohort_definition_id BIGINT,
	 tar_id BIGINT,
	 subgroup_id BIGINT,
	 outcome_id BIGINT,
	 age_group_id INT,
	 gender_id INT,
	 gender_name VARCHAR(255),
	 start_year INT,
	 persons_at_risk_pe BIGINT,
	 persons_at_risk BIGINT,
	 person_days_pe BIGINT,
	 person_days BIGINT,
	 person_outcomes_pe BIGINT,
	 person_outcomes BIGINT,
	 outcomes_pe BIGINT,
	 outcomes BIGINT,
	 incidence_proportion_p100p FLOAT,
	 incidence_rate_p100py FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@ci_target_def (
  	 ref_id INT NOT NULL,
	 target_cohort_definition_id BIGINT NOT NULL,
	 target_name VARCHAR(255),
	PRIMARY KEY(ref_id,target_cohort_definition_id)
);
 
CREATE TABLE @database_schema.@table_prefix@ci_outcome_def (
  	 ref_id INT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 outcome_cohort_definition_id BIGINT,
	 outcome_name VARCHAR(255),
	 clean_window BIGINT,
	 excluded_cohort_definition_id BIGINT,
	PRIMARY KEY(ref_id,outcome_id)
);
 
CREATE TABLE @database_schema.@table_prefix@ci_tar_def (
  	 ref_id INT NOT NULL,
	 tar_id BIGINT NOT NULL,
	 tar_start_with VARCHAR(10),
	 tar_start_offset BIGINT,
	 tar_end_with VARCHAR(10),
	 tar_end_offset BIGINT,
	PRIMARY KEY(ref_id,tar_id)
);
 
CREATE TABLE @database_schema.@table_prefix@ci_age_group_def (
  	 ref_id INT NOT NULL,
	 age_group_id INT NOT NULL,
	 age_group_name VARCHAR(255),
	 min_age INT,
	 max_age INT,
	PRIMARY KEY(ref_id,age_group_id)
);
 
CREATE TABLE @database_schema.@table_prefix@ci_subgroup_def (
  	 ref_id INT NOT NULL,
	 subgroup_id BIGINT NOT NULL,
	 subgroup_name VARCHAR(255),
	PRIMARY KEY(ref_id,subgroup_id)
);
 
CREATE TABLE @database_schema.@table_prefix@ci_target_outcome_ref (
  	 ref_id INT NOT NULL,
	 target_cohort_id BIGINT NOT NULL,
	 outcome_cohort_id BIGINT NOT NULL,
	PRIMARY KEY(ref_id,target_cohort_id,outcome_cohort_id)
);
-- CohortMethodModule Tables
{DEFAULT @table_prefix = ''}
{DEFAULT @cm_attrition = cm_attrition}
{DEFAULT @cm_follow_up_dist = cm_follow_up_dist}
{DEFAULT @cm_analysis = cm_analysis}
{DEFAULT @cm_result = cm_result}
{DEFAULT @cm_interaction_result = cm_interaction_result}
{DEFAULT @cm_covariate = cm_covariate}
{DEFAULT @cm_covariate_analysis = cm_covariate_analysis}
{DEFAULT @cm_covariate_balance = cm_covariate_balance}
{DEFAULT @cm_diagnostics_summary = cm_diagnostics_summary}
{DEFAULT @cm_target_comparator_outcome = cm_target_comparator_outcome}
{DEFAULT @cm_kaplan_meier_dist = cm_kaplan_meier_dist}
{DEFAULT @cm_likelihood_profile = cm_likelihood_profile}
{DEFAULT @cm_preference_score_dist = cm_preference_score_dist}
{DEFAULT @cm_propensity_model = cm_propensity_model}
{DEFAULT @cm_shared_covariate_balance = cm_shared_covariate_balance}
  
CREATE TABLE @database_schema.@table_prefix@cm_attrition (
  	 sequence_number INT NOT NULL,
	 description VARCHAR,
	 subjects INT,
	 exposure_id BIGINT NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(sequence_number,exposure_id,target_id,comparator_id,analysis_id,outcome_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_follow_up_dist (
  	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 target_min_days FLOAT,
	 target_p_10_days FLOAT,
	 target_p_25_days FLOAT,
	 target_median_days FLOAT,
	 target_p_75_days FLOAT,
	 target_p_90_days FLOAT,
	 target_max_days FLOAT,
	 comparator_min_days FLOAT,
	 comparator_p_10_days FLOAT,
	 comparator_p_25_days FLOAT,
	 comparator_median_days FLOAT,
	 comparator_p_75_days FLOAT,
	 comparator_p_90_days FLOAT,
	 comparator_max_days FLOAT,
	 target_min_date DATE,
	 target_max_date DATE,
	 comparator_min_date DATE,
	 comparator_max_date DATE,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(target_id,comparator_id,outcome_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_analysis (
  	 analysis_id INT NOT NULL,
	 description VARCHAR,
	 definition VARCHAR,
	PRIMARY KEY(analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_result (
  	 analysis_id INT NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 rr FLOAT,
	 ci_95_lb FLOAT,
	 ci_95_ub FLOAT,
	 p FLOAT,
	 one_sided_p FLOAT,
	 target_subjects INT,
	 comparator_subjects INT,
	 target_days INT,
	 comparator_days INT,
	 target_outcomes INT,
	 comparator_outcomes INT,
	 log_rr FLOAT,
	 se_log_rr FLOAT,
	 llr FLOAT,
	 calibrated_rr FLOAT,
	 calibrated_ci_95_lb FLOAT,
	 calibrated_ci_95_ub FLOAT,
	 calibrated_p FLOAT,
	 calibrated_one_sided_p FLOAT,
	 calibrated_log_rr FLOAT,
	 calibrated_se_log_rr FLOAT,
	 target_estimator VARCHAR,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,target_id,comparator_id,outcome_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_interaction_result (
  	 analysis_id INT NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 interaction_covariate_id INT NOT NULL,
	 rr FLOAT,
	 ci_95_lb FLOAT,
	 ci_95_ub FLOAT,
	 p FLOAT,
	 target_subjects INT,
	 comparator_subjects INT,
	 target_days INT,
	 comparator_days INT,
	 target_outcomes INT,
	 comparator_outcomes INT,
	 log_rr FLOAT,
	 se_log_rr FLOAT,
	 calibrated_rr FLOAT,
	 calibrated_ci_95_lb FLOAT,
	 calibrated_ci_95_ub FLOAT,
	 calibrated_p FLOAT,
	 calibrated_log_rr FLOAT,
	 calibrated_se_log_rr FLOAT,
	 target_estimator VARCHAR,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,target_id,comparator_id,outcome_id,interaction_covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_covariate (
  	 covariate_id BIGINT NOT NULL,
	 covariate_name VARCHAR,
	 analysis_id INT NOT NULL,
	 covariate_analysis_id INT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(covariate_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_covariate_analysis (
  	 covariate_analysis_id INT NOT NULL,
	 covariate_analysis_name VARCHAR,
	 analysis_id INT NOT NULL,
	PRIMARY KEY(covariate_analysis_id,analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_covariate_balance (
  	 database_id VARCHAR NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 target_mean_before FLOAT,
	 comparator_mean_before FLOAT,
	 mean_before FLOAT,
	 std_diff_before FLOAT,
	 mean_after FLOAT,
	 target_mean_after FLOAT,
	 comparator_mean_after FLOAT,
	 std_diff_after FLOAT,
	 target_std_diff FLOAT,
	 comparator_std_diff FLOAT,
	 target_comparator_std_diff FLOAT,
	PRIMARY KEY(database_id,target_id,comparator_id,outcome_id,analysis_id,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_diagnostics_summary (
  	 analysis_id INT NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 max_sdm FLOAT,
	 shared_max_sdm FLOAT,
	 equipoise FLOAT,
	 mdrr FLOAT,
	 attrition_fraction FLOAT,
	 generalizability_max_sdm FLOAT,
	 ease FLOAT,
	 balance_diagnostic VARCHAR(20),
	 shared_balance_diagnostic VARCHAR(20),
	 equipoise_diagnostic VARCHAR(20),
	 mdrr_diagnostic VARCHAR(20),
	 attrition_diagnostic VARCHAR(20),
	 generalizability_diagnostic VARCHAR(20),
	 ease_diagnostic VARCHAR(20),
	 unblind INT,
	 unblind_for_evidence_synthesis INT,
	PRIMARY KEY(analysis_id,target_id,comparator_id,outcome_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_target_comparator_outcome (
  	 outcome_id BIGINT NOT NULL,
	 outcome_of_interest INT,
	 true_effect_size FLOAT,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	PRIMARY KEY(outcome_id,target_id,comparator_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_kaplan_meier_dist (
  	 time_day INT NOT NULL,
	 target_survival FLOAT,
	 target_survival_lb FLOAT,
	 target_survival_ub FLOAT,
	 comparator_survival FLOAT,
	 comparator_survival_lb FLOAT,
	 comparator_survival_ub FLOAT,
	 target_at_risk INT,
	 comparator_at_risk INT,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(time_day,target_id,comparator_id,outcome_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_likelihood_profile (
  	 log_rr FLOAT NOT NULL,
	 log_likelihood FLOAT,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(log_rr,target_id,comparator_id,outcome_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_preference_score_dist (
  	 analysis_id INT NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 preference_score FLOAT NOT NULL,
	 target_density FLOAT,
	 comparator_density FLOAT,
	PRIMARY KEY(analysis_id,target_id,comparator_id,database_id,preference_score)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_propensity_model (
  	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 coefficient FLOAT,
	PRIMARY KEY(target_id,comparator_id,analysis_id,database_id,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_shared_covariate_balance (
  	 database_id VARCHAR NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 mean_before FLOAT,
	 target_mean_before FLOAT,
	 comparator_mean_before FLOAT,
	 std_diff_before FLOAT,
	 mean_after FLOAT,
	 target_mean_after FLOAT,
	 comparator_mean_after FLOAT,
	 std_diff_after FLOAT,
	 target_std_diff FLOAT,
	 comparator_std_diff FLOAT,
	 target_comparator_std_diff FLOAT,
	PRIMARY KEY(database_id,target_id,comparator_id,analysis_id,covariate_id)
);
-- EvidenceSynthesisModule Tables
{DEFAULT @table_prefix = ''}
{DEFAULT @es_analysis = es_analysis}
{DEFAULT @es_cm_diagnostics_summary = es_cm_diagnostics_summary}
{DEFAULT @es_cm_result = es_cm_result}
{DEFAULT @es_sccs_diagnostics_summary = es_sccs_diagnostics_summary}
{DEFAULT @es_sccs_result = es_sccs_result}
  
CREATE TABLE @database_schema.@table_prefix@es_analysis (
  	 evidence_synthesis_analysis_id INT NOT NULL,
	 evidence_synthesis_description VARCHAR(255),
	 source_method VARCHAR(100),
	 definition VARCHAR,
	PRIMARY KEY(evidence_synthesis_analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@es_cm_diagnostics_summary (
  	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 evidence_synthesis_analysis_id INT NOT NULL,
	 mdrr FLOAT,
	 i_2 FLOAT,
	 tau FLOAT,
	 ease FLOAT,
	 mdrr_diagnostic VARCHAR(13),
	 i_2_diagnostic VARCHAR(13),
	 tau_diagnostic VARCHAR(13),
	 ease_diagnostic VARCHAR(13),
	 unblind INT,
	PRIMARY KEY(target_id,comparator_id,outcome_id,analysis_id,evidence_synthesis_analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@es_cm_result (
  	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 evidence_synthesis_analysis_id INT NOT NULL,
	 rr FLOAT,
	 ci_95_lb FLOAT,
	 ci_95_ub FLOAT,
	 p FLOAT,
	 one_sided_p FLOAT,
	 log_rr FLOAT,
	 se_log_rr FLOAT,
	 target_subjects INT,
	 comparator_subjects INT,
	 target_days INT,
	 comparator_days INT,
	 target_outcomes INT,
	 comparator_outcomes INT,
	 n_databases INT,
	 calibrated_rr FLOAT,
	 calibrated_ci_95_lb FLOAT,
	 calibrated_ci_95_ub FLOAT,
	 calibrated_p FLOAT,
	 calibrated_one_sided_p FLOAT,
	 calibrated_log_rr FLOAT,
	 calibrated_se_log_rr FLOAT,
	PRIMARY KEY(target_id,comparator_id,outcome_id,analysis_id,evidence_synthesis_analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@es_sccs_diagnostics_summary (
  	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 evidence_synthesis_analysis_id INT NOT NULL,
	 mdrr FLOAT,
	 i_2 FLOAT,
	 tau FLOAT,
	 ease FLOAT,
	 mdrr_diagnostic VARCHAR(13),
	 i_2_diagnostic VARCHAR(13),
	 tau_diagnostic VARCHAR(13),
	 ease_diagnostic VARCHAR(13),
	 unblind INT,
	PRIMARY KEY(exposures_outcome_set_id,covariate_id,analysis_id,evidence_synthesis_analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@es_sccs_result (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 evidence_synthesis_analysis_id INT NOT NULL,
	 rr FLOAT,
	 ci_95_lb FLOAT,
	 ci_95_ub FLOAT,
	 p FLOAT,
	 one_sided_p FLOAT,
	 outcome_subjects INT,
	 outcome_events INT,
	 outcome_observation_periods INT,
	 covariate_subjects INT,
	 covariate_days INT,
	 covariate_eras INT,
	 covariate_outcomes INT,
	 observed_days INT,
	 n_databases INT,
	 log_rr FLOAT,
	 se_log_rr FLOAT,
	 calibrated_rr FLOAT,
	 calibrated_ci_95_lb FLOAT,
	 calibrated_ci_95_ub FLOAT,
	 calibrated_p FLOAT,
	 calibrated_one_sided_p FLOAT,
	 calibrated_log_rr FLOAT,
	 calibrated_se_log_rr FLOAT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,evidence_synthesis_analysis_id)
);
-- PatientLevelPredictionModule Tables
{DEFAULT @table_prefix = ''}
{DEFAULT @plp_cohorts = plp_cohorts}
{DEFAULT @plp_cohort_definition = plp_cohort_definition}
{DEFAULT @plp_database_meta_data = plp_database_meta_data}
{DEFAULT @plp_database_details = plp_database_details}
{DEFAULT @plp_tars = plp_tars}
{DEFAULT @plp_population_settings = plp_population_settings}
{DEFAULT @plp_covariate_settings = plp_covariate_settings}
{DEFAULT @plp_model_settings = plp_model_settings}
{DEFAULT @plp_split_settings = plp_split_settings}
{DEFAULT @plp_plp_data_settings = plp_plp_data_settings}
{DEFAULT @plp_feature_engineering_settings = plp_feature_engineering_settings}
{DEFAULT @plp_tidy_covariates_settings = plp_tidy_covariates_settings}
{DEFAULT @plp_sample_settings = plp_sample_settings}
{DEFAULT @plp_model_designs = plp_model_designs}
{DEFAULT @plp_diagnostics = plp_diagnostics}
{DEFAULT @plp_diagnostic_summary = plp_diagnostic_summary}
{DEFAULT @plp_diagnostic_predictors = plp_diagnostic_predictors}
{DEFAULT @plp_diagnostic_participants = plp_diagnostic_participants}
{DEFAULT @plp_diagnostic_outcomes = plp_diagnostic_outcomes}
{DEFAULT @plp_diagnostic_designs = plp_diagnostic_designs}
{DEFAULT @plp_models = plp_models}
{DEFAULT @plp_recalibrations = plp_recalibrations}
{DEFAULT @plp_performances = plp_performances}
{DEFAULT @plp_attrition = plp_attrition}
{DEFAULT @plp_prediction_distribution = plp_prediction_distribution}
{DEFAULT @plp_covariate_summary = plp_covariate_summary}
{DEFAULT @plp_threshold_summary = plp_threshold_summary}
{DEFAULT @plp_calibration_summary = plp_calibration_summary}
{DEFAULT @plp_evaluation_statistics = plp_evaluation_statistics}
{DEFAULT @plp_demographic_summary = plp_demographic_summary}
  
CREATE TABLE @database_schema.@table_prefix@plp_cohorts (
  	 cohort_id INT NOT NULL,
	 cohort_definition_id BIGINT,
	 cohort_name VARCHAR,
	PRIMARY KEY(cohort_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_cohort_definition (
  	 cohort_definition_id BIGINT,
	 cohort_name VARCHAR,
	 description TEXT,
	 json TEXT,
	 sql_command TEXT
);
 
CREATE TABLE @database_schema.@table_prefix@plp_database_meta_data (
  	 database_id VARCHAR NOT NULL,
	 cdm_source_name VARCHAR,
	 cdm_source_abbreviation VARCHAR,
	 cdm_holder VARCHAR,
	 source_description TEXT,
	 source_documentation_reference VARCHAR,
	 cdm_etl_reference VARCHAR,
	 source_release_date VARCHAR,
	 cdm_release_date VARCHAR,
	 cdm_version VARCHAR,
	 vocabulary_version VARCHAR,
	 max_obs_period_end_date VARCHAR,
	PRIMARY KEY(database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_database_details (
  	 database_id INT NOT NULL,
	 database_meta_data_id VARCHAR,
	PRIMARY KEY(database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_tars (
  	 tar_id INT NOT NULL,
	 tar_start_day INT,
	 tar_start_anchor VARCHAR,
	 tar_end_day INT,
	 tar_end_anchor VARCHAR,
	PRIMARY KEY(tar_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_population_settings (
  	 population_setting_id INT NOT NULL,
	 population_settings_json TEXT,
	PRIMARY KEY(population_setting_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_covariate_settings (
  	 covariate_setting_id INT NOT NULL,
	 covariate_settings_json TEXT,
	PRIMARY KEY(covariate_setting_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_model_settings (
  	 model_setting_id INT NOT NULL,
	 model_type VARCHAR,
	 model_settings_json VARCHAR,
	PRIMARY KEY(model_setting_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_split_settings (
  	 split_setting_id INT NOT NULL,
	 split_settings_json TEXT,
	PRIMARY KEY(split_setting_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_plp_data_settings (
  	 plp_data_setting_id INT NOT NULL,
	 plp_data_settings_json TEXT,
	PRIMARY KEY(plp_data_setting_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_feature_engineering_settings (
  	 feature_engineering_setting_id INT NOT NULL,
	 feature_engineering_settings_json TEXT,
	PRIMARY KEY(feature_engineering_setting_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_tidy_covariates_settings (
  	 tidy_covariates_setting_id INT NOT NULL,
	 tidy_covariates_settings_json TEXT,
	PRIMARY KEY(tidy_covariates_setting_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_sample_settings (
  	 sample_setting_id INT NOT NULL,
	 sample_settings_json TEXT,
	PRIMARY KEY(sample_setting_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_model_designs (
  	 model_design_id INT NOT NULL,
	 target_id INT,
	 outcome_id INT,
	 tar_id INT,
	 plp_data_setting_id INT,
	 population_setting_id INT,
	 model_setting_id INT,
	 covariate_setting_id INT,
	 sample_setting_id INT,
	 split_setting_id INT,
	 feature_engineering_setting_id INT,
	 tidy_covariates_setting_id INT,
	PRIMARY KEY(model_design_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_diagnostics (
  	 diagnostic_id INT NOT NULL,
	 model_design_id INT,
	 database_id INT,
	 execution_date_time VARCHAR,
	PRIMARY KEY(diagnostic_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_diagnostic_summary (
  	 diagnostic_id INT,
	 probast_id VARCHAR,
	 result_value VARCHAR
);
 
CREATE TABLE @database_schema.@table_prefix@plp_diagnostic_predictors (
  	 diagnostic_id INT,
	 days_to_event INT,
	 outcome_at_time INT,
	 observed_at_start_of_day BIGINT,
	 input_type VARCHAR
);
 
CREATE TABLE @database_schema.@table_prefix@plp_diagnostic_participants (
  	 diagnostic_id INT,
	 design VARCHAR,
	 metric VARCHAR,
	 value FLOAT,
	 probast_id VARCHAR
);
 
CREATE TABLE @database_schema.@table_prefix@plp_diagnostic_outcomes (
  	 diagnostic_id INT,
	 xvalue INT,
	 outcome_percent FLOAT,
	 aggregation VARCHAR,
	 probast_id VARCHAR,
	 input_type VARCHAR
);
 
CREATE TABLE @database_schema.@table_prefix@plp_diagnostic_designs (
  	 diagnostic_id INT NOT NULL,
	 probast_id VARCHAR,
	 value VARCHAR,
	PRIMARY KEY(diagnostic_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_models (
  	 model_id INT NOT NULL,
	 analysis_id VARCHAR,
	 model_design_id INT,
	 database_id INT,
	 model_type VARCHAR,
	 plp_model_file TEXT,
	 train_details TEXT,
	 preprocessing TEXT,
	 execution_date_time VARCHAR,
	 training_time VARCHAR,
	 intercept FLOAT,
	PRIMARY KEY(model_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_recalibrations (
  	 recalibration_id INT NOT NULL,
	 original_model_id INT,
	 recalibrated_model_id INT,
	 recalibration_type VARCHAR,
	 recalibration_json VARCHAR,
	PRIMARY KEY(recalibration_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_performances (
  	 performance_id INT NOT NULL,
	 model_design_id INT,
	 development_database_id INT,
	 validation_database_id INT,
	 target_id INT,
	 outcome_id INT,
	 tar_id INT,
	 plp_data_setting_id INT,
	 population_setting_id INT,
	 model_development INT,
	 execution_date_time VARCHAR,
	 plp_version VARCHAR,
	PRIMARY KEY(performance_id)
);
 
CREATE TABLE @database_schema.@table_prefix@plp_attrition (
  	 performance_id INT,
	 outcome_id INT,
	 description VARCHAR,
	 target_count INT,
	 unique_people INT,
	 outcomes INT
);
 
CREATE TABLE @database_schema.@table_prefix@plp_prediction_distribution (
  	 performance_id INT,
	 evaluation VARCHAR,
	 class_label INT,
	 person_count INT,
	 average_predicted_probability FLOAT,
	 st_dev_predicted_probability FLOAT,
	 min_predicted_probability FLOAT,
	 p_05_predicted_probability FLOAT,
	 p_25_predicted_probability FLOAT,
	 median_predicted_probability FLOAT,
	 p_75_predicted_probability FLOAT,
	 p_95_predicted_probability FLOAT,
	 max_predicted_probability FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@plp_covariate_summary (
  	 performance_id INT,
	 covariate_id BIGINT,
	 covariate_name VARCHAR,
	 concept_id FLOAT,
	 covariate_value FLOAT,
	 covariate_count INT,
	 covariate_mean FLOAT,
	 covariate_st_dev FLOAT,
	 with_no_outcome_covariate_count INT,
	 with_no_outcome_covariate_mean FLOAT,
	 with_no_outcome_covariate_st_dev FLOAT,
	 with_outcome_covariate_count INT,
	 with_outcome_covariate_mean FLOAT,
	 with_outcome_covariate_st_dev FLOAT,
	 standardized_mean_diff FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@plp_threshold_summary (
  	 performance_id INT,
	 evaluation VARCHAR,
	 prediction_threshold FLOAT,
	 preference_threshold FLOAT,
	 positive_count INT,
	 negative_count INT,
	 true_count INT,
	 false_count INT,
	 true_positive_count INT,
	 true_negative_count INT,
	 false_positive_count INT,
	 false_negative_count INT,
	 f_1_score FLOAT,
	 accuracy FLOAT,
	 sensitivity FLOAT,
	 false_negative_rate FLOAT,
	 false_positive_rate FLOAT,
	 specificity FLOAT,
	 positive_predictive_value FLOAT,
	 false_discovery_rate FLOAT,
	 negative_predictive_value FLOAT,
	 false_omission_rate FLOAT,
	 positive_likelihood_ratio FLOAT,
	 negative_likelihood_ratio FLOAT,
	 diagnostic_odds_ratio FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@plp_calibration_summary (
  	 performance_id INT,
	 evaluation VARCHAR,
	 prediction_threshold FLOAT,
	 person_count_at_risk INT,
	 person_count_with_outcome INT,
	 average_predicted_probability FLOAT,
	 st_dev_predicted_probability FLOAT,
	 min_predicted_probability FLOAT,
	 p_25_predicted_probability FLOAT,
	 median_predicted_probability FLOAT,
	 p_75_predicted_probability FLOAT,
	 max_predicted_probability FLOAT,
	 observed_incidence FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@plp_evaluation_statistics (
  	 performance_id INT,
	 evaluation VARCHAR,
	 metric VARCHAR,
	 value FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@plp_demographic_summary (
  	 performance_id INT,
	 evaluation VARCHAR,
	 age_group VARCHAR,
	 gen_group VARCHAR,
	 person_count_at_risk INT,
	 person_count_with_outcome INT,
	 average_predicted_probability FLOAT,
	 st_dev_predicted_probability FLOAT,
	 min_predicted_probability FLOAT,
	 p_25_predicted_probability FLOAT,
	 p_50_predicted_probability FLOAT,
	 p_75_predicted_probability FLOAT,
	 max_predicted_probability FLOAT
);
-- SelfControlledCaseSeriesModule Tables
{DEFAULT @table_prefix = ''}
{DEFAULT @sccs_sccs_analysis = sccs_sccs_analysis}
{DEFAULT @sccs_sccs_covariate_analysis = sccs_sccs_covariate_analysis}
{DEFAULT @sccs_sccs_covariate = sccs_sccs_covariate}
{DEFAULT @sccs_sccs_era = sccs_sccs_era}
{DEFAULT @sccs_sccs_exposures_outcome_set = sccs_sccs_exposures_outcome_set}
{DEFAULT @sccs_sccs_exposure = sccs_sccs_exposure}
{DEFAULT @sccs_sccs_spline = sccs_sccs_spline}
{DEFAULT @sccs_sccs_censor_model = sccs_sccs_censor_model}
{DEFAULT @sccs_sccs_result = sccs_sccs_result}
{DEFAULT @sccs_sccs_covariate_result = sccs_sccs_covariate_result}
{DEFAULT @sccs_sccs_attrition = sccs_sccs_attrition}
{DEFAULT @sccs_sccs_likelihood_profile = sccs_sccs_likelihood_profile}
{DEFAULT @sccs_sccs_time_trend = sccs_sccs_time_trend}
{DEFAULT @sccs_sccs_time_to_event = sccs_sccs_time_to_event}
{DEFAULT @sccs_sccs_age_spanning = sccs_sccs_age_spanning}
{DEFAULT @sccs_sccs_calendar_time_spanning = sccs_sccs_calendar_time_spanning}
{DEFAULT @sccs_sccs_event_dep_observation = sccs_sccs_event_dep_observation}
{DEFAULT @sccs_sccs_diagnostics_summary = sccs_sccs_diagnostics_summary}
  
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_analysis (
  	 analysis_id INT NOT NULL,
	 description VARCHAR,
	 definition VARCHAR,
	PRIMARY KEY(analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_covariate_analysis (
  	 analysis_id INT NOT NULL,
	 covariate_analysis_id INT NOT NULL,
	 covariate_analysis_name VARCHAR,
	 variable_of_interest INT,
	PRIMARY KEY(analysis_id,covariate_analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_covariate (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 covariate_name VARCHAR,
	 era_id INT,
	 covariate_analysis_id INT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_era (
  	 exposures_outcome_set_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 era_type VARCHAR NOT NULL,
	 era_id INT NOT NULL,
	 era_name VARCHAR,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(exposures_outcome_set_id,analysis_id,era_type,era_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_exposures_outcome_set (
  	 exposures_outcome_set_id INT NOT NULL,
	 outcome_id INT,
	 nesting_cohort_id INT,
	PRIMARY KEY(exposures_outcome_set_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_exposure (
  	 exposures_outcome_set_id INT NOT NULL,
	 era_id INT NOT NULL,
	 true_effect_size FLOAT,
	PRIMARY KEY(exposures_outcome_set_id,era_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_spline (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 spline_type VARCHAR NOT NULL,
	 knot_month FLOAT NOT NULL,
	 rr FLOAT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,spline_type,knot_month)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_censor_model (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 parameter_id INT NOT NULL,
	 parameter_value FLOAT,
	 model_type VARCHAR,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,parameter_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_result (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 rr FLOAT,
	 ci_95_lb FLOAT,
	 ci_95_ub FLOAT,
	 p FLOAT,
	 one_sided_p FLOAT,
	 outcome_subjects INT,
	 outcome_events INT,
	 outcome_observation_periods INT,
	 covariate_subjects INT,
	 covariate_days INT,
	 covariate_eras INT,
	 covariate_outcomes INT,
	 observed_days BIGINT,
	 log_rr FLOAT,
	 se_log_rr FLOAT,
	 llr FLOAT,
	 calibrated_rr FLOAT,
	 calibrated_ci_95_lb FLOAT,
	 calibrated_ci_95_ub FLOAT,
	 calibrated_p FLOAT,
	 calibrated_one_sided_p FLOAT,
	 calibrated_log_rr FLOAT,
	 calibrated_se_log_rr FLOAT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_covariate_result (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 covariate_id INT NOT NULL,
	 rr FLOAT,
	 ci_95_lb FLOAT,
	 ci_95_ub FLOAT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_attrition (
  	 sequence_number INT NOT NULL,
	 description VARCHAR,
	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 outcome_subjects INT,
	 outcome_events INT,
	 outcome_observation_periods INT,
	 observed_days BIGINT,
	PRIMARY KEY(sequence_number,analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_likelihood_profile (
  	 log_rr FLOAT NOT NULL,
	 log_likelihood FLOAT,
	 covariate_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(log_rr,covariate_id,exposures_outcome_set_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_time_trend (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 calendar_year INT NOT NULL,
	 calendar_month INT NOT NULL,
	 observed_subjects INT,
	 ratio FLOAT,
	 adjusted_ratio FLOAT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,calendar_year,calendar_month)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_time_to_event (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 era_id INT NOT NULL,
	 week INT NOT NULL,
	 observed_subjects INT,
	 outcomes INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,era_id,week)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_age_spanning (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 age_month INT NOT NULL,
	 cover_before_after_subjects INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,age_month)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_calendar_time_spanning (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 calendar_year INT NOT NULL,
	 calendar_month INT NOT NULL,
	 cover_before_after_subjects INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,calendar_year,calendar_month)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_event_dep_observation (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 months_to_end INT NOT NULL,
	 censored INT NOT NULL,
	 outcomes INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,months_to_end,censored)
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_sccs_diagnostics_summary (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 mdrr FLOAT,
	 ease FLOAT,
	 time_trend_p FLOAT,
	 pre_exposure_p FLOAT,
	 mdrr_diagnostic VARCHAR(20),
	 ease_diagnostic VARCHAR(20),
	 time_trend_diagnostic VARCHAR(20),
	 pre_exposure_diagnostic VARCHAR(20),
	 unblind INT,
	 unblind_for_evidence_synthesis INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);

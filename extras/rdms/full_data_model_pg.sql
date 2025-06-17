-- Strategus Tables
CREATE TABLE results.database_meta_data (
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
CREATE TABLE results.c_time_to_event (
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
CREATE TABLE results.c_rechallenge_fail_case_series (
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
CREATE TABLE results.c_dechallenge_rechallenge (
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
	 pct_dechallenge_attempt NUMERIC,
	 pct_dechallenge_success NUMERIC,
	 pct_dechallenge_fail NUMERIC,
	 pct_rechallenge_attempt NUMERIC,
	 pct_rechallenge_success NUMERIC,
	 pct_rechallenge_fail NUMERIC,
	PRIMARY KEY(database_id,dechallenge_stop_interval,dechallenge_evaluation_window,target_cohort_definition_id,outcome_cohort_definition_id)
);
CREATE TABLE results.c_analysis_ref (
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
CREATE TABLE results.c_covariate_ref (
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
CREATE TABLE results.c_covariates (
  	 database_id VARCHAR(100) NOT NULL,
	 setting_id VARCHAR(30) NOT NULL,
	 cohort_type VARCHAR(12) NOT NULL,
	 target_cohort_id INT NOT NULL,
	 outcome_cohort_id INT NOT NULL,
	 min_characterization_mean NUMERIC NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 sum_value INT,
	 average_value NUMERIC,
	PRIMARY KEY(database_id,setting_id,cohort_type,target_cohort_id,outcome_cohort_id,min_characterization_mean,covariate_id)
);
CREATE TABLE results.c_covariates_continuous (
  	 database_id VARCHAR(100) NOT NULL,
	 setting_id VARCHAR(30) NOT NULL,
	 cohort_type VARCHAR(12) NOT NULL,
	 target_cohort_id INT NOT NULL,
	 outcome_cohort_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 count_value INT,
	 min_value NUMERIC,
	 max_value NUMERIC,
	 average_value NUMERIC,
	 standard_deviation NUMERIC,
	 median_value NUMERIC,
	 p_10_value NUMERIC,
	 p_25_value NUMERIC,
	 p_75_value NUMERIC,
	 p_90_value NUMERIC,
	PRIMARY KEY(database_id,setting_id,cohort_type,target_cohort_id,outcome_cohort_id,covariate_id)
);
CREATE TABLE results.c_settings (
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
CREATE TABLE results.c_cohort_details (
  	 database_id VARCHAR(100) NOT NULL,
	 setting_id VARCHAR(30) NOT NULL,
	 cohort_type VARCHAR(12) NOT NULL,
	 target_cohort_id INT NOT NULL,
	 outcome_cohort_id INT NOT NULL,
	PRIMARY KEY(database_id,setting_id,cohort_type,target_cohort_id,outcome_cohort_id)
);
CREATE TABLE results.c_cohort_counts (
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
CREATE TABLE results.cd_cohort (
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
CREATE TABLE results.cd_subset_definition (
  	 subset_definition_id BIGINT NOT NULL,
	 json VARCHAR NOT NULL,
	PRIMARY KEY(subset_definition_id,json)
);
CREATE TABLE results.cd_cohort_count (
  	 cohort_id BIGINT NOT NULL,
	 cohort_entries NUMERIC,
	 cohort_subjects NUMERIC,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,database_id)
);
CREATE TABLE results.cd_cohort_inclusion (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 rule_sequence BIGINT NOT NULL,
	 name VARCHAR,
	 description VARCHAR,
	PRIMARY KEY(database_id,cohort_id,rule_sequence)
);
CREATE TABLE results.cd_cohort_inc_result (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 mode_id BIGINT NOT NULL,
	 inclusion_rule_mask BIGINT NOT NULL,
	 person_count NUMERIC,
	PRIMARY KEY(database_id,cohort_id,mode_id,inclusion_rule_mask)
);
CREATE TABLE results.cd_cohort_inc_stats (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 rule_sequence BIGINT NOT NULL,
	 mode_id BIGINT NOT NULL,
	 person_count NUMERIC,
	 gain_count NUMERIC,
	 person_total NUMERIC,
	PRIMARY KEY(database_id,cohort_id,rule_sequence,mode_id)
);
CREATE TABLE results.cd_cohort_summary_stats (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 mode_id BIGINT NOT NULL,
	 base_count NUMERIC,
	 final_count NUMERIC,
	PRIMARY KEY(database_id,cohort_id,mode_id)
);
CREATE TABLE results.cd_concept (
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
CREATE TABLE results.cd_concept_ancestor (
  	 ancestor_concept_id BIGINT NOT NULL,
	 descendant_concept_id BIGINT NOT NULL,
	 min_levels_of_separation INT,
	 max_levels_of_separation INT,
	PRIMARY KEY(ancestor_concept_id,descendant_concept_id)
);
CREATE TABLE results.cd_concept_relationship (
  	 concept_id_1 BIGINT NOT NULL,
	 concept_id_2 BIGINT NOT NULL,
	 relationship_id VARCHAR(20) NOT NULL,
	 valid_start_date DATE,
	 valid_end_date DATE,
	 invalid_reason VARCHAR(1),
	PRIMARY KEY(concept_id_1,concept_id_2,relationship_id)
);
CREATE TABLE results.cd_concept_sets (
  	 cohort_id BIGINT NOT NULL,
	 concept_set_id INT NOT NULL,
	 concept_set_sql VARCHAR,
	 concept_set_name VARCHAR(255),
	 concept_set_expression VARCHAR,
	PRIMARY KEY(cohort_id,concept_set_id)
);
CREATE TABLE results.cd_concept_synonym (
  	 concept_id BIGINT NOT NULL,
	 concept_synonym_name VARCHAR NOT NULL,
	 language_concept_id BIGINT NOT NULL,
	PRIMARY KEY(concept_id,concept_synonym_name,language_concept_id)
);
CREATE TABLE results.cd_database (
  	 database_id VARCHAR NOT NULL,
	 database_name VARCHAR,
	 description VARCHAR,
	 is_meta_analysis VARCHAR(1),
	 vocabulary_version VARCHAR,
	 vocabulary_version_cdm VARCHAR,
	PRIMARY KEY(database_id)
);
CREATE TABLE results.cd_domain (
  	 domain_id VARCHAR(20) NOT NULL,
	 domain_name VARCHAR(255),
	 domain_concept_id BIGINT,
	PRIMARY KEY(domain_id)
);
CREATE TABLE results.cd_incidence_rate (
  	 cohort_count NUMERIC,
	 person_years NUMERIC,
	 gender VARCHAR NOT NULL,
	 age_group VARCHAR NOT NULL,
	 calendar_year VARCHAR(4) NOT NULL,
	 incidence_rate NUMERIC,
	 cohort_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(gender,age_group,calendar_year,cohort_id,database_id)
);
CREATE TABLE results.cd_included_source_concept (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 concept_set_id INT NOT NULL,
	 concept_id BIGINT NOT NULL,
	 source_concept_id BIGINT NOT NULL,
	 concept_subjects NUMERIC,
	 concept_count NUMERIC,
	PRIMARY KEY(database_id,cohort_id,concept_set_id,concept_id,source_concept_id)
);
CREATE TABLE results.cd_index_event_breakdown (
  	 concept_id BIGINT NOT NULL,
	 concept_count NUMERIC,
	 subject_count NUMERIC,
	 cohort_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 domain_field VARCHAR NOT NULL,
	 domain_table VARCHAR NOT NULL,
	PRIMARY KEY(concept_id,cohort_id,database_id,domain_field,domain_table)
);
CREATE TABLE results.cd_metadata (
  	 database_id VARCHAR NOT NULL,
	 start_time VARCHAR NOT NULL,
	 variable_field VARCHAR NOT NULL,
	 value_field VARCHAR,
	PRIMARY KEY(database_id,start_time,variable_field)
);
CREATE TABLE results.cd_orphan_concept (
  	 cohort_id BIGINT NOT NULL,
	 concept_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 concept_id BIGINT NOT NULL,
	 concept_count NUMERIC,
	 concept_subjects NUMERIC,
	PRIMARY KEY(cohort_id,concept_set_id,database_id,concept_id)
);
CREATE TABLE results.cd_relationship (
  	 relationship_id VARCHAR(20) NOT NULL,
	 relationship_name VARCHAR(255),
	 is_hierarchical VARCHAR(1),
	 defines_ancestry VARCHAR(1),
	 reverse_relationship_id VARCHAR(20) NOT NULL,
	 relationship_concept_id BIGINT NOT NULL,
	PRIMARY KEY(relationship_id,reverse_relationship_id,relationship_concept_id)
);
CREATE TABLE results.cd_resolved_concepts (
  	 cohort_id BIGINT NOT NULL,
	 concept_set_id INT NOT NULL,
	 concept_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,concept_set_id,concept_id,database_id)
);
CREATE TABLE results.cd_temporal_analysis_ref (
  	 analysis_id INT NOT NULL,
	 analysis_name VARCHAR,
	 domain_id VARCHAR(20) NOT NULL,
	 is_binary VARCHAR(1),
	 missing_means_zero VARCHAR(1),
	PRIMARY KEY(analysis_id,domain_id)
);
CREATE TABLE results.cd_temporal_covariate_ref (
  	 covariate_id BIGINT NOT NULL,
	 covariate_name VARCHAR,
	 analysis_id INT,
	 concept_id BIGINT,
	 value_as_concept_id BIGINT,
	PRIMARY KEY(covariate_id)
);
CREATE TABLE results.cd_temporal_covariate_value (
  	 cohort_id BIGINT NOT NULL,
	 time_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 sum_value NUMERIC,
	 mean NUMERIC,
	 sd NUMERIC,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,time_id,covariate_id,database_id)
);
CREATE TABLE results.cd_temporal_covariate_value_dist (
  	 cohort_id BIGINT NOT NULL,
	 time_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 count_value NUMERIC,
	 min_value NUMERIC,
	 max_value NUMERIC,
	 mean NUMERIC,
	 sd NUMERIC,
	 median_value NUMERIC,
	 p_10_value NUMERIC,
	 p_25_value NUMERIC,
	 p_75_value NUMERIC,
	 p_90_value NUMERIC,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,time_id,covariate_id,database_id)
);
CREATE TABLE results.cd_temporal_time_ref (
  	 time_id INT NOT NULL,
	 start_day NUMERIC,
	 end_day NUMERIC,
	PRIMARY KEY(time_id)
);
CREATE TABLE results.cd_time_series (
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
CREATE TABLE results.cd_visit_context (
  	 cohort_id BIGINT NOT NULL,
	 visit_concept_id BIGINT NOT NULL,
	 visit_context VARCHAR NOT NULL,
	 subjects NUMERIC,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,visit_concept_id,visit_context,database_id)
);
CREATE TABLE results.cd_vocabulary (
  	 vocabulary_id VARCHAR(50),
	 vocabulary_name VARCHAR(255),
	 vocabulary_reference VARCHAR,
	 vocabulary_version VARCHAR,
	 vocabulary_concept_id BIGINT
);
-- CohortGeneratorModule Tables
CREATE TABLE results.cg_cohort_definition (
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
CREATE TABLE results.cg_cohort_generation (
  	 cohort_id BIGINT NOT NULL,
	 cohort_name VARCHAR,
	 generation_status VARCHAR,
	 start_time TIMESTAMP,
	 end_time TIMESTAMP,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_id,database_id)
);
CREATE TABLE results.cg_cohort_inclusion (
  	 cohort_definition_id BIGINT NOT NULL,
	 rule_sequence INT NOT NULL,
	 name VARCHAR NOT NULL,
	 description VARCHAR,
	PRIMARY KEY(cohort_definition_id,rule_sequence,name)
);
CREATE TABLE results.cg_cohort_inc_result (
  	 database_id VARCHAR NOT NULL,
	 cohort_definition_id BIGINT NOT NULL,
	 inclusion_rule_mask INT NOT NULL,
	 person_count BIGINT NOT NULL,
	 mode_id INT NOT NULL,
	PRIMARY KEY(database_id,cohort_definition_id,inclusion_rule_mask,person_count,mode_id)
);
CREATE TABLE results.cg_cohort_inc_stats (
  	 database_id VARCHAR NOT NULL,
	 cohort_definition_id BIGINT NOT NULL,
	 rule_sequence INT NOT NULL,
	 person_count BIGINT NOT NULL,
	 gain_count BIGINT NOT NULL,
	 person_total BIGINT NOT NULL,
	 mode_id INT NOT NULL,
	PRIMARY KEY(database_id,cohort_definition_id,rule_sequence,person_count,gain_count,person_total,mode_id)
);
CREATE TABLE results.cg_cohort_summary_stats (
  	 database_id VARCHAR NOT NULL,
	 cohort_definition_id BIGINT NOT NULL,
	 base_count BIGINT NOT NULL,
	 final_count BIGINT NOT NULL,
	 mode_id INT NOT NULL,
	PRIMARY KEY(database_id,cohort_definition_id,base_count,final_count,mode_id)
);
CREATE TABLE results.cg_cohort_censor_stats (
  	 cohort_definition_id BIGINT NOT NULL,
	 lost_count BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(cohort_definition_id,lost_count,database_id)
);
CREATE TABLE results.cg_cohort_count (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 cohort_entries BIGINT NOT NULL,
	 cohort_subjects BIGINT NOT NULL,
	PRIMARY KEY(database_id,cohort_id,cohort_entries,cohort_subjects)
);
CREATE TABLE results.cg_cohort_count_neg_ctrl (
  	 database_id VARCHAR NOT NULL,
	 cohort_id BIGINT NOT NULL,
	 cohort_entries BIGINT NOT NULL,
	 cohort_subjects BIGINT NOT NULL,
	PRIMARY KEY(database_id,cohort_id,cohort_entries,cohort_subjects)
);
CREATE TABLE results.cg_cohort_subset_definition (
  	 subset_definition_id BIGINT NOT NULL,
	 json TEXT,
	PRIMARY KEY(subset_definition_id)
);
CREATE TABLE results.cg_cohort_definition_neg_ctrl (
  	 cohort_id BIGINT NOT NULL,
	 outcome_concept_id BIGINT,
	 cohort_name VARCHAR,
	 occurrence_type VARCHAR,
	 detect_on_descendants INT,
	PRIMARY KEY(cohort_id)
);
-- CohortIncidenceModule Tables
CREATE TABLE results.ci_incidence_summary (
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
	 incidence_proportion_p100p NUMERIC,
	 incidence_rate_p100py NUMERIC
);
CREATE TABLE results.ci_target_def (
  	 ref_id INT NOT NULL,
	 target_cohort_definition_id BIGINT NOT NULL,
	 target_name VARCHAR(255),
	PRIMARY KEY(ref_id,target_cohort_definition_id)
);
CREATE TABLE results.ci_outcome_def (
  	 ref_id INT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 outcome_cohort_definition_id BIGINT,
	 outcome_name VARCHAR(255),
	 clean_window BIGINT,
	 excluded_cohort_definition_id BIGINT,
	PRIMARY KEY(ref_id,outcome_id)
);
CREATE TABLE results.ci_tar_def (
  	 ref_id INT NOT NULL,
	 tar_id BIGINT NOT NULL,
	 tar_start_with VARCHAR(10),
	 tar_start_offset BIGINT,
	 tar_end_with VARCHAR(10),
	 tar_end_offset BIGINT,
	PRIMARY KEY(ref_id,tar_id)
);
CREATE TABLE results.ci_age_group_def (
  	 ref_id INT NOT NULL,
	 age_group_id INT NOT NULL,
	 age_group_name VARCHAR(255),
	 min_age INT,
	 max_age INT,
	PRIMARY KEY(ref_id,age_group_id)
);
CREATE TABLE results.ci_subgroup_def (
  	 ref_id INT NOT NULL,
	 subgroup_id BIGINT NOT NULL,
	 subgroup_name VARCHAR(255),
	PRIMARY KEY(ref_id,subgroup_id)
);
CREATE TABLE results.ci_target_outcome_ref (
  	 ref_id INT NOT NULL,
	 target_cohort_id BIGINT NOT NULL,
	 outcome_cohort_id BIGINT NOT NULL,
	PRIMARY KEY(ref_id,target_cohort_id,outcome_cohort_id)
);
-- CohortMethodModule Tables
CREATE TABLE results.cm_attrition (
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
CREATE TABLE results.cm_follow_up_dist (
  	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 target_min_days NUMERIC,
	 target_p_10_days NUMERIC,
	 target_p_25_days NUMERIC,
	 target_median_days NUMERIC,
	 target_p_75_days NUMERIC,
	 target_p_90_days NUMERIC,
	 target_max_days NUMERIC,
	 comparator_min_days NUMERIC,
	 comparator_p_10_days NUMERIC,
	 comparator_p_25_days NUMERIC,
	 comparator_median_days NUMERIC,
	 comparator_p_75_days NUMERIC,
	 comparator_p_90_days NUMERIC,
	 comparator_max_days NUMERIC,
	 target_min_date DATE,
	 target_max_date DATE,
	 comparator_min_date DATE,
	 comparator_max_date DATE,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(target_id,comparator_id,outcome_id,analysis_id,database_id)
);
CREATE TABLE results.cm_analysis (
  	 analysis_id INT NOT NULL,
	 description VARCHAR,
	 definition VARCHAR,
	PRIMARY KEY(analysis_id)
);
CREATE TABLE results.cm_result (
  	 analysis_id INT NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 rr NUMERIC,
	 ci_95_lb NUMERIC,
	 ci_95_ub NUMERIC,
	 p NUMERIC,
	 one_sided_p NUMERIC,
	 target_subjects INT,
	 comparator_subjects INT,
	 target_days INT,
	 comparator_days INT,
	 target_outcomes INT,
	 comparator_outcomes INT,
	 log_rr NUMERIC,
	 se_log_rr NUMERIC,
	 llr NUMERIC,
	 calibrated_rr NUMERIC,
	 calibrated_ci_95_lb NUMERIC,
	 calibrated_ci_95_ub NUMERIC,
	 calibrated_p NUMERIC,
	 calibrated_one_sided_p NUMERIC,
	 calibrated_log_rr NUMERIC,
	 calibrated_se_log_rr NUMERIC,
	 target_estimator VARCHAR,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,target_id,comparator_id,outcome_id,database_id)
);
CREATE TABLE results.cm_interaction_result (
  	 analysis_id INT NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 interaction_covariate_id INT NOT NULL,
	 rr NUMERIC,
	 ci_95_lb NUMERIC,
	 ci_95_ub NUMERIC,
	 p NUMERIC,
	 target_subjects INT,
	 comparator_subjects INT,
	 target_days INT,
	 comparator_days INT,
	 target_outcomes INT,
	 comparator_outcomes INT,
	 log_rr NUMERIC,
	 se_log_rr NUMERIC,
	 calibrated_rr NUMERIC,
	 calibrated_ci_95_lb NUMERIC,
	 calibrated_ci_95_ub NUMERIC,
	 calibrated_p NUMERIC,
	 calibrated_log_rr NUMERIC,
	 calibrated_se_log_rr NUMERIC,
	 target_estimator VARCHAR,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,target_id,comparator_id,outcome_id,interaction_covariate_id,database_id)
);
CREATE TABLE results.cm_covariate (
  	 covariate_id BIGINT NOT NULL,
	 covariate_name VARCHAR,
	 analysis_id INT NOT NULL,
	 covariate_analysis_id INT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(covariate_id,analysis_id,database_id)
);
CREATE TABLE results.cm_covariate_analysis (
  	 covariate_analysis_id INT NOT NULL,
	 covariate_analysis_name VARCHAR,
	 analysis_id INT NOT NULL,
	PRIMARY KEY(covariate_analysis_id,analysis_id)
);
CREATE TABLE results.cm_covariate_balance (
  	 database_id VARCHAR NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 target_mean_before NUMERIC,
	 comparator_mean_before NUMERIC,
	 mean_before NUMERIC,
	 std_diff_before NUMERIC,
	 mean_after NUMERIC,
	 target_mean_after NUMERIC,
	 comparator_mean_after NUMERIC,
	 std_diff_after NUMERIC,
	 target_std_diff NUMERIC,
	 comparator_std_diff NUMERIC,
	 target_comparator_std_diff NUMERIC,
	PRIMARY KEY(database_id,target_id,comparator_id,outcome_id,analysis_id,covariate_id)
);
CREATE TABLE results.cm_diagnostics_summary (
  	 analysis_id INT NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 max_sdm NUMERIC,
	 shared_max_sdm NUMERIC,
	 equipoise NUMERIC,
	 mdrr NUMERIC,
	 attrition_fraction NUMERIC,
	 generalizability_max_sdm NUMERIC,
	 ease NUMERIC,
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
CREATE TABLE results.cm_target_comparator_outcome (
  	 outcome_id BIGINT NOT NULL,
	 outcome_of_interest INT,
	 true_effect_size NUMERIC,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	PRIMARY KEY(outcome_id,target_id,comparator_id)
);
CREATE TABLE results.cm_kaplan_meier_dist (
  	 time_day INT NOT NULL,
	 target_survival NUMERIC,
	 target_survival_lb NUMERIC,
	 target_survival_ub NUMERIC,
	 comparator_survival NUMERIC,
	 comparator_survival_lb NUMERIC,
	 comparator_survival_ub NUMERIC,
	 target_at_risk INT,
	 comparator_at_risk INT,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(time_day,target_id,comparator_id,outcome_id,analysis_id,database_id)
);
CREATE TABLE results.cm_likelihood_profile (
  	 log_rr NUMERIC NOT NULL,
	 log_likelihood NUMERIC,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 outcome_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(log_rr,target_id,comparator_id,outcome_id,analysis_id,database_id)
);
CREATE TABLE results.cm_preference_score_dist (
  	 analysis_id INT NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 preference_score NUMERIC NOT NULL,
	 target_density NUMERIC,
	 comparator_density NUMERIC,
	PRIMARY KEY(analysis_id,target_id,comparator_id,database_id,preference_score)
);
CREATE TABLE results.cm_propensity_model (
  	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 coefficient NUMERIC,
	PRIMARY KEY(target_id,comparator_id,analysis_id,database_id,covariate_id)
);
CREATE TABLE results.cm_shared_covariate_balance (
  	 database_id VARCHAR NOT NULL,
	 target_id BIGINT NOT NULL,
	 comparator_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 mean_before NUMERIC,
	 target_mean_before NUMERIC,
	 comparator_mean_before NUMERIC,
	 std_diff_before NUMERIC,
	 mean_after NUMERIC,
	 target_mean_after NUMERIC,
	 comparator_mean_after NUMERIC,
	 std_diff_after NUMERIC,
	 target_std_diff NUMERIC,
	 comparator_std_diff NUMERIC,
	 target_comparator_std_diff NUMERIC,
	PRIMARY KEY(database_id,target_id,comparator_id,analysis_id,covariate_id)
);
-- EvidenceSynthesisModule Tables
CREATE TABLE results.es_analysis (
  	 evidence_synthesis_analysis_id INT NOT NULL,
	 evidence_synthesis_description VARCHAR(255),
	 source_method VARCHAR(100),
	 definition VARCHAR,
	PRIMARY KEY(evidence_synthesis_analysis_id)
);
CREATE TABLE results.es_cm_diagnostics_summary (
  	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 evidence_synthesis_analysis_id INT NOT NULL,
	 mdrr NUMERIC,
	 i_2 NUMERIC,
	 tau NUMERIC,
	 ease NUMERIC,
	 mdrr_diagnostic VARCHAR(13),
	 i_2_diagnostic VARCHAR(13),
	 tau_diagnostic VARCHAR(13),
	 ease_diagnostic VARCHAR(13),
	 unblind INT,
	PRIMARY KEY(target_id,comparator_id,outcome_id,analysis_id,evidence_synthesis_analysis_id)
);
CREATE TABLE results.es_cm_result (
  	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 evidence_synthesis_analysis_id INT NOT NULL,
	 rr NUMERIC,
	 ci_95_lb NUMERIC,
	 ci_95_ub NUMERIC,
	 p NUMERIC,
	 one_sided_p NUMERIC,
	 log_rr NUMERIC,
	 se_log_rr NUMERIC,
	 target_subjects INT,
	 comparator_subjects INT,
	 target_days BIGINT,
	 comparator_days BIGINT,
	 target_outcomes INT,
	 comparator_outcomes INT,
	 n_databases INT,
	 calibrated_rr NUMERIC,
	 calibrated_ci_95_lb NUMERIC,
	 calibrated_ci_95_ub NUMERIC,
	 calibrated_p NUMERIC,
	 calibrated_one_sided_p NUMERIC,
	 calibrated_log_rr NUMERIC,
	 calibrated_se_log_rr NUMERIC,
	PRIMARY KEY(target_id,comparator_id,outcome_id,analysis_id,evidence_synthesis_analysis_id)
);
CREATE TABLE results.es_sccs_diagnostics_summary (
  	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 evidence_synthesis_analysis_id INT NOT NULL,
	 mdrr NUMERIC,
	 i_2 NUMERIC,
	 tau NUMERIC,
	 ease NUMERIC,
	 mdrr_diagnostic VARCHAR(13),
	 i_2_diagnostic VARCHAR(13),
	 tau_diagnostic VARCHAR(13),
	 ease_diagnostic VARCHAR(13),
	 unblind INT,
	PRIMARY KEY(exposures_outcome_set_id,covariate_id,analysis_id,evidence_synthesis_analysis_id)
);
CREATE TABLE results.es_sccs_result (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 evidence_synthesis_analysis_id INT NOT NULL,
	 rr NUMERIC,
	 ci_95_lb NUMERIC,
	 ci_95_ub NUMERIC,
	 p NUMERIC,
	 one_sided_p NUMERIC,
	 outcome_subjects INT,
	 outcome_events INT,
	 outcome_observation_periods INT,
	 covariate_subjects INT,
	 covariate_days BIGINT,
	 covariate_eras INT,
	 covariate_outcomes INT,
	 observed_days BIGINT,
	 n_databases INT,
	 log_rr NUMERIC,
	 se_log_rr NUMERIC,
	 calibrated_rr NUMERIC,
	 calibrated_ci_95_lb NUMERIC,
	 calibrated_ci_95_ub NUMERIC,
	 calibrated_p NUMERIC,
	 calibrated_one_sided_p NUMERIC,
	 calibrated_log_rr NUMERIC,
	 calibrated_se_log_rr NUMERIC,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,evidence_synthesis_analysis_id)
);
-- PatientLevelPredictionModule Tables
CREATE TABLE results.plp_cohorts (
  	 cohort_id INT NOT NULL,
	 cohort_definition_id BIGINT,
	 cohort_name VARCHAR,
	PRIMARY KEY(cohort_id)
);
CREATE TABLE results.plp_cohort_definition (
  	 cohort_definition_id BIGINT,
	 cohort_name VARCHAR,
	 description TEXT,
	 json TEXT,
	 sql_command TEXT
);
CREATE TABLE results.plp_database_meta_data (
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
CREATE TABLE results.plp_database_details (
  	 database_id INT NOT NULL,
	 database_meta_data_id VARCHAR,
	PRIMARY KEY(database_id)
);
CREATE TABLE results.plp_tars (
  	 tar_id INT NOT NULL,
	 tar_start_day INT,
	 tar_start_anchor VARCHAR,
	 tar_end_day INT,
	 tar_end_anchor VARCHAR,
	PRIMARY KEY(tar_id)
);
CREATE TABLE results.plp_population_settings (
  	 population_setting_id INT NOT NULL,
	 population_settings_json TEXT,
	PRIMARY KEY(population_setting_id)
);
CREATE TABLE results.plp_covariate_settings (
  	 covariate_setting_id INT NOT NULL,
	 covariate_settings_json TEXT,
	PRIMARY KEY(covariate_setting_id)
);
CREATE TABLE results.plp_model_settings (
  	 model_setting_id INT NOT NULL,
	 model_type VARCHAR,
	 model_settings_json VARCHAR,
	PRIMARY KEY(model_setting_id)
);
CREATE TABLE results.plp_split_settings (
  	 split_setting_id INT NOT NULL,
	 split_settings_json TEXT,
	PRIMARY KEY(split_setting_id)
);
CREATE TABLE results.plp_plp_data_settings (
  	 plp_data_setting_id INT NOT NULL,
	 plp_data_settings_json TEXT,
	PRIMARY KEY(plp_data_setting_id)
);
CREATE TABLE results.plp_feature_engineering_settings (
  	 feature_engineering_setting_id INT NOT NULL,
	 feature_engineering_settings_json TEXT,
	PRIMARY KEY(feature_engineering_setting_id)
);
CREATE TABLE results.plp_tidy_covariates_settings (
  	 tidy_covariates_setting_id INT NOT NULL,
	 tidy_covariates_settings_json TEXT,
	PRIMARY KEY(tidy_covariates_setting_id)
);
CREATE TABLE results.plp_sample_settings (
  	 sample_setting_id INT NOT NULL,
	 sample_settings_json TEXT,
	PRIMARY KEY(sample_setting_id)
);
CREATE TABLE results.plp_model_designs (
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
CREATE TABLE results.plp_diagnostics (
  	 diagnostic_id INT NOT NULL,
	 model_design_id INT,
	 database_id INT,
	 execution_date_time VARCHAR,
	PRIMARY KEY(diagnostic_id)
);
CREATE TABLE results.plp_diagnostic_summary (
  	 diagnostic_id INT,
	 probast_id VARCHAR,
	 result_value VARCHAR
);
CREATE TABLE results.plp_diagnostic_predictors (
  	 diagnostic_id INT,
	 days_to_event INT,
	 outcome_at_time INT,
	 observed_at_start_of_day BIGINT,
	 input_type VARCHAR
);
CREATE TABLE results.plp_diagnostic_participants (
  	 diagnostic_id INT,
	 design VARCHAR,
	 metric VARCHAR,
	 value NUMERIC,
	 probast_id VARCHAR
);
CREATE TABLE results.plp_diagnostic_outcomes (
  	 diagnostic_id INT,
	 xvalue INT,
	 outcome_percent NUMERIC,
	 aggregation VARCHAR,
	 probast_id VARCHAR,
	 input_type VARCHAR
);
CREATE TABLE results.plp_diagnostic_designs (
  	 diagnostic_id INT NOT NULL,
	 probast_id VARCHAR,
	 value VARCHAR,
	PRIMARY KEY(diagnostic_id)
);
CREATE TABLE results.plp_models (
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
	 intercept NUMERIC,
	PRIMARY KEY(model_id)
);
CREATE TABLE results.plp_recalibrations (
  	 recalibration_id INT NOT NULL,
	 original_model_id INT,
	 recalibrated_model_id INT,
	 recalibration_type VARCHAR,
	 recalibration_json VARCHAR,
	PRIMARY KEY(recalibration_id)
);
CREATE TABLE results.plp_performances (
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
CREATE TABLE results.plp_attrition (
  	 performance_id INT,
	 outcome_id INT,
	 description VARCHAR,
	 target_count INT,
	 unique_people INT,
	 outcomes INT
);
CREATE TABLE results.plp_prediction_distribution (
  	 performance_id INT,
	 evaluation VARCHAR,
	 class_label INT,
	 person_count INT,
	 average_predicted_probability NUMERIC,
	 st_dev_predicted_probability NUMERIC,
	 min_predicted_probability NUMERIC,
	 p_05_predicted_probability NUMERIC,
	 p_25_predicted_probability NUMERIC,
	 median_predicted_probability NUMERIC,
	 p_75_predicted_probability NUMERIC,
	 p_95_predicted_probability NUMERIC,
	 max_predicted_probability NUMERIC
);
CREATE TABLE results.plp_covariate_summary (
  	 performance_id INT,
	 covariate_id BIGINT,
	 covariate_name VARCHAR,
	 concept_id NUMERIC,
	 covariate_value NUMERIC,
	 covariate_count INT,
	 covariate_mean NUMERIC,
	 covariate_st_dev NUMERIC,
	 with_no_outcome_covariate_count INT,
	 with_no_outcome_covariate_mean NUMERIC,
	 with_no_outcome_covariate_st_dev NUMERIC,
	 with_outcome_covariate_count INT,
	 with_outcome_covariate_mean NUMERIC,
	 with_outcome_covariate_st_dev NUMERIC,
	 standardized_mean_diff NUMERIC
);
CREATE TABLE results.plp_threshold_summary (
  	 performance_id INT,
	 evaluation VARCHAR,
	 prediction_threshold NUMERIC,
	 preference_threshold NUMERIC,
	 positive_count INT,
	 negative_count INT,
	 true_count INT,
	 false_count INT,
	 true_positive_count INT,
	 true_negative_count INT,
	 false_positive_count INT,
	 false_negative_count INT,
	 f_1_score NUMERIC,
	 accuracy NUMERIC,
	 sensitivity NUMERIC,
	 false_negative_rate NUMERIC,
	 false_positive_rate NUMERIC,
	 specificity NUMERIC,
	 positive_predictive_value NUMERIC,
	 false_discovery_rate NUMERIC,
	 negative_predictive_value NUMERIC,
	 false_omission_rate NUMERIC,
	 positive_likelihood_ratio NUMERIC,
	 negative_likelihood_ratio NUMERIC,
	 diagnostic_odds_ratio NUMERIC
);
CREATE TABLE results.plp_calibration_summary (
  	 performance_id INT,
	 evaluation VARCHAR,
	 prediction_threshold NUMERIC,
	 person_count_at_risk INT,
	 person_count_with_outcome INT,
	 average_predicted_probability NUMERIC,
	 st_dev_predicted_probability NUMERIC,
	 min_predicted_probability NUMERIC,
	 p_25_predicted_probability NUMERIC,
	 median_predicted_probability NUMERIC,
	 p_75_predicted_probability NUMERIC,
	 max_predicted_probability NUMERIC,
	 observed_incidence NUMERIC
);
CREATE TABLE results.plp_evaluation_statistics (
  	 performance_id INT,
	 evaluation VARCHAR,
	 metric VARCHAR,
	 value NUMERIC
);
CREATE TABLE results.plp_demographic_summary (
  	 performance_id INT,
	 evaluation VARCHAR,
	 age_group VARCHAR,
	 gen_group VARCHAR,
	 person_count_at_risk INT,
	 person_count_with_outcome INT,
	 average_predicted_probability NUMERIC,
	 st_dev_predicted_probability NUMERIC,
	 min_predicted_probability NUMERIC,
	 p_25_predicted_probability NUMERIC,
	 p_50_predicted_probability NUMERIC,
	 p_75_predicted_probability NUMERIC,
	 max_predicted_probability NUMERIC
);
-- SelfControlledCaseSeriesModule Tables
CREATE TABLE results.sccs_analysis (
  	 analysis_id INT NOT NULL,
	 description VARCHAR,
	 definition VARCHAR,
	PRIMARY KEY(analysis_id)
);
CREATE TABLE results.sccs_covariate_analysis (
  	 analysis_id INT NOT NULL,
	 covariate_analysis_id INT NOT NULL,
	 covariate_analysis_name VARCHAR,
	 variable_of_interest INT,
	 pre_exposure INT,
	 end_of_observation_period INT,
	PRIMARY KEY(analysis_id,covariate_analysis_id)
);
CREATE TABLE results.sccs_covariate (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 covariate_name VARCHAR,
	 era_id INT,
	 covariate_analysis_id INT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);
CREATE TABLE results.sccs_era (
  	 exposures_outcome_set_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 era_type VARCHAR NOT NULL,
	 era_id INT NOT NULL,
	 era_name VARCHAR,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(exposures_outcome_set_id,analysis_id,era_type,era_id,database_id)
);
CREATE TABLE results.sccs_exposures_outcome_set (
  	 exposures_outcome_set_id INT NOT NULL,
	 outcome_id INT,
	 nesting_cohort_id INT,
	PRIMARY KEY(exposures_outcome_set_id)
);
CREATE TABLE results.sccs_exposure (
  	 exposures_outcome_set_id INT NOT NULL,
	 era_id INT NOT NULL,
	 true_effect_size NUMERIC,
	PRIMARY KEY(exposures_outcome_set_id,era_id)
);
CREATE TABLE results.sccs_spline (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 spline_type VARCHAR NOT NULL,
	 knot_month NUMERIC NOT NULL,
	 rr NUMERIC,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,spline_type,knot_month)
);
CREATE TABLE results.sccs_censor_model (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 parameter_id INT NOT NULL,
	 parameter_value NUMERIC,
	 model_type VARCHAR,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,parameter_id)
);
CREATE TABLE results.sccs_result (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 rr NUMERIC,
	 ci_95_lb NUMERIC,
	 ci_95_ub NUMERIC,
	 p NUMERIC,
	 one_sided_p NUMERIC,
	 outcome_subjects INT,
	 outcome_events INT,
	 outcome_observation_periods INT,
	 covariate_subjects INT,
	 covariate_days INT,
	 covariate_eras INT,
	 covariate_outcomes INT,
	 observed_days BIGINT,
	 log_rr NUMERIC,
	 se_log_rr NUMERIC,
	 llr NUMERIC,
	 calibrated_rr NUMERIC,
	 calibrated_ci_95_lb NUMERIC,
	 calibrated_ci_95_ub NUMERIC,
	 calibrated_p NUMERIC,
	 calibrated_one_sided_p NUMERIC,
	 calibrated_log_rr NUMERIC,
	 calibrated_se_log_rr NUMERIC,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);
CREATE TABLE results.sccs_covariate_result (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 covariate_id INT NOT NULL,
	 rr NUMERIC,
	 ci_95_lb NUMERIC,
	 ci_95_ub NUMERIC,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,covariate_id)
);
CREATE TABLE results.sccs_attrition (
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
CREATE TABLE results.sccs_likelihood_profile (
  	 log_rr NUMERIC NOT NULL,
	 log_likelihood NUMERIC,
	 gradient NUMERIC,
	 covariate_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(log_rr,covariate_id,exposures_outcome_set_id,analysis_id,database_id)
);
CREATE TABLE results.sccs_time_trend (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 calendar_year INT NOT NULL,
	 calendar_month INT NOT NULL,
	 observed_subjects INT,
	 ratio NUMERIC,
	 adjusted_ratio NUMERIC,
	 outcome_rate NUMERIC,
	 adjusted_rate NUMERIC,
	 stable INT,
	 p NUMERIC,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,calendar_year,calendar_month)
);
CREATE TABLE results.sccs_time_to_event (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 era_id INT NOT NULL,
	 week INT NOT NULL,
	 observed_subjects INT,
	 outcomes INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,era_id,week)
);
CREATE TABLE results.sccs_age_spanning (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 age_month INT NOT NULL,
	 cover_before_after_subjects INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,age_month)
);
CREATE TABLE results.sccs_calendar_time_spanning (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 calendar_year INT NOT NULL,
	 calendar_month INT NOT NULL,
	 cover_before_after_subjects INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,calendar_year,calendar_month)
);
CREATE TABLE results.sccs_diagnostics_summary (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 time_stability_p NUMERIC,
	 time_stability_diagnostic VARCHAR(20),
	 event_exposure_lb NUMERIC,
	 event_exposure_ub NUMERIC,
	 event_exposure_diagnostic VARCHAR(20),
	 event_observation_lb NUMERIC,
	 event_observation_ub NUMERIC,
	 event_observation_diagnostic VARCHAR(20),
	 rare_outcome_prevalence NUMERIC,
	 rare_outcome_diagnostic VARCHAR(20),
	 ease NUMERIC,
	 ease_diagnostic VARCHAR(20),
	 mdrr NUMERIC,
	 mdrr_diagnostic VARCHAR(20),
	 unblind INT,
	 unblind_for_evidence_synthesis INT,
	 time_trend_p NUMERIC,
	 pre_exposure_p NUMERIC,
	 time_trend_diagnostic VARCHAR(20),
	 pre_exposure_diagnostic VARCHAR(20),
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);
CREATE TABLE results.sccs_event_dep_observation (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 months_to_end INT NOT NULL,
	 censored INT NOT NULL,
	 outcomes INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,months_to_end,censored)
);
-- TreatmentPatternsModule Tables
CREATE TABLE results.tp_analyses (
  	 analysis_id INT,
	 description VARCHAR
);
CREATE TABLE results.tp_arguments (
  	 analysis_id INT,
	 arguments VARCHAR,
	 database_id INTEGER
);
CREATE TABLE results.tp_attrition (
  	 analysis_id INT,
	 database_id INTEGER,
	 number_records INT,
	 number_subjects INT,
	 reason VARCHAR,
	 reason_id INT,
	 target_cohort_id INTEGER,
	 target_cohort_name VARCHAR,
	 time_stamp BIGINT
);
CREATE TABLE results.tp_cdm_source_info (
  	 analysis_id INT,
	 cdm_etl_reference VARCHAR,
	 cdm_holder VARCHAR,
	 cdm_release_date DATE,
	 cdm_source_abbreviation VARCHAR,
	 cdm_source_name VARCHAR,
	 cdm_version VARCHAR,
	 database_id INTEGER,
	 source_description VARCHAR,
	 source_documentation_reference VARCHAR,
	 source_release_date DATE,
	 vocabulary_version VARCHAR
);
CREATE TABLE results.tp_counts_age (
  	 age INT,
	 analysis_id INT,
	 database_id INTEGER,
	 n VARCHAR,
	 target_cohort_id INTEGER,
	 target_cohort_name VARCHAR
);
CREATE TABLE results.tp_counts_sex (
  	 analysis_id INT,
	 database_id INTEGER,
	 n VARCHAR,
	 sex VARCHAR,
	 target_cohort_id INTEGER,
	 target_cohort_name VARCHAR
);
CREATE TABLE results.tp_counts_year (
  	 analysis_id INT,
	 database_id INTEGER,
	 n VARCHAR,
	 target_cohort_id INTEGER,
	 target_cohort_name VARCHAR,
	 index_year INTEGER
);
CREATE TABLE results.tp_metadata (
  	 analysis_id INT,
	 database_id INTEGER,
	 execution_end BIGINT,
	 execution_start BIGINT,
	 package_version VARCHAR,
	 platform VARCHAR,
	 r_version VARCHAR
);
CREATE TABLE results.tp_summary_event_duration (
  	 analysis_id INT,
	 duration_average NUMERIC,
	 event_count INT,
	 database_id INTEGER,
	 event_name VARCHAR,
	 line VARCHAR,
	 duration_max INT,
	 duration_median INT,
	 duration_min INT,
	 duration_q_1 INT,
	 duration_q_2 INT,
	 duration_sd NUMERIC,
	 target_cohort_id INTEGER,
	 target_cohort_name VARCHAR
);
CREATE TABLE results.tp_treatment_pathways (
  	 age VARCHAR,
	 analysis_id INT,
	 database_id INTEGER,
	 freq INT,
	 index_year VARCHAR,
	 pathway VARCHAR,
	 sex VARCHAR,
	 target_cohort_id INTEGER,
	 target_cohort_name VARCHAR
);

CREATE TABLE @database_schema.@table_prefixsurvival_results (
    row_id BIGSERIAL PRIMARY KEY,
    result_id INTEGER,
    cdm_name VARCHAR(255),
    group_name VARCHAR(255),
    group_level VARCHAR(255),
    strata_name VARCHAR(255),
    strata_level VARCHAR(255),
    variable_name VARCHAR(255),
    variable_level VARCHAR(255),
    estimate_name VARCHAR(255),
    estimate_type VARCHAR(100),
    estimate_value TEXT,
    additional_name VARCHAR(255),
    additional_level VARCHAR(255)
);

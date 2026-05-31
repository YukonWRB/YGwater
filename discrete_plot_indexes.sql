-- Recommended indexes for the YGwater discrete plot selector flow.
--
-- Apply during a maintenance/update window.  The CONCURRENTLY form avoids
-- blocking writes, but each statement must be run outside an explicit
-- transaction block.

-- Location-first date-range and location/date -> parameter lookups.
CREATE INDEX CONCURRENTLY IF NOT EXISTS samples_location_datetime_sample_id_idx
ON discrete.samples (location_id, datetime, sample_id);

-- Location + sample media + date-range lookups.
CREATE INDEX CONCURRENTLY IF NOT EXISTS samples_location_media_datetime_sample_id_idx
ON discrete.samples (location_id, media_id, datetime, sample_id);

-- Parameter-first result lookups used by plotting and selected-parameter
-- availability checks.
CREATE INDEX CONCURRENTLY IF NOT EXISTS results_parameter_sample_id_idx
ON discrete.results (parameter_id, sample_id);

-- Speeds up advanced selector availability once samples have been narrowed.
-- This has the same leading keys as idx_results_sample_parameter, but includes
-- result qualifier dimensions so Postgres can answer selector queries with
-- fewer heap visits on large result tables.
CREATE INDEX CONCURRENTLY IF NOT EXISTS results_sample_parameter_selector_values_idx
ON discrete.results (sample_id, parameter_id)
INCLUDE (
  result_type,
  sample_fraction_id,
  result_value_type,
  result_speciation_id
);

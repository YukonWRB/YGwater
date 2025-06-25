-- Add table for storing discrete data column mappings used by the YGwater app

CREATE TABLE IF NOT EXISTS application.discrete_mappings (
    mapping_name text PRIMARY KEY,
    mapping jsonb NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);

COMMENT ON TABLE application.discrete_mappings IS 'Stores column to parameter mappings for discrete data uploads.';
COMMENT ON COLUMN application.discrete_mappings.mapping IS 'JSON structure with datetime column and parameter associations.';

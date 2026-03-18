# This helper script creates and updates tables required for YGwater usage
# analytics. It expects an existing DBI connection object named `con`.

# Extend session table with close metadata
DBI::dbExecute(
  con,
  "ALTER TABLE application.shiny_app_usage
   ADD COLUMN IF NOT EXISTS session_end_source TEXT;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE application.shiny_app_usage
   ADD COLUMN IF NOT EXISTS session_end_note TEXT;"
)
DBI::dbExecute(
  con,
  "GRANT UPDATE (session_end, session_end_source, session_end_note, error_message, login_to)
   ON application.shiny_app_usage TO PUBLIC;"
)

# Event table for page-level and action-level usage tracking
DBI::dbExecute(
  con,
  "CREATE TABLE IF NOT EXISTS application.shiny_app_usage_event (
     id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
     usage_id INTEGER NOT NULL REFERENCES application.shiny_app_usage(id) ON DELETE CASCADE,
     event_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
     event_type TEXT NOT NULL,
     page_id TEXT NULL,
     app_side TEXT NOT NULL CHECK (app_side IN ('public', 'admin', 'system')),
     duration_ms INTEGER NULL CHECK (duration_ms >= 0),
     payload JSONB NOT NULL DEFAULT '{}'::jsonb
   );"
)

# Comments
DBI::dbExecute(
  con,
  "COMMENT ON TABLE application.shiny_app_usage_event IS
   'Append-only usage events for YGwater sessions (page enters/leaves, downloads, plot requests, map filter activity, and session close diagnostics).';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN application.shiny_app_usage_event.usage_id IS
   'Foreign key to application.shiny_app_usage.id for the parent session record.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN application.shiny_app_usage_event.app_side IS
   'Application side classification: public, admin, or system.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN application.shiny_app_usage_event.payload IS
   'Redacted event metadata captured as JSONB (ids, ranges, counts, and flags).';"
)

# Indexes for reporting queries
DBI::dbExecute(
  con,
  "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_usage_ts_idx
   ON application.shiny_app_usage_event (usage_id, event_ts);"
)
DBI::dbExecute(
  con,
  "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_type_ts_idx
   ON application.shiny_app_usage_event (event_type, event_ts);"
)
DBI::dbExecute(
  con,
  "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_page_ts_idx
   ON application.shiny_app_usage_event (page_id, event_ts);"
)
DBI::dbExecute(
  con,
  "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_side_ts_idx
   ON application.shiny_app_usage_event (app_side, event_ts);"
)
DBI::dbExecute(
  con,
  "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_payload_gin_idx
   ON application.shiny_app_usage_event USING GIN (payload jsonb_path_ops);"
)

# Permissions
DBI::dbExecute(
  con,
  "GRANT INSERT ON TABLE application.shiny_app_usage_event TO PUBLIC;"
)

DBI::dbDisconnect(con)

-- =============================================================================
-- ontologyR: Core Schema
-- =============================================================================
-- This schema supports evidence-based ontology governance:
--   1. Object types map concepts to underlying tables
--   2. Link types define relationships between objects
--   3. Concepts are versioned definitions (SQL expressions)
--   4. Audits record human judgments against concept evaluations
--   5. Governance log tracks adoption, deprecation, and policy actions
-- =============================================================================

-- -----------------------------------------------------------------------------
-- OBJECT TYPES
-- Maps a logical object type (e.g., "Encounter", "Patient") to its physical
-- storage (table name + primary key column).
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_object_types (
    object_type     TEXT PRIMARY KEY,
    table_name      TEXT NOT NULL,
    pk_column       TEXT NOT NULL,
    description     TEXT,
    owner_domain    TEXT,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by      TEXT
);

-- -----------------------------------------------------------------------------
-- LINK TYPES
-- Defines relationships between object types. Supports temporal validity
-- (valid_from/valid_to columns) for links that change over time.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_link_types (
    link_type       TEXT PRIMARY KEY,
    from_object     TEXT NOT NULL,
    to_object       TEXT NOT NULL,
    link_table      TEXT NOT NULL,
    from_key        TEXT NOT NULL,
    to_key          TEXT NOT NULL,
    valid_from_col  TEXT,               -- Column name for temporal start (nullable)
    valid_to_col    TEXT,               -- Column name for temporal end (nullable)
    cardinality     TEXT DEFAULT 'many-to-many',  -- one-to-one, one-to-many, many-to-many
    description     TEXT,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by      TEXT,
    FOREIGN KEY (from_object) REFERENCES ont_object_types(object_type),
    FOREIGN KEY (to_object) REFERENCES ont_object_types(object_type)
);

-- -----------------------------------------------------------------------------
-- CONCEPTS
-- A concept is a named, governed definition (e.g., "ready_for_discharge").
-- The actual logic lives in ont_concept_versions; this table holds metadata.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_concepts (
    concept_id      TEXT PRIMARY KEY,
    object_type     TEXT NOT NULL,      -- Which object type this concept applies to
    description     TEXT,
    owner_domain    TEXT,               -- Which team/domain owns this concept
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by      TEXT,
    FOREIGN KEY (object_type) REFERENCES ont_object_types(object_type)
);

-- -----------------------------------------------------------------------------
-- CONCEPT VERSIONS
-- Each concept can have multiple versions, scoped by context (e.g., "flow",
-- "clinical", "regulatory"). The sql_expr is evaluated against the object's
-- underlying table.
-- 
-- Status lifecycle: draft -> active -> deprecated -> retired
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_concept_versions (
    concept_id      TEXT NOT NULL,
    scope           TEXT NOT NULL,      -- e.g., "flow", "clinical", "regulatory"
    version         INTEGER NOT NULL,
    sql_expr        TEXT NOT NULL,      -- SQL expression returning BOOLEAN or numeric
    status          TEXT NOT NULL DEFAULT 'draft',  -- draft, active, deprecated, retired
    rationale       TEXT,               -- Why this version exists
    valid_from      DATE,               -- When this version becomes applicable
    valid_to        DATE,               -- When this version stops being applicable
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by      TEXT,
    approved_at     TIMESTAMP,
    approved_by     TEXT,
    PRIMARY KEY (concept_id, scope, version),
    FOREIGN KEY (concept_id) REFERENCES ont_concepts(concept_id)
);

-- -----------------------------------------------------------------------------
-- AUDITS
-- Records human judgments against concept evaluations. This is the core
-- mechanism for detecting drift: compare system_value to reviewer_value.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_audits (
    audit_id        TEXT PRIMARY KEY,
    concept_id      TEXT NOT NULL,
    scope           TEXT NOT NULL,
    version         INTEGER NOT NULL,
    object_key      TEXT NOT NULL,      -- Primary key of the audited object
    system_value    BOOLEAN,            -- What the concept evaluated to
    reviewer_value  BOOLEAN,            -- What the human judged
    reviewer_id     TEXT,
    audited_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    notes           TEXT,
    FOREIGN KEY (concept_id, scope, version) 
        REFERENCES ont_concept_versions(concept_id, scope, version)
);

-- Index for efficient drift queries
CREATE INDEX IF NOT EXISTS idx_audits_concept_time 
    ON ont_audits(concept_id, scope, version, audited_at);

-- -----------------------------------------------------------------------------
-- DRIFT EVENTS
-- Records when drift is detected, investigated, and resolved. Links audits
-- to governance actions.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_drift_events (
    drift_id        TEXT PRIMARY KEY,
    concept_id      TEXT NOT NULL,
    scope           TEXT NOT NULL,
    version         INTEGER NOT NULL,
    detected_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    detection_type  TEXT NOT NULL,      -- 'threshold', 'trend', 'manual'
    disagreement_rate REAL,             -- Rate at time of detection
    window_start    TIMESTAMP,
    window_end      TIMESTAMP,
    audit_count     INTEGER,            -- Number of audits in detection window
    status          TEXT DEFAULT 'open', -- open, investigating, resolved, accepted
    resolution      TEXT,               -- How it was resolved
    resolved_at     TIMESTAMP,
    resolved_by     TEXT,
    FOREIGN KEY (concept_id, scope, version) 
        REFERENCES ont_concept_versions(concept_id, scope, version)
);

-- -----------------------------------------------------------------------------
-- GOVERNANCE LOG
-- Tracks all governance actions: adoptions, deprecations, reviews, etc.
-- Provides full audit trail for definition changes.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_governance_log (
    log_id          TEXT PRIMARY KEY,
    action_type     TEXT NOT NULL,      -- adopt, deprecate, retire, review, block, unblock
    concept_id      TEXT NOT NULL,
    scope           TEXT,
    version         INTEGER,
    actor           TEXT NOT NULL,
    action_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    rationale       TEXT,
    evidence        TEXT,               -- JSON: audit stats, drift event refs, etc.
    blocked_by      TEXT,               -- Reference to blocking condition (e.g., drift event)
    FOREIGN KEY (concept_id) REFERENCES ont_concepts(concept_id)
);

-- -----------------------------------------------------------------------------
-- DASHBOARD REGISTRY (optional)
-- Tracks which dashboards/reports use which concept versions.
-- Enables impact analysis when definitions change.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_dashboard_registry (
    dashboard_id    TEXT NOT NULL,
    dashboard_name  TEXT,
    concept_id      TEXT NOT NULL,
    scope           TEXT NOT NULL,
    version         INTEGER NOT NULL,
    registered_at   TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    registered_by   TEXT,
    PRIMARY KEY (dashboard_id, concept_id, scope, version),
    FOREIGN KEY (concept_id, scope, version) 
        REFERENCES ont_concept_versions(concept_id, scope, version)
);

-- -----------------------------------------------------------------------------
-- VIEWS: Convenience views for common queries
-- -----------------------------------------------------------------------------

-- Current active version of each concept/scope combination
CREATE VIEW IF NOT EXISTS ont_active_concepts AS
SELECT 
    cv.*,
    c.description AS concept_description,
    c.owner_domain
FROM ont_concept_versions cv
JOIN ont_concepts c ON cv.concept_id = c.concept_id
WHERE cv.status = 'active'
  AND (cv.valid_from IS NULL OR cv.valid_from <= CURRENT_DATE)
  AND (cv.valid_to IS NULL OR cv.valid_to >= CURRENT_DATE);

-- Drift summary per concept version
CREATE VIEW IF NOT EXISTS ont_drift_summary AS
SELECT
    concept_id,
    scope,
    version,
    COUNT(*) AS audit_count,
    SUM(CASE WHEN system_value = reviewer_value THEN 1 ELSE 0 END) AS agreements,
    SUM(CASE WHEN system_value != reviewer_value THEN 1 ELSE 0 END) AS disagreements,
    ROUND(1.0 * SUM(CASE WHEN system_value != reviewer_value THEN 1 ELSE 0 END) / COUNT(*), 4) AS disagreement_rate,
    MIN(audited_at) AS first_audit,
    MAX(audited_at) AS last_audit
FROM ont_audits
GROUP BY concept_id, scope, version;

-- =============================================================================
-- OBSERVATIONS & ANALYSIS
-- =============================================================================
-- Observations capture point-in-time snapshots of concept evaluations.
-- Unlike audits (human vs system), observations are recorded facts about
-- what the system evaluated at a specific moment.
--
-- This enables:
--   - Trend analysis: How does concept prevalence change over time?
--   - Version comparison: How do different versions evaluate the same data?
--   - Cohort analysis: How do concept values differ across object groups?
-- =============================================================================

-- -----------------------------------------------------------------------------
-- OBSERVATIONS
-- Aggregate snapshots of concept evaluations at a point in time.
-- Each observation records summary statistics for a concept evaluation run.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_observations (
    observation_id   TEXT PRIMARY KEY,
    concept_id       TEXT NOT NULL,
    scope            TEXT NOT NULL,
    version          INTEGER NOT NULL,
    observed_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    observation_type TEXT NOT NULL DEFAULT 'snapshot',  -- snapshot, scheduled, triggered

    -- Aggregate statistics from the observation
    total_objects    INTEGER NOT NULL,
    concept_true     INTEGER NOT NULL,
    concept_false    INTEGER NOT NULL,
    concept_null     INTEGER DEFAULT 0,

    -- Derived metrics (stored for query efficiency)
    prevalence_rate  REAL,              -- concept_true / total_objects

    -- Optional filter that was applied
    filter_expr      TEXT,

    -- Metadata
    triggered_by     TEXT,              -- 'manual', 'schedule', 'drift_check', 'governance'
    observer_id      TEXT,              -- Who/what initiated this observation
    notes            TEXT,

    FOREIGN KEY (concept_id, scope, version)
        REFERENCES ont_concept_versions(concept_id, scope, version)
);

-- Index for time-series queries
CREATE INDEX IF NOT EXISTS idx_observations_time
    ON ont_observations(concept_id, scope, version, observed_at);

-- -----------------------------------------------------------------------------
-- OBSERVATION DETAILS (optional)
-- Object-level details for observations where granular tracking is needed.
-- Only populated when store_details = TRUE in ont_observe().
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_observation_details (
    observation_id   TEXT NOT NULL,
    object_key       TEXT NOT NULL,
    concept_value    BOOLEAN,

    PRIMARY KEY (observation_id, object_key),
    FOREIGN KEY (observation_id) REFERENCES ont_observations(observation_id)
);

-- -----------------------------------------------------------------------------
-- COHORTS
-- Named groups of objects for comparative analysis.
-- Cohorts can be defined by SQL expressions or explicit membership.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_cohorts (
    cohort_id        TEXT PRIMARY KEY,
    cohort_name      TEXT NOT NULL,
    object_type      TEXT NOT NULL,
    definition_type  TEXT NOT NULL DEFAULT 'sql',  -- 'sql' or 'explicit'
    sql_expr         TEXT,               -- SQL WHERE clause for dynamic cohorts
    description      TEXT,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by       TEXT,

    FOREIGN KEY (object_type) REFERENCES ont_object_types(object_type)
);

-- -----------------------------------------------------------------------------
-- COHORT MEMBERS (for explicit cohorts)
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_cohort_members (
    cohort_id        TEXT NOT NULL,
    object_key       TEXT NOT NULL,
    added_at         TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    added_by         TEXT,

    PRIMARY KEY (cohort_id, object_key),
    FOREIGN KEY (cohort_id) REFERENCES ont_cohorts(cohort_id)
);

-- -----------------------------------------------------------------------------
-- ANALYSIS RUNS
-- Records of analysis executions for reproducibility and audit trail.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_analysis_runs (
    analysis_id      TEXT PRIMARY KEY,
    analysis_type    TEXT NOT NULL,      -- 'trend', 'cohort_compare', 'version_compare', 'distribution'
    concept_id       TEXT NOT NULL,
    scope            TEXT,
    parameters       TEXT,               -- JSON: analysis parameters
    results_summary  TEXT,               -- JSON: key findings
    executed_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    executed_by      TEXT,

    FOREIGN KEY (concept_id) REFERENCES ont_concepts(concept_id)
);

-- -----------------------------------------------------------------------------
-- VIEWS: Analysis convenience views
-- -----------------------------------------------------------------------------

-- Observation time series for trend analysis
CREATE VIEW IF NOT EXISTS ont_observation_trends AS
SELECT
    concept_id,
    scope,
    version,
    DATE(observed_at) AS observation_date,
    COUNT(*) AS observation_count,
    AVG(prevalence_rate) AS avg_prevalence,
    MIN(prevalence_rate) AS min_prevalence,
    MAX(prevalence_rate) AS max_prevalence,
    SUM(total_objects) AS total_objects_observed
FROM ont_observations
GROUP BY concept_id, scope, version, DATE(observed_at);


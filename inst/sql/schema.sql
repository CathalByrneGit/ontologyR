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

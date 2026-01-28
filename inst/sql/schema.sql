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

-- =============================================================================
-- DATASETS, TRANSFORMS & LINEAGE (Foundry-inspired)
-- =============================================================================
-- This layer adds:
--   - Dataset registry: track all data assets (source tables, materialized outputs)
--   - Transforms: define how datasets are produced
--   - Runs: record each execution with inputs/outputs
--   - Lineage edges: track data flow for impact analysis
-- =============================================================================

-- -----------------------------------------------------------------------------
-- DATASETS
-- Registry of all data assets - source tables, materialized concepts, derived datasets.
-- A dataset can be:
--   - 'source': an external table registered for use
--   - 'materialized': output of ont_materialize()
--   - 'derived': output of a transform
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_datasets (
    dataset_id       TEXT PRIMARY KEY,
    dataset_name     TEXT NOT NULL,
    dataset_type     TEXT NOT NULL DEFAULT 'source',  -- source, materialized, derived
    physical_name    TEXT NOT NULL,           -- actual table/view name in DB
    object_type      TEXT,                    -- linked object type (if applicable)
    description      TEXT,
    schema_json      TEXT,                    -- JSON: column names and types
    row_count        INTEGER,                 -- cached row count
    owner            TEXT,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by       TEXT,
    updated_at       TIMESTAMP,

    -- For materialized datasets, link to source concept
    source_concept_id TEXT,
    source_scope      TEXT,
    source_version    INTEGER,
    source_filter     TEXT,                   -- filter expression used

    FOREIGN KEY (object_type) REFERENCES ont_object_types(object_type)
);

-- -----------------------------------------------------------------------------
-- TRANSFORMS
-- Defines how a dataset is produced. Can be SQL or reference to R function.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_transforms (
    transform_id     TEXT PRIMARY KEY,
    transform_name   TEXT NOT NULL,
    output_dataset_id TEXT NOT NULL,
    transform_type   TEXT NOT NULL DEFAULT 'sql',  -- sql, r_function, concept_eval
    code             TEXT,                    -- SQL text or R function name
    description      TEXT,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by       TEXT,

    FOREIGN KEY (output_dataset_id) REFERENCES ont_datasets(dataset_id)
);

-- -----------------------------------------------------------------------------
-- TRANSFORM INPUTS
-- Links transforms to their input datasets (for DAG construction).
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_transform_inputs (
    transform_id     TEXT NOT NULL,
    input_dataset_id TEXT NOT NULL,
    input_role       TEXT DEFAULT 'primary',  -- primary, join, lookup, filter

    PRIMARY KEY (transform_id, input_dataset_id),
    FOREIGN KEY (transform_id) REFERENCES ont_transforms(transform_id),
    FOREIGN KEY (input_dataset_id) REFERENCES ont_datasets(dataset_id)
);

-- -----------------------------------------------------------------------------
-- RUNS
-- Records each execution of a transform or materialization.
-- This is the core of reproducibility - "this output came from these inputs."
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_runs (
    run_id           TEXT PRIMARY KEY,
    transform_id     TEXT,                    -- NULL for ad-hoc materializations
    run_type         TEXT NOT NULL,           -- 'transform', 'materialize', 'evaluate'
    status           TEXT NOT NULL DEFAULT 'running',  -- running, success, failed
    started_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    ended_at         TIMESTAMP,

    -- Input snapshot for reproducibility
    input_snapshot   TEXT,                    -- JSON: dataset_id -> {row_count, hash, ...}

    -- Output info
    output_dataset_id TEXT,
    output_row_count  INTEGER,
    output_hash       TEXT,                   -- hash of output for change detection

    -- Provenance
    concept_id       TEXT,                    -- if this run evaluated a concept
    scope            TEXT,
    version          INTEGER,
    sql_executed     TEXT,                    -- actual SQL that ran
    filter_expr      TEXT,

    -- Metadata
    triggered_by     TEXT,                    -- 'manual', 'schedule', 'dependency'
    executed_by      TEXT,
    log              TEXT,                    -- execution log/errors

    FOREIGN KEY (transform_id) REFERENCES ont_transforms(transform_id),
    FOREIGN KEY (output_dataset_id) REFERENCES ont_datasets(dataset_id)
);

-- Index for querying run history
CREATE INDEX IF NOT EXISTS idx_runs_time ON ont_runs(started_at);
CREATE INDEX IF NOT EXISTS idx_runs_output ON ont_runs(output_dataset_id);

-- -----------------------------------------------------------------------------
-- LINEAGE EDGES
-- Explicit edges in the data lineage graph.
-- Captures: "dataset A was used to produce dataset B in run R"
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_lineage_edges (
    edge_id          TEXT PRIMARY KEY,
    run_id           TEXT NOT NULL,
    from_dataset_id  TEXT NOT NULL,
    to_dataset_id    TEXT NOT NULL,
    edge_type        TEXT NOT NULL,           -- 'input', 'join', 'filter', 'concept_eval'
    details_json     TEXT,                    -- additional context
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (run_id) REFERENCES ont_runs(run_id),
    FOREIGN KEY (from_dataset_id) REFERENCES ont_datasets(dataset_id),
    FOREIGN KEY (to_dataset_id) REFERENCES ont_datasets(dataset_id)
);

-- Index for lineage queries
CREATE INDEX IF NOT EXISTS idx_lineage_from ON ont_lineage_edges(from_dataset_id);
CREATE INDEX IF NOT EXISTS idx_lineage_to ON ont_lineage_edges(to_dataset_id);

-- -----------------------------------------------------------------------------
-- VIEWS: Lineage convenience views
-- -----------------------------------------------------------------------------

-- Upstream lineage: what datasets feed into this one?
CREATE VIEW IF NOT EXISTS ont_upstream_lineage AS
SELECT
    to_dataset_id AS dataset_id,
    from_dataset_id AS upstream_dataset_id,
    edge_type,
    run_id,
    r.started_at AS run_time
FROM ont_lineage_edges le
JOIN ont_runs r ON le.run_id = r.run_id;

-- Downstream lineage: what datasets depend on this one?
CREATE VIEW IF NOT EXISTS ont_downstream_lineage AS
SELECT
    from_dataset_id AS dataset_id,
    to_dataset_id AS downstream_dataset_id,
    edge_type,
    run_id,
    r.started_at AS run_time
FROM ont_lineage_edges le
JOIN ont_runs r ON le.run_id = r.run_id;

-- =============================================================================
-- RBAC-LITE & GOVERNANCE GATES (Foundry-inspired)
-- =============================================================================
-- This layer adds:
--   - Roles: Named permission sets (viewer, editor, approver, admin)
--   - User roles: Assign roles to users per domain/scope
--   - Permissions: Fine-grained action permissions
--   - Governance gates: Conditions that must pass before status transitions
--   - Gate checks: Record of gate evaluations
-- =============================================================================

-- -----------------------------------------------------------------------------
-- ROLES
-- Predefined roles with associated permissions.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_roles (
    role_id          TEXT PRIMARY KEY,
    role_name        TEXT NOT NULL,
    description      TEXT,
    permissions      TEXT NOT NULL,           -- JSON array of permission strings
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert default roles if not exist
INSERT OR IGNORE INTO ont_roles (role_id, role_name, description, permissions) VALUES
    ('viewer', 'Viewer', 'Can view concepts and evaluations', '["concept:read", "dataset:read", "audit:read"]'),
    ('editor', 'Editor', 'Can create and modify draft concepts', '["concept:read", "concept:write", "concept:evaluate", "dataset:read", "dataset:write", "audit:read", "audit:write"]'),
    ('approver', 'Approver', 'Can approve and activate concepts', '["concept:read", "concept:write", "concept:evaluate", "concept:approve", "concept:activate", "dataset:read", "dataset:write", "audit:read", "audit:write", "gate:override"]'),
    ('admin', 'Admin', 'Full access to all operations', '["*"]');

-- -----------------------------------------------------------------------------
-- USER ROLES
-- Assigns roles to users, optionally scoped to domains or specific concepts.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_user_roles (
    user_id          TEXT NOT NULL,
    role_id          TEXT NOT NULL,
    scope_type       TEXT DEFAULT 'global',   -- global, domain, concept
    scope_value      TEXT,                    -- domain name or concept_id if scoped
    granted_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    granted_by       TEXT,
    expires_at       TIMESTAMP,               -- NULL = no expiration

    PRIMARY KEY (user_id, role_id, scope_type, scope_value),
    FOREIGN KEY (role_id) REFERENCES ont_roles(role_id)
);

-- Index for permission lookups
CREATE INDEX IF NOT EXISTS idx_user_roles_user ON ont_user_roles(user_id);

-- -----------------------------------------------------------------------------
-- GOVERNANCE GATES
-- Defines conditions that must pass before certain actions (like activation).
-- Gates can require: audit coverage, drift thresholds, approvals, etc.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_governance_gates (
    gate_id          TEXT PRIMARY KEY,
    gate_name        TEXT NOT NULL,
    gate_type        TEXT NOT NULL,           -- 'audit_coverage', 'drift_threshold', 'approval_required', 'custom'
    applies_to       TEXT NOT NULL,           -- 'activation', 'deprecation', 'materialization', 'all'
    condition_json   TEXT NOT NULL,           -- JSON: gate-specific conditions
    severity         TEXT DEFAULT 'blocking', -- 'blocking', 'warning'
    enabled          BOOLEAN DEFAULT TRUE,
    scope_filter     TEXT,                    -- Optional: only apply to certain scopes
    domain_filter    TEXT,                    -- Optional: only apply to certain domains
    description      TEXT,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by       TEXT
);

-- Insert default gates
INSERT OR IGNORE INTO ont_governance_gates (gate_id, gate_name, gate_type, applies_to, condition_json, severity, description) VALUES
    ('gate_audit_coverage', 'Minimum Audit Coverage', 'audit_coverage', 'activation',
     '{"min_audits": 10, "min_agreement_rate": 0.9}', 'blocking',
     'Requires minimum audit coverage and agreement rate before activation'),
    ('gate_no_open_drift', 'No Open Drift Events', 'drift_threshold', 'activation',
     '{"max_open_drift_events": 0}', 'blocking',
     'Cannot activate if there are open drift events'),
    ('gate_approval_required', 'Approval Required', 'approval_required', 'activation',
     '{"min_approvals": 1, "approver_roles": ["approver", "admin"]}', 'blocking',
     'Requires at least one approval from authorized role');

-- -----------------------------------------------------------------------------
-- GATE CHECKS
-- Records each evaluation of a governance gate.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_gate_checks (
    check_id         TEXT PRIMARY KEY,
    gate_id          TEXT NOT NULL,
    concept_id       TEXT NOT NULL,
    scope            TEXT NOT NULL,
    version          INTEGER NOT NULL,
    action_type      TEXT NOT NULL,           -- What action triggered this check
    check_result     TEXT NOT NULL,           -- 'passed', 'failed', 'overridden'
    check_details    TEXT,                    -- JSON: evaluation details
    checked_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    checked_by       TEXT,
    override_reason  TEXT,                    -- If overridden, why

    FOREIGN KEY (gate_id) REFERENCES ont_governance_gates(gate_id),
    FOREIGN KEY (concept_id, scope, version)
        REFERENCES ont_concept_versions(concept_id, scope, version)
);

-- Index for gate check history
CREATE INDEX IF NOT EXISTS idx_gate_checks_concept
    ON ont_gate_checks(concept_id, scope, version);

-- -----------------------------------------------------------------------------
-- APPROVAL REQUESTS
-- Tracks approval workflows for concept versions.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_approval_requests (
    request_id       TEXT PRIMARY KEY,
    concept_id       TEXT NOT NULL,
    scope            TEXT NOT NULL,
    version          INTEGER NOT NULL,
    requested_action TEXT NOT NULL,           -- 'activate', 'deprecate', 'retire'
    status           TEXT DEFAULT 'pending',  -- 'pending', 'approved', 'rejected', 'expired'
    requested_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    requested_by     TEXT,
    decided_at       TIMESTAMP,
    decided_by       TEXT,
    decision_notes   TEXT,

    FOREIGN KEY (concept_id, scope, version)
        REFERENCES ont_concept_versions(concept_id, scope, version)
);

-- Index for pending approvals
CREATE INDEX IF NOT EXISTS idx_approval_requests_status
    ON ont_approval_requests(status, requested_at);

-- -----------------------------------------------------------------------------
-- VIEWS: RBAC convenience views
-- -----------------------------------------------------------------------------

-- Effective permissions for a user (considering all assigned roles)
CREATE VIEW IF NOT EXISTS ont_user_permissions AS
SELECT
    ur.user_id,
    ur.scope_type,
    ur.scope_value,
    r.role_id,
    r.role_name,
    r.permissions
FROM ont_user_roles ur
JOIN ont_roles r ON ur.role_id = r.role_id
WHERE (ur.expires_at IS NULL OR ur.expires_at > CURRENT_TIMESTAMP);

-- Pending approvals summary
CREATE VIEW IF NOT EXISTS ont_pending_approvals AS
SELECT
    ar.*,
    cv.sql_expr,
    cv.rationale,
    c.description AS concept_description,
    c.owner_domain
FROM ont_approval_requests ar
JOIN ont_concept_versions cv ON ar.concept_id = cv.concept_id
    AND ar.scope = cv.scope AND ar.version = cv.version
JOIN ont_concepts c ON ar.concept_id = c.concept_id
WHERE ar.status = 'pending';

-- =============================================================================
-- CONCEPT TEMPLATES
-- =============================================================================
-- Templates allow creating base concept definitions that can be inherited by
-- country/scope-specific variants. This supports patterns like:
--   - ILO "unemployed" template -> US, UK, Ireland variants
--   - Base "employed" definition -> sector-specific variants
-- =============================================================================

-- -----------------------------------------------------------------------------
-- TEMPLATES
-- A template is a base concept definition that other concepts can inherit from.
-- The sql_expr contains placeholders (e.g., {{age_threshold}}) that variants
-- can customize.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_templates (
    template_id      TEXT PRIMARY KEY,
    template_name    TEXT NOT NULL,
    object_type      TEXT NOT NULL,
    base_sql_expr    TEXT NOT NULL,           -- SQL with optional {{placeholders}}
    parameters       TEXT,                    -- JSON: parameter definitions with defaults
    description      TEXT,
    source_standard  TEXT,                    -- e.g., "ILO", "OECD", "internal"
    owner_domain     TEXT,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by       TEXT,

    FOREIGN KEY (object_type) REFERENCES ont_object_types(object_type)
);

-- -----------------------------------------------------------------------------
-- TEMPLATE INHERITANCE
-- Links concepts to their parent templates with parameter overrides.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_template_inheritance (
    concept_id       TEXT NOT NULL,
    template_id      TEXT NOT NULL,
    parameter_values TEXT,                    -- JSON: actual values for template parameters
    inheritance_type TEXT DEFAULT 'extends',  -- 'extends', 'implements', 'adapts'
    deviation_notes  TEXT,                    -- Why this variant differs from template
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    PRIMARY KEY (concept_id, template_id),
    FOREIGN KEY (concept_id) REFERENCES ont_concepts(concept_id),
    FOREIGN KEY (template_id) REFERENCES ont_templates(template_id)
);

-- -----------------------------------------------------------------------------
-- VIEWS: Template convenience views
-- -----------------------------------------------------------------------------

-- List all template variants
CREATE VIEW IF NOT EXISTS ont_template_variants AS
SELECT
    t.template_id,
    t.template_name,
    t.source_standard,
    ti.concept_id,
    c.description AS concept_description,
    c.owner_domain AS concept_owner,
    ti.inheritance_type,
    ti.parameter_values,
    ti.deviation_notes
FROM ont_templates t
JOIN ont_template_inheritance ti ON t.template_id = ti.template_id
JOIN ont_concepts c ON ti.concept_id = c.concept_id;

-- =============================================================================
-- ACTIONS & WRITEBACK (Foundry-inspired)
-- =============================================================================
-- Actions enable users to take governed actions based on concept evaluations.
-- This bridges analytics to operations - instead of just viewing data, users
-- can trigger workflows, escalations, and decisions with full audit trails.
-- =============================================================================

-- -----------------------------------------------------------------------------
-- ACTION TYPES
-- Defines types of actions users can take on objects.
-- Each action type specifies:
--   - What concept triggers it (optional)
--   - What parameters users must provide
--   - What table to write results to
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_action_types (
    action_type_id   TEXT PRIMARY KEY,
    action_name      TEXT NOT NULL,
    description      TEXT,
    object_type      TEXT NOT NULL,           -- Which object type this action applies to
    trigger_concept  TEXT,                    -- Optional: concept that enables this action
    trigger_scope    TEXT,                    -- Scope of trigger concept
    trigger_condition TEXT,                   -- SQL condition on concept_value (e.g., "concept_value = TRUE")
    parameters       TEXT,                    -- JSON: parameter definitions {name: {type, required, default, values}}
    writeback_table  TEXT,                    -- Table to write action results
    writeback_columns TEXT,                   -- JSON: column mappings
    require_note     BOOLEAN DEFAULT FALSE,   -- Whether a note is required
    require_approval BOOLEAN DEFAULT FALSE,   -- Whether approval is needed before execution
    allowed_roles    TEXT,                    -- JSON: array of roles that can execute
    enabled          BOOLEAN DEFAULT TRUE,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by       TEXT,

    FOREIGN KEY (object_type) REFERENCES ont_object_types(object_type),
    FOREIGN KEY (trigger_concept) REFERENCES ont_concepts(concept_id)
);

-- -----------------------------------------------------------------------------
-- ACTION LOG
-- Records every action execution with full audit trail.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_action_log (
    action_id        TEXT PRIMARY KEY,
    action_type_id   TEXT NOT NULL,
    object_key       TEXT NOT NULL,           -- Primary key of the object acted upon
    parameters       TEXT,                    -- JSON: actual parameter values
    concept_value    BOOLEAN,                 -- Concept value at time of action (if triggered)
    status           TEXT DEFAULT 'completed', -- 'pending_approval', 'completed', 'rejected', 'failed'
    executed_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    executed_by      TEXT NOT NULL,
    approved_at      TIMESTAMP,
    approved_by      TEXT,
    notes            TEXT,
    result           TEXT,                    -- JSON: outcome/result data
    error_message    TEXT,                    -- If failed, why

    FOREIGN KEY (action_type_id) REFERENCES ont_action_types(action_type_id)
);

-- Index for querying action history
CREATE INDEX IF NOT EXISTS idx_action_log_type ON ont_action_log(action_type_id, executed_at);
CREATE INDEX IF NOT EXISTS idx_action_log_object ON ont_action_log(object_key, executed_at);

-- =============================================================================
-- COMPOSITE SCORES
-- =============================================================================
-- Composite scores combine multiple concept evaluations into a single metric.
-- This enables "risk scores", "health scores", etc. that aggregate signals
-- from multiple boolean concepts into a weighted numeric score.
-- =============================================================================

-- -----------------------------------------------------------------------------
-- SCORES
-- Defines composite score calculations.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_scores (
    score_id         TEXT PRIMARY KEY,
    score_name       TEXT NOT NULL,
    description      TEXT,
    object_type      TEXT NOT NULL,           -- Which object type this score applies to
    aggregation      TEXT DEFAULT 'weighted_sum', -- 'weighted_sum', 'weighted_avg', 'max', 'min', 'any_true', 'all_true', 'count_true'
    score_range_min  REAL DEFAULT 0,          -- Minimum possible score
    score_range_max  REAL DEFAULT 100,        -- Maximum possible score
    thresholds       TEXT,                    -- JSON: {low: 30, medium: 60, high: 80} for tier assignment
    enabled          BOOLEAN DEFAULT TRUE,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by       TEXT,

    FOREIGN KEY (object_type) REFERENCES ont_object_types(object_type)
);

-- -----------------------------------------------------------------------------
-- SCORE COMPONENTS
-- Links concepts to scores with weights and transformations.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_score_components (
    score_id         TEXT NOT NULL,
    component_id     TEXT NOT NULL,           -- Unique within score
    concept_id       TEXT NOT NULL,
    scope            TEXT NOT NULL,
    version          INTEGER,                 -- NULL = use active version
    weight           REAL DEFAULT 1.0,        -- Weight in aggregation
    transform        TEXT,                    -- Optional SQL transform (e.g., "CASE WHEN concept_value THEN 1 ELSE 0 END")
    invert           BOOLEAN DEFAULT FALSE,   -- If TRUE, use NOT concept_value
    required         BOOLEAN DEFAULT TRUE,    -- If TRUE, NULL concept_value fails the score
    display_order    INTEGER DEFAULT 0,

    PRIMARY KEY (score_id, component_id),
    FOREIGN KEY (score_id) REFERENCES ont_scores(score_id),
    FOREIGN KEY (concept_id) REFERENCES ont_concepts(concept_id)
);

-- -----------------------------------------------------------------------------
-- SCORE OBSERVATIONS
-- Records point-in-time score calculations for trend analysis.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_score_observations (
    observation_id   TEXT PRIMARY KEY,
    score_id         TEXT NOT NULL,
    observed_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    total_objects    INTEGER NOT NULL,
    avg_score        REAL,
    min_score        REAL,
    max_score        REAL,
    std_dev          REAL,
    tier_counts      TEXT,                    -- JSON: {low: 100, medium: 200, high: 50}
    observer_id      TEXT,

    FOREIGN KEY (score_id) REFERENCES ont_scores(score_id)
);

-- Index for score trends
CREATE INDEX IF NOT EXISTS idx_score_observations_time
    ON ont_score_observations(score_id, observed_at);

-- =============================================================================
-- SCENARIO ANALYSIS
-- =============================================================================
-- Scenario analysis allows comparing concept definitions before deployment.
-- Records analysis runs for audit trail and reproducibility.
-- =============================================================================

-- -----------------------------------------------------------------------------
-- SCENARIOS
-- Records scenario analysis runs.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_scenarios (
    scenario_id      TEXT PRIMARY KEY,
    scenario_name    TEXT,
    concept_id       TEXT NOT NULL,
    scope            TEXT NOT NULL,
    current_version  INTEGER NOT NULL,
    proposed_sql     TEXT NOT NULL,           -- The proposed new SQL expression
    analysis_date    TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    analyzed_by      TEXT,

    -- Results summary
    current_matches  INTEGER,                 -- Objects matching current definition
    proposed_matches INTEGER,                 -- Objects matching proposed definition
    newly_included   INTEGER,                 -- Objects that would be added
    newly_excluded   INTEGER,                 -- Objects that would be removed
    unchanged        INTEGER,                 -- Objects with same result

    -- Detailed results stored as JSON
    results_json     TEXT,                    -- JSON: detailed comparison data

    -- Outcome
    status           TEXT DEFAULT 'analyzed', -- 'analyzed', 'approved', 'rejected', 'implemented'
    decision_notes   TEXT,
    decided_by       TEXT,
    decided_at       TIMESTAMP,

    FOREIGN KEY (concept_id) REFERENCES ont_concepts(concept_id)
);

-- Index for scenario history
CREATE INDEX IF NOT EXISTS idx_scenarios_concept
    ON ont_scenarios(concept_id, scope, analysis_date);

-- =============================================================================
-- ALERTS & THRESHOLDS
-- =============================================================================
-- Alert rules trigger notifications when concepts cross thresholds.
-- =============================================================================

-- -----------------------------------------------------------------------------
-- ALERT RULES
-- Defines conditions that trigger alerts.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_alert_rules (
    rule_id          TEXT PRIMARY KEY,
    rule_name        TEXT NOT NULL,
    description      TEXT,
    concept_id       TEXT NOT NULL,
    scope            TEXT NOT NULL,
    version          INTEGER,                 -- NULL = use active version
    condition_type   TEXT NOT NULL,           -- 'threshold', 'change', 'absence'
    condition_expr   TEXT NOT NULL,           -- SQL expression or threshold value
    severity         TEXT DEFAULT 'warning',  -- 'info', 'warning', 'critical'
    notify_roles     TEXT,                    -- JSON: roles to notify
    notify_users     TEXT,                    -- JSON: specific users to notify
    cooldown_minutes INTEGER DEFAULT 60,      -- Don't re-alert within this period
    enabled          BOOLEAN DEFAULT TRUE,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by       TEXT,

    FOREIGN KEY (concept_id) REFERENCES ont_concepts(concept_id)
);

-- -----------------------------------------------------------------------------
-- ALERTS
-- Records triggered alerts.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_alerts (
    alert_id         TEXT PRIMARY KEY,
    rule_id          TEXT NOT NULL,
    triggered_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    trigger_value    TEXT,                    -- The value that triggered the alert
    severity         TEXT NOT NULL,
    message          TEXT,                    -- Generated alert message
    status           TEXT DEFAULT 'open',     -- 'open', 'acknowledged', 'resolved', 'suppressed'
    acknowledged_at  TIMESTAMP,
    acknowledged_by  TEXT,
    resolution_notes TEXT,
    resolved_at      TIMESTAMP,
    resolved_by      TEXT,

    FOREIGN KEY (rule_id) REFERENCES ont_alert_rules(rule_id)
);

-- Index for active alerts
CREATE INDEX IF NOT EXISTS idx_alerts_status ON ont_alerts(status, triggered_at);
CREATE INDEX IF NOT EXISTS idx_alerts_rule ON ont_alerts(rule_id, triggered_at);

-- =============================================================================
-- SPATIAL / GEOSPATIAL SUPPORT
-- =============================================================================
-- Enables visualization of ontology data on maps (Cesium, Leaflet, etc.)
-- by storing geometry metadata for object types.
-- =============================================================================

-- -----------------------------------------------------------------------------
-- OBJECT TYPE GEOMETRY
-- Stores spatial column mappings for object types that have location data.
-- Supports point (lon/lat), polygon (WKT/GeoJSON), and 3D (with altitude).
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_object_geometry (
    object_type      TEXT PRIMARY KEY,
    geometry_type    TEXT NOT NULL DEFAULT 'point',  -- 'point', 'polygon', 'linestring', 'wkt', 'geojson'
    lon_column       TEXT,                    -- Column containing longitude (for point)
    lat_column       TEXT,                    -- Column containing latitude (for point)
    alt_column       TEXT,                    -- Column containing altitude/elevation (optional)
    geometry_column  TEXT,                    -- Column containing WKT or GeoJSON (for complex geometries)
    srid             INTEGER DEFAULT 4326,    -- Spatial Reference ID (4326 = WGS84)
    default_style    TEXT,                    -- JSON: default visualization style
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (object_type) REFERENCES ont_object_types(object_type)
);

-- -----------------------------------------------------------------------------
-- SPATIAL LAYERS
-- Named layers for organizing spatial visualizations.
-- Each layer can combine multiple concepts/scores with styling rules.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_spatial_layers (
    layer_id         TEXT PRIMARY KEY,
    layer_name       TEXT NOT NULL,
    description      TEXT,
    object_type      TEXT NOT NULL,
    concept_id       TEXT,                    -- Optional: filter by concept
    scope            TEXT,
    score_id         TEXT,                    -- Optional: color/size by score
    style_rules      TEXT,                    -- JSON: visualization rules
    visible          BOOLEAN DEFAULT TRUE,
    layer_order      INTEGER DEFAULT 0,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by       TEXT,

    FOREIGN KEY (object_type) REFERENCES ont_object_types(object_type),
    FOREIGN KEY (concept_id) REFERENCES ont_concepts(concept_id),
    FOREIGN KEY (score_id) REFERENCES ont_scores(score_id)
);

-- -----------------------------------------------------------------------------
-- SPATIAL REGIONS
-- Named geographic regions for filtering and aggregation.
-- Can be defined by bounding box, polygon, or reference to external data.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_spatial_regions (
    region_id        TEXT PRIMARY KEY,
    region_name      TEXT NOT NULL,
    region_type      TEXT NOT NULL DEFAULT 'bbox',  -- 'bbox', 'polygon', 'reference'
    bbox_west        REAL,                    -- Bounding box west longitude
    bbox_east        REAL,                    -- Bounding box east longitude
    bbox_south       REAL,                    -- Bounding box south latitude
    bbox_north       REAL,                    -- Bounding box north latitude
    geometry_wkt     TEXT,                    -- WKT for polygon regions
    reference_table  TEXT,                    -- External table for complex regions
    reference_column TEXT,                    -- Column in reference table
    reference_value  TEXT,                    -- Value to filter by
    description      TEXT,
    created_at       TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- -----------------------------------------------------------------------------
-- SPATIAL EXPORTS
-- Records of GeoJSON/CZML exports for audit trail.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ont_spatial_exports (
    export_id        TEXT PRIMARY KEY,
    export_type      TEXT NOT NULL,           -- 'geojson', 'czml', 'kml', 'csv'
    object_type      TEXT NOT NULL,
    concept_id       TEXT,
    scope            TEXT,
    score_id         TEXT,
    region_id        TEXT,                    -- Optional spatial filter
    record_count     INTEGER,
    file_path        TEXT,                    -- Where exported (if saved)
    exported_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    exported_by      TEXT,
    parameters       TEXT,                    -- JSON: export parameters

    FOREIGN KEY (object_type) REFERENCES ont_object_types(object_type)
);

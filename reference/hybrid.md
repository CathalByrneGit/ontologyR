# Hybrid API Layer

Provides ontologyIndex-style ergonomics (named list access, \$ operator)
on top of the table-based storage. This gives you the best of both
worlds:

- Database as source of truth (versioning, audit trails, governance)

- R-native feel with \$ accessors and cached lookups

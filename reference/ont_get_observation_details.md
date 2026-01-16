# Get Observation Details

Retrieves object-level details for an observation (if stored).

## Usage

``` r
ont_get_observation_details(observation_id, con = NULL)
```

## Arguments

- observation_id:

  Character. The observation ID.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble with object_key and concept_value columns.

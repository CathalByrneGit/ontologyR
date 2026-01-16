# Get Concept Inheritance

Returns the template(s) that a concept inherits from.

## Usage

``` r
ont_get_concept_inheritance(concept_id, con = NULL)
```

## Arguments

- concept_id:

  Character. The concept ID.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of templates this concept inherits from.

## Examples

``` r
if (FALSE) { # \dontrun{
ont_get_concept_inheritance("unemployed_ireland")
} # }
```

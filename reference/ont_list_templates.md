# List All Templates

Returns a tibble of all defined templates.

## Usage

``` r
ont_list_templates(object_type = NULL, source_standard = NULL, con = NULL)
```

## Arguments

- object_type:

  Character. Filter by object type (optional).

- source_standard:

  Character. Filter by source standard (optional).

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of templates.

## Examples

``` r
if (FALSE) { # \dontrun{
ont_list_templates()
ont_list_templates(source_standard = "ILO")
} # }
```

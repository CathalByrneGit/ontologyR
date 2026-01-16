# Get Template Details

Retrieves information about a specific template.

## Usage

``` r
ont_get_template(template_id, con = NULL)
```

## Arguments

- template_id:

  Character. The template ID.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A list with template details including parsed parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
template <- ont_get_template("ilo_unemployed")
template$parameters
} # }
```

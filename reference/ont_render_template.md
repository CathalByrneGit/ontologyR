# Render Template SQL

Renders a template's SQL expression with specific parameter values,
without creating a concept. Useful for previewing or ad-hoc evaluation.

## Usage

``` r
ont_render_template(template_id, parameter_values = list(), con = NULL)
```

## Arguments

- template_id:

  Character. The template ID.

- parameter_values:

  Named list. Parameter values (uses defaults for missing).

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Character. The rendered SQL expression.

## Examples

``` r
if (FALSE) { # \dontrun{
sql <- ont_render_template(
    "ilo_unemployed",
    list(min_age = 18, max_age = 65)
)
cat(sql)
} # }
```

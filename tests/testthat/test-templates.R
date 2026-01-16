# =============================================================================
# Tests for Concept Templates
# =============================================================================

test_that("ont_define_template creates a template", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    # Create object type
    ont_register_object("Person", "persons", "person_id")

    # Define template
    result <- ont_define_template(
        template_id = "ilo_unemployed",
        template_name = "ILO Unemployed Definition",
        object_type = "Person",
        base_sql_expr = "age >= {{min_age}} AND age <= {{max_age}} AND NOT employed",
        parameters = list(
            min_age = list(default = 15, type = "integer", description = "Minimum age"),
            max_age = list(default = 74, type = "integer", description = "Maximum age")
        ),
        source_standard = "ILO",
        description = "International Labour Organization unemployment definition"
    )

    expect_equal(result, "ilo_unemployed")

    # Verify it exists
    templates <- ont_list_templates()
    expect_equal(nrow(templates), 1)
    expect_equal(templates$template_id[1], "ilo_unemployed")
})

test_that("ont_define_template rejects duplicate template_id", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    ont_define_template(
        template_id = "test_template",
        template_name = "Test",
        object_type = "Person",
        base_sql_expr = "TRUE"
    )

    expect_error(
        ont_define_template(
            template_id = "test_template",
            template_name = "Test 2",
            object_type = "Person",
            base_sql_expr = "FALSE"
        ),
        "already exists"
    )
})

test_that("ont_get_template retrieves template with parsed parameters", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    ont_define_template(
        template_id = "test_template",
        template_name = "Test Template",
        object_type = "Person",
        base_sql_expr = "value >= {{threshold}}",
        parameters = list(threshold = 10),
        source_standard = "internal"
    )

    template <- ont_get_template("test_template")

    expect_equal(template$template_id, "test_template")
    expect_equal(template$template_name, "Test Template")
    expect_equal(template$source_standard, "internal")
    expect_equal(template$parameters$threshold, 10)
})

test_that("ont_get_template errors for non-existent template", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    expect_error(ont_get_template("nonexistent"), "not found")
})

test_that("ont_list_templates filters by object_type and source_standard", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")
    ont_register_object("Encounter", "encounters", "encounter_id")

    ont_define_template("t1", "Template 1", "Person", "TRUE", source_standard = "ILO")
    ont_define_template("t2", "Template 2", "Person", "TRUE", source_standard = "OECD")
    ont_define_template("t3", "Template 3", "Encounter", "TRUE", source_standard = "ILO")

    # All templates
    all_templates <- ont_list_templates()
    expect_equal(nrow(all_templates), 3)

    # Filter by object_type
    person_templates <- ont_list_templates(object_type = "Person")
    expect_equal(nrow(person_templates), 2)

    # Filter by source_standard
    ilo_templates <- ont_list_templates(source_standard = "ILO")
    expect_equal(nrow(ilo_templates), 2)

    # Combined filter
    person_ilo <- ont_list_templates(object_type = "Person", source_standard = "ILO")
    expect_equal(nrow(person_ilo), 1)
    expect_equal(person_ilo$template_id[1], "t1")
})

test_that("ont_inherit_concept creates concept from template", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    # Define template
    ont_define_template(
        template_id = "ilo_unemployed",
        template_name = "ILO Unemployed",
        object_type = "Person",
        base_sql_expr = "age >= {{min_age}} AND age <= {{max_age}}",
        parameters = list(
            min_age = list(default = 15, type = "integer"),
            max_age = list(default = 74, type = "integer")
        )
    )

    # Create variant with custom parameters
    ont_inherit_concept(
        concept_id = "unemployed_ireland",
        template_id = "ilo_unemployed",
        scope = "ireland",
        parameter_values = list(min_age = 16, max_age = 66),
        deviation_notes = "Ireland uses 16-66 age range"
    )

    # Verify concept was created
    concepts <- ont_list_concepts()
    expect_equal(nrow(concepts), 1)
    expect_equal(concepts$concept_id[1], "unemployed_ireland")

    # Verify version was created with substituted SQL
    version <- ont_get_version("unemployed_ireland", "ireland")
    expect_true(grepl("age >= 16", version$sql_expr))
    expect_true(grepl("age <= 66", version$sql_expr))
})

test_that("ont_inherit_concept uses defaults for missing parameters", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    ont_define_template(
        template_id = "test_template",
        template_name = "Test",
        object_type = "Person",
        base_sql_expr = "a >= {{x}} AND b <= {{y}}",
        parameters = list(
            x = list(default = 10),
            y = list(default = 20)
        )
    )

    # Only override x
    ont_inherit_concept(
        concept_id = "variant1",
        template_id = "test_template",
        scope = "test",
        parameter_values = list(x = 5)
    )

    version <- ont_get_version("variant1", "test")
    expect_true(grepl("a >= 5", version$sql_expr))
    expect_true(grepl("b <= 20", version$sql_expr))  # default
})

test_that("ont_get_template_variants returns all variants", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    ont_define_template(
        template_id = "base_template",
        template_name = "Base",
        object_type = "Person",
        base_sql_expr = "x >= {{threshold}}",
        parameters = list(threshold = 0)
    )

    # Create multiple variants
    ont_inherit_concept("variant_a", "base_template", "scope_a", list(threshold = 10))
    ont_inherit_concept("variant_b", "base_template", "scope_b", list(threshold = 20))
    ont_inherit_concept("variant_c", "base_template", "scope_c", list(threshold = 30))

    variants <- ont_get_template_variants("base_template")

    expect_equal(nrow(variants), 3)
    expect_setequal(variants$concept_id, c("variant_a", "variant_b", "variant_c"))
})

test_that("ont_get_concept_inheritance returns template info", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    ont_define_template(
        template_id = "ilo_unemployed",
        template_name = "ILO Unemployed",
        object_type = "Person",
        base_sql_expr = "TRUE",
        source_standard = "ILO"
    )

    ont_inherit_concept(
        concept_id = "my_variant",
        template_id = "ilo_unemployed",
        scope = "test",
        deviation_notes = "Custom variant"
    )

    inheritance <- ont_get_concept_inheritance("my_variant")

    expect_equal(nrow(inheritance), 1)
    expect_equal(inheritance$template_id[1], "ilo_unemployed")
    expect_equal(inheritance$source_standard[1], "ILO")
    expect_equal(inheritance$deviation_notes[1], "Custom variant")
})

test_that("ont_compare_template_variants shows parameter differences", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    ont_define_template(
        template_id = "age_template",
        template_name = "Age Template",
        object_type = "Person",
        base_sql_expr = "age >= {{min}} AND age <= {{max}}",
        parameters = list(
            min = list(default = 15),
            max = list(default = 74)
        )
    )

    ont_inherit_concept("us_variant", "age_template", "us", list(min = 16, max = 65))
    ont_inherit_concept("uk_variant", "age_template", "uk", list(min = 16))  # uses default max

    comparison <- ont_compare_template_variants("age_template")

    expect_equal(nrow(comparison), 2)
    expect_true("param_min" %in% names(comparison))
    expect_true("param_max" %in% names(comparison))

    us_row <- comparison[comparison$concept_id == "us_variant", ]
    expect_equal(us_row$param_min, "16")
    expect_equal(us_row$param_max, "65")

    uk_row <- comparison[comparison$concept_id == "uk_variant", ]
    expect_equal(uk_row$param_min, "16")
    expect_true(grepl("74", uk_row$param_max))  # default
})

test_that("ont_render_template produces correct SQL", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    ont_define_template(
        template_id = "test_template",
        template_name = "Test",
        object_type = "Person",
        base_sql_expr = "  age  >=  {{min_age}}   AND   age  <=  {{max_age}}  ",
        parameters = list(
            min_age = list(default = 15),
            max_age = list(default = 74)
        )
    )

    # Use defaults
    sql1 <- ont_render_template("test_template")
    expect_equal(sql1, "age >= 15 AND age <= 74")

    # Override parameters
    sql2 <- ont_render_template("test_template", list(min_age = 18, max_age = 65))
    expect_equal(sql2, "age >= 18 AND age <= 65")

    # Partial override
    sql3 <- ont_render_template("test_template", list(min_age = 16))
    expect_equal(sql3, "age >= 16 AND age <= 74")
})

test_that("templates work with simple parameter values (not nested)", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    # Simple parameters (just values, not nested lists)
    ont_define_template(
        template_id = "simple_template",
        template_name = "Simple",
        object_type = "Person",
        base_sql_expr = "x >= {{threshold}}",
        parameters = list(threshold = 100)  # simple value, not nested
    )

    sql <- ont_render_template("simple_template")
    expect_equal(sql, "x >= 100")

    sql2 <- ont_render_template("simple_template", list(threshold = 50))
    expect_equal(sql2, "x >= 50")
})

test_that("inheritance_type is recorded correctly", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Person", "persons", "person_id")

    ont_define_template("base", "Base", "Person", "TRUE")

    ont_inherit_concept("c1", "base", "s1", inheritance_type = "extends")
    ont_inherit_concept("c2", "base", "s2", inheritance_type = "implements")
    ont_inherit_concept("c3", "base", "s3", inheritance_type = "adapts")

    variants <- ont_get_template_variants("base")

    expect_equal(variants$inheritance_type[variants$concept_id == "c1"], "extends")
    expect_equal(variants$inheritance_type[variants$concept_id == "c2"], "implements")
    expect_equal(variants$inheritance_type[variants$concept_id == "c3"], "adapts")
})

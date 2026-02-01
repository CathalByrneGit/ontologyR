# =============================================================================
# Tests for Spatial/Geospatial Functions
# =============================================================================

test_that("ont_register_geometry registers point geometry", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Asset", "assets", "asset_id")

    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE assets (
            asset_id TEXT PRIMARY KEY,
            name TEXT,
            longitude REAL,
            latitude REAL,
            elevation REAL
        )
    ")

    # Register point geometry
    ont_register_geometry("Asset", "point",
        lon_column = "longitude",
        lat_column = "latitude",
        alt_column = "elevation"
    )

    # Verify registration
    geom <- ont_get_geometry("Asset")
    expect_equal(geom$geometry_type, "point")
    expect_equal(geom$lon_column, "longitude")
    expect_equal(geom$lat_column, "latitude")
    expect_equal(geom$alt_column, "elevation")
    expect_equal(geom$srid, 4326)
})

test_that("ont_register_geometry validates required columns", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Location", "locations", "loc_id")
    DBI::dbExecute(ont_get_connection(), "CREATE TABLE locations (loc_id TEXT PRIMARY KEY)")

    # Point geometry requires lon/lat
    expect_error(
        ont_register_geometry("Location", "point"),
        "lon_column and lat_column"
    )

    # WKT geometry requires geometry_column
    expect_error(
        ont_register_geometry("Location", "wkt"),
        "geometry_column"
    )
})

test_that("ont_export_geojson creates valid GeoJSON", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Sensor", "sensors", "sensor_id")

    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE sensors (
            sensor_id TEXT PRIMARY KEY,
            name TEXT,
            lon REAL,
            lat REAL,
            reading REAL
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO sensors VALUES
        ('S1', 'Sensor 1', -122.4, 37.8, 25.5),
        ('S2', 'Sensor 2', -122.5, 37.7, 30.0),
        ('S3', 'Sensor 3', -122.3, 37.9, 22.0)
    ")

    # Register geometry
    ont_register_geometry("Sensor", "point",
        lon_column = "lon",
        lat_column = "lat"
    )

    # Export GeoJSON
    geojson <- ont_export_geojson("Sensor", properties = c("name", "reading"))

    # Validate structure
    expect_equal(geojson$type, "FeatureCollection")
    expect_equal(length(geojson$features), 3)

    # Validate first feature
    f1 <- geojson$features[[1]]
    expect_equal(f1$type, "Feature")
    expect_equal(f1$geometry$type, "Point")
    expect_equal(length(f1$geometry$coordinates), 2)  # lon, lat
    expect_true("object_key" %in% names(f1$properties))
})

test_that("ont_export_geojson includes concept values", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Device", "devices", "device_id")

    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE devices (
            device_id TEXT PRIMARY KEY,
            lon REAL,
            lat REAL,
            status TEXT
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO devices VALUES
        ('D1', -73.9, 40.7, 'active'),
        ('D2', -73.8, 40.6, 'inactive'),
        ('D3', -73.7, 40.8, 'active')
    ")

    ont_register_geometry("Device", "point", lon_column = "lon", lat_column = "lat")

    # Define concept
    ont_define_concept("is_active", "Device")
    ont_add_version("is_active", "ops", 1, "status = 'active'", "active")

    # Export with concept coloring
    geojson <- ont_export_geojson("Device",
        concept_id = "is_active",
        scope = "ops",
        style = list(color_true = "#00FF00", color_false = "#FF0000")
    )

    # Check that concept_value is included
    props <- geojson$features[[1]]$properties
    expect_true("concept_value" %in% names(props))

    # Check that marker-color is set
    expect_true("marker-color" %in% names(props))
})

test_that("ont_export_geojson can save to file", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Point", "points", "point_id")
    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE points (point_id TEXT, lon REAL, lat REAL)
    ")
    DBI::dbExecute(ont_get_connection(), "INSERT INTO points VALUES ('P1', 0, 0)")

    ont_register_geometry("Point", "point", lon_column = "lon", lat_column = "lat")

    # Export to temp file
    tmp_file <- tempfile(fileext = ".geojson")
    result <- ont_export_geojson("Point", file = tmp_file)

    expect_true(file.exists(tmp_file))
    expect_equal(result, tmp_file)

    # Verify file contents
    content <- jsonlite::fromJSON(tmp_file)
    expect_equal(content$type, "FeatureCollection")

    # Cleanup
    unlink(tmp_file)
})

test_that("ont_export_czml creates valid CZML", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Station", "stations", "station_id")

    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE stations (
            station_id TEXT PRIMARY KEY,
            lon REAL,
            lat REAL,
            alt REAL
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO stations VALUES
        ('ST1', -118.2, 34.0, 100),
        ('ST2', -118.3, 34.1, 150)
    ")

    ont_register_geometry("Station", "point",
        lon_column = "lon",
        lat_column = "lat",
        alt_column = "alt"
    )

    # Export CZML
    czml <- ont_export_czml("Station")

    # CZML should start with document packet
    expect_equal(czml[[1]]$id, "document")
    expect_equal(czml[[1]]$version, "1.0")

    # Should have packets for each station
    expect_equal(length(czml), 3)  # 1 document + 2 stations

    # Verify station packet structure
    station_packet <- czml[[2]]
    expect_true("position" %in% names(station_packet))
    expect_true("point" %in% names(station_packet))
})

test_that("ont_define_layer creates spatial layers", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Building", "buildings", "building_id")
    DBI::dbExecute(ont_get_connection(), "CREATE TABLE buildings (building_id TEXT)")

    ont_define_layer(
        layer_id = "buildings_layer",
        layer_name = "Buildings",
        object_type = "Building",
        style_rules = list(color = "#3388ff", fillOpacity = 0.5)
    )

    # Verify layer exists
    layers <- ont_list_layers()
    expect_equal(nrow(layers), 1)
    expect_equal(layers$layer_id[1], "buildings_layer")
    expect_equal(layers$layer_name[1], "Buildings")
})

test_that("ont_define_region creates bbox regions", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    # Define a bounding box region (California approximate)
    ont_define_region(
        region_id = "california",
        region_name = "California",
        bbox = c(-124.5, 32.5, -114.0, 42.0),
        description = "State of California"
    )

    # Verify region exists
    regions <- ont_list_regions()
    expect_equal(nrow(regions), 1)
    expect_equal(regions$region_id[1], "california")
    expect_equal(regions$region_type[1], "bbox")
    expect_equal(regions$bbox_west[1], -124.5)
    expect_equal(regions$bbox_north[1], 42.0)
})

test_that("ont_filter_by_region filters objects", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("City", "cities", "city_id")

    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE cities (
            city_id TEXT PRIMARY KEY,
            name TEXT,
            lon REAL,
            lat REAL
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO cities VALUES
        ('LA', 'Los Angeles', -118.2, 34.0),
        ('SF', 'San Francisco', -122.4, 37.8),
        ('NYC', 'New York', -74.0, 40.7),
        ('SEA', 'Seattle', -122.3, 47.6)
    ")

    ont_register_geometry("City", "point", lon_column = "lon", lat_column = "lat")

    # Define California region
    ont_define_region("california", "California",
        bbox = c(-124.5, 32.5, -114.0, 42.0))

    # Filter cities in California
    ca_cities <- ont_filter_by_region("City", "california")

    # Should include LA and SF, not NYC or Seattle
    expect_equal(nrow(ca_cities), 2)
    expect_true(all(ca_cities$city_id %in% c("LA", "SF")))
})

test_that("ont_list_spatial_exports records exports", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Marker", "markers", "marker_id")
    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE markers (marker_id TEXT, lon REAL, lat REAL)
    ")
    DBI::dbExecute(ont_get_connection(), "INSERT INTO markers VALUES ('M1', 0, 0)")

    ont_register_geometry("Marker", "point", lon_column = "lon", lat_column = "lat")

    # Do an export
    ont_export_geojson("Marker")

    # Check export history
    exports <- ont_list_spatial_exports()
    expect_equal(nrow(exports), 1)
    expect_equal(exports$export_type[1], "geojson")
    expect_equal(exports$object_type[1], "Marker")
    expect_equal(exports$record_count[1], 1)
})

test_that("score_to_color interpolates colors correctly", {
    # Test internal color interpolation function
    style <- list(
        color_scale = c("#00FF00", "#FFFF00", "#FF0000"),
        score_min = 0,
        score_max = 100
    )

    # Low score should be green
    low_color <- ontologyR:::score_to_color(10, style)
    expect_true(grepl("^#", low_color))

    # High score should be red
    high_color <- ontologyR:::score_to_color(90, style)
    expect_true(grepl("^#", high_color))

    # NA should be gray
    na_color <- ontologyR:::score_to_color(NA, style)
    expect_equal(na_color, "#808080")
})

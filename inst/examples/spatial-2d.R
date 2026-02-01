# =============================================================================
# Example: 2D Spatial Viewer with Leaflet
# =============================================================================
# This example demonstrates how to use the 2D spatial viewer to visualize
# objects with geographic coordinates on an interactive Leaflet map.
#
# Features demonstrated:
#   - Registering object types with geometry
#   - Creating sample data with coordinates
#   - Defining concepts for categorization
#   - Visualizing on a 2D map with color-coding
# =============================================================================

library(ontologyR)

# -----------------------------------------------------------------------------
# Step 1: Initialize an in-memory database
# -----------------------------------------------------------------------------
cli::cli_h1("Setting up the database")

ont_connect(":memory:")

# -----------------------------------------------------------------------------
# Step 2: Register an object type (e.g., weather stations)
# -----------------------------------------------------------------------------
cli::cli_h2("Registering object type: WeatherStation")

ont_register_object(
    object_type = "WeatherStation",
    table_name = "weather_stations",
    pk_column = "station_id"
)

# -----------------------------------------------------------------------------
# Step 3: Create sample data with geographic coordinates
# -----------------------------------------------------------------------------
cli::cli_h2("Creating sample weather station data")

con <- ont_get_connection()

# Create the weather stations table with coordinates and measurements
DBI::dbExecute(con, "
    CREATE TABLE weather_stations (
        station_id TEXT PRIMARY KEY,
        station_name TEXT,
        latitude DOUBLE,
        longitude DOUBLE,
        elevation_m INTEGER,
        temperature_c DOUBLE,
        humidity_pct DOUBLE,
        wind_speed_kmh DOUBLE,
        status TEXT
    )
")

# Insert sample stations across different locations
stations <- data.frame(
    station_id = paste0("WS", sprintf("%03d", 1:20)),
    station_name = c(
        "Dublin Airport", "Cork Airport", "Shannon Airport", "Galway Bay",
        "Belfast City", "Limerick Central", "Waterford Port", "Sligo Town",
        "Killarney National Park", "Donegal Highlands", "Wexford Harbour",
        "Athlone Midlands", "Dundalk Bay", "Tralee Town", "Letterkenny",
        "Carlow Town", "Kilkenny Castle", "Tullamore Central", "Longford Town",
        "Mullingar Lake"
    ),
    latitude = c(
        53.4264, 51.8413, 52.7019, 53.2707, 54.6180, 52.6638, 52.2593, 54.2766,
        52.0599, 54.6538, 52.3369, 53.4239, 54.0167, 52.2704, 54.9558,
        52.8408, 52.6541, 53.2743, 53.7276, 53.5263
    ),
    longitude = c(
        -6.2499, -8.4911, -8.9246, -9.0568, -5.8722, -8.6267, -7.1101, -8.4761,
        -9.5047, -8.1096, -6.4633, -7.9407, -6.4167, -9.7026, -7.7342,
        -6.9261, -7.2448, -7.4934, -7.7932, -7.3378
    ),
    elevation_m = c(
        68, 153, 14, 5, 4, 15, 8, 20, 65, 180, 3, 40, 5, 12, 35,
        55, 65, 75, 45, 100
    ),
    temperature_c = c(
        12.5, 14.2, 13.8, 13.0, 11.5, 13.5, 14.0, 12.0, 15.0, 10.5, 13.5,
        12.8, 11.8, 14.5, 10.0, 12.5, 13.0, 12.0, 11.5, 11.0
    ),
    humidity_pct = c(
        78, 82, 85, 88, 75, 80, 83, 87, 72, 90, 84, 79, 81, 76, 92,
        77, 78, 80, 83, 86
    ),
    wind_speed_kmh = c(
        25, 18, 22, 35, 20, 15, 28, 32, 12, 40, 30, 10, 22, 20, 45,
        8, 12, 14, 16, 18
    ),
    status = c(
        "active", "active", "active", "active", "active",
        "active", "maintenance", "active", "active", "active",
        "active", "active", "offline", "active", "active",
        "active", "active", "active", "maintenance", "active"
    ),
    stringsAsFactors = FALSE
)

DBI::dbWriteTable(con, "weather_stations", stations, append = TRUE)

cli::cli_alert_success("Created {nrow(stations)} weather stations")

# -----------------------------------------------------------------------------
# Step 4: Register geometry for the object type
# -----------------------------------------------------------------------------
cli::cli_h2("Registering point geometry")

ont_register_geometry(
    object_type = "WeatherStation",
    geometry_type = "point",
    lon_column = "longitude",
    lat_column = "latitude"
)

cli::cli_alert_success("Geometry registered for WeatherStation")

# -----------------------------------------------------------------------------
# Step 5: Create concepts for categorizing stations
# -----------------------------------------------------------------------------
cli::cli_h2("Creating concepts")

# Concept: High wind stations
ont_define_concept(
    concept_id = "high_wind",
    description = "Weather stations with wind speed above 25 km/h",
    object_type = "WeatherStation"
)

ont_add_version(
    concept_id = "high_wind",
    scope = "operational",
    version = 1,
    sql_expr = "wind_speed_kmh > 25",
    status = "active"
)

# Concept: Active stations
ont_define_concept(
    concept_id = "active_station",
    description = "Weather stations currently operational",
    object_type = "WeatherStation"
)

ont_add_version(
    concept_id = "active_station",
    scope = "operational",
    version = 1,
    sql_expr = "status = 'active'",
    status = "active"
)

# Concept: Coastal stations (low elevation, high humidity)
ont_define_concept(
    concept_id = "coastal_station",
    description = "Stations likely near the coast (low elevation, high humidity)",
    object_type = "WeatherStation"
)

ont_add_version(
    concept_id = "coastal_station",
    scope = "operational",
    version = 1,
    sql_expr = "elevation_m < 20 AND humidity_pct > 80",
    status = "active"
)

cli::cli_alert_success("Created 3 concepts: high_wind, active_station, coastal_station")

# -----------------------------------------------------------------------------
# Step 5b: Create a composite score
# -----------------------------------------------------------------------------
cli::cli_h2("Creating composite score")

# Weather severity score combines all three concepts
ont_define_score(
    score_id = "weather_severity",
    score_name = "Weather Severity",
    object_type = "WeatherStation",
    description = "Combined severity score based on wind, status, and coastal exposure",
    components = list(
        list(concept_id = "high_wind", scope = "operational", weight = 0.4),
        list(concept_id = "active_station", scope = "operational", weight = 0.3),
        list(concept_id = "coastal_station", scope = "operational", weight = 0.3)
    ),
    aggregation = "weighted_sum",
    score_range_min = 0,
    score_range_max = 100,
    thresholds = list(low = 30, medium = 60, high = 80)
)

cli::cli_alert_success("Created composite score: weather_severity (0-100)")

# -----------------------------------------------------------------------------
# Step 6: Evaluate and display results
# -----------------------------------------------------------------------------
cli::cli_h2("Evaluating concepts")

high_wind_stations <- ont_evaluate("high_wind", "operational")
cli::cli_alert_info("High wind stations: {sum(high_wind_stations$concept_value)} of {nrow(high_wind_stations)}")

active_stations <- ont_evaluate("active_station", "operational")
cli::cli_alert_info("Active stations: {sum(active_stations$concept_value)} of {nrow(active_stations)}")

coastal_stations <- ont_evaluate("coastal_station", "operational")
cli::cli_alert_info("Coastal stations: {sum(coastal_stations$concept_value)} of {nrow(coastal_stations)}")

# -----------------------------------------------------------------------------
# Step 7: Launch the 2D Spatial Viewer
# -----------------------------------------------------------------------------
cli::cli_h1("Launching 2D Spatial Viewer")

cli::cli_alert_info("The viewer will open in your browser.")
cli::cli_alert_info("Try the following in the app:")
cli::cli_ul(c(
    "Select 'WeatherStation' as the object type",
    "Set 'Color By' to 'Concept' and pick 'high_wind' to see red/green markers",
    "Set 'Color By' to 'Score' and pick 'Weather Severity' for gradient colors",
    "Click on markers to see station details",
    "Use the layer control to switch base maps"
))

# Run the viewer (uses the in-memory connection)
ont_run_spatial_viewer_2d()

# Cleanup happens automatically when the app closes

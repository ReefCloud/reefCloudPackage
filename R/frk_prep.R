#' Prepare objects for the FRK model
#'
#' @title frk_prep
#' @param data.grp.tier Data frame with observations for model fitting, including columns: LONGITUDE, LATITUDE, fYEAR, TOTAL, etc.
#' @param HexPred_reefid2 Spatial polygons with covariates and reef IDs
#' @param i Current iteration index (for progress tracking)
#' @param N Total number of iterations (for progress tracking)
#' @param basis_resolution Number of resolution levels for basis functions (default: 3L). 3 is required by the science (per Julie's design); 2 may be used as a temporary speed-up for very large grids at the cost of underestimating fine-scale uncertainty.
#' @return A list with prepared objects for FRK: ST_BAUs, STObj, basis functions
#' @examples
#' \dontrun{
#' # Example usage:
#' obj <- frk_prep(data.grp.tier = my_data, HexPred_reefid2 = my_spatial_covariates,
#'                 i = 1, N = 10, basis_resolution = 3L)
#' plot(obj$basis)
#' }
#' @author Julie Vercelloni
#' @export
frk_prep <- function(data.grp.tier, HexPred_reefid2, i, N, basis_resolution = 3L) {
  # REMOVED outer status_try_catch() wrapper - it was swallowing errors and returning error objects
  # instead of letting errors propagate properly. Individual operations inside still use
  # status_try_catch() for status tracking, but function-level errors now propagate naturally.
      # Helper function to save and upload debug CSV
      save_and_upload_debug <- function(data, filename_base, stage_num, focal_tier = "unknown") {
        debug_dir <- paste0(DATA_PATH, "debug/frk_prep/")
        dir.create(debug_dir, showWarnings = FALSE, recursive = TRUE)
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

        # Create explanatory filename
        full_filename <- paste0(
          "debug_frk_prep_",
          sprintf("%02d", stage_num), "_",
          filename_base, "_",
          focal_tier, "_", TIER, "_",
          timestamp,
          ".csv"
        )

        local_path <- file.path(debug_dir, full_filename)

        # Save CSV locally
        write.csv(data, local_path, row.names = FALSE)

        # Upload to S3 with compression
        s3_key <- paste0("frk_prep/", full_filename)
        upload_debug_to_s3(local_path, s3_key, compress = TRUE)
      }

      # DEBUG: Debug dumps disabled to reduce S3 storage usage
      # tryCatch({
      #   focal_tier_name <- paste0('Tier', as.numeric(BY_TIER) - 1)
      #
      #   # Save input data.grp.tier (observation data)
      #   save_and_upload_debug(
      #     data.grp.tier,
      #     "input_data_grp_tier",
      #     1,
      #     focal_tier_name
      #   )
      #
      #   # Save input HexPred_reefid2 (spatial covariates)
      #   # Convert sf to data.frame for CSV export
      #   if(inherits(HexPred_reefid2, "sf")) {
      #     hexpred_df <- sf::st_drop_geometry(HexPred_reefid2)
      #   } else {
      #     hexpred_df <- as.data.frame(HexPred_reefid2)
      #   }
      #   save_and_upload_debug(
      #     hexpred_df,
      #     "input_HexPred_reefid2",
      #     2,
      #     focal_tier_name
      #   )
      #
      #   status::status_try_catch(
      #     message(paste0("DEBUG: Saved and uploaded frk_prep input datasets for TIER ", TIER)),
      #     stage_ = 4,
      #     name_ = "Debug input dump",
      #     item_ = "debug_input"
      #   )
      # }, error = function(e) {
      #   status::status_try_catch(
      #     warning(paste0("Failed to dump input datasets: ", e$message)),
      #     stage_ = 4,
      #     name_ = "Debug input dump failed",
      #     item_ = "debug_input_error"
      #   )
      # })

      # Silent success marker (debug disabled)
      status::status_try_catch(
        invisible(NULL),
        stage_ = 4,
        name_ = "Debug input dump",
        item_ = "debug_input"
      )

      # Check temporal coverage BEFORE creating spacetime objects
      unique_years <- unique(data.grp.tier$fYEAR)
      n_years <- length(unique_years)

      if (n_years < 2) {
        msg <- paste0(
          "Insufficient temporal coverage for tier ", TIER, ": ",
          n_years, " year(s) of data. ",
          "FRK requires at least 2 time points. Skipping tier."
        )
        cat(paste0("  ERROR: ", msg, "\n"))
        status::status_try_catch(
          warning(msg),
          stage_ = 4,
          name_ = "Insufficient temporal data",
          item_ = "temporal_check"
        )
        return(NULL)
      }

      # Convert fYEAR to Date (start of year)
      # DEFENSIVE: Ensure Year column doesn't already exist as matrix
      if ("Year" %in% names(data.grp.tier)) {
        if (is.matrix(data.grp.tier$Year)) {
          cat("  WARNING: Year column is matrix, converting to vector\n")
          data.grp.tier$Year <- data.grp.tier$Year[, 1]  # Take first column
        }
        cat("  WARNING: Year column already exists, overwriting\n")
        data.grp.tier$Year <- NULL  # Remove to prevent conflicts
      }

      # Create Year column as Date (matches Julie's frk_prep.R line 19)
      # CRITICAL: Must be as.Date(), NOT as.POSIXct(). The time class affects how
      # spacetime::stConstruct() builds temporal intervals, which in turn affects
      # how FRK::SRE()/over() matches observations to BAU time slices.
      # Using POSIXct causes observations to map to wrong spatio-temporal BAUs,
      # spreading 374 obs across 292 cells instead of the correct 51 cells.
      data.grp.tier$Year <- as.Date(paste0(as.character(data.grp.tier$fYEAR), "-01-01"))
      data.grp.tier$k_Z <- data.grp.tier$TOTAL  # number of trials

      # VALIDATION: Ensure Year is atomic (not list/matrix/data.frame)
      # Note: POSIXct/Date objects fail is.vector() but are valid atomic vectors
      if (is.matrix(data.grp.tier$Year) || is.data.frame(data.grp.tier$Year) ||
          is.list(data.grp.tier$Year) || length(dim(data.grp.tier$Year)) > 1) {
        msg <- paste0(
          "Year column has invalid structure for tier ", TIER,
          " (class=", paste(class(data.grp.tier$Year), collapse=", "),
          ", dimensions=", paste(dim(data.grp.tier$Year), collapse="x"), "). ",
          "spacetime::stConstruct requires atomic time column (POSIXct or Date). ",
          "This usually indicates duplicate column names or upstream data corruption."
        )
        cat(paste0("  ERROR: ", msg, "\n"))
        status::status_try_catch(
          stop(msg),
          stage_ = 4,
          name_ = "Year column validation failed",
          item_ = "year_vector_check"
        )
        return(NULL)
      }

      cat(paste0("  Year column validated: ", class(data.grp.tier$Year),
                 ", length=", length(data.grp.tier$Year), "\n"))

      lon_idx <- which(names(data.grp.tier) == "LONGITUDE")
      lat_idx <- which(names(data.grp.tier) == "LATITUDE")

      # DIAGNOSTIC: Pre-stConstruct validation and data dump
      cat("  PRE-STCONSTRUCT DIAGNOSTIC:\n")
      cat(paste0("    Rows: ", nrow(data.grp.tier), "\n"))
      cat(paste0("    Columns: ", ncol(data.grp.tier), "\n"))
      cat(paste0("    Column names: ", paste(names(data.grp.tier), collapse=", "), "\n"))
      cat(paste0("    lon_idx: ", lon_idx, ", lat_idx: ", lat_idx, "\n"))

      # Check for duplicates in Year
      dup_years <- duplicated(data.grp.tier$Year)
      n_dup_years <- sum(dup_years)
      cat(paste0("    Duplicate Year values: ", n_dup_years, "\n"))
      if (n_dup_years > 0) {
        cat(paste0("    WARNING: Found ", n_dup_years, " duplicate timestamps\n"))
      }

      # Check for NA values
      na_lon <- sum(is.na(data.grp.tier$LONGITUDE))
      na_lat <- sum(is.na(data.grp.tier$LATITUDE))
      na_year <- sum(is.na(data.grp.tier$Year))
      cat(paste0("    NA values - LONGITUDE: ", na_lon, ", LATITUDE: ", na_lat, ", Year: ", na_year, "\n"))

      # Check coordinate ranges
      if (na_lon == 0 && na_lat == 0) {
        cat(paste0("    LONGITUDE range: [", min(data.grp.tier$LONGITUDE), ", ", max(data.grp.tier$LONGITUDE), "]\n"))
        cat(paste0("    LATITUDE range: [", min(data.grp.tier$LATITUDE), ", ", max(data.grp.tier$LATITUDE), "]\n"))
      }

      # Check Year range
      if (na_year == 0) {
        cat(paste0("    Year range: [", min(data.grp.tier$Year), ", ", max(data.grp.tier$Year), "]\n"))
        cat(paste0("    Unique years: ", length(unique(data.grp.tier$Year)), "\n"))
      }

      # Save data to S3 for analysis
      tryCatch({
        debug_dir <- paste0(DATA_PATH, "/debug/stconstruct/")
        dir.create(debug_dir, showWarnings = FALSE, recursive = TRUE)
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

        focal_tier_name <- paste0("Tier", as.numeric(BY_TIER) - 1)
        filename <- paste0("stconstruct_input_", focal_tier_name, "_", TIER, "_", timestamp, ".csv")
        local_path <- file.path(debug_dir, filename)

        # Save full dataset
        write.csv(data.grp.tier, local_path, row.names = FALSE)

        # Compress and upload to S3
        local_path_zip <- paste0(tools::file_path_sans_ext(local_path), ".zip")
        zip(local_path_zip, local_path, flags = "-q9j")
        file.remove(local_path)

        # Construct S3 path using ORIGINAL_DATA_PATH
        # ORIGINAL_DATA_PATH format: s3://bucket/path/to/domain
        # Target: s3://bucket/path/to/domain/debug/stconstruct/filename.zip
        s3_debug_path <- paste0(Sys.getenv("ORIGINAL_DATA_PATH"), "/debug/stconstruct/", basename(local_path_zip))
        upload_cmd <- paste0("aws s3 cp ", local_path_zip, " ", s3_debug_path)
        system(upload_cmd)

        cat(paste0("    ✓ Saved input data to S3: ", s3_debug_path, "\n"))
      }, error = function(e) {
        cat(paste0("    WARNING: Failed to save stConstruct input data: ", e$message, "\n"))
      })

      # Construct spacetime object with error details
      STObj <- tryCatch({
        spacetime::stConstruct(x = data.grp.tier,
                               space = c(lon_idx, lat_idx),
                               time = "Year",
                               interval = TRUE)
      }, error = function(e) {
        msg <- paste0("spacetime::stConstruct() failed for tier ", TIER, ": ", e$message)
        cat(paste0("  ERROR: ", msg, "\n"))
        cat(paste0("  ERROR DETAILS: class=", paste(class(e), collapse=", "), "\n"))
        if (!is.null(e$call)) {
          cat(paste0("  ERROR CALL: ", deparse(e$call), "\n"))
        }
        stop(msg)
      })

      # Convert HexPred_reefid2 to sp object for BAU construction with error details
      HexPred_sp <- tryCatch({
        sf::as_Spatial(HexPred_reefid2)
      }, error = function(e) {
        msg <- paste0("sf::as_Spatial() failed for tier ", TIER, ": ", e$message)
        cat(paste0("  ERROR: ", msg, "\n"))
        stop(msg)
      })

      # DEFENSIVE VALIDATION: Check for duplicate (Tier5, fYEAR) that would cause dimension mismatch
      dup_check <- tryCatch({
        as.data.frame(HexPred_sp@data) %>%
          dplyr::group_by(Tier5, fYEAR) %>%
          dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(n > 1)
      }, error = function(e) {
        msg <- paste0("Duplicate check failed for tier ", TIER, ": ", e$message)
        cat(paste0("  ERROR: ", msg, "\n"))
        stop(msg)
      })

      if (nrow(dup_check) > 0) {
        msg <- paste0(
          "CRITICAL ERROR: HexPred_sp has ", nrow(dup_check), " duplicate (Tier5, fYEAR) combinations. ",
          "This will cause dimension mismatch in FRK::auto_basis(). ",
          "Expected: 1 row per hexagon per year. ",
          "Actual: Multiple rows detected. ",
          "Cannot proceed with FRK model fitting."
        )
        cat(paste0("  ERROR: ", msg, "\n"))
        status::status_try_catch(
          stop(msg),
          stage_ = 4,
          name_ = "HexPred_sp validation failed",
          item_ = "hexpred_sp_validation"
        )
        return(NULL)
      }

      # Calculate spatial and temporal dimensions
      unique_years <- sort(unique(HexPred_sp@data$fYEAR))
      nYEAR <- length(unique_years)
      nHEX <- nrow(subset(HexPred_sp, fYEAR == min(HexPred_sp@data$fYEAR)))

      # Validate that row count matches expected dimensions
      expected_rows <- nHEX * nYEAR
      actual_rows <- nrow(HexPred_sp@data)

      if (actual_rows != expected_rows) {
        msg <- paste0(
          "WARNING: HexPred_sp dimension mismatch. ",
          "Expected: ", nHEX, " hexagons × ", nYEAR, " years = ", expected_rows, " rows. ",
          "Actual: ", actual_rows, " rows (", round(actual_rows / expected_rows, 2), "x). ",
          "This indicates data quality issues that may cause FRK to fail."
        )
        cat(paste0("  WARNING: ", msg, "\n"))
        status::status_try_catch(
          warning(msg),
          stage_ = 4,
          name_ = "HexPred_sp dimension check",
          item_ = "hexpred_sp_dimensions"
        )
      }

      # Assign spatial index: each hex keeps the same n_spat across all years.
      # Data is ordered by (Tier5, fYEAR) so all nYEAR rows for hex 1 come first,
      # then all nYEAR rows for hex 2, etc.  "each = nYEAR" is correct.
      HexPred_sp@data$n_spat <- rep(1:nHEX, each = nYEAR)

      # Extract spatial BAUs (first year only)
      BAUs_spat <- subset(HexPred_sp, fYEAR == min(HexPred_sp@data$fYEAR))
      sp::coordnames(BAUs_spat) <- c("LONGITUDE", "LATITUDE")

      # Create spatiotemporal BAUs
      ST_BAUs <- FRK::auto_BAUs(manifold = FRK::STplane(),
                           data = STObj,
                           spatial_BAUs = BAUs_spat,
                           tunit = "years")

      # Trim extra year: auto_BAUs creates nYEAR+1 time slices; subset to observed years only.
      # This matches Julie's frk_prep.R:151: ST_BAUs <- ST_BAUs[, 1:nYEAR, 1:2]
      ST_BAUs <- ST_BAUs[, 1:nYEAR, 1:2]

      # Get actual dimensions of created BAUs
      bau_dims <- dim(ST_BAUs)
      n_spatial <- bau_dims[1]
      n_temporal <- bau_dims[2]

      # Map BAU time indices to actual years
      # auto_BAUs creates indices 1, 2, 3, ... which map to min_year, min_year+1, min_year+2, ...
      min_year <- min(unique_years)
      max_year <- max(unique_years)
      year_range <- max_year - min_year + 1

      # Validate temporal grid dimensions (log concisely)
      if (n_temporal != nYEAR) {
        # auto_BAUs created different number of time points than observed years
        # This happens with sparse temporal data (missing years in sequence)
        # Concise warning - will be followed by subset log
        msg <- paste0(
          "Temporal mismatch: ", n_temporal, " time points vs ", nYEAR, " years ",
          "(", min_year, "-", max_year, ", span=", year_range, "). ",
          "Will subset to observed years."
        )
        cat(paste0("  WARNING: ", msg, "\n"))
        status::status_try_catch(
          warning(msg),
          stage_ = 4,
          name_ = "BAU temporal mismatch",
          item_ = "bau_temporal_mismatch"
        )
      }

      # COMPREHENSIVE DEBUG: Log state before subsetting
      msg_pre_subset <- paste0(
        "PRE-SUBSET: n_spatial=", n_spatial,
        " n_temporal=", n_temporal,
        " nYEAR=", nYEAR,
        " ST_BAUs dims=", paste(dim(ST_BAUs), collapse="x"),
        " ST_BAUs@data rows=", nrow(ST_BAUs@data),
        " unique_years length=", length(unique_years)
      )
      status::status_try_catch(
        message(msg_pre_subset),
        stage_ = 4,
        name_ = "State before subsetting",
        item_ = "debug_pre_subset"
      )

      # NOTE: n_temporal may differ from nYEAR when observed years have gaps
      # (e.g., 20 time slices vs 19 observed years). This is expected behavior.
      # auto_BAUs creates a complete temporal grid from min to max year,
      # while nYEAR counts only observed years. We handle this by extracting
      # actual time indices from ST_BAUs below.

      # COMPREHENSIVE DEBUG: Log state before validation
      msg_pre_validation <- paste0(
        "PRE-VALIDATION: n_spatial=", n_spatial,
        " n_temporal=", n_temporal,
        " nYEAR=", nYEAR,
        " ST_BAUs@data rows=", nrow(ST_BAUs@data),
        " expected=", n_spatial * n_temporal
      )
      status::status_try_catch(
        message(msg_pre_validation),
        stage_ = 4,
        name_ = "State before validation",
        item_ = "debug_pre_validation"
      )

      # Validate dimensions before assignment
      # Note: We now create vectors for the FULL temporal grid (n_temporal),
      # not just observed years (nYEAR), to avoid subsetting corruption
      expected_fYEAR_length <- n_spatial * n_temporal
      actual_data_rows <- nrow(ST_BAUs@data)

      if (actual_data_rows != expected_fYEAR_length) {
        msg <- paste0(
          "CRITICAL: Dimension mismatch before year assignment. ",
          "ST_BAUs@data has ", actual_data_rows, " rows, ",
          "but trying to assign ", expected_fYEAR_length, " values ",
          "(", n_spatial, " spatial × ", n_temporal, " temporal). ",
          "This indicates corrupted BAU structure."
        )
        cat(paste0("  ERROR: ", msg, "\n"))
        status::status_try_catch(
          stop(msg),
          stage_ = 4,
          name_ = "Pre-assignment validation failed",
          item_ = "pre_assignment_validation"
        )
        return(NULL)
      }

      # Assign year labels to BAUs (dimensions validated and guaranteed to match)
      # CRITICAL: Assign directly to @data, not to slots, after subsetting
      # CRITICAL: Convert unique_years to integer BEFORE creating rep vector

      # DEBUG: Check unique_years before conversion
      msg_before <- paste0(
        "DEBUG before conversion: unique_years class=", class(unique_years)[1],
        " length=", length(unique_years),
        " first_few=", paste(head(unique_years, 3), collapse=",")
      )
      status::status_try_catch(
        message(msg_before),
        stage_ = 4,
        name_ = "unique_years before conversion",
        item_ = "debug_before"
      )

      # Convert unique_years to integer first
      unique_years_int <- as.integer(unique_years)

      # DEBUG: Check after conversion
      msg_after <- paste0(
        "DEBUG after conversion: unique_years_int class=", class(unique_years_int)[1],
        " length=", length(unique_years_int),
        " first_few=", paste(head(unique_years_int, 3), collapse=",")
      )
      status::status_try_catch(
        message(msg_after),
        stage_ = 4,
        name_ = "unique_years after conversion",
        item_ = "debug_after"
      )

      # Create assignment vectors for the FULL ST_BAUs grid (n_spatial × n_temporal)
      # CRITICAL: Must match n_temporal exactly, not year_range
      # auto_BAUs may create n_temporal != (max_year - min_year + 1) due to internal logic
      # Extract actual time points from ST_BAUs to ensure correct mapping
      time_indices <- sort(unique(ST_BAUs@time))
      all_years <- min_year + (time_indices - 1)  # Map indices to years

      # Validate we got expected number of time points
      if (length(all_years) != n_temporal) {
        msg <- paste0(
          "CRITICAL: Time extraction mismatch. ",
          "Expected n_temporal=", n_temporal, " but extracted ", length(all_years), " time points. ",
          "Cannot proceed with year assignment."
        )
        cat(paste0("  ERROR: ", msg, "\n"))
        status::status_try_catch(
          stop(msg),
          stage_ = 4,
          name_ = "Time extraction failed",
          item_ = "time_extraction"
        )
        return(NULL)
      }

      fyear_vec <- rep(all_years, each = n_spatial)
      nspat_vec <- rep(1:n_spatial, times = n_temporal)

      # DEBUG: Verify vector creation
      msg_vectors <- paste0(
        "DEBUG vectors: n_spatial=", n_spatial,
        " nYEAR=", nYEAR,
        " fyear_vec length=", length(fyear_vec),
        " nspat_vec length=", length(nspat_vec),
        " ST_BAUs@data rows=", nrow(ST_BAUs@data)
      )
      status::status_try_catch(
        message(msg_vectors),
        stage_ = 4,
        name_ = "vector creation check",
        item_ = "debug_vectors"
      )

      # Prepare data for year/spatial assignment
      status::status_try_catch(
        message("STEP 1: Extracting ST_BAUs@data slot"),
        stage_ = 4,
        name_ = "Extract data slot",
        item_ = "step_1_extract_data"
      )
      bau_data <- slot(ST_BAUs, "data")

      status::status_try_catch(
        message(paste0("STEP 2: Assigning fYEAR (length=", length(fyear_vec), ") to bau_data (rows=", nrow(bau_data), ")")),
        stage_ = 4,
        name_ = "Assign fYEAR",
        item_ = "step_2_assign_fyear"
      )
      bau_data$fYEAR <- fyear_vec

      status::status_try_catch(
        message(paste0("STEP 3: Assigning n_spat (length=", length(nspat_vec), ") to bau_data")),
        stage_ = 4,
        name_ = "Assign n_spat",
        item_ = "step_3_assign_nspat"
      )
      bau_data$n_spat <- nspat_vec

      # Store original row count before join
      status::status_try_catch(
        message("STEP 4: Preparing for left_join"),
        stage_ = 4,
        name_ = "Prepare join",
        item_ = "step_4_prepare_join"
      )
      bau_data_current <- bau_data
      st_baus_rows_before <- nrow(bau_data_current)

      # DEBUG: Dump pre-join data for analysis
      tryCatch({
        focal_tier_name <- paste0('Tier', as.numeric(BY_TIER) - 1)

        # Sample first 1000 rows from each dataset
        bau_sample <- head(bau_data_current[, c("fYEAR", "n_spat")], 1000)
        hexpred_sample <- head(HexPred_sp@data[, c("fYEAR", "n_spat", if("reefid" %in% names(HexPred_sp@data)) "reefid" else NULL)], 1000)

        # Create summary statistics
        bau_summary <- data.frame(
          dataset = "bau_data_current",
          n_rows = nrow(bau_data_current),
          n_spat_min = min(bau_data_current$n_spat),
          n_spat_max = max(bau_data_current$n_spat),
          unique_years = paste(sort(unique(bau_data_current$fYEAR)), collapse = ","),
          unique_n_spat_count = length(unique(bau_data_current$n_spat))
        )

        hexpred_summary <- data.frame(
          dataset = "HexPred_sp",
          n_rows = nrow(HexPred_sp@data),
          n_spat_min = min(HexPred_sp@data$n_spat),
          n_spat_max = max(HexPred_sp@data$n_spat),
          unique_years = paste(sort(unique(HexPred_sp@data$fYEAR)), collapse = ","),
          unique_n_spat_count = length(unique(HexPred_sp@data$n_spat))
        )

        summary_df <- rbind(bau_summary, hexpred_summary)

        # Save and upload files
        save_and_upload_debug(bau_sample, "bau_data_sample", 3, focal_tier_name)
        save_and_upload_debug(hexpred_sample, "hexpred_sample", 4, focal_tier_name)
        save_and_upload_debug(summary_df, "join_summary", 5, focal_tier_name)

        status::status_try_catch(
          message(paste0("DEBUG: Saved and uploaded pre-join samples and summary")),
          stage_ = 4,
          name_ = "Debug pre-join dump",
          item_ = "debug_prejoin"
        )
      }, error = function(e) {
        status::status_try_catch(
          warning(paste0("Failed to dump pre-join debug data: ", e$message)),
          stage_ = 4,
          name_ = "Debug pre-join failed",
          item_ = "debug_prejoin_error"
        )
      })

      # Join covariates from HexPred_sp - must use slot() to avoid replacement method
      status::status_try_catch(
        message(paste0(
          "STEP 5: left_join bau_data (", st_baus_rows_before, " rows, n_spat range: ",
          min(bau_data_current$n_spat), "-", max(bau_data_current$n_spat),
          ") with HexPred_sp@data (", nrow(HexPred_sp@data), " rows, n_spat range: ",
          min(HexPred_sp@data$n_spat), "-", max(HexPred_sp@data$n_spat),
          ", has reefid: ", "reefid" %in% names(HexPred_sp@data), ")"
        )),
        stage_ = 4,
        name_ = "Perform left_join",
        item_ = "step_5_left_join"
      )
      bau_data_joined <- dplyr::left_join(
        bau_data_current,
        HexPred_sp@data,
        by = c("fYEAR", "n_spat"),
        suffix = c("", ".y")
      )

      # Zero-fill any NA covariate values (e.g. if a BAU-year has no geoserver data).
      # CRITICAL: Do NOT reorder rows (no arrange/group_by). The STFDF data slot
      # must stay in TIME-MAJOR order (all spatial units at time 1, then time 2, etc.)
      # as produced by auto_BAUs. Reordering breaks the polygon-to-data mapping
      # when the data.frame is later passed to the STFDF constructor.
      numeric_cols <- names(bau_data_joined)[vapply(bau_data_joined, is.numeric, logical(1))]
      for (col in numeric_cols) {
        if (anyNA(bau_data_joined[[col]])) {
          bau_data_joined[[col]][is.na(bau_data_joined[[col]])] <- 0
        }
      }

      # CRITICAL VALIDATION: Check if join caused row explosion
      st_baus_rows_after <- nrow(bau_data_joined)
      reefid_exists <- "reefid" %in% names(bau_data_joined)
      reefid_na_count <- if(reefid_exists) sum(is.na(bau_data_joined$reefid)) else "N/A"
      status::status_try_catch(
        message(paste0(
          "STEP 6: Join completed, result has ", st_baus_rows_after, " rows (expected ", st_baus_rows_before, "), ",
          "reefid column exists: ", reefid_exists, ", reefid NAs: ", reefid_na_count
        )),
        stage_ = 4,
        name_ = "Join validation",
        item_ = "step_6_join_validation"
      )

      # DEBUG: Dump post-join diagnostics
      tryCatch({
        focal_tier_name <- paste0('Tier', as.numeric(BY_TIER) - 1)

        # Identify rows with no match (all covariate columns are NA)
        # Assuming reefid is a good indicator of successful join
        if(reefid_exists) {
          unmatched_rows <- bau_data_joined[is.na(bau_data_joined$reefid), c("fYEAR", "n_spat")]
          matched_rows <- bau_data_joined[!is.na(bau_data_joined$reefid), c("fYEAR", "n_spat")]

          # Create match statistics
          match_stats <- data.frame(
            total_rows = st_baus_rows_after,
            matched_rows = nrow(matched_rows),
            unmatched_rows = nrow(unmatched_rows),
            match_rate_pct = round(100 * nrow(matched_rows) / st_baus_rows_after, 2)
          )

          # Sample up to 1000 unmatched (fYEAR, n_spat) pairs
          unmatched_sample <- head(unmatched_rows, 1000)

          # Sample up to 1000 matched pairs for comparison
          matched_sample <- head(matched_rows, 1000)

          # Save and upload files
          save_and_upload_debug(match_stats, "join_match_stats", 6, focal_tier_name)
          save_and_upload_debug(unmatched_sample, "unmatched_keys", 7, focal_tier_name)
          save_and_upload_debug(matched_sample, "matched_keys", 8, focal_tier_name)

          status::status_try_catch(
            message(paste0(
              "DEBUG: Saved and uploaded post-join diagnostics. Match rate: ",
              round(100 * nrow(matched_rows) / st_baus_rows_after, 2), "%"
            )),
            stage_ = 4,
            name_ = "Debug post-join dump",
            item_ = "debug_postjoin"
          )
        } else {
          status::status_try_catch(
            warning("reefid column missing - cannot compute join match statistics"),
            stage_ = 4,
            name_ = "Debug post-join skipped",
            item_ = "debug_postjoin_skipped"
          )
        }
      }, error = function(e) {
        status::status_try_catch(
          warning(paste0("Failed to dump post-join debug data: ", e$message)),
          stage_ = 4,
          name_ = "Debug post-join failed",
          item_ = "debug_postjoin_error"
        )
      })

      if (st_baus_rows_after != st_baus_rows_before) {
        msg <- paste0(
          "CRITICAL ERROR: ST_BAUs row explosion after join with HexPred_sp! ",
          "Before join: ", st_baus_rows_before, " rows. ",
          "After join: ", st_baus_rows_after, " rows (", round(st_baus_rows_after / st_baus_rows_before, 2), "x). ",
          "This indicates duplicate keys in HexPred_sp causing cartesian product. ",
          "FRK::auto_basis() will fail with 'data length not multiple of columns' error. ",
          "Cannot proceed."
        )
        cat(paste0("  ERROR: ", msg, "\n"))
        status::status_try_catch(
          stop(msg),
          stage_ = 4,
          name_ = "ST_BAUs join validation failed",
          item_ = "st_baus_join_validation"
        )
        return(NULL)
      }

      # Remove duplicate columns from join by setting to NULL (preserves data.frame integrity)
      ycols <- grep("\\.y$", names(bau_data_joined), value = TRUE)
      status::status_try_catch(
        message(paste0("STEP 7: Removing ", length(ycols), " .y suffix columns: ", paste(ycols, collapse=", "))),
        stage_ = 4,
        name_ = "Remove duplicate columns",
        item_ = "step_7_remove_ycols"
      )
      for(col in ycols) {
        bau_data_joined[[col]] <- NULL
      }

      status::status_try_catch(
        message(paste0("STEP 8: After column removal, bau_data_joined has ", nrow(bau_data_joined), " rows, ", ncol(bau_data_joined), " cols")),
        stage_ = 4,
        name_ = "Post-removal state",
        item_ = "step_8_post_removal"
      )

      # CRITICAL FIX: Reconstruct data.frame completely to avoid ALL assignment operators
      # Both $ and [[]] trigger replacement dispatch when row metadata is corrupted
      # Solution: Create new columns in a list, then construct fresh data.frame
      status::status_try_catch(
        message("STEP 9: Preparing new columns (reefid factor, yearid factor, fs=1)"),
        stage_ = 4,
        name_ = "Prepare new columns",
        item_ = "step_9_prepare_cols"
      )

      # Extract existing data as list to avoid any data.frame operations
      existing_cols <- as.list(bau_data_joined)

      # Get expected length from a column that must exist (fYEAR)
      n_rows <- length(existing_cols$fYEAR)

      # CRITICAL FIX: Handle missing or all-NA reefid column
      # This happens when join finds no matches (misaligned n_spat ranges)
      if (is.null(existing_cols$reefid) || all(is.na(existing_cols$reefid))) {
        status::status_try_catch(
          warning(paste0(
            "reefid column is NULL or all NA after join. ",
            "This indicates n_spat mismatch between auto_BAUs and HexPred_sp. ",
            "Creating placeholder reefid='unknown' for all ", n_rows, " rows."
          )),
          stage_ = 4,
          name_ = "Handle missing reefid",
          item_ = "missing_reefid_fix"
        )
        existing_cols$reefid <- rep("unknown", n_rows)
      } else {
        # Replace NA values in reefid with "unknown"
        existing_cols$reefid[is.na(existing_cols$reefid)] <- "unknown"
      }

      # CRITICAL: Convert reefid to factor IN PLACE within existing_cols.
      # FRK's random effect formula (1 | reefid) requires reefid to be a factor.
      # Previously, a duplicate reefid was created in new_cols which caused
      # data.frame() to produce both 'reefid' (character) and 'reefid.1' (factor),
      # and FRK would find the character version — collapsing sigma2gamma to 0.
      existing_cols$reefid <- as.factor(as.character(existing_cols$reefid))

      # Create new columns (yearid and fs only — reefid already converted above)
      new_cols <- list(
        yearid = as.factor(existing_cols$fYEAR),
        fs = rep(1, n_rows)
      )

      status::status_try_catch(
        message("STEP 10: Reconstructing data.frame from scratch with new columns"),
        stage_ = 4,
        name_ = "Reconstruct data.frame",
        item_ = "step_10_reconstruct_df"
      )

      # DEBUG: Dump data structures to S3 for analysis
      tryCatch({
        focal_tier_name <- paste0('Tier', as.numeric(BY_TIER) - 1)

        # Convert existing_cols and new_cols to data frames for CSV export
        existing_df <- as.data.frame(existing_cols, stringsAsFactors = FALSE)
        new_df <- as.data.frame(new_cols, stringsAsFactors = FALSE)

        # Save and upload files
        save_and_upload_debug(existing_df, "existing_cols", 9, focal_tier_name)
        save_and_upload_debug(new_df, "new_cols", 10, focal_tier_name)

        status::status_try_catch(
          message(paste0("DEBUG: Saved and uploaded existing_cols and new_cols for TIER ", TIER)),
          stage_ = 4,
          name_ = "Debug dump before combine",
          item_ = "debug_dump_before"
        )
      }, error = function(e) {
        status::status_try_catch(
          warning(paste0("Failed to dump debug data: ", e$message)),
          stage_ = 4,
          name_ = "Debug dump failed",
          item_ = "debug_dump_error"
        )
      })

      # Combine all columns and build fresh data.frame (no assignment operators)
      all_cols <- c(existing_cols, new_cols)

      # DEBUG: Dump all_cols after combination
      tryCatch({
        focal_tier_name <- paste0('Tier', as.numeric(BY_TIER) - 1)

        # Convert all_cols to data frame for CSV export
        all_cols_df <- as.data.frame(all_cols, stringsAsFactors = FALSE)

        # Save and upload
        save_and_upload_debug(all_cols_df, "all_cols", 11, focal_tier_name)

        status::status_try_catch(
          message(paste0("DEBUG: Saved and uploaded all_cols for TIER ", TIER)),
          stage_ = 4,
          name_ = "Debug dump after combine",
          item_ = "debug_dump_after"
        )
      }, error = function(e) {
        status::status_try_catch(
          warning(paste0("Failed to dump all_cols: ", e$message)),
          stage_ = 4,
          name_ = "Debug dump all_cols failed",
          item_ = "debug_dump_all_cols_error"
        )
      })

      bau_data_final <- data.frame(all_cols, stringsAsFactors = FALSE, row.names = NULL)

      status::status_try_catch(
        message(paste0("STEP 11: Data reconstruction complete, final dimensions: ", nrow(bau_data_final), " rows × ", ncol(bau_data_final), " cols")),
        stage_ = 4,
        name_ = "Data prep complete",
        item_ = "step_11_data_complete"
      )

      # Reconstruct ST_BAUs using constructor to avoid broken slot assignment
      # This creates a new STFDF object with all components properly initialized
      status::status_try_catch(
        message("STEP 13: Saving reference to ST_BAUs_old for reconstruction"),
        stage_ = 4,
        name_ = "Save ST_BAUs reference",
        item_ = "step_13_save_reference"
      )
      ST_BAUs_old <- ST_BAUs  # Keep reference for memory cleanup

      # COMPREHENSIVE DEBUG: Log exact state before STFDF construction
      expected_rows_stfdf <- length(ST_BAUs_old@sp) * length(ST_BAUs_old@time)
      msg_pre_stfdf <- paste0(
        "STEP 14 PRE-STFDF: sp length=", length(ST_BAUs_old@sp),
        " time length=", length(ST_BAUs_old@time),
        " data rows=", nrow(bau_data_final),
        " data cols=", ncol(bau_data_final),
        " expected rows=", expected_rows_stfdf,
        " match=", (nrow(bau_data_final) == expected_rows_stfdf),
        " data class=", paste(class(bau_data_final), collapse=","),
        " has rownames=", !is.null(rownames(bau_data_final)),
        " rownames class=", class(rownames(bau_data_final))
      )
      status::status_try_catch(
        message(msg_pre_stfdf),
        stage_ = 4,
        name_ = "Pre-STFDF state",
        item_ = "step_14_pre_stfdf_state"
      )

      # Wrap STFDF construction with detailed error capture
      status::status_try_catch(
        message("STEP 15: Calling spacetime::STFDF constructor"),
        stage_ = 4,
        name_ = "Call STFDF constructor",
        item_ = "step_15_call_stfdf"
      )

      ST_BAUs <- tryCatch({
        spacetime::STFDF(
          sp = ST_BAUs_old@sp,
          time = ST_BAUs_old@time,
          data = bau_data_final,
          endTime = ST_BAUs_old@endTime
        )
      }, error = function(e) {
        # Capture detailed error information
        msg_error <- paste0(
          "STFDF construction failed: ", e$message,
          " | data class: ", paste(class(bau_data_final), collapse=","),
          " | data rows: ", nrow(bau_data_final),
          " | sp length: ", length(ST_BAUs_old@sp),
          " | time length: ", length(ST_BAUs_old@time),
          " | expected: ", expected_rows_stfdf,
          " | rownames: ", class(rownames(bau_data_final)),
          " | is.data.frame: ", is.data.frame(bau_data_final)
        )
        status::status_try_catch(
          stop(msg_error),
          stage_ = 4,
          name_ = "STFDF construction error",
          item_ = "stfdf_construction_error"
        )
      })

      status::status_try_catch(
        message(paste0("STEP 16: STFDF construction successful, dimensions: ", paste(dim(ST_BAUs), collapse=" x "))),
        stage_ = 4,
        name_ = "STFDF construction success",
        item_ = "step_16_stfdf_success"
      )

      # Clean up old object to free memory
      status::status_try_catch(
        message("STEP 17: Cleaning up temporary objects and running garbage collection"),
        stage_ = 4,
        name_ = "Memory cleanup",
        item_ = "step_17_cleanup"
      )
      rm(ST_BAUs_old, bau_data, bau_data_current, bau_data_joined, bau_data_final, existing_cols, new_cols, all_cols)
      gc(verbose = FALSE)

      # Set projection
      status::status_try_catch(
        message("STEP 18: Setting projection to empty CRS"),
        stage_ = 4,
        name_ = "Set projection",
        item_ = "step_18_set_projection"
      )
      ST_BAUs@sp@proj4string <- sp::CRS()

      # Remove overlapping covariates from STObj
      status::status_try_catch(
        message("STEP 19: Checking for overlapping fields between STObj and ST_BAUs"),
        stage_ = 4,
        name_ = "Check overlapping fields",
        item_ = "step_19_check_overlap"
      )
      overlapping_fields <- intersect(names(STObj@data), names(slot(ST_BAUs, "data")))
      if (length(overlapping_fields) > 0) {
        status::status_try_catch(
          message(paste0("STEP 20: Removing ", length(overlapping_fields), " overlapping fields from STObj: ", paste(overlapping_fields, collapse=", "))),
          stage_ = 4,
          name_ = "Remove overlapping fields",
          item_ = "step_20_remove_overlap"
        )
        STObj@data[, overlapping_fields] <- NULL
      } else {
        status::status_try_catch(
          message("STEP 20: No overlapping fields found, skipping removal"),
          stage_ = 4,
          name_ = "No overlap to remove",
          item_ = "step_20_no_overlap"
        )
      }

      # Create basis functions
      # nres controls resolution: 3L is the science default (proper fine-scale uncertainty);
      # 2L is a temporary fallback for very large grids only.
      status::status_try_catch(
        message(paste0("STEP 21: Calling FRK::auto_basis with nres=", basis_resolution)),
        stage_ = 4,
        name_ = "Call auto_basis",
        item_ = "step_21_call_auto_basis"
      )

      basis <- FRK::auto_basis(FRK::STplane(),
                          ST_BAUs,
                          tunit = "years",
                          nres = basis_resolution,
                          regular = TRUE)

      status::status_try_catch(
        message(paste0("STEP 22: auto_basis completed successfully, basis class=", class(basis)[1], ", n_basis=", length(basis))),
        stage_ = 4,
        name_ = "auto_basis success",
        item_ = "step_22_auto_basis_success"
      )

    # Update status (wrapped in tryCatch since get_status_name may not exist in test env)
      tryCatch({
        old_item_name <- get_status_name(4, "prep_FRK_objects")
        if (!stringr::str_detect(old_item_name, "\\[")) {
          new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
          new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
        ## ##  status:::update_status_name (  # Disabled - private function not exportedstage = 4, item = "prep_FRK_objects", name = new_item_name)  # Disabled - private function
      }, error = function(e) {
        # Ignore status update errors (function may not exist in test environment)
        NULL
      })

      # Return results
      obj_frk <- list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis)
      return(obj_frk)
}

#' @title Safe System Commands
#' @description Wrapper functions for system commands using processx for safer execution
#' @details Replaces unsafe system() calls with processx::run() (Suggestion 60)
#' @author Generated for optimization

#' @title Run System Command Safely
#' @description Executes a system command using processx for safer execution
#' @param command The command to execute (as a string or vector)
#' @param intern If TRUE, return stdout; if FALSE, return exit status
#' @param error_on_status If TRUE, throw error on non-zero exit status
#' @return Command output (if intern=TRUE) or exit status (if intern=FALSE)
#' @export
safe_system <- function(command, intern = FALSE, error_on_status = TRUE) {
  # Parse command if it's a string
  if (is.character(command) && length(command) == 1) {
    # Split command string into program and args
    parts <- unlist(strsplit(command, " +"))
    program <- parts[1]
    args <- if (length(parts) > 1) parts[-1] else character(0)
  } else if (is.character(command) && length(command) > 1) {
    program <- command[1]
    args <- command[-1]
  } else {
    stop("command must be a character string or vector")
  }

  # Execute using processx
  result <- tryCatch({
    processx::run(
      program,
      args = args,
      error_on_status = error_on_status,
      echo = !intern,
      stderr_to_stdout = TRUE
    )
  }, error = function(e) {
    if (error_on_status) {
      stop(paste0("Command failed: ", command, "\n", e$message))
    }
    list(status = 1, stdout = "", stderr = as.character(e))
  })

  if (intern) {
    return(result$stdout)
  } else {
    return(result$status)
  }
}

#' @title Run AWS CLI Command
#' @description Safely executes an AWS CLI command
#' @param args Character vector of AWS CLI arguments
#' @param profile AWS profile to use (default: "stats")
#' @return processx result object
#' @export
safe_aws <- function(args, profile = "stats") {
  full_args <- c(args, "--profile", profile)

  result <- processx::run(
    "aws",
    args = full_args,
    error_on_status = FALSE,
    echo = FALSE
  )

  if (result$status != 0) {
    warning(paste0("AWS command failed: aws ", paste(full_args, collapse = " "),
                   "\nError: ", result$stderr))
  }

  return(result)
}

#' @title Safely Copy File
#' @description Copies a file using processx instead of system("cp ...")
#' @param from Source file path
#' @param to Destination file path
#' @param recursive If TRUE, copy directories recursively
#' @return TRUE if successful, FALSE otherwise
#' @export
safe_copy <- function(from, to, recursive = FALSE) {
  args <- if (recursive) c("-r", from, to) else c(from, to)

  result <- processx::run(
    "cp",
    args = args,
    error_on_status = FALSE
  )

  if (result$status != 0) {
    warning(paste0("Copy failed: ", from, " -> ", to, "\nError: ", result$stderr))
    return(FALSE)
  }

  return(TRUE)
}

#' @title Safely Unzip File
#' @description Unzips a file using processx instead of system("unzip ...")
#' @param zipfile Path to zip file
#' @param exdir Extraction directory
#' @param overwrite If TRUE, overwrite existing files (-o flag)
#' @param junkpaths If TRUE, junk paths (-j flag)
#' @return TRUE if successful, FALSE otherwise
#' @export
safe_unzip <- function(zipfile, exdir, overwrite = TRUE, junkpaths = FALSE) {
  args <- character(0)

  if (overwrite) args <- c(args, "-o")
  if (junkpaths) args <- c(args, "-j")

  args <- c(args, zipfile, "-d", exdir)

  result <- processx::run(
    "unzip",
    args = args,
    error_on_status = FALSE
  )

  if (result$status != 0) {
    warning(paste0("Unzip failed: ", zipfile, "\nError: ", result$stderr))
    return(FALSE)
  }

  return(TRUE)
}

#' @title Safely Read Log Tail
#' @description Reads last N lines of a file using processx instead of system("tail ...")
#' @param file Path to file
#' @param n Number of lines to read (default: 5)
#' @return Character vector of lines
#' @export
safe_tail <- function(file, n = 5) {
  result <- processx::run(
    "tail",
    args = c("-n", as.character(n), file),
    error_on_status = FALSE
  )

  if (result$status != 0) {
    warning(paste0("tail failed: ", file, "\nError: ", result$stderr))
    return(character(0))
  }

  # Split stdout by newlines and return
  lines <- strsplit(result$stdout, "\n")[[1]]
  return(lines)
}

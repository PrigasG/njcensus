#' @importFrom futile.logger flog.logger flog.threshold flog.appender
NULL
# logging.R contents:
#' Setup Logging for njcensus Package
#'
#' @description Initializes logging configuration for the package
#' @param log_level Character. The logging level (default: "INFO")
#' @param log_file Character. Path to log file (default: tempdir())
#' @import futile.logger
#' @export
setup_logging <- function(log_level = "INFO",
                          log_file = file.path(tempdir(), "njcensus.log")) {
  # Create log directory if it doesn't exist
  log_dir <- dirname(log_file)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Configure logger
  flog.appender(appender.file(log_file))
  flog.threshold(log_level)
}

#' Log Message with Context
#'
#' @param level Character. Log level
#' @param msg Character. Message to log
#' @param ... Additional parameters for string formatting
#' @keywords internal
log_with_context <- function(level, msg, ...) {
  # Add context information
  context <- list(
    timestamp = Sys.time(),
    pid = Sys.getpid(),
    user = Sys.info()["user"]
  )

  formatted_msg <- sprintf("[%s][PID:%d][%s] %s",
                           context$timestamp,
                           context$pid,
                           context$user,
                           sprintf(msg, ...))

  do.call(paste0("flog.", tolower(level)), list(formatted_msg))
}

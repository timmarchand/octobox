# =============================================================================
# utils/performance_utils.R - Performance Monitoring & Memory Guardrails ----
# =============================================================================

library(memoise)
library(digest)

# -----------------------------------------------------------------------------
# 1. MONITORING ENGINE ----
# -----------------------------------------------------------------------------

#' @description Wraps any function to track execution time, memory usage, and throughput.
monitor_operation_performance <- function(operation_name, func, ..., 
                                         track_memory = TRUE, 
                                         track_cache = FALSE,
                                         verbose = TRUE) {
  start_time <- Sys.time()
  start_mem  <- if (track_memory) gc(verbose = FALSE)[2,2] else 0
  
  # Execute
  result <- func(...)
  
  end_time <- Sys.time()
  end_mem  <- if (track_memory) gc(verbose = FALSE)[2,2] else 0
  
  # Metrics
  elapsed     <- as.numeric(end_time - start_time, units = "secs")
  mem_change  <- end_mem - start_mem
  
  if (verbose) {
    cat(sprintf("\n=== [%s] REPORT ===\n", toupper(operation_name)))
    cat(sprintf("Time: %.3f s | Mem Change: %.2f MB\n", elapsed, mem_change))
    
    # Context-aware throughput
    n_items <- if(is.data.frame(result)) nrow(result) else length(result)
    if (!is.null(n_items) && elapsed > 0) {
      cat(sprintf("Throughput: %s units/sec\n", format(round(n_items/elapsed), big.mark=",")))
    }
    cat("==========================\n")
  }
  
  attr(result, "performance") <- list(time = elapsed, mem = mem_change, ts = Sys.time())
  return(result)
}

# -----------------------------------------------------------------------------
# 2. CACHE & MEMORY MANAGEMENT ----
# -----------------------------------------------------------------------------

#' @description Triggers garbage collection and clears temp objects if RAM exceeds threshold.
smart_memory_cleanup <- function(force = FALSE, memory_threshold_mb = 500, verbose = FALSE) {
  current_mem <- gc(verbose = FALSE)[2,2]
  
  if (force || current_mem > memory_threshold_mb) {
    if (verbose) cat("Triggering Adaptive Cleanup (Current:", round(current_mem), "MB)\n")
    
    # Clear specific global caches
    temp_objs <- ls(envir = .GlobalEnv, pattern = "^(pasted_text_|temp_cache_|last_processed_)")
    if (length(temp_objs) > 0) rm(list = temp_objs, envir = .GlobalEnv)
    
    gc(verbose = FALSE)
    return(TRUE)
  }
  return(FALSE)
}

#' @description Accesses memoise internals to trim or clear function caches.
manage_all_caches <- function(max_cache_size = 100, aggressive = FALSE) {
  # Common cached functions in this app
  cached_fs <- c("get_frequency_info_cached", "quick_conc", "process_pasted_text")
  
  for (f_name in cached_fs) {
    if (!exists(f_name, envir = .GlobalEnv)) next
    
    f_obj <- get(f_name, envir = .GlobalEnv)
    tryCatch({
      c_env <- environment(f_obj)
      if ("_cache" %in% names(c_env)) {
        if (aggressive) {
          c_env[["_cache"]] <- list() 
        } else if (length(c_env[["_cache"]]) > max_cache_size) {
          # Keep the 50 most recent entries
          c_env[["_cache"]] <- tail(c_env[["_cache"]], 50)
        }
      }
    }, error = function(e) NULL)
  }
}

# -----------------------------------------------------------------------------
# 3. CHUNKED PROCESSING STRATEGY ----
# -----------------------------------------------------------------------------

#' @description Determines the best processing mode based on input size.
get_processing_strategy <- function(n) {
  if (n < 5000)   return(list(mode = "standard", chunk = n, parallel = FALSE))
  if (n < 50000)  return(list(mode = "optimized", chunk = 5000, parallel = FALSE))
  return(list(mode = "enterprise", chunk = 10000, parallel = TRUE))
}

#' @description Splits data into manageable bites to prevent memory spikes.
process_with_strategy <- function(data, process_func, ...) {
  n <- if (is.data.frame(data)) nrow(data) else length(data)
  strat <- get_processing_strategy(n)
  
  if (strat$mode == "standard") return(process_func(data, ...))
  
  # Chunking logic
  idx <- split(seq_len(n), ceiling(seq_len(n) / strat$chunk))
  results <- lapply(idx, function(i) {
    chunk <- if(is.data.frame(data)) data[i, , drop=FALSE] else data[i]
    res <- process_func(chunk, ...)
    if (n > 20000) gc() # Mid-process cleanup for large data
    return(res)
  })
  
  if (is.data.frame(results[[1]])) return(do.call(rbind, results))
  return(unlist(results, recursive = FALSE))
}

# -----------------------------------------------------------------------------
# 4. INITIALIZATION ----
# -----------------------------------------------------------------------------

initialize_performance_monitoring <- function(mem_limit = 1000, cache_limit = 150) {
  options(perf_mem_limit = mem_limit, perf_cache_limit = cache_limit)
  
  # Schedule background cleanup if 'later' is available
  if (requireNamespace("later", quietly = TRUE)) {
    later::later(function() {
      smart_memory_cleanup(memory_threshold_mb = mem_limit)
      manage_all_caches(max_cache_size = cache_limit)
    }, 60)
  }
  cat("✓ Performance Watchdog: Active (Limit:", mem_limit, "MB)\n")
}

# Run init
initialize_performance_monitoring()
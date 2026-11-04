# PhD Research Project
# Mitafidis Panagiotis
# Capitol Technology University
# Chair: Dr. Leila Halawi
#
# PhDResearchProject_MATLABConversion_16Hz_v1c.R
#
# v1c = v1b + (1) a source folder such as 652_1 is deleted from INPUT_DIR as soon
# as archiving has emptied it; (2) the console prints "Converting from <folder>."
# before each chunk.
#
# v1a = v1 + archiving: after each .mat file has been processed (converted, or
# found to have no PH = 7) it is copied to every folder in BACKUP_DIRS
# (D: and E:), keeping the same sub-folder structure (e.g. D:\MATLAB Files\652_1\...),
# each copy is verified (file size), and only then is the original deleted from
# INPUT_DIR (C:/OneDrive) so the OneDrive file count stays low.
# A file is NEVER deleted unless every backup copy exists with identical size.
# Files that failed to convert are left in place (and retried on the next run).
# Set MOVE_TO_BACKUP <- FALSE to convert without touching the .mat files.
#
# Single-pass conversion + 16 Hz alignment. Merges
#   PhDResearchProject_MATLABConversion_v5a.R  (read .mat, PH=7 filter, trim)
#   PhDResearchProject_UniformRate16Hz_v2.R    (16 Hz grid, header rows, JPG)
# Each .mat file is read exactly once.
#
# Row-selection logic (unchanged from v5a):
#   Skip  : files with no PH = 7 sample.
#   Start : first sample where ALT < LANDALT + 300, searching from the start
#           of the last continuous PH = 6 block immediately before PH = 7.
#           LANDALT = ALT at the first PH = 7 sample.
#   End   : last sample where PH = 7.
#   Trim applied proportionally to every variable in the file.
#
# 16 Hz alignment:
#   Every parameter is spread onto a common 16 Hz grid using its TRUE native
#   rate (the Rate field of the .mat struct). Sample k lands on data row
#   1 + (k-1) * (16 / rate); the rows in between are NA
#   (8 Hz -> 1 NA, 4 Hz -> 3, 2 Hz -> 7, 1 Hz -> 15, 0.25 Hz -> 63).
#   If a parameter has no valid Rate, its rate is inferred from its trimmed
#   length (snapped to 16/8/4/2/1/0.25 Hz) and counted in the log.
#   Rows = max(longest 16 Hz column, 1 + (n-1)*ratio over all columns): no
#   sample is dropped; 16 Hz columns may carry a few trailing NAs.
#
# Layout of each .rds (one column per parameter):
#   row 1 : rate_hz       native sample rate (Hz)
#   row 2 : units
#   row 3 : description
#   row 4 : alpha
#   row 5+: data on the 16 Hz grid
# The header rows are text, so every column is character. To get numbers back:
#   d <- readRDS(f); dat <- d[-(1:4)]; dat[, (names(dat)) := lapply(.SD, as.numeric)]
#
# Notes vs v5a:
#   * MATLAB struct field names are read from dimnames (R.matlab does not set
#     names()), so Rate / Units / Description / Alpha are now actually found
#     and the data vector no longer picks up the Rate value as an extra sample.
#   * Non-finite samples are kept as NA in place (v5a dropped them, which
#     would shift later samples in time).
#
# Output per qualifying file (flat in OUTPUT_DIR):
#   <stem>.rds             — header rows + 16 Hz data, xz-compressed
#   <stem>.jpg             — 12-panel chart vs. time (s)
#   _conversion_log.csv    — appended after each chunk
#   _qualifying_files.txt  — files that had PH = 7
#
# Restart: files already converted (.rds exists) and files previously logged as
# "no PH=7" are skipped; failed files are retried.
# =============================================================================

# ---- Keep Windows awake while the conversion runs ---------------------------
keep_awake_flag <- tempfile(fileext = ".awake")
writeLines("delete this file to release the keep-awake request", keep_awake_flag)

keep_awake_ps1 <- tempfile(fileext = ".ps1")
writeLines(c(
  '$sig = \'[DllImport("kernel32.dll")] public static extern uint SetThreadExecutionState(uint esFlags);\'',
  'Add-Type -MemberDefinition $sig -Name Power -Namespace Win32',
  '# 2147483649 = ES_CONTINUOUS | ES_SYSTEM_REQUIRED (screen may still turn off)',
  '[void][Win32.Power]::SetThreadExecutionState([uint32]2147483649)',
  sprintf('while ((Test-Path "%s") -and (Get-Process -Id %d -ErrorAction SilentlyContinue)) { Start-Sleep -Seconds 5 }',
          gsub("\\\\", "/", keep_awake_flag), Sys.getpid())
), keep_awake_ps1)

system2("powershell",
        c("-NoProfile", "-ExecutionPolicy", "Bypass", "-WindowStyle", "Hidden",
          "-File", shQuote(keep_awake_ps1)),
        wait = FALSE)
cat("Keep-awake request active (system will not sleep while this script runs).\n")

# ---- Start of main script ---------------------------------------------------
INPUT_DIR  <- "C:\\Users\\panos\\OneDrive\\Drive\\CapitolTech\\Research Project LCL\\Data_and_Code\\MATLAB Files"
OUTPUT_DIR <- "C:\\Users\\panos\\OneDrive\\Drive\\CapitolTech\\Research Project LCL\\Data_and_Code\\Converted_16Hz"

BACKUP_DIRS    <- c("D:\\MATLAB Files", "E:\\MATLAB Files")   # same structure as INPUT_DIR
MOVE_TO_BACKUP <- TRUE   # copy to every BACKUP_DIRS, verify, then delete the original

N_WORKERS     <- 5L      # physical cores of the i5-10400; use 5 to keep the PC responsive
WRITE_JPG     <- TRUE
SKIP_EXISTING <- TRUE
ONLY_STEMS    <- NULL    # e.g. c("652200104182036") to run just these; NULL = all

JPG_PARAMS <- c("ALT", "RALT", "CAS", "GS",
                "CCPC", "CCPF", "VRTG", "LONG",
                "LATG", "CTAC", "GLS",  "PTCH")

MAX_HZ      <- 16
VALID_RATES <- c(16, 8, 4, 2, 1, 0.25)

# ---- Packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  for (pkg in c("R.matlab", "data.table", "parallel", if (WRITE_JPG) "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Missing package '%s'. Run: install.packages('%s')", pkg, pkg))
  }
  library(R.matlab); library(data.table); library(parallel)
  if (WRITE_JPG) library(ggplot2)
})

# ---- Helpers ----------------------------------------------------------------
clean_path <- function(p) gsub("\\\\", "/", p)

.scalar_local <- function(x) {
  if (is.null(x)) return(NA_character_)
  v <- unlist(x); if (!length(v)) return(NA_character_)
  as.character(v[[1L]])
}

# R.matlab stores MATLAB struct field names in dimnames[[1]], not names()
field_ci <- function(lst, key) {
  nms <- names(lst)
  if (is.null(nms)) nms <- dimnames(lst)[[1L]]
  i <- which(tolower(nms) == tolower(key))
  if (!length(i)) NULL else lst[[i[1L]]]
}

# Returns list(var_data = named list of numeric vectors (non-finite -> NA),
#              meta     = data.table(variable, rate_hz, units, description, alpha))
extract_variables <- function(mat) {
  var_data  <- list()
  meta_rows <- list()
  for (nm in names(mat)) {
    entry <- mat[[nm]]
    if (is.list(entry)) {
      dv <- field_ci(entry, "data")
      if (is.null(dv)) next
      v <- suppressWarnings(as.numeric(unlist(dv)))
      v[!is.finite(v)] <- NA_real_
      if (!length(v) || all(is.na(v))) next
      var_data[[nm]]  <- v
      meta_rows[[nm]] <- data.table(
        variable    = nm,
        rate_hz     = suppressWarnings(as.numeric(.scalar_local(field_ci(entry, "rate")))),
        units       = .scalar_local(field_ci(entry, "units")),
        description = .scalar_local(field_ci(entry, "description")),
        alpha       = .scalar_local(field_ci(entry, "alpha")))
    } else if (is.numeric(entry) || is.integer(entry)) {
      v <- as.numeric(entry); v[!is.finite(v)] <- NA_real_
      if (!length(v) || all(is.na(v))) next
      var_data[[nm]]  <- v
      meta_rows[[nm]] <- data.table(variable = nm, rate_hz = NA_real_,
                                    units = NA_character_, description = NA_character_,
                                    alpha = NA_character_)
    }
  }
  list(var_data = var_data,
       meta     = if (length(meta_rows)) rbindlist(meta_rows) else data.table())
}

# ---- Single-pass worker ------------------------------------------------------
process_core <- function(job, output_dir, jpg_params, write_jpg, valid_rates, max_hz) {
  suppressPackageStartupMessages({
    library(R.matlab); library(data.table)
    if (write_jpg) library(ggplot2)
  })
  data.table::setDTthreads(1L)

  mat_path <- job$mat
  out_stem <- job$stem
  t0       <- Sys.time()
  secs     <- function() round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2L)

  tryCatch({

    # ---- Read file once ------------------------------------------------------
    mat    <- R.matlab::readMat(mat_path, fixNames = FALSE)
    parsed <- extract_variables(mat)
    vd     <- parsed$var_data
    meta   <- parsed$meta
    if (!length(vd)) stop("no usable variables found")

    get_vd <- function(nm) {
      k <- names(vd)[tolower(names(vd)) == tolower(nm)]
      if (length(k)) vd[[k[1L]]] else NULL
    }

    # ---- Check PH = 7 --------------------------------------------------------
    ph_vec <- get_vd("PH")
    if (is.null(ph_vec) || !any(round(ph_vec) == 7, na.rm = TRUE)) {
      return(data.table::data.table(
        file = mat_path, out_stem = out_stem, ok = TRUE, skipped = TRUE,
        qualifying = FALSE, rows = NA_integer_, cols = NA_integer_,
        LANDALT = NA_real_, n_rate_fallback = NA_integer_,
        secs = secs(), message = "no PH=7"))
    }

    ph_int  <- as.integer(round(ph_vec))
    n_ph    <- length(ph_int)
    alt_vec <- get_vd("ALT")

    # ---- Trim end: last PH = 7 ----------------------------------------------
    last_ph7 <- max(which(ph_int == 7L))
    end_frac <- last_ph7 / n_ph

    # ---- Trim start: LANDALT + 300 from last PH = 6 block --------------------
    first_ph7     <- which(ph_int == 7L)[1L]
    start_frac    <- 0
    LANDALT       <- NA_real_
    alt_threshold <- NA_real_

    if (!is.null(alt_vec)) {
      scan_back <- first_ph7 - 1L
      if (scan_back >= 1L && !is.na(ph_int[scan_back]) && ph_int[scan_back] == 6L) {
        ph6_start <- scan_back
        while (ph6_start > 1L && !is.na(ph_int[ph6_start - 1L]) && ph_int[ph6_start - 1L] == 6L)
          ph6_start <- ph6_start - 1L

        ph6_frac    <- (ph6_start - 1L) / n_ph
        ph7_frac    <- (first_ph7  - 1L) / n_ph
        alt_ph7_idx <- min(length(alt_vec), max(1L, floor(ph7_frac * length(alt_vec)) + 1L))
        LANDALT       <- alt_vec[alt_ph7_idx]
        alt_threshold <- LANDALT + 300

        alt_ph6_idx <- min(length(alt_vec), max(1L, floor(ph6_frac * length(alt_vec)) + 1L))
        drop_rel    <- which(alt_vec[alt_ph6_idx:length(alt_vec)] < alt_threshold)[1L]

        if (!is.na(drop_rel)) {
          alt_drop_abs <- alt_ph6_idx + drop_rel - 1L
          start_frac   <- max(ph6_frac, (alt_drop_abs - 1L) / length(alt_vec))
        } else {
          start_frac <- ph6_frac
        }
      } else {
        start_frac <- (first_ph7 - 1L) / n_ph
      }
    }

    # ---- Apply trim to all variables ----------------------------------------
    trim_v <- function(v) {
      n <- length(v)
      s <- max(1L, floor(start_frac * n) + 1L)
      e <- min(n,  ceiling(end_frac  * n))
      if (s > e) return(numeric(0L))
      v[s:e]
    }
    vd_trim <- Filter(function(v) length(v) > 0L, lapply(vd, trim_v))
    if (!length(vd_trim)) stop("trim produced empty vectors for all variables")

    vars <- names(vd_trim)
    n    <- vapply(vd_trim, length, integer(1L))
    m    <- match(vars, meta$variable)

    # ---- True native rate (fallback: infer from trimmed length) --------------
    rate <- meta$rate_hz[m]
    rate[!(rate %in% valid_rates)] <- NA_real_
    need <- is.na(rate)
    n_fallback <- sum(need)
    if (any(need)) {
      ref_n <- if (any(!need & rate == max_hz)) max(n[!need & rate == max_hz]) else max(n)
      rate[need] <- vapply(n[need], function(k)
        valid_rates[which.min(abs(log(max_hz * k / ref_n) - log(valid_rates)))], numeric(1L))
    }
    ratio <- as.integer(max_hz / rate)

    # ---- Spread onto the 16 Hz grid + header rows ---------------------------
    n_ref  <- max(c(0L, n[ratio == 1L]))
    n_rows <- as.integer(max(n_ref, (n - 1L) * ratio + 1L))

    out <- vector("list", length(vars)); names(out) <- vars
    for (i in seq_along(vars)) {
      col <- rep(NA_character_, n_rows)
      col[seq.int(1L, by = ratio[i], length.out = n[i])] <- as.character(vd_trim[[i]])
      out[[i]] <- c(as.character(rate[i]),
                    meta$units[m[i]], meta$description[m[i]], meta$alpha[m[i]],
                    col)
    }
    dat <- data.table::setDT(out)

    saveRDS(dat, file.path(output_dir, paste0(out_stem, ".rds")), compress = "xz")

    # ---- Save JPG: selected parameters vs. time (s) -------------------------
    if (write_jpg) {
      plot_rows <- do.call(rbind, lapply(jpg_params, function(p) {
        i <- which(tolower(vars) == tolower(p))[1L]
        if (is.na(i)) return(NULL)
        v   <- vd_trim[[i]]
        pos <- seq.int(1L, by = ratio[i], length.out = n[i])
        ok  <- !is.na(v)
        if (!any(ok)) return(NULL)
        data.frame(time_s = (pos[ok] - 1L) / max_hz, value = v[ok],
                   param = p, stringsAsFactors = FALSE)
      }))

      if (!is.null(plot_rows) && nrow(plot_rows) > 0L) {
        found <- jpg_params[jpg_params %in% unique(plot_rows$param)]
        plot_rows$param <- factor(plot_rows$param, levels = found)

        subtitle_str <- if (!is.na(LANDALT))
          sprintf("LANDALT = %.0f ft  |  threshold = %.0f ft  |  %d rows at 16 Hz (%.0f s)",
                  LANDALT, alt_threshold, n_rows, (n_rows - 1L) / max_hz)
        else
          sprintf("No LANDALT — trimmed at PH=6 start  |  %d rows at 16 Hz (%.0f s)",
                  n_rows, (n_rows - 1L) / max_hz)

        p <- ggplot2::ggplot(plot_rows, ggplot2::aes(x = time_s, y = value)) +
          ggplot2::geom_line(colour = "#2E5FA3", linewidth = 0.35) +
          ggplot2::facet_wrap(~param, ncol = 1L, scales = "free_y",
                              strip.position = "left") +
          ggplot2::labs(title    = basename(mat_path),
                        subtitle = subtitle_str,
                        x = "Time (s)", y = NULL) +
          ggplot2::theme_minimal(base_size = 9) +
          ggplot2::theme(
            strip.placement  = "outside",
            strip.text       = ggplot2::element_text(face = "bold", size = 8),
            panel.grid.minor = ggplot2::element_blank(),
            panel.spacing    = ggplot2::unit(0.4, "lines"),
            plot.title       = ggplot2::element_text(face = "bold", size = 9),
            plot.subtitle    = ggplot2::element_text(size = 7, colour = "grey40"))

        ggplot2::ggsave(
          file.path(output_dir, paste0(out_stem, ".jpg")),
          plot = p, width = 12, height = max(6, length(found) * 2),
          dpi = 150, units = "in", device = "jpeg")
      }
    }

    data.table::data.table(
      file = mat_path, out_stem = out_stem, ok = TRUE, skipped = FALSE,
      qualifying = TRUE, rows = n_rows, cols = ncol(dat),
      LANDALT = LANDALT, n_rate_fallback = n_fallback,
      secs = secs(), message = "")

  }, error = function(e) {
    data.table::data.table(
      file = mat_path, out_stem = out_stem, ok = FALSE, skipped = FALSE,
      qualifying = NA, rows = NA_integer_, cols = NA_integer_,
      LANDALT = NA_real_, n_rate_fallback = NA_integer_,
      secs = secs(), message = conditionMessage(e))
  })
}

# ---- Archiving: copy to every backup, verify, then delete the original ------
# Returns list(ok, msg). The source is removed only if every backup copy exists
# and has exactly the source's size.
archive_mat <- function(src, rel, backup_dirs, input_dir) {
  src_size <- file.size(src)
  if (is.na(src_size)) return(list(ok = FALSE, msg = "source not found"))
  for (b in backup_dirs) {
    dest <- file.path(b, rel)
    if (!(file.exists(dest) && isTRUE(file.size(dest) == src_size))) {
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      okc <- suppressWarnings(file.copy(src, dest, overwrite = TRUE))
      if (!isTRUE(okc) || !isTRUE(file.size(dest) == src_size))
        return(list(ok = FALSE, msg = sprintf("copy to %s failed", b)))
    }
  }
  if (!isTRUE(suppressWarnings(file.remove(src))))
    return(list(ok = FALSE, msg = "backups OK but could not delete original"))

  # Remove the source folder (e.g. 652_1) once it is empty, walking up through
  # empty parents but never touching INPUT_DIR itself. The folder is removed
  # only if list.files() shows it completely empty (hidden files included), so
  # one that still holds anything (or that another worker is still emptying)
  # is left alone. (file.remove() cannot delete directories on Windows.)
  d <- dirname(src)
  while (nchar(d) > nchar(input_dir) && startsWith(d, input_dir) &&
         !length(list.files(d, all.files = TRUE, no.. = TRUE))) {
    unlink(d, recursive = TRUE)
    if (dir.exists(d)) break
    d <- dirname(d)
  }
  list(ok = TRUE, msg = "")
}

# Convert one file, then (if it succeeded or had no PH=7) archive its .mat
process_one <- function(job, output_dir, jpg_params, write_jpg, valid_rates, max_hz,
                        backup_dirs, move_to_backup, input_dir) {
  res <- process_core(job, output_dir, jpg_params, write_jpg, valid_rates, max_hz)
  archived <- NA; archive_msg <- ""
  if (move_to_backup && isTRUE(res$ok)) {
    a <- tryCatch(archive_mat(job$mat, job$rel, backup_dirs, input_dir),
                  error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    archived <- a$ok; archive_msg <- a$msg
  }
  cbind(res, data.table::data.table(archived = archived, archive_msg = archive_msg))
}

# ---- Paths ------------------------------------------------------------------
INPUT_DIR   <- sub("/+$", "", clean_path(INPUT_DIR))
OUTPUT_DIR  <- clean_path(OUTPUT_DIR)
BACKUP_DIRS <- sub("/+$", "", clean_path(BACKUP_DIRS))
if (!dir.exists(INPUT_DIR)) stop(sprintf("INPUT_DIR not found:\n  %s", INPUT_DIR))
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(OUTPUT_DIR, "_conversion_log.csv")

# Pre-flight: every backup location must be writable before anything is moved
if (MOVE_TO_BACKUP) {
  for (b in BACKUP_DIRS) {
    dir.create(b, recursive = TRUE, showWarnings = FALSE)
    probe <- file.path(b, ".write_test")
    ok <- tryCatch({ writeLines("x", probe); file.exists(probe) }, error = function(e) FALSE,
                   warning = function(w) FALSE)
    if (!ok) stop(sprintf("Backup folder not writable: %s", b))
    unlink(probe)
  }
  cat(sprintf("Backups: %s\n", paste(BACKUP_DIRS, collapse = "  +  ")))
}

# Older logs (no archive columns): add them once so appending stays consistent
if (file.exists(log_path)) {
  old <- data.table::fread(file = log_path, colClasses = list(character = c("file", "out_stem")))
  if (!"archived" %in% names(old)) {
    old[, archived := NA][, archive_msg := ""]
    data.table::fwrite(old, log_path)
  }
  rm(old)
}

# ---- Discover files ---------------------------------------------------------
cat("Scanning for .mat files...\n")
all_files <- sort(list.files(INPUT_DIR, pattern = "\\.mat$",
                             recursive = TRUE, full.names = TRUE,
                             ignore.case = TRUE))
cat(sprintf("Found %d .mat file(s) total.\n", length(all_files)))
if (!length(all_files)) stop("No .mat files found.")

rel_paths   <- substring(all_files, nchar(INPUT_DIR) + 2L)   # path below INPUT_DIR

# Stem naming (basename, or parent__basename on a clash) must stay the same
# across restarts even though converted .mat files are moved away, so clashes
# are judged over the files still in INPUT_DIR PLUS those already archived.
known_rel <- rel_paths
if (MOVE_TO_BACKUP) {
  arch <- list.files(BACKUP_DIRS[1L], pattern = "\\.mat$", recursive = TRUE, ignore.case = TRUE)
  known_rel <- unique(c(rel_paths, arch))
}
known_base <- tools::file_path_sans_ext(basename(known_rel))
dup_base   <- unique(known_base[duplicated(known_base)])

parent_dirs <- basename(dirname(all_files))
basenames   <- tools::file_path_sans_ext(basename(all_files))
clash       <- basenames %in% dup_base
out_stems   <- ifelse(clash, paste0(parent_dirs, "__", basenames), basenames)

if (!is.null(ONLY_STEMS)) {
  keep <- out_stems %in% ONLY_STEMS
  all_files <- all_files[keep]; out_stems <- out_stems[keep]; rel_paths <- rel_paths[keep]
}

# ---- Skip already-converted / already-known no-PH=7 -------------------------
if (SKIP_EXISTING) {
  done <- file.exists(file.path(OUTPUT_DIR, paste0(out_stems, ".rds")))
  if (file.exists(log_path)) {
    prev <- data.table::fread(file = log_path, select = c("out_stem", "ok", "skipped"),
                              colClasses = list(character = "out_stem"))
    done <- done | out_stems %in% prev[ok == TRUE & skipped == TRUE, out_stem]
  }
  if (any(done)) {
    cat(sprintf("Skipping %d already-processed file(s).\n", sum(done)))
    all_files <- all_files[!done]
    out_stems <- out_stems[!done]
    rel_paths <- rel_paths[!done]
  }
  if (!length(all_files)) { cat("Nothing to do.\n"); q(save = "no") }
}

cat(sprintf("Files to process: %d\n\n", length(all_files)))

# ---- Dispatch ---------------------------------------------------------------
jobs <- mapply(function(m, s, r) list(mat = m, stem = s, rel = r),
               all_files, out_stems, rel_paths, SIMPLIFY = FALSE, USE.NAMES = FALSE)

n_workers  <- min(N_WORKERS, length(jobs))
chunk_size <- max(n_workers * 4L, 16L)
chunks     <- split(seq_along(jobs), ceiling(seq_along(jobs) / chunk_size))
results    <- vector("list", length(jobs))
done_n     <- 0L
t_start    <- Sys.time()

cat(sprintf("Processing %d file(s) with %d worker(s)...\n", length(jobs), n_workers))
cat(sprintf("Output : %s\n\n", OUTPUT_DIR))

cl <- parallel::makeCluster(n_workers, type = "PSOCK")

tryCatch({

  parallel::clusterExport(cl,
    varlist = c("process_one", "process_core", "archive_mat",
                "extract_variables", "field_ci", ".scalar_local"),
    envir   = environment())
  parallel::clusterCall(cl, function() {
    library(R.matlab); library(data.table)
  })

  for (ci in seq_along(chunks)) {
    idx       <- chunks[[ci]]

    # Which source folder(s) this chunk comes from, e.g. "Converting from 652_1."
    src_dirs <- unique(dirname(rel_paths[idx]))
    src_dirs[src_dirs == "."] <- "(root)"
    src_txt  <- if (length(src_dirs) <= 3L) paste(src_dirs, collapse = ", ")
                else sprintf("%s ... %s (%d folders)", src_dirs[1L], src_dirs[length(src_dirs)],
                             length(src_dirs))
    cat(sprintf("Converting from %s.\n", src_txt))
    flush.console()

    chunk_res <- parallel::parLapplyLB(cl, jobs[idx], process_one,
                                       output_dir  = OUTPUT_DIR,
                                       jpg_params  = JPG_PARAMS,
                                       write_jpg   = WRITE_JPG,
                                       valid_rates = VALID_RATES,
                                       max_hz      = MAX_HZ,
                                       backup_dirs    = BACKUP_DIRS,
                                       move_to_backup = MOVE_TO_BACKUP,
                                       input_dir      = INPUT_DIR)
    results[idx] <- chunk_res
    done_n       <- done_n + length(idx)

    chunk_dt    <- data.table::rbindlist(chunk_res, fill = TRUE)
    n_converted <- sum(chunk_dt$ok & !chunk_dt$skipped, na.rm = TRUE)
    n_skipped   <- sum(chunk_dt$skipped,                na.rm = TRUE)
    n_failed    <- sum(!chunk_dt$ok,                    na.rm = TRUE)
    elap        <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    avg         <- elap / done_n
    eta_min     <- (length(jobs) - done_n) * avg / 60

    n_arch_fail <- sum(chunk_dt$archived == FALSE, na.rm = TRUE)

    cat(sprintf(
      "[%6d/%d] %5.1f%% | chunk %d/%d: converted %d, skipped (no PH=7) %d, failed %d%s | avg %.2fs/file | ETA %.1f min\n",
      done_n, length(jobs), 100 * done_n / length(jobs),
      ci, length(chunks), n_converted, n_skipped, n_failed,
      if (n_arch_fail) sprintf(", ARCHIVE FAILED %d", n_arch_fail) else "",
      avg, eta_min))
    flush.console()

    data.table::fwrite(chunk_dt, log_path, append = file.exists(log_path))
  }

}, finally = {
  parallel::stopCluster(cl)
})

# ---- Summary ----------------------------------------------------------------
elapsed     <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
summary_dt  <- data.table::rbindlist(results, fill = TRUE)
n_converted <- sum(summary_dt$ok & !summary_dt$skipped, na.rm = TRUE)
n_skipped   <- sum(summary_dt$skipped,                  na.rm = TRUE)
n_failed    <- sum(!summary_dt$ok,                      na.rm = TRUE)

cat(sprintf(
  "\nDone in %.1f min (%.2f s/file avg).\nConverted: %d  |  Skipped (no PH=7): %d  |  Failed: %d\n",
  elapsed / 60, elapsed / max(1L, length(jobs)), n_converted, n_skipped, n_failed))
cat(sprintf("Output : %s\n", OUTPUT_DIR))
cat(sprintf("Log    : %s\n", log_path))

# Qualifying list across all runs (read from the cumulative log)
full_log <- data.table::fread(file = log_path, colClasses = list(character = c("file", "out_stem")))
qualifying_files <- unique(full_log[qualifying == TRUE, file])
writeLines(qualifying_files, file.path(OUTPUT_DIR, "_qualifying_files.txt"))
cat(sprintf("Qualifying files (all runs): %d -> %s\n", length(qualifying_files),
            file.path(OUTPUT_DIR, "_qualifying_files.txt")))

fb <- summary_dt[ok == TRUE & !skipped & n_rate_fallback > 0L]
if (nrow(fb)) {
  cat(sprintf("\n%d file(s) had parameters with no valid Rate (inferred from length):\n", nrow(fb)))
  print(head(fb[, .(file = basename(file), n_rate_fallback)], 20))
}
if (MOVE_TO_BACKUP) {
  cat(sprintf("\nArchived (copied to %d backup(s), original deleted): %d | Archive problems: %d\n",
              length(BACKUP_DIRS), sum(summary_dt$archived == TRUE, na.rm = TRUE),
              sum(summary_dt$archived == FALSE, na.rm = TRUE)))
  af <- summary_dt[archived == FALSE]
  if (nrow(af)) {
    cat("Originals NOT deleted (still in INPUT_DIR):\n")
    print(head(af[, .(file = basename(file), archive_msg)], 50))
  }
}
if (n_failed > 0L) {
  cat("\nFailures:\n")
  print(head(summary_dt[ok == FALSE, .(file = basename(file), message)], 50))
}

# ---- Finished: notification file, restore power settings, ask about shutdown -
writeLines(sprintf("Conversion finished %s | converted %d | no PH=7 %d | failed %d",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   n_converted, n_skipped, n_failed),
           file.path(OUTPUT_DIR, "_CONVERSION_COMPLETE.txt"))

# ---- Release keep-awake: the helper exits within 5 s, your normal power plan resumes
unlink(keep_awake_flag)

# ---- Alarm tone: "shave and a haircut ... two bits" ---------------------------
system2("powershell", c("-NoProfile", "-Command", shQuote(paste(
  '$n = @(@(523,200),@(392,100),@(392,100),@(440,200),@(392,200),@(0,350),@(494,200),@(523,400));',
  'foreach ($x in $n) { if ($x[0] -eq 0) { Start-Sleep -Milliseconds $x[1] } else { [console]::Beep($x[0], $x[1]) } }'
))))

PROMPT_TIMEOUT_SEC <- 300   # 5 min : no answer = nobody is at the PC

ans <- suppressWarnings(system2(
  "powershell",
  c("-NoProfile", "-Command",
    shQuote(sprintf(
      "(New-Object -ComObject WScript.Shell).Popup('Conversion complete. Would you like to keep the computer on?', %d, 'Doharig', 4132)",
      PROMPT_TIMEOUT_SEC))),
  stdout = TRUE))
ans <- suppressWarnings(as.integer(trimws(ans[length(ans)])))

# Popup returns 6 = Yes, 7 = No, -1 = timed out
if (identical(ans, 6L)) {
  cat("Keeping the computer on. Done.\n")
} else {
  cat("Shutting down in 60 seconds (run 'shutdown /a' in a terminal to cancel)...\n")
  system2("shutdown", c("/s", "/t", "60", "/c", shQuote("Conversion complete - shutting down")))
}


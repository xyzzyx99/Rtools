# helper: build the prompt path using username-splitting
.make_prompt_path <- function(wd = getwd()) {
  user <- Sys.getenv(ifelse(.Platform$OS.type == "windows", "USERNAME", "USER"))
  # remove everything up to and including the first occurrence of <user>
  right <- sub(paste0(".*", user), "", wd, perl = TRUE)

  if (identical(right, wd)) {
    # username wasn't found → not inside ~, show absolute path
    wd
  } else {
    # we're “inside ~”: clean leading separators on the right side
    right <- sub("^[\\\\/]+", "", right)
    if (nchar(right) == 0) {
      "~"
    } else {
      paste0("~/", right)
    }
  }
}

# set initial prompt
options(prompt = paste0("R:", .make_prompt_path(getwd()), " > "))

# override setwd: update prompt after changing directory
setwd <- function(dir) {
  base::setwd(dir)
  prompt_path <- .make_prompt_path(getwd())
  options(prompt = paste0("R:", prompt_path, " > "))
  cat("Working directory changed to:", getwd(), "\n")
}

ls

# ------- helpers -------
w_nchar <- function(x) nchar(x, type = "width")

# Truncate to width WITHOUT padding; preserve extension and add "..."
trunc_name_nopad <- function(x, width) {
  if (w_nchar(x) <= width) return(x)
  ell <- "..."
  m <- regexpr("\\.[^.]+$", x)
  has_ext <- (m > 1)
  if (has_ext) {
    ext  <- substr(x, m, nchar(x))
    base <- substr(x, 1, m - 1)
    room <- width - w_nchar(ext) - nchar(ell)
    if (room <= 0) return(substr(paste0(ell, ext), 1, width))
    take <- 0L
    for (i in seq_len(nchar(base))) {
      cand <- substr(base, 1, i)
      if (w_nchar(cand) > room) break
      take <- i
    }
    paste0(substr(base, 1, take), ell, ext)
  } else {
    room <- width - nchar(ell)
    if (room <= 0) return(substr(ell, 1, width))
    take <- 0L
    for (i in seq_len(nchar(x))) {
      cand <- substr(x, 1, i)
      if (w_nchar(cand) > room) break
      take <- i
    }
    paste0(substr(x, 1, take), ell)
  }
}

get_console_width <- function() {
  if (requireNamespace("cli", quietly = TRUE)) {
    w <- tryCatch(cli::console_width(), error = function(e) NA_integer_)
    if (is.finite(w) && w > 0) return(w)
  }
  getOption("width", 80L)
}

# ------- DF builder (FIXED: add IsDir) -------
show_files_df <- function(path = getwd()) {
  files <- list.files(path, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  info  <- file.info(files)
  ord   <- order(info$mtime)  # oldest -> newest
  files <- files[ord]; info <- info[ord, , drop = FALSE]
  data.frame(
    Name     = basename(files),
    Size_KB  = round(info$size / 1024, 1),
    Modified = info$mtime,
    IsDir    = info$isdir,                                # <-- new
    stringsAsFactors = FALSE
  )
}

# ---------- DF builder (Linux-safe; filter early; returns IsDir) ----------
show_files_df <- function(path = getwd(), show = c("both", "files", "dirs")) {
  show <- match.arg(show)

  files <- list.files(path, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  info  <- file.info(files)
  ord   <- order(info$mtime)               # oldest -> newest (ls -ltr style)
  files <- files[ord]; info <- info[ord, , drop = FALSE]

  is_dir <- info$isdir
  keep <- switch(show,
                 both  = rep(TRUE, length(is_dir)),
                 files = !is_dir,
                 dirs  =  is_dir)

  files <- files[keep]
  info  <- info [keep, , drop = FALSE]
  is_dir <- is_dir[keep]

  data.frame(
    Name     = basename(files),
    Size_KB  = round(info$size / 1024, 1),
    Modified = info$mtime,
    IsDir    = is_dir,
    stringsAsFactors = FALSE
  )
}

# ---------- pretty printer (Linux-safe; Name last; quote-smart; width-safe) ----------
show_files_table <- function(path = getwd(),
                             term_width = NULL,
                             use_ascii_type = NA,
                             show = c("both", "files", "dirs")) {
  show <- match.arg(show)
  df <- show_files_df(path, show = show)
  tw <- if (is.null(term_width)) get_console_width() else as.integer(term_width)

  # leave a margin so a line never hits the right edge (avoids phantom blank line)
  safety_pad <- 1L

  # Linux / encoding heuristics
  is_linux <- identical(tolower(Sys.info()[["sysname"]]), "linux")
  if (is.na(use_ascii_type)) use_ascii_type <- is_linux || !isTRUE(l10n_info()[["UTF-8"]])

  # Type labels from IsDir (robust; no leading spaces)
  type_chr <- if (isTRUE(use_ascii_type)) {
    ifelse(df$IsDir, "[DIR]", "[FILE]")
  } else {
    ifelse(df$IsDir, "📁 Directory", "📄 File")
  }

  # Fixed columns
  mod_chr  <- format(df$Modified, "%Y-%m-%d %H:%M:%S")  # two-digit seconds
  size_chr <- as.character(df$Size_KB)
  idx_chr  <- as.character(seq_len(nrow(df)))

  # Widths (Name is last & dynamic)
  w_idx  <- max(nchar(idx_chr), 1L)
  w_size <- max(w_nchar(size_chr), w_nchar("Size_KB"))
  w_mod  <- max(w_nchar(mod_chr),  w_nchar("Modified"))
  w_type <- max(w_nchar(type_chr), w_nchar("Type"))

  # Spacing (use 2 spaces after Type on Linux for clarity)
  gap1 <- 2
  extra_after_size <- 3
  gap3 <- 1
  gap4 <- if (is_linux) 2 else 1

  # Fixed width BEFORE unquoted Name starts
  fixed_before_name <- w_idx + gap1 + w_size + extra_after_size + w_mod + gap3 + w_type + gap4

  # Name truncation (no padding)
  NAME_MIN <- 10
  NAME_MAX <- 60
  name_nat <- if (nrow(df)) max(w_nchar(df$Name), w_nchar("Name")) else w_nchar("Name")
  max_fit  <- max(5, tw - safety_pad - fixed_before_name)
  w_name_trunc <- if (name_nat <= max_fit) {
    min(max(name_nat, NAME_MIN), NAME_MAX)
  } else {
    max(NAME_MIN, min(NAME_MAX, max_fit))
  }

  # Header (align Name with start column; no extra shift)
  header <- paste0(
    format("",        width = w_idx,  justify = "right"),
    strrep(" ", gap1),
    format("Size_KB", width = w_size, justify = "right"),
    strrep(" ", extra_after_size),
    format("Modified",width = w_mod,  justify = "left"),
    strrep(" ", gap3),
    format("Type",    width = w_type, justify = "left"),
    strrep(" ", gap4),
    "Name"
  )
  cat(header, "\n")
  cat(strrep("-", min(tw, nchar(header))), "\n")

  # Rows
  for (i in seq_len(nrow(df))) {
    idx_fmt  <- format(idx_chr[i],  width = w_idx,  justify = "right")
    size_fmt <- format(size_chr[i], width = w_size, justify = "right")
    mod_fmt  <- format(mod_chr[i],  width = w_mod,  justify = "left")
    type_fmt <- format(type_chr[i], width = w_type, justify = "left")

    left_fixed <- paste0(
      idx_fmt,
      strrep(" ", gap1),
      size_fmt,
      strrep(" ", extra_after_size),
      mod_fmt,
      strrep(" ", gap3),
      type_fmt
    )

    # Remaining width for Name (unquoted), leaving safety_pad
    rem_unquoted <- max(0, tw - safety_pad - (w_nchar(left_fixed) + gap4))
    cand_unquoted <- if (rem_unquoted > 0)
      trunc_name_nopad(df$Name[i], min(rem_unquoted, w_name_trunc))
    else ""

    # Quote only if the printed (possibly truncated) name still contains spaces
    needs_quote <- grepl("\\s", cand_unquoted)

    if (!needs_quote) {
      line <- paste0(left_fixed, strrep(" ", gap4), cand_unquoted)
    } else {
      # Quoted: opening quote one col earlier; reserve 2 quotes + safety_pad
      pre_gap   <- gap4 - 1
      rem_inner <- max(0, tw - safety_pad - ((w_nchar(left_fixed) + pre_gap) + 2))
      inner     <- if (rem_inner > 0)
        trunc_name_nopad(df$Name[i], min(rem_inner, w_name_trunc))
      else ""
      name_out <- paste0("'", inner, "'")
      line <- paste0(left_fixed, strrep(" ", pre_gap), name_out)
    }

    if (w_nchar(line) > tw) line <- substr(line, 1, tw)  # hard cap
    cat(line, "\n")
  }

  invisible(df)
}

# 3) Active binding: typing `show_files` prints once
if (exists("lls", envir = .GlobalEnv, inherits = FALSE)) {
  if (bindingIsLocked("lls", .GlobalEnv)) unlockBinding("lls", .GlobalEnv)
  rm("lls", envir = .GlobalEnv)
}

# now create the active binding
makeActiveBinding(
  "lls",
  function() { show_files_table(); invisible(NULL) },
  .GlobalEnv
)

if (exists("llf", envir = .GlobalEnv, inherits = FALSE)) {
  if (bindingIsLocked("llf", .GlobalEnv)) unlockBinding("llf", .GlobalEnv)
  rm("llf", envir = .GlobalEnv)
}

# now create the active binding
makeActiveBinding(
  "llf",
  function() { show_files_table(show = 'files'); invisible(NULL) },
  .GlobalEnv
)

if (exists("lld", envir = .GlobalEnv, inherits = FALSE)) {
  if (bindingIsLocked("llf", .GlobalEnv)) unlockBinding("lld", .GlobalEnv)
  rm("lld", envir = .GlobalEnv)
}

# now create the active binding
makeActiveBinding(
  "lld",
  function() { show_files_table(show = 'dirs'); invisible(NULL) },
  .GlobalEnv
)

# With brackets, call the plain function:
# show_files_table()  # prints once

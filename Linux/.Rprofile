options(prompt = paste0("R:~", sub(paste0(".*", Sys.getenv("USER")), "", getwd()), " > "))

setwd <- function(dir) {
  base::setwd(dir)
  # compute relative path after $USER
  rel_path <- sub(paste0(".*", Sys.getenv("USER")), "", getwd())
  # build prompt: R:~/... >
  options(prompt = paste0("R:~", rel_path, " > "))
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

# ------- pretty printer: Name LAST; quote only if printed name has spaces -------

show_files_table <- function(path = getwd(), term_width = NULL, use_ascii_type = NA) {
  df <- show_files_df(path)
  tw <- if (is.null(term_width)) get_console_width() else as.integer(term_width)

  # Always leave a column so we never hit the right edge exactly
  safety_pad <- 1L

  # OS / encoding heuristics
  is_linux <- identical(tolower(Sys.info()[["sysname"]]), "linux")
  if (is.na(use_ascii_type)) use_ascii_type <- is_linux || !isTRUE(l10n_info()[["UTF-8"]])

  # Type labels from IsDir (robust)
  type_chr <- if (isTRUE(use_ascii_type)) {
    ifelse(df$IsDir, "[DIR]", "[FILE]")
  } else {
    ifelse(df$IsDir, " Directory", " File")
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

  # Spacing (2 spaces after Type on Linux helps clarity)
  gap1 <- 2
  extra_after_size <- 3
  gap3 <- 1
  gap4 <- if (is_linux) 2 else 1

  # Fixed width BEFORE unquoted Name starts
  fixed_before_name <- w_idx + gap1 + w_size + extra_after_size + w_mod + gap3 + w_type + gap4

  # Name truncation policy (no padding on names)
  NAME_MIN <- 10
  NAME_MAX <- 60
  name_nat <- max(w_nchar(df$Name), w_nchar("Name"))
  max_fit  <- max(5, tw - safety_pad - fixed_before_name)
  w_name_trunc <- if (name_nat <= max_fit) {
    min(max(name_nat, NAME_MIN), NAME_MAX)
  } else {
    max(NAME_MIN, min(NAME_MAX, max_fit))
  }

  # Header (shift "Name" 1 char right)
  header <- paste0(
  format("",        width = w_idx,  justify = "right"),
  strrep(" ", gap1),
  format("Size_KB", width = w_size, justify = "right"),
  strrep(" ", extra_after_size),
  format("Modified",width = w_mod,  justify = "left"),
  strrep(" ", gap3),
  format("Type",    width = w_type, justify = "left"),
  strrep(" ", gap4),   #  shift only by gap4 (1 less space)
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

    # Unquoted candidate: leave safety_pad at right edge
    rem_unquoted <- max(0, tw - safety_pad - (w_nchar(left_fixed) + gap4))
    cand_unquoted <- if (rem_unquoted > 0)
      trunc_name_nopad(df$Name[i], min(rem_unquoted, w_name_trunc))
    else ""

    needs_quote <- grepl("\\s", cand_unquoted)

    if (!needs_quote) {
      line <- paste0(left_fixed, strrep(" ", gap4), cand_unquoted)
    } else {
      # Quoted: opening quote one col earlier; reserve 2 quotes + safety_pad
      pre_gap <- gap4 - 1
      rem_inner <- max(0, tw - safety_pad - ((w_nchar(left_fixed) + pre_gap) + 2))
      inner <- if (rem_inner > 0)
        trunc_name_nopad(df$Name[i], min(rem_inner, w_name_trunc))
      else ""
      name_out <- paste0("'", inner, "'")
      line <- paste0(left_fixed, strrep(" ", pre_gap), name_out)
    }

    # Final guard: hard-trim if anything slipped over (shouldn’t now)
    if (w_nchar(line) > tw) line <- substr(line, 1, tw)
    cat(line, "\n")
  }

  invisible(df)
}

# now create the active binding
makeActiveBinding(
  "lls",
  function() { show_files_table(); invisible(NULL) },
  .GlobalEnv
)
# With brackets, call the plain function:
# show_files_table()  # prints once

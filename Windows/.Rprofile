options(prompt = paste0(getwd(), " > "))

setwd <- function(dir) {
  base::setwd(dir)
  options(prompt = paste0(getwd(), " > "))
  cat("📂 Working directory changed to:", getwd(), "\n")
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

# ------- DF builder -------
show_files_df <- function(path = getwd()) {
  files <- list.files(path, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  info  <- file.info(files)
  ord   <- order(info$mtime)  # oldest -> newest
  files <- files[ord]; info <- info[ord, , drop = FALSE]
  data.frame(
    Name     = basename(files),
    Size_KB  = round(info$size / 1024, 1),
    Modified = info$mtime,
    Type     = ifelse(info$isdir, "📁 Directory", "📄 File"),
    stringsAsFactors = FALSE
  )
}

# ------- pretty printer: Name LAST; quote only if printed name has spaces -------
show_files_table <- function(path = getwd(),
                             term_width = NULL,
                             use_ascii_type = NA,
                             show = c("both", "files", "dirs")) {
  show <- match.arg(show)
  df <- show_files_df(path)
  tw <- if (is.null(term_width)) get_console_width() else as.integer(term_width)
  
  # derive robust directory flag
  is_dir <- if ("IsDir" %in% names(df)) {
    df$IsDir
  } else if ("Type" %in% names(df)) {
    grepl("Directory|\\[DIR\\]", df$Type)
  } else {
    rep(FALSE, nrow(df))
  }
  
  # apply filter
  keep <- switch(show,
                 both  = rep(TRUE, nrow(df)),
                 files = !is_dir,
                 dirs  =  is_dir)
  df <- df[keep, , drop = FALSE]
  is_dir <- is_dir[keep]
  
  # --- detect whether to fall back to ASCII for Type labels ---
  if (is.na(use_ascii_type)) {
    use_ascii_type <- !isTRUE(l10n_info()[["UTF-8"]])
  }
  
  # choose icons or ASCII from is_dir (robust)
  type_chr <- if (isTRUE(use_ascii_type)) {
    ifelse(is_dir, "[DIR]", "[FILE]")
  } else {
    ifelse(is_dir, "📁 Directory", "📄 File")
  }
  
  # fixed columns (pre-format)
  mod_chr  <- format(df$Modified, "%Y-%m-%d %H:%M:%S")  # two-digit seconds
  size_chr <- as.character(df$Size_KB)
  idx_chr  <- as.character(seq_len(nrow(df)))
  
  # widths (Name is last & dynamic)
  w_idx  <- max(nchar(idx_chr), 1L)
  w_size <- max(w_nchar(size_chr), w_nchar("Size_KB"))
  w_mod  <- max(w_nchar(mod_chr),  w_nchar("Modified"))
  w_type <- max(w_nchar(type_chr), w_nchar("Type"))
  
  # spacing
  gap1 <- 2                  # after index
  extra_after_size <- 3      # after Size_KB
  gap3 <- 1                  # between Modified and Type
  gap4 <- if (isTRUE(use_ascii_type)) 2 else 1  # larger gap in ASCII mode
  
  # fixed width BEFORE name starts
  fixed_before_name <- w_idx + gap1 + w_size + extra_after_size + w_mod + gap3 + w_type + gap4
  
  # Name width used ONLY for truncation (no padding)
  NAME_MIN <- 10
  NAME_MAX <- 60
  name_nat <- max(w_nchar(df$Name), w_nchar("Name"))
  max_fit  <- max(5, tw - fixed_before_name)
  w_name_trunc <- if (name_nat <= max_fit) {
    min(max(name_nat, NAME_MIN), NAME_MAX)
  } else {
    max(NAME_MIN, min(NAME_MAX, max_fit))
  }
  
  # ----- Header: move "Name" 1 char to the RIGHT -----
  header <- paste0(
    format("",        width = w_idx,  justify = "right"),
    strrep(" ", gap1),
    format("Size_KB", width = w_size, justify = "right"),
    strrep(" ", extra_after_size),
    format("Modified",width = w_mod,  justify = "left"),
    strrep(" ", gap3),
    format("Type",    width = w_type, justify = "left"),
    strrep(" ", gap4 + 1),  # shift header right by 1
    "Name"
  )
  cat(header, "\n")
  cat(strrep("-", min(tw, nchar(header))), "\n")
  
  # ----- Rows -----
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
    
    # remaining width for Name if unquoted
    rem_unquoted <- max(0, tw - (w_nchar(left_fixed) + gap4))
    cand_unquoted <- if (rem_unquoted > 0)
      trunc_name_nopad(df$Name[i], min(rem_unquoted, w_name_trunc))
    else ""
    needs_quote <- grepl("\\s", cand_unquoted)
    
    if (!needs_quote) {
      line <- paste0(left_fixed, strrep(" ", gap4), cand_unquoted)
    } else {
      # quoted: opening quote one col earlier; reserve 2 for quotes
      rem_quoted_text <- max(0, tw - (w_nchar(left_fixed) + (gap4 - 1) + 2))
      inner <- if (rem_quoted_text > 0)
        trunc_name_nopad(df$Name[i], min(rem_quoted_text, w_name_trunc))
      else ""
      name_out <- paste0("'", inner, "'")
      line <- paste0(left_fixed, strrep(" ", gap4 - 1), name_out)
    }
    
    if (w_nchar(line) > tw) line <- substr(line, 1, tw)
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

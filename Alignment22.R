# --- SLR helper script for alignment ----------------------------
# Packages ---------------------------------------------------------------------
pkgs <- c("tidyverse","janitor","stringr","openxlsx","irr","psych","ggplot2")
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Paths ------------------------------------------------------------------------
in_csv <- "PhdSLR-Multilingual_Software_Interoperability_Assessment (SQ2) - PhdSLR-Multilingual_Software_Interoperability_Assessment(SQ2)-Sub-question2.2-SCREENING(2).csv"
out_dir <- "slr_outputs_sq22"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -- helper: guess the delimiter from the first line ---------------------------
guess_delim <- function(path) {
  l1 <- readLines(path, n = 1, warn = FALSE)
  l1 <- sub("^\ufeff", "", l1)  # remove any BOM
  cands <- c("," = ",", ";" = ";", "\t" = "\t", "|" = "|")
  counts <- sapply(names(cands), function(d) stringr::str_count(l1, stringr::fixed(d)))
  if (all(counts == 0)) return(",") # fallback : comma
  names(which.max(counts))[1]
}

# Read & normalize -------------------------------------------------------------
delim <- guess_delim(in_csv)
df_raw <- readr::read_delim(
  in_csv,
  delim  = delim,
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

norm <- function(x) { x <- ifelse(is.na(x),"",trimws(as.character(x))); tolower(x) }

# Useful column sets
fields_struct <- c("Full-Text Accessibility","Publication Language","Multiple Paradigms",
                   "Paradigmatic Compatibility Focus","Quantitative Components",
                   "Empirical Evidence","Validation Methods","Study Type")
fields_struct <- intersect(fields_struct, names(df_raw))

fields_reason <- c('Reasoning for "Screening judgement"',
                   'Reasoning for "Study Type"',
                   'Reasoning for "Full-Text Accessibility"',
                   'Reasoning for "Publication Language"')
fields_reason <- intersect(fields_reason, names(df_raw))

df <- df_raw %>%
  mutate(
    judgement_norm = norm(`Screening judgement`),
    score_num = suppressWarnings(as.numeric(gsub(",", ".", `Screening score`)))
  ) %>%
  mutate(across(all_of(fields_struct), norm))

# ReasonCode / ReasonNote ------------------------------------------------------
pat_ec3 <- regex("(duplicate|duplicat|redundan|short\\s+paper|extended\\s+version|journal\\s+version|poster|summary\\s+version|less\\s+complete|supersed|preprint\\s+version|earlier\\s+version)", ignore_case=TRUE)
pat_ec2 <- regex("\\b(opinion|tutorial|tool\\s+description|product|editorial)\\b", ignore_case=TRUE)

best_note <- function(...) {
  cands <- list(...)
  for (s in cands) {
    if (!is.null(s) && nzchar(trimws(as.character(s)))) {
      txt <- gsub("\\s+", " ", trimws(as.character(s)))
      return(ifelse(nchar(txt)>260, paste0(substr(txt,1,260),"…"), txt))
    }
  }
  return("")
}

inclusion_note <- function(row) {
  bits <- character(0)
  
  add <- function(col, label) {
    if (col %in% names(row)) {
      ok <- row[[col]]
      if (length(ok) > 0 && !is.na(ok)) {
        ok_chr <- tolower(trimws(as.character(ok)))
        if (identical(ok_chr, "yes")) bits <<- c(bits, label)
      }
    }
  }
  
  add("Multiple Paradigms",               "multi-paradigm")
  add("Paradigmatic Compatibility Focus", "compatibility focus")
  add("Quantitative Components",          "quantitative component")
  add("Empirical Evidence",               "empirical evidence")
  add("Validation Methods",               "validation method")
  add("Study Type",                       "research contribution")
  add("Full-Text Accessibility",          "full text accessible")
  add("Publication Language",             "supported language")
  
  paste0("Meets IC; ",
         if (length(bits)) paste0(paste(bits, collapse = ", "), ".")
         else "no EC triggered.")
}

pick_reason <- function(row) {
  free <- paste(na.omit(as.character(row[fields_reason])), collapse=" ")
  # Priority: EC3 > EC4 > EC5 > EC2 > EC1 > EC6; else IC for includes
  if (str_detect(free, pat_ec3)) return(c("EC3", best_note(row[['Reasoning for "Screening judgement"']], row[['Reasoning for "Study Type"']])))
  if ("Full-Text Accessibility" %in% names(row) && row[["Full-Text Accessibility"]]=="no") 
    return(c("EC4", best_note(row[['Reasoning for "Full-Text Accessibility"']], row[['Reasoning for "Screening judgement"']])) )
  if ("Publication Language" %in% names(row) && row[["Publication Language"]]=="no") 
    return(c("EC5", best_note(row[['Reasoning for "Publication Language"']], row[['Reasoning for "Screening judgement"']])) )
  if (("Study Type" %in% names(row) && row[["Study Type"]]=="no") || str_detect(free, pat_ec2)) 
    return(c("EC2", best_note(row[['Reasoning for "Study Type"']], row[['Reasoning for "Screening judgement"']])) )
  if (("Multiple Paradigms" %in% names(row) && row[["Multiple Paradigms"]]=="no") ||
      ("Paradigmatic Compatibility Focus" %in% names(row) && row[["Paradigmatic Compatibility Focus"]]=="no"))
    return(c("EC1", best_note(row[['Reasoning for "Multiple Paradigms"']], row[['Reasoning for "Paradigmatic Compatibility Focus"']], row[['Reasoning for "Screening judgement"']])) )
  if ("Quantitative Components" %in% names(row) && row[["Quantitative Components"]]=="no")
    return(c("EC6", best_note(row[['Reasoning for "Quantitative Components"']], row[['Reasoning for "Screening judgement"']])) )
  if ("judgement_norm" %in% names(row) &&
      !is.na(row[["judgement_norm"]]) &&
      tolower(trimws(as.character(row[["judgement_norm"]]))) == "include") {
    return(c("IC", inclusion_note(row)))
  }
  
  return(c("EC1", best_note(row[['Reasoning for "Screening judgement"']])))
}

rc <- df %>% rowwise() %>%
  do({
    pr <- pick_reason(.)
    tibble(ReasonCode = pr[1], ReasonNote = pr[2])
  }) %>% ungroup()

df2 <- bind_cols(df_raw, rc)

# Save enriched CSV
write_csv(df2, file.path(out_dir, "SQ2.2_with_ReasonCode.csv"))

# Annex: Full-text exclusions with reasons -------------------------------------
annex_cols <- c("Title","Year","Venue","DOI link","Screening judgement","ReasonCode","ReasonNote",
                "Full-Text Accessibility","Publication Language","Multiple Paradigms","Paradigmatic Compatibility Focus")
annex_cols <- intersect(annex_cols, names(df2))
annex <- df2 %>% filter(tolower(`Screening judgement`) != "include") %>% select(all_of(annex_cols))
write_csv(annex, file.path(out_dir, "Annex_FullText_Exclusions_SQ2.1.csv"))

# Integrity: counts, crosstab, threshold sweep, contradictions -----------------
counts <- df2 %>% count(ReasonCode, name="count") %>% arrange(desc(count))
xtab <- df2 %>% mutate(judgement=tolower(`Screening judgement`)) %>%
  count(ReasonCode, judgement) %>% tidyr::pivot_wider(names_from=judgement, values_from=n, values_fill=0)
write_csv(counts, file.path(out_dir, "Counts_by_Reason.csv"))
write_csv(xtab,   file.path(out_dir, "Reason_by_Judgement.csv"))

# Threshold sweep (requires Screening score)
thresh_sweep <- function(d, taus = c(2,3,4)) {
  d <- d %>% mutate(jbin = case_when(tolower(`Screening judgement`)=="include" ~ 1L, TRUE ~ 0L),
                    round_score = floor(suppressWarnings(as.numeric(gsub(",", ".", `Screening score`))) + 0.5))
  purrr::map_dfr(taus, function(tau) {
    inc_hat <- as.integer(d$round_score >= tau)
    tibble(tau = tau,
           n_include_hat = sum(inc_hat, na.rm=TRUE),
           n_exclude_hat = sum(1L - inc_hat, na.rm=TRUE),
           agree_with_judgement = sum(inc_hat == d$jbin, na.rm=TRUE),
           n_records = nrow(d))
  })
}
sweep <- thresh_sweep(df2)
write_csv(sweep, file.path(out_dir, "Threshold_Sweep.csv"))

# Contradictions
contr_tabs <- list()
if ("Full-Text Accessibility" %in% names(df2))
  contr_tabs$Include_with_FT_No <- df2 %>% filter(tolower(`Screening judgement`)=="include", norm(`Full-Text Accessibility`)=="no") %>%
  select(Title, `Screening judgement`, `Full-Text Accessibility`)
if ("Publication Language" %in% names(df2))
  contr_tabs$Include_with_Language_No <- df2 %>% filter(tolower(`Screening judgement`)=="include", norm(`Publication Language`)=="no") %>%
  select(Title, `Screening judgement`, `Publication Language`)
contr_tabs$EC6_with_Include <- df2 %>% filter(ReasonCode=="EC6", tolower(`Screening judgement`)=="include") %>% select(Title, `Screening judgement`, ReasonNote)

wb <- createWorkbook()
addWorksheet(wb, "Counts_by_Reason"); writeData(wb, "Counts_by_Reason", counts)
addWorksheet(wb, "Reason_by_Judgement"); writeData(wb, "Reason_by_Judgement", xtab)
addWorksheet(wb, "Threshold_Sweep"); writeData(wb, "Threshold_Sweep", sweep)
for (nm in names(contr_tabs)) {
  if (nrow(contr_tabs[[nm]])>0) { addWorksheet(wb, nm); writeData(wb, nm, contr_tabs[[nm]]) }
}
saveWorkbook(wb, file.path(out_dir, "Integrity_bundle.xlsx"), overwrite = TRUE)

# Plots (PNG) ------------------------------------------------------------------
ggsave(file.path(out_dir, "plot_counts_by_reason.png"),
       ggplot(counts, aes(x=reorder(ReasonCode, -count), y=count)) + geom_col() + labs(x=NULL, y="Count", title="Counts by ReasonCode") + theme_minimal(base_size = 12),
       width=7, height=4, dpi=150)
xtab_long <- df2 %>% mutate(judgement=tolower(`Screening judgement`)) %>% count(ReasonCode, judgement)
ggsave(file.path(out_dir, "plot_reason_x_judgement.png"),
       ggplot(xtab_long, aes(ReasonCode, judgement, fill=n)) + geom_tile() + geom_text(aes(label=n)) + labs(x=NULL,y=NULL,title="ReasonCode × Judgement") + theme_minimal(base_size = 12),
       width=6.5, height=4.5, dpi=150)

# --- Blinded re-coding pack (20%) + answer key --------------------------------
set.seed(42)

meta_doi_col <- intersect(c("DOI/URL","DOI link","DOI","URL"), names(df2))[1]
if (is.na(meta_doi_col)) meta_doi_col <- NULL
meta_cols <- unique(c("Title", meta_doi_col, intersect(c("Year","Venue"), names(df2))))

# Sample (stratified if available)
strata <- intersect(c("Year","Venue","Screening judgement"), names(df2))

sample_idx <- df2 |>
  dplyr::mutate(.rid = dplyr::row_number()) |>
  (\(x) if (length(strata)) dplyr::group_by(x, dplyr::across(dplyr::all_of(strata))) else x)() |>
  dplyr::sample_frac(size = 0.20, replace = FALSE) |>
  (\(x) if (dplyr::is_grouped_df(x)) dplyr::ungroup(x) else x)() |>
  dplyr::pull(.rid)

# Meta + empty recoding columns (NA)
pack <- df2[sample_idx, , drop=FALSE] |>
  dplyr::mutate(RandomID = sprintf("R%05d", sample(1e6, dplyr::n()))) |>
  dplyr::select(dplyr::all_of(c("RandomID", meta_cols)))

# Add recoding columns if missing, and fill them with NA
for (f in fields_struct) {
  if (!f %in% names(pack)) pack[[f]] <- NA_character_
}

pack <- pack |>
  dplyr::relocate(dplyr::all_of(fields_struct), .after = dplyr::last_col())

readr::write_csv(pack, file.path(out_dir, "recoding_pack_blinded.csv"))

# --- Answer key: same RandomID + actual values + judgement/score --------------
key_src <- df2[sample_idx, , drop = FALSE]

meta_extra <- intersect(c("Year","Venue"), names(key_src))

# Key base (RandomID + Title + potentially DOI/URL + Year/Venue)
key_out <- tibble::tibble(
  RandomID = pack$RandomID,
  Title    = key_src$Title
)
if (!is.null(meta_doi_col)) {
  key_out[[meta_doi_col]] <- key_src[[meta_doi_col]]
}
if (length(meta_extra)) {
  key_out <- dplyr::bind_cols(key_out, key_src[, meta_extra, drop = FALSE])
}

# Add structured fields (actual values)
struct_keep <- intersect(fields_struct, names(key_src))
if (length(struct_keep)) {
  key_out <- dplyr::bind_cols(key_out, key_src[, struct_keep, drop = FALSE])
}

# Add the judgement and numerical score
key_out <- dplyr::mutate(
  key_out,
  Screening_judgement = key_src$`Screening judgement`,
  score_num = suppressWarnings(as.numeric(gsub(",", ".", key_src$`Screening score`)))
)

readr::write_csv(key_out, file.path(out_dir, "recoding_answer_key.csv"))

message("Done. Outputs in: ", normalizePath(out_dir))

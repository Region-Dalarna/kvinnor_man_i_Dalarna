# Funktioner som används för att läsa in data för arbetslöshet från 2008 och framåt från arbetsförmedlingen

if (!require("pacman")) install.packages("pacman")
p_load(xml2, 
       purrr, 
       dplyr, 
       stringr, 
       curl)

read_pivot_cache <- function(path, cache_num = 1) {
  tmp <- tempfile()
  unzip(path, exdir = tmp)
  
  def_path <- file.path(tmp, "xl/pivotCache", paste0("pivotCacheDefinition", cache_num, ".xml"))
  rec_path <- file.path(tmp, "xl/pivotCache", paste0("pivotCacheRecords", cache_num, ".xml"))
  
  def <- read_xml(def_path)
  rec <- read_xml(rec_path)
  ns <- xml_ns(def)
  
  all_fields <- xml_find_all(def, ".//d1:cacheField", ns)
  db_field_attr <- xml_attr(all_fields, "databaseField")
  is_db_field <- is.na(db_field_attr) | db_field_attr == "1"
  
  db_fields <- all_fields[is_db_field]
  field_names <- xml_attr(db_fields, "name")
  
  shared_items <- map(db_fields, function(f) {
    items <- xml_find_all(f, ".//d1:sharedItems/*", ns)
    xml_attr(items, "v")
  })
  
  records <- xml_find_all(rec, ".//d1:r", ns)
  
  row_list <- map(records, function(r) {
    cells <- xml_children(r)
    map_chr(seq_along(cells), function(i) {
      cell <- cells[[i]]
      tag <- xml_name(cell)
      if (tag == "x") {
        idx <- as.integer(xml_attr(cell, "v")) + 1
        shared_items[[i]][idx]
      } else if (tag == "m") {
        NA_character_
      } else {
        xml_attr(cell, "v")
      }
    })
  })
  
  df <- as.data.frame(do.call(rbind, row_list), stringsAsFactors = FALSE)
  names(df) <- make.unique(field_names)   # make.unique as a safety net too
  df
}

get_andel_data <- function(path, cache_num = 1) {
  
  calc_fields <- tribble(
    ~name,           ~formula,
    "Totalt",        "TOTSOK/TOTAK",
    "Kvinnor",       "KVISOK/KVIAK",
    "Män",           "MANSOK/MANAK",
    "Unga 18-24",    "UNGSOK/UNGAK",
    "Unga KVI",      "UNGKSOK/UNGKAK",
    "Unga MÄN",      "UNGMSOK/UNGMAK",
    "Äldre 55-64",   "GAMSOK/GAMAK",
    "Äldre KVI",     "GAMKSOK/GAMKAK",
    "Äldre MÄN",     "GAMMSOK/GAMMAK",
    "Inrikesfödda",  "INRSOK/INRAK",
    "Inrikes KVI",   "INRKSOK/INRKAK",
    "Inrikes MÄN",   "INRMSOK/INRMAK",
    "Utrikesfödda",  "UTRSOK/UTRAK",
    "Utrikes KVI",   "UTRKSOK/UTRKAK",
    "Utrikes MÄN",   "UTRMSOK/UTRMAK",
    "Förgymn",       "FGSOK/FGAK",
    "Fg KVI",        "FGKSOK/FGKAK",
    "Fg MÄN",        "FGMSOK/FGMAK",
    "Gymn",          "GYSOK/GYAK",
    "Gy KVI",        "GYKSOK/GYKAK",
    "Gy MÄN",        "GYMSOK/GYMAK",
    "Eftergymn",     "EGSOK/EGAK",
    "Eg KVI",        "EGKSOK/EGKAK",
    "Eg MÄN",        "EGMSOK/EGMAK"
  ) %>%
    mutate(
      numerator   = str_trim(str_extract(formula, "^[^/]+")),
      denominator = str_trim(str_extract(formula, "(?<=/).+$"))
    )
  
  antal_df <- read_pivot_cache(path, cache_num = cache_num)
  
  num_cols <- setdiff(names(antal_df), c("PERIOD", "LAN", "KOM"))
  antal_df <- antal_df %>%
    mutate(across(all_of(num_cols), as.numeric))
  
  # build a "whole län" row per PERIOD/LAN by summing the raw counts across kommuner,
  # BEFORE any division happens — KOM is set to the län name itself
  lan_totals <- antal_df %>%
    group_by(PERIOD, LAN) %>%
    summarise(across(all_of(num_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    mutate(KOM = LAN) %>%
    relocate(PERIOD, LAN, KOM)
  
  antal_df <- bind_rows(antal_df, lan_totals)
  
  # now compute Andel as before, on both kommun-level and län-level rows together
  andel_df <- antal_df
  
  for (i in seq_len(nrow(calc_fields))) {
    new_col <- calc_fields$name[i]
    num_col <- calc_fields$numerator[i]
    den_col <- calc_fields$denominator[i]
    
    vals <- andel_df[[num_col]] / andel_df[[den_col]]
    vals[is.infinite(vals)] <- NA
    andel_df[[new_col]] <- vals
  }
  
  andel_df %>%
    select(PERIOD, LAN, KOM, all_of(calc_fields$name)) %>%
    mutate(
      year  = as.integer(substr(PERIOD, 1, 4)),
      month = as.integer(substr(PERIOD, 6, 7))
    ) %>%
    arrange(year, month, LAN, KOM) %>%
    select(-year, -month)
}

# combine an "old" and "new" source, using old only up to cutoff, new from cutoff onward
combine_old_new <- function(old_df, new_df, cutoff = "2022-12") {
  old_df <- old_df %>% filter(PERIOD <= cutoff)
  new_df <- new_df %>% filter(PERIOD > cutoff)
  
  bind_rows(old_df, new_df) %>%
    mutate(
      year  = as.integer(substr(PERIOD, 1, 4)),
      month = as.integer(substr(PERIOD, 6, 7))
    ) %>%
    arrange(year, month, LAN, KOM) %>%
    select(-year, -month)
}

process_url <- function(url) {
  tmpfile <- tempfile(fileext = ".xlsx")
  curl_download(url, tmpfile)
  get_andel_data(tmpfile, cache_num = 1)
}
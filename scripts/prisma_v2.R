current_total <- search_details %>%
  select(total_included) %>%
  sum()

previous_total <- search_details %>%
  filter(review_version %in% previous_review_versions) %>%
  select(total_included) %>%
  sum()

a <- search_details %>%
  group_by(review_version) %>%
  filter(review_version == current_review_version) %>%
  mutate(title_abstract = sum(ovid_number_results, medrxiv_number_results, other_source_included),
         full_texts_assessed = sum(ovid_screened, medrxiv_screened, other_source_included),
         title_abstract_included = sum(ovid_included, medrxiv_included, other_source_included),
         title_abstract_excluded = title_abstract - full_texts_assessed,
         full_text_excluded = full_texts_assessed-total_included,
         previous_total = previous_total,
         previous_minus_superseded = previous_total-superseded,
         current_total = current_total) %>%
  select("Review version" = review_version,
         "Date Screened" = date_screened,
         "OVID results" = "ovid_number_results",
         "medRxiv results" = "medrxiv_number_results",
         "Other sources" = "other_source_included",
         "Title/abstracts screened" = "title_abstract",
         "Title/abstracts excluded" = "title_abstract_excluded",
         "Full texts assessed" = "full_texts_assessed",
         "Full texts excluded" = "full_text_excluded",
         "Records carried forward" = "previous_minus_superseded",
         "Supersed from previous version" = superseded,
         "Studies in narrative synthesis" = "current_total")

flextable(a)

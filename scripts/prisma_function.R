#PRISMA function

PRISMA <- function(search_details) {
PRISMA <- search_details %>%
    filter(review_version == current_version)
  OVID_records <- PRISMA[1,2]
  medRxiv_records <- PRISMA[1,6]
  other_records <- PRISMA[1,9]
  OVID_screened <- PRISMA[1,3]
  medRxiv_screened <- PRISMA[1,7]
  OVID_included <- PRISMA[1,5]
  medRxiv_included <- PRISMA[1,8]
  others_included <- PRISMA[1,9]
  
  top_row <- as_vector(c(OVID_records, medRxiv_records, other_records))
  total_title_abstracts <- tibble(sum(top_row))
  colnames(total_title_abstracts) <- 'titles_screened'
  abstract_screened <- tibble(sum(OVID_screened, medRxiv_screened, other_records))
  colnames(abstract_screened) <- 'abstract_screened'
  excluded_abstract <- tibble(sum(top_row) - sum(abstract_screened))
  colnames(excluded_abstract) <- 'excluded_abstract'
  full_texts <- tibble(sum(top_row) - sum(excluded_abstract))
  colnames(full_texts) <- 'full_texts_reviewed'
  included_full_text <- as_vector(c(OVID_included, medRxiv_included, others_included))
  excluded_full_text <- tibble(full_texts - sum(included_full_text))
  colnames(excluded_full_text) <- 'full_texts_excluded'
  narrative_synthesis_v <- tibble(sum(included_full_text))
  colnames(narrative_synthesis_v) <- 'full_texts_included_narrative'
  narrative_synthesis_total <- tibble(sum(from_prev_version + narrative_synthesis_v))
  colnames(narrative_synthesis_total) <- 'total_all_versions'
  prisma_list <- c(top_row, total_title_abstracts, excluded_abstract, full_texts, excluded_full_text, narrative_synthesis_v, narrative_synthesis_total)
  return(prisma_list)
}


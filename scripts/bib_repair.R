library(rbbt)
library(xciter)
library(ngr)

# could get into this type thing but will skip for now and hand bomb a few
# the moving target of the yearly changes to refs like bcfishobs and the provincial layers should prob
# be dealt with by duplicating updated refs and reverting the copies to the old years so that we keep accurate
# https://github.com/NewGraphEnvironment/restoration_wedzin_kwa_2024/blob/main/scripts/bib_repair.R

# this is for the existing bib file
path_bib <- "references.bib"


# check for missing keys and grab them manually (note that there are a few in the packages file....)
keys_missing <- xciter::xct_bib_keys_missing(
  path_bib = path_bib,
  citations = rbbt::bbt_detect_citations(
    list.files(
      pattern = "*.Rmd")
  )
)




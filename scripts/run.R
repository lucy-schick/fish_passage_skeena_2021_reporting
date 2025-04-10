##this is for as we work through
preview_chapter('0100-intro.Rmd')



#################################################################################################
##go to the index.Rmd and change gitbook_on <- TRUE
#################################################################################################


# run if bcfishpass changed
source('scripts/02_prep_reporting/0190-build-html-map-tables.R')

# if you need to update after renameing photos to keep and add to map....
source('scripts/02_prep_reporting/0180-photos-extract-metadata.R')
## move the file over from the 2023 repo, you must have the 2023 Skeena repo cloned to your computer.
# only needs to be done if the file changed
fs::file_copy("~/Projects/repo/fish_passage_skeena_2023_reporting/data/habitat_confirmations.xls",
              "data/2023/habitat_confirmations.xls", overwrite = TRUE)

{
  source('scripts/functions.R')
  staticimports::import()
  source('scripts/staticimports.R')
  my_news_to_appendix()
  # These files are included in the gitbook version already so we move them out of the build
  files_to_move <- list.files(pattern = ".Rmd$") |>
    stringr::str_subset('2300', negate = F) #move the attachments out
  files_destination <- paste0('hold/', files_to_move)

  ##move the files
  mapply(file.rename, from = files_to_move, to = files_destination)

  rmarkdown::render_site(output_format = 'bookdown::gitbook',
                         encoding = 'UTF-8')

  ##move the files from the hold file back to the main file
  mapply(file.rename, from = files_destination, to = files_to_move)
}



# pdf version

#################################################################################################
##go to the index.Rmd and change gitbook_on <- FALSE
#################################################################################################
##move the phase 1 appendix out of the main directory to a backup file or else the file is too big



# define the _bookfile_name from _bookdown.yml
filename_html <- 'Skeena2021'

{

  file.rename('0600-appendix.Rmd', 'hold/0600-appendix.Rmd')

  ##   then make our printable pdf
  rmarkdown::render_site(output_format = 'pagedown::html_paged',
                         encoding = 'UTF-8')

  #move the phase 1 appendix back to main directory
  file.rename('hold/0600-appendix.Rmd', '0600-appendix.Rmd')

  # print to pdf
  pagedown::chrome_print(
    paste0(filename_html, '.html'),
    output = paste0('docs/', filename_html, '.pdf'),
    timeout = 180,
  )

  # reduce the size
  tools::compactPDF(paste0("docs/", filename_html, ".pdf"),
                    gs_quality = 'screen',
                    ##this was on the windows machine
                    # gs_cmd = "C:/Program Files/gs/gs9.56.1/bin/gswin64.exe"
                    gs_cmd = "opt/homebrew/bin/gs"
                    )

  # get rid of the html as its too big and not needed
  file.remove(paste0(filename_html, '.html'))

}



##########################################make Phase 1 appendix seperately only when updated
#################################################################################################
##we need a workflow to print the Phase 1 attachment


# lets install pdftools here since it is throwing error on us with build

# pacman::p_load('pdftools')
# pacman::p_load('rayshader')


files_to_move <- list.files(pattern = ".Rmd$") %>%
  stringr::str_subset(., 'index|Skeena2021|0600', negate = T)
files_destination <- paste0('hold/', files_to_move)

##move the files
mapply(file.rename, from = files_to_move, to = files_destination)


##   then make our printable pdf
rmarkdown::render_site(output_format = 'pagedown::html_paged', encoding = 'UTF-8')

##  move it to the docs folder so that it can be in the same place as the report
# file.rename('Elk2021.html', 'docs/Attachment_3_Phase_1_Data_and_Photos.html')

##move the files from the hold file back to the main file
mapply(file.rename, from = files_destination, to = files_to_move)

#print the attachment to pdf with chrome print
# openHTML('docs/Attachment_3_Phase_1_Data_and_Photos_prep.html')

pagedown::chrome_print(
  paste0(getwd(),'/', filename_html,'.html'),
  output = paste0(getwd(),'/docs/Attachment_2_prep.pdf'),
  timeout = 120
)


##now get rid of the first 10 pages
length <- pdftools::pdf_length(paste0(getwd(), "/docs/Attachment_2_prep.pdf"))

# this changes so let's define
crop_this_many_pages <- 6

# trim up the file.  We ditch the last page only when there are references.  In the case of the bulkley there are due to the yaml file
pdftools::pdf_subset(paste0(getwd(), "/docs/Attachment_2_prep.pdf"),
           pages = (crop_this_many_pages + 1):(length - 1), output = paste0(getwd(), "/docs/Attachment_2.pdf"))

##clean out the old files
file.remove(paste0(getwd(), "/docs/Attachment_2_prep.pdf"))
file.remove(paste0(getwd(),'/', filename_html,'.html'))

##it is very important to shrink the size of the pdf so we donèt blow up our github repo.
# We will set up ghostscript on the mac but for now we do manually.

tools::compactPDF(paste0(getwd(), "/docs/Attachment_2.pdf"),
                  gs_quality = 'ebook',
                  # gs_cmd = "C:/Program Files/gs/gs9.56.1/bin/gswin64.exe"
                  gs_cmd = "opt/homebrew/bin/gs"
                  )


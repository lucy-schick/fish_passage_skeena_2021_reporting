# fish_passage_skeena_2021_reporting 0.0.8 
* 20220427
* add Thompson
* add Maxan trib
* reduce pdfs with ghostscript in `scripts/run.R`.  Requires GS install and point to `exe` file. 
* add Attachments to TOC
* add placeholders for multiple crossings upstream on Station Creek
* add placeholders for 197909 - Trib to Maxan
* add placeholder for 
* move Mcbride and Owen to the git ignored hold folder to clean up reporting review for CWF


# fish_passage_skeena_2021_reporting 0.0.7

* add fish sampling summary data
* add Taman and Richfield

# fish_passage_skeena_2021_reporting 0.0.6

* stopped tracking the fisheries submission template in repo and started making backups in `data/backups` since it is .xls and binary.  Used `git update-index --assume-unchanged data/habitat_confirmations.xls` (#7) .  When changes actually occur we use `git update-index --no-assume-unchanged data/habitat_confirmations.xls` to log a commit


# fish_passage_skeena_2021_reporting 0.0.9.1


* 20220502
* fpr tag v.20220502b  and version v.0.1.0.9004
* point to 2022 pdf maps
* add Morice sites to facilitate discussions
* add Attachment 4 - Richfield Fencing report
* add track for Ailport
* Include Thompson hwy 16 crossing as hab con 
* add `barrier_status` to `priorities.json` file for mapping


# fish_passage_skeena_2021_reporting 0.0.9 

* add reference to Thompson for historic restoration recommendations
* update Station Creek with upstream culverts
* change name of Mission Creek to Station Creek for consistency with FWA



# fish_passage_skeena_2021_reporting 0.0.8 

* 20220427
* `fpr` tag v.20220427 and v.0.1.0.9002
* add Thompson
* add Maxan trib
* reduce pdfs with ghostscript in `scripts/run.R`.  Requires GS install and point to `exe` file. 
* add Attachments to TOC
* add placeholders for multiple crossings upstream on Station Creek
* add placeholders for 197909 - Trib to Maxan
* add placeholder for 
* move Mcbride and Owen to the git ignored hold folder to clean up reporting review for CWF
* stopped tracking the pscis submission templates in repo unless changes actually happen. Used `git update-index --assume-unchanged data/pscis_*` (#7) .  When changes actually occur we use `git update-index --no-assume-unchanged data/pscis_*.xlsm` to log a commit


# fish_passage_skeena_2021_reporting 0.0.7

* add fish sampling summary data
* add Taman and Richfield

# fish_passage_skeena_2021_reporting 0.0.6

* stopped tracking the fisheries submission template in repo and started making backups in `data/backups` since it is .xls and binary.  Used 
`git update-index --assume-unchanged data/habitat_confirmations.xls` 
(#7) .  When changes actually occur we use 
`git update-index --no-assume-unchanged data/habitat_confirmations.xls` 
to log a commit


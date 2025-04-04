# fish_passage_skeena_2021_reporting 0.1.8 (20250328)
- Update Tributary to Owen Creek - 197378 on Klate Lake Road with updated background, map and engineering design 
- Engineering design added as attachment
- Switch to white layout

# fish_passage_skeena_2021_reporting 0.1.7 (20240625)

- bump cost estimates to 30k/m minimum
- add Changelog
- UPdate 197378 - Trib to Owen (Klate Lake Road) with fish sampling data


# fish_passage_skeena_2021_reporting 0.1.6

* 20240513
* Updated site memos for Thompson Creek (123377) with fish sampling data and photos from 2023
* add NEWS.md to the report with `news_to_appendix()` function

# fish_passage_skeena_2021_reporting 0.1.5

* 20230501
* added site memos for nanika (198008), thautil (198016), and lamprey (198064)

# fish_passage_skeena_2021_reporting 0.1.4

  * 20230321
  * clean up Thompson Creek, add detail and lidar as per https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/issues/18

# fish_passage_skeena_2021_reporting 0.1.3

  * bump to fpr v1.1.0.  
  * Correct `fpr_my_channel_sentence` which was referenceing `my_site` for all calls to `average_gradient`

# fish_passage_skeena_2021_reporting 0.1.2

  * Build with Mac M1.  Everything seemed to compile fine.
  * included `pacman::p_load` option to run on `source('scripts/packages.R') call to simplify setup.



# fish_passage_skeena_2021_reporting 0.1.1
 * 20220511
 * add Cesford upstream of top site



# fish_passage_skeena_2021_reporting 0.1.0

 * 20220511
 * Address comments from CWF review.  Please see issue [#15](https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/issues/)


# fish_passage_skeena_2021_reporting 0.0.9.2

* fpr version `v0.1.0.9005` 
* add Peacock
* burn `hab_fish_collect` with all species for each site in one column.  Test first with `hab_fish_collect2`
* fix issue with commas from `fpr_my_habitat_paragraph()` 




# fish_passage_skeena_2021_reporting 0.0.9.1


* 20220502
* fpr tag v.20220502b  and version v.0.1.0.9004
* point to 2022 pdf maps wihtin an archived folder
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


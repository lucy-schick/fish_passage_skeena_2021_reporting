# fish_passage_skeena_2021_reporting 0.0.6

* stopped tracking the fisheries submission template in repo and started making backups in `data/backups` since it is .xls and binary.  Used `git update-index --assume-unchanged data/habitat_confirmations.xls` (#7) .  When changes actually occure we use `git update-index --no-assume-unchanged data/habitat_confirmations.xls` to log a commit
* add fish sampling summary data
* add Taman and Richfield

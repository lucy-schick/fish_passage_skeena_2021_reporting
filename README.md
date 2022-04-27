The repository builds an [interactive report](https://newgraphenvironment.github.io/fish_passage_skeena_2021_reporting/) documenting planning, fish passage assessments and habitat confirmation assessments at road-stream crossings within the Bulkley River and Morice River watershed groups in the summer/fall of 2021. The report will also provide planning inputs for fish passage/fisheries assessments as well as restoration activities in both watersheds in coming years.

<br>

Reporting was generated with `bookdown` [@bookdown2016] from `Rmarkdown` [@R-rmarkdown] with primarily `R` and `SQL` scripts. The `R` package [`fpr`](https://github.com/NewGraphEnvironment/fpr) contains many specialized custom functions related to the work.  In addition to numerous spatial layers sourced through the BC Data Catalogue then stored and queried in a local `postgresql` and `sqlite` databases [data inputs](https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/tree/master/data) for this project include: 

 + Populated [Fish Data Submission Spreadsheet Template - V 2.0, January 20, 2020 ](https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/fish-and-fish-habitat-data-information/fish-data-submission/submit-fish-data#submitfish) 

 + Populated [pscis_assessment_template_v24.xls](https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/aquatic-habitat-management/fish-passage/fish-passage-technical/assessment-projects)
 
 
 + [`bcfishpass`](https://github.com/smnorris/bcfishpass) outputs.


 + [Custom CSV file](https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/blob/master/data/habitat_confirmations_priorities.csv) detailing Phase 2 site:
     - priority level for proceeding to design for replacement
     - length of survey upstream and downstream
     - a conservative estimate of the linear length of mainstem habitat potentially available upstream of the crossing 
     - fish species confirmed as present upstream of the crossing

 + [GPS tracks](https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/tree/master/data/habitat_confirmation_tracks.gpx) from field surveys.  

 + [Photos](https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/tree/master/data/photos) and photo metadata

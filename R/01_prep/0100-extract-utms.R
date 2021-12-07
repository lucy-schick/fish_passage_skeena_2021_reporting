source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

##make a dataframe to pull info from the db
##we should probably break each row out and determine the crs by the utm_zone attribute
##lets do both phases at once to create a file for feeding back to bcfishpass


##this is weird but we know these will be dups because we check at the end of this script.
##lets pull these out of these files at the start

# dups <- c(4600183, 4600069, 4600367, 4605732, 4600070)

dat1 <- import_pscis(workbook_name = 'pscis_phase1.xlsm') %>%
  tibble::rownames_to_column()
  # filter(!my_crossing_reference %in% dups)

dat2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm') %>%
  tibble::rownames_to_column()

dat3 <- import_pscis(workbook_name = 'pscis_reassessments.xlsm') %>%
  tibble::rownames_to_column()

dat <- bind_rows(
  dat1,
  dat2,
  dat3
)
  # distinct(.keep_all = T)
  # sf::st_as_sf(coords = c("easting", "northing"),
  #              crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
  # sf::st_transform(crs = 3005) ##convert to match the bcfishpass format


##get the utm info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname_wsl,
  host = host_wsl,
  port = port_wsl,
  user = user_wsl,
  password = password_wsl
)
#
# ##listthe schemas in the database
# dbGetQuery(conn,
#            "SELECT schema_name
#            FROM information_schema.schemata")
# #
# #
# # # ##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='bcfishpass'")
# # # # #
# # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='modelled_stream_crossings'")



##pull out the details for the crossings that match the modelled ids in our dat
##derive coordinates useing sf
##burn csvs for each of the input files so we can copy and paste back in

##isolate the id's that we want info for
id <- dat %>%
  # filter(is.na(easting)) %>%
  pull(my_crossing_reference)

# ##this is for phase 1
# sql <- glue::glue_sql("SELECT * FROM bcfishpass.modelled_stream_crossings x WHERE x.modelled_crossing_id IN ({id*})",
#                       .con = conn)


##we need to tweak it a bit for the phase 2
##we are using the pscis model combined layer from way back but will work for now
sql <- glue::glue_sql("SELECT x.*, ST_X(ST_TRANSFORM(x.geom, 26909)) as utm_easting, ST_Y(ST_TRANSFORM(x.geom, 26909)) as utm_northing FROM bcfishpass.modelled_stream_crossings x WHERE x.modelled_crossing_id IN ({id*})",
                      .con = conn)


query <- DBI::dbSendQuery(conn, sql)
df <- DBI::dbFetch(query)
dbClearResult(query)

id_joined <- left_join(
  dat %>% select(rowname, my_crossing_reference, source, easting, northing),
  df %>% select(modelled_crossing_id, utm_easting, utm_northing),
  by = c('my_crossing_reference' = 'modelled_crossing_id')
) %>%
  mutate(utm_easting = case_when(!is.na(easting) ~ easting,
         T ~ utm_easting),
         utm_northing = case_when(!is.na(northing) ~ northing,
         T ~ utm_northing)
  )


##now export csvs for each of the sources
id_joined %>%
  filter(source %like% 'phase1') %>%
  write_csv("data/inputs_extracted/temp/utms_modelled_phase1.csv")

id_joined %>%
  filter(source %like% 'phase2') %>%
  write_csv("data/inputs_extracted/temp/utms_modelled_phase2.csv")

id_joined %>%
  filter(source %like% 'reassessments') %>%
  write_csv("data/inputs_extracted/temp/utms_modelled_reassessments.csv")














##now join our ids to the utm info - phase 1
id_joined <- left_join(
  as_tibble(id),
  select(df,
         crossing_id, utm_easting, utm_northing),
  by = c('value' = 'crossing_id'))

# ##now join our ids to the utm info - phase 2
# id_joined <- left_join(
#   as_tibble(id),
#   select(df,
#          pscis_stream_crossing_id, utm_easting, utm_northing),
#   by = c('value' = 'pscis_stream_crossing_id'))

##join it back to the original spreadsheet and fill in the easting northing columns where not already filled
##burn to a csv so you can cut and paste into your spreadsheet
utms <- left_join(
  pscis,
  id_joined,
  # by = c('my_crossing_reference' = 'value') ##this is for phase 1
  by = c('my_crossing_reference' = 'value') ##this is for phase 2
) %>%
  mutate(easting = case_when(is.na(easting) ~ utm_easting,
                             T ~ easting),
         northing = case_when(is.na(northing) ~ utm_northing,
                              T ~ northing))  %>%
  select(my_crossing_reference, easting, northing) %>% ##this works for both phases although my_crossing_reference is mislabeled and should be pscis_crossing_id
  write_csv("data/utms_modelled.csv")  ##burn to a csv so you can cut and paste into your spreadsheet


##always disconnect from the database
dbDisconnect(conn = conn)













##burn it all to a file we can input to pscis submission spreadsheet
# pscis_reassessmeents_rd_tenure %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis_reassessmeents_rd_tenure.csv'))
##we need to qa which are our modelled crossings at least for our phase 2 crossings




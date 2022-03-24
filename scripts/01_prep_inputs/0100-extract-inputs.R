# source('scripts/functions.R')
source('scripts/private_info.R')
source('scripts/packages.R')


##make a dataframe to pull info from the db
##we should probably break each row out and determine the crs by the utm_zone attribute
##lets do both phases at once to create a file for feeding back to bcfishpass


pscis_list <- fpr_import_pscis_all()
pscis_phase1 <- pscis_list %>% pluck('pscis_phase1')
pscis_phase2 <- pscis_list %>% pluck('pscis_phase2')
pscis_reassessments <- pscis_list %>% pluck('pscis_reassessments')
pscis_all <- bind_rows(pscis_list)
# n_distinct(pscis_all$aggregated_crossings_id)

# test for dupicate sites in every input
ls <- pscis_all %>%
  dplyr::group_split(source) %>%
  purrr::set_names(nm = unique(pscis_all$source))

dups <- ls %>%
  purrr::map(janitor::get_dupes, aggregated_crossings_id)

dat <- pscis_all %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
  sf::st_transform(crs = 3005) ##get the crs same as the layers we want to hit up


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
           WHERE table_name='crossings'")




# UTMs --------------------------------------------------------------------



##pull out the details for the crossings that match the modelled ids in our dat
##derive coordinates useing sf
##burn csvs for each of the input files so we can copy and paste back in

##isolate the id's that we want info for
id <- dat %>%
  # filter(is.na(easting)) %>%
  pull(my_crossing_reference)

# ##this is for phase 1
sql <- glue::glue_sql("SELECT x.*, ST_X(ST_TRANSFORM(x.geom, 26909)) as utm_easting_derived, ST_Y(ST_TRANSFORM(x.geom, 26909)) as utm_northing_derived FROM bcfishpass.crossings x WHERE x.modelled_crossing_id IN ({id*})",
                      .con = conn)


query <- DBI::dbSendQuery(conn, sql)
df <- DBI::dbFetch(query)
dbClearResult(query)

id_joined <- left_join(
  dat %>% select(rowid, pscis_crossing_id, my_crossing_reference, source, easting, northing),
  df %>% select(modelled_crossing_id, utm_easting_derived, utm_northing_derived),
  by = c('my_crossing_reference' = 'modelled_crossing_id')
) %>%
  mutate(utm_easting = case_when(!is.na(easting) ~ easting,
         T ~ utm_easting_derived),
         utm_northing = case_when(!is.na(northing) ~ northing,
         T ~ utm_northing_derived)
  ) %>%
  select(-utm_easting_derived, -utm_northing_derived)





id <- dat %>%
  # filter(is.na(easting)) %>%
  pull(pscis_crossing_id)


##we need to tweak it a bit for the phase 2
##we are using the pscis model combined layer from way back but will work for now
sql <- glue::glue_sql("SELECT x.*, ST_X(ST_TRANSFORM(x.geom, 26909)) as utm_easting_derived, ST_Y(ST_TRANSFORM(x.geom, 26909)) as utm_northing_derived FROM bcfishpass.crossings x WHERE x.stream_crossing_id IN ({id*})",
                      .con = conn)

query <- DBI::dbSendQuery(conn, sql)
df2 <- DBI::dbFetch(query)
dbClearResult(query)

id_joined2 <- left_join(
  id_joined,
  df2 %>% select(stream_crossing_id, utm_easting_derived, utm_northing_derived),
  by = c('pscis_crossing_id' = 'stream_crossing_id')
) %>%
  mutate(utm_easting = case_when(!is.na(utm_easting) ~ utm_easting,
                                 T ~ utm_easting_derived),
         utm_northing = case_when(!is.na(utm_northing) ~ utm_northing,
                                  T ~ utm_northing_derived)
  )%>%
  select(-utm_easting_derived, -utm_northing_derived)


##burn------------------------------------------------------------
##now export csvs for each of the sources
id_joined %>%
  filter(source %like% 'phase1') %>%
  write_csv("data/inputs_extracted/temp/utms_modelled_phase1.csv")


id_joined2 %>%
  filter(source %like% 'phase2')  %>%
  write_csv("data/inputs_extracted/temp/utms_modelled_phase2.csv")

id_joined2 %>%
  filter(source %like% 'reassessments') %>%
  write_csv("data/inputs_extracted/temp/utms_modelled_reassessments.csv")


##always disconnect from the database
dbDisconnect(conn = conn)


# Road Tenure -------------------------------------------------------------

pscis_all_sf <- dat

##get the road info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
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
           WHERE table_name='modelled_crossings_closed_bottom'")

dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='crossings'")


# test <- dbGetQuery(conn, "SELECT * FROM bcfishpass.waterfalls")

# add a unique id - we could just use the reference number
pscis_all_sf$misc_point_id <- seq.int(nrow(pscis_all_sf))

# dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "test_hack",";"))
# load to database
sf::st_write(obj = pscis_all_sf, dsn = conn, Id(schema= "ali", table = "misc"))



# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON ali.misc USING GIST (geometry)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE ali.misc ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

dat_info <- dbGetQuery(conn, "SELECT
  a.misc_point_id,
  b.*,
  ST_Distance(ST_Transform(a.geometry,3005), b.geom) AS distance
FROM
  ali.misc AS a
CROSS JOIN LATERAL
  (SELECT *
   FROM fish_passage.modelled_crossings_closed_bottom
   ORDER BY
     a.geometry <-> geom
   LIMIT 1) AS b")


##swapped out fish_passage.modelled_crossings_closed_bottom for bcfishpass.barriers_anthropogenic

##join the modelling data to our pscis submission info
dat_joined <- left_join(
  select(pscis_all_sf, misc_point_id, pscis_crossing_id, my_crossing_reference, source), ##traded pscis_crossing_id for my_crossing_reference
  dat_info,
  by = "misc_point_id"
) %>%
  mutate(downstream_route_measure = as.integer(downstream_route_measure))


dbDisconnect(conn = conn)


##we also need to know if the culverts are within a municipality so we should check
##get the road info from our database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname_wsl,
  host = host_wsl,
  port = port_wsl,
  user = user_wsl,
  password = password_wsl
)

# load to database
sf::st_write(obj = pscis_all_sf, dsn = conn, Id(schema= "working", table = "misc"))

dat_info <- dbGetQuery(conn,
                       "

                                  SELECT a.misc_point_id, b.admin_area_abbreviation, c.map_tile_display_name
                                  FROM working.misc a
                                  INNER JOIN
                                  whse_basemapping.dbm_mof_50k_grid c
                                  ON ST_Intersects(c.geom, ST_Transform(a.geometry,3005))
                                  LEFT OUTER JOIN
                                  whse_legal_admin_boundaries.abms_municipalities_sp b
                                  ON ST_Intersects(b.geom, ST_Transform(a.geometry,3005))
                       ")

dbDisconnect(conn = conn)

##add the municipality info
dat_joined2 <- left_join(
  dat_joined,
  dat_info,
  by = "misc_point_id"
)

# ##clean up the workspace
rm(dat_info, dat_joined, res)
#

##this no longer works because we were using the fish_passage.modelled_crossings_closed_bottom and now we don't have the rd info
##make a tibble of the client names so you can summarize in the report
##we do not need to repeat this step but this is how we make a dat to paste into a kable in rmarkdown then paste tibble as a rstudio addin so we can
##populate the client_name_abb...

##we already did this but can do it again I guess.  you cut and paste the result into kable then back
##into here using addin for datapasta
# tab_rd_tenure_xref <- unique(dat_joined2$client_name) %>%
#   as_tibble() %>%
#   purrr::set_names(nm = 'client_name') %>%
#   mutate(client_name_abb = NA)

tab_rd_tenure_xref <- tibble::tribble(
                                           ~client_name, ~client_name_abb,
                                                     NA,               NA,
                        "DISTRICT MANAGER NADINA (DND)",       "FLNR DND",
                        "CANADIAN FOREST PRODUCTS LTD.",         "Canfor",
                               "WEST FRASER MILLS LTD.",    "West Fraser",
                                       "CHARLES PRIEST", "Charles Priest",
                         "SOLID GROUND CONTRACTING LTD",   "Solid Ground",
                               "CHINOOK COMFOR LIMITED", "Chinook Comfor"
                        )

##add that to your dat file for later
dat_joined3 <- left_join(
  dat_joined2,
  tab_rd_tenure_xref,
  by = 'client_name'
)

##make a dat to make it easier to see so we can summarize the road info we might want to use
dat_joined4 <- dat_joined3 %>%
  mutate(admin_area_abbreviation = case_when(
    is.na(admin_area_abbreviation) & (road_class %ilike% 'arterial' | road_class %ilike% 'local') ~ 'MoTi',
    T ~ admin_area_abbreviation),
    admin_area_abbreviation = replace_na(admin_area_abbreviation, ''),
    my_road_tenure =
      case_when(!is.na(client_name_abb) ~ paste0(client_name_abb, ' ', forest_file_id),
                !is.na(road_class) ~ paste0(admin_area_abbreviation, ' ', stringr::str_to_title(road_class)),
                !is.na(owner_name) ~ owner_name)) %>%
  mutate(my_road_tenure =
           case_when(distance > 100 ~ 'Unknown',  ##we need to get rid of the info for the ones that are far away
                     T ~ my_road_tenure)) %>%
  rename(geom_modelled_crossing = geom) %>%
  mutate(
    my_road_tenure =stringr::str_trim(my_road_tenure),
    aggregated_crossings_id = case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                                        my_crossing_reference > 200000000 ~ my_crossing_reference,
                                        T ~ my_crossing_reference + 1000000000)) %>%
  sf::st_drop_geometry()

##we cannot use base R to add a column named 'source' so we choose a different name
col_new <- pscis_all_sf$source
dat_joined4$source_wkb <- col_new


##build tables to populate the pscis spreadsheets
pscis1_rd_tenure <- left_join(
  select(pscis_phase1, rowid, my_crossing_reference, road_tenure),
  dat_joined4 %>% filter(source_wkb %ilike% 'phase1') %>% select(my_crossing_reference, my_road_tenure),
  by = 'my_crossing_reference'
) %>%
  # for some reason there is a duplicate. not messing withi it
  distinct(rowid, .keep_all = T)




##burn it all to a file we can input to pscis submission spreadsheet
pscis1_rd_tenure %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_tenure_pscis1.csv'),
                   na = '')



pscis_reassessments_rd_tenure <- left_join(
  select(pscis_reassessments, rowid, pscis_crossing_id, road_tenure),
  dat_joined4 %>% filter(source_wkb %ilike% 'reassess') %>% select(pscis_crossing_id, my_road_tenure),
  by = 'pscis_crossing_id'
)


## burn --------------------------------------------------------------------

##burn it all to a file we can input to pscis submission spreadsheet
pscis_reassessments_rd_tenure %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_tenure_reassessments.csv'),
                   na = '')
##we need to qa which are our modelled crossings at least for our phase 2 crossings

pscis2_rd_tenure <- left_join(
  select(pscis_phase2, rowid, aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, road_tenure),
  dat_joined4 %>% filter(source_wkb %ilike% 'phase2') %>% select(aggregated_crossings_id, my_road_tenure),
  by = 'aggregated_crossings_id'
)


##burn it all to a file we can input to pscis submission spreadsheet
pscis2_rd_tenure %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_tenure_pscis2.csv'),
                   na = '')



# structure-size-type -----------------------------------------------------



##assign a value that we want to call standard fill
fill_dpth <- 3

##assign a multiplier to determine the length of a bridge above the standard
##that you get when you go deeper
##standard bridge width - we go with 12 b/c not seeing 10s in the field
brdg_wdth <- 10

## define a channel width where the bridge should start to be more than  brdg_wdth much larger than the width
# we are going to arbitrarily choose 5 m extra width
chn_wdth_max <- brdg_wdth - 5

##fill depth multiplier
##for every 1 m deeper than 3m, we need a 1.5:1 slope so there is 3m more bridge required
fill_dpth_mult <- 3

##according to the moe specs in MoE 2011 - backwatering requires od<30 and slope <2, swr <1.2 see if there are options
tab_backwater <- pscis_all %>%  ##changed this to pscis2!
  filter(barrier_result != 'Passable' &
           barrier_result != 'Unknown' &
           outlet_drop_meters < 0.3 &
           stream_width_ratio_score < 1.2 &
           culvert_slope_percent <= 2 )



##this will require some ore work for cost estimates too.
str_type <- pscis_all %>%
  select(rowid, aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, source, barrier_result, downstream_channel_width_meters, fill_depth_meters) %>%
  mutate(fill_dpth_over = fill_depth_meters - fill_dpth_mult) %>%
  mutate(crossing_fix = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                  & downstream_channel_width_meters >= 2 ~ 'Replace with New Open Bottom Structure',
                                  barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_character_,
                                  T ~ 'Replace Structure with Streambed Simulation CBS'))  %>%
  mutate(span_input = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                & downstream_channel_width_meters >= 2 ~ brdg_wdth,
                                barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_real_,
                                T ~ 3))  %>%
  mutate(span_input = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                & fill_dpth_over > 0 & !crossing_fix %ilike% 'Simulation' ~
                                  (brdg_wdth + fill_dpth_mult * fill_dpth_over),  ##1m more fill = 3 m more bridge
                                T ~ span_input)) %>%
  mutate(span_input = case_when(span_input < (downstream_channel_width_meters + 4) & ##span not need be extended if already 4m bigger than channel width
                                  downstream_channel_width_meters > chn_wdth_max ~
                                  (downstream_channel_width_meters - chn_wdth_max) + span_input,  ##for every m bigger than a 5 m channel add that much to each side in terms of span
                                T ~ span_input)) %>%
  ##let's add an option that if the stream is under 3.5m wide and under more than 5m of fill we do a streambed simulation with a 4.5m embedded multiplate like 4607464 on Flathead fsr
  mutate(crossing_fix = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                  & downstream_channel_width_meters > 2 &
                                    downstream_channel_width_meters <= 3.5 &
                                    fill_depth_meters > 5 ~ 'Replace Structure with Streambed Simulation CBS',
                                  T ~ crossing_fix),
         span_input = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                & downstream_channel_width_meters > 2 &
                                  downstream_channel_width_meters <= 3.5 &
                                  fill_depth_meters > 5 ~ 4.5,
                                T ~ span_input)) %>%
  mutate(span_input = plyr::round_any(span_input, 0.5))

##burn to a csvs so we can copy and paste into spreadsheet (could make a function to do this all at once....)
str_type %>%
  filter(source %ilike% 'phase1') %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/str_type_pscis1.csv'),
                   na = '')
str_type %>%
  filter(source %ilike% 'phase2') %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/str_type_pscis2.csv'),
                   na = '')
str_type %>%
  filter(source %ilike% 'reasses') %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/str_type_pscis_reassessments.csv'),
                   na = '')



# build priority spreadsheet ----------------------------------------------

# spreadsheet to build for input includes site lengths, surveyors initials, time, priority for remediation, updated fish species (if changed from my_fish_sp())
# thing is that we don't really have the fish info

hab_con <- fpr_import_hab_con()
hab_priority_prep1 <- hab_con %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date))) %>%
  select(reference_number:alias_local_name, survey_date) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(time_start = NA_character_,
         surveyors = NA_character_,
         length_surveyed = NA_character_,
         hab_value = NA_character_, #get from pscis sheets later.  Too much of a pain to join now due to multiple ids
         priority = NA_character_,
         upstream_habitat_length_m = NA_character_,
         species_codes = NA_character_ #get from bcfishpass later
  )

# grab the comments
hab_priority_prep <- left_join(

  hab_priority_prep1,

  hab_con %>%
    purrr::pluck("step_4_stream_site_data") %>%
    select(reference_number, comments),
  by = 'reference_number'
)
  # filter(!comments %ilike% 'feature_record_only') #we don't need to bin for these but we left them for now



# burn to csv to use as your template.  For safety use a different name then rename manually
hab_priority_prep %>%
  readr::write_csv('data/habitat_confirmations_priorities_raw.csv', na = '')




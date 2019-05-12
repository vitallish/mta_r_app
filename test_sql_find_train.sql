-- !preview conn=DBI::dbConnect(odbc::odbc(), "mta_gtfs", timeout = 10, dbname = "mta_trains")

select distinct route_id from trainID
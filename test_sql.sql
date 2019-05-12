-- !preview conn=DBI::dbConnect(odbc::odbc(), "mta_gtfs", timeout = 10, dbname = "mta_trains")

select * from stops where stop_id like '%S'and (

 stop_name like '145 St%' or stop_name like 'Times Sq%'
)

;


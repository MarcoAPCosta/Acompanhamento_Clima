box::use(
  shiny[onStop],
  pool[dbPool, poolClose],
  RPostgres[Postgres]
)

pool <- dbPool(
  Postgres(),
  host = "208.87.134.52",
  dbname = "admin",
  user = "superuser",
  password = "S3nh@123!",
  port = 24801
)

onStop(function() {
  print("Fechando a conexão")
  pool::poolClose(pool)
})
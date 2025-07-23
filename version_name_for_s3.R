# Create Latest Version Name
# File to be uploaded to s3 bucket

version_latest <- "version 1.2"
saveRDS(version_latest, file.path("~/Desktop", "version_latest.Rds"))


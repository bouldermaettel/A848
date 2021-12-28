# https://cran.r-project.org/web/packages/cyphr/vignettes/data.html

path_key_matthias <- cyphr::ssh_keygen(password = FALSE)
path_key_martha <- cyphr::ssh_keygen(password = FALSE)


data_dir <- file.path("./data")
dir.create(data_dir)
dir(data_dir)

cyphr::data_admin_init(data_dir, path_user = path_key_matthias)

key <- cyphr::data_key(data_dir, path_user = path_key_matthias)

# open RDS file
historic_data <- readRDS("data/historic_data.rds")

filename <- file.path(data_dir, "data_encrypted.rds")
cyphr::encrypt(saveRDS(historic_data, filename), key)
dir(data_dir)

readRDS(filename) # does not work!

data-decrypted <- cyphr::decrypt(readRDS(filename), key)
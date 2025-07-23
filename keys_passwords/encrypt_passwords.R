# Encrypt Passwords

# Creates
# 1. Dataframe with usernames and encrypted password
# 2. For each user, an encrypted file that contains the key for the data

#library(dplyr)
#library(readtext) PREVENTS APP FROM DEPLOYING; CAN'T USE PDFTOOLS
#library(bcrypt)
#library(openssl)

## Set directory to github repo
github_dir <- file.path("~", "Documents", "Github", "Kenya-Police-Dashboard-Test") 

# Load passwords/keys ----------------------------------------------------------
passwords_df <- read.csv(file.path(github_dir, "keys_passwords", "unsecured", "user_passwords.csv"),
                         stringsAsFactors = F)
encrypt_key <- readtext(file.path(github_dir, "keys_passwords", "unsecured", "data_encryption_key.txt"))
encrypt_key <- encrypt_key$text

# Hash passwords ---------------------------------------------------------------
passwords_df$password_hashed <- passwords_df$password %>% 
  lapply(function(x) hashpw(x)) %>% unlist()

passwords_df %>%
  dplyr::select(username, password_hashed, role) %>%
  saveRDS(file.path(github_dir, "keys_passwords", "secured", "passwords_hashed.Rds"))

# Save data encryption key into encrypted files for each user ------------------
for(user in passwords_df$username){
  password <- passwords_df$password[passwords_df$username %in% user]
  data_key <- sha256(charToRaw(password))
  
  saveRDS(aes_cbc_encrypt(serialize(encrypt_key, NULL), key = data_key),
          file.path(github_dir, "keys_passwords", "secured", "users", paste0(user, ".Rds")),
          version = 2)
}

# Save Encrypted File of AWS Keys ----------------------------------------------
api_keys <- file.path("~", "Dropbox", "World Bank", "Webscraping", "Files for Server", "api_keys.csv") %>%
  read.csv(stringsAsFactors = F) %>%
  dplyr::filter(Account %in% "robmarty3@gmail.com",
                Service %in% c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")) %>%
  dplyr::select(Service, Key)

data_key <- sha256(charToRaw(encrypt_key))

saveRDS(aes_cbc_encrypt(serialize(api_keys, NULL), key = data_key),
        file.path(github_dir, "keys_passwords", "secured", "api_keys.Rds"),
        version = 2)



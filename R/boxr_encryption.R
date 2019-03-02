#' returns path to ssh folder, given the OS
boxr_ssh_path <- function() {
  
}

# returns the local path to the parivate key
boxr_ssh_key_path <- function(name = "id_rsa_boxr", path = boxr_ssh_path()) {
  
}

#' returns the value of the private key 
boxr_ssh_key <- function(path = boxr_ssh_key_path()) {
  
}

#' returns the value of the public key
boxr_ssh_pubkey <- function(path = boxr_ssh_key_path()) {
  
}

#' encrypts the rds file described at the path
boxr_encrypt_auth <- function(path = NULL) {
  
}

#' decrypts the rds file described at the path
boxr_decrypt_auth <- function() {
  
}

boxr_cat_key <- function(key = boxr_ssh_key()) {
  
}


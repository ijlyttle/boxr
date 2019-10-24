if (gargle:::secret_can_decrypt("boxr")) {
  json <- gargle:::secret_read("boxr", "boxr-testing.json")
  box_auth_service(token_file = rawToChar(json))
}

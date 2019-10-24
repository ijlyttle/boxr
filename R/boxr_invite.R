#' Invite a collaborator to a Box folder
#' 
#' default to lowest permissions
#' 
#' @inheritParams box_setwd
#' @param account_id `character` ID for Box account to invite
#' @param login `character` email address of account to invite, can be used instead of 
#'   `account_id`
#' @param role `character` role of the collaborator
#' @param can_view_path `logical` indicates to allow the collaborator to navigate 
#'   parent-folders at Box.
#' 
#' @return Invisible `list()` containing collaboration-information.
#' @export
#' 
box_dir_invite <- function(dir_id, account_id = box_user_id(), login = NULL,
                           role = "viewer", can_view_path = FALSE) {

  # if login is provided, ignore account_id
  if (!is_void(login)) {
    account_id <- NULL
  }
  
  item <-
    list(
      type = "folder", # imagine box_file_invite()
      id = as.character(dir_id)
    )
  
  accessible_by <-
    list(
      type = "user", #  imagine inviting a group
      id = as.character(account_id),
      login = login
    )
  
  box_invite(item, accessible_by, role, can_view_path)
} 


box_invite <- function(item, accessible_by, role, can_view_path = FALSE) {

  # ref: https://developer.box.com/reference#collaboration-object
  
  # validate
  item_type_legal <- c("file", "folder")
  
  acc_type_legal <- c("user", "group")
  
  role_legal <- c(
    "editor", 
    "viewer", 
    "previewer", 
    "uploader", 
    "previewer uploader", 
    "viewer uploader",
    "co-owner", 
    "owner"
  )
  
  assertthat::assert_that(
    # item
    is.list(item),
    rlang::is_string(item$type),
    item$type %in% item_type_legal,
    rlang::is_string(item$id),
    # accessible_by
    is.list(accessible_by),
    rlang::is_string(accessible_by$type),
    accessible_by$type %in% acc_type_legal,
    rlang::is_string(accessible_by$id) || rlang::is_string(accessible_by$login),
    # role
    rlang::is_string(role),
    role %in% role_legal,   
    # can_view_path
    rlang::is_bool(can_view_path)
  )
  
  # call the API
  resp <- httr::POST(
    "https://api.box.com/2.0/collaborations",
    get_token(),
    encode = "multipart",
    body = 
      jsonlite::toJSON(
        list(
          item = item, 
          accessible_by = accessible_by, 
          role = role, 
          can_view_path = can_view_path
        ),
        auto_unbox = TRUE
      )
  ) 
  
  httr::stop_for_status(resp, task = "invite collaborator")

  # TODO: create an S3 class
  resp <- httr::content(resp)
  
  # feedback
  message(
    glue::glue(
      "{resp$created_by$name} ({resp$created_by$login}) has invited",
      "{resp$accessible_by$name} ({resp$accessible_by$login})",
      "to collaborate on {resp$item$type} `{resp$item$name}`",
      "as {resp$role}.",
      .sep = " "
    )
  )
  
  invisible(resp)
}
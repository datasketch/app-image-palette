


if(Sys.info()[['sysname']] == 'Linux'){
  message("create .fonts")
  dir.create('~/.fonts')
  message("copy to .fonts")
  file.copy("fonts/IBMPlexSans-Regular.ttf", "~/.fonts")
  message("fc-cache -f ~/.fonts")
  system('fc-cache -f -v ~/.fonts')
  message("\n\nls ~/.fonts\n\n")
  system('ls ~/.fonts')
  message("\n\nfc-list\n\n")
  system('fc-list')
  message("\n\nfc-match IBM Plex Sans\n\n")
  system('fc-match IBM Plex Sans')
}
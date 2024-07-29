port module Ports exposing (openDialog, share)


port share : { title : String } -> Cmd msg


port openDialog : String -> Cmd msg

module Command.RenameUtils where

ownerFieldRename :: String -> String
ownerFieldRename "type_" = "type"
ownerFieldRename name = name

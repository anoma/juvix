module Commands.Version.Options where

import CommonOptions

data VersionCommand
  = VersionHuman
  | VersionNumeric
  | VersionCommit
  deriving stock (Data)

parseVersionCommand :: Parser VersionCommand
parseVersionCommand =
  fromMaybe VersionHuman <$> optional parseVersionCommand'

parseVersionCommand' :: Parser VersionCommand
parseVersionCommand' =
  hsubparser $
    mconcat
      ( [ command
            "human"
            (info (pure VersionHuman) (progDesc "[Default] Print a human-readable version with detailed information")),
          command
            "numeric"
            (info (pure VersionNumeric) (progDesc "Print the version in the form x.y.z")),
          command
            "numeric-commit"
            (info (pure VersionCommit) (progDesc "Print the version in the form x.y.z-commit"))
        ]
      )

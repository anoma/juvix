module Commands.Doctor.Options where

import CommonOptions

data DoctorOptions = DoctorOptions
  { _doctorOffline :: Bool,
    _doctorVerbose :: Bool
  }
  deriving stock (Data)

parseDoctorOptions :: Parser DoctorOptions
parseDoctorOptions = do
  _doctorOffline <-
    switch
      ( long "offline"
          <> help "Run the doctor offline"
      )
  _doctorVerbose <-
    switch
      ( long "verbose"
          <> short 'v'
          <> help "Print verbose output"
      )
  pure DoctorOptions {..}

module Commands.Doctor.Options where

import CommonOptions

newtype DoctorOptions = DoctorOptions
  { _doctorVerbose :: Bool
  }
  deriving stock (Data)

parseDoctorOptions :: Parser DoctorOptions
parseDoctorOptions = do
  _doctorVerbose <-
    switch
      ( long "verbose"
          <> short 'v'
          <> help "Print verbose output"
      )
  pure DoctorOptions {..}

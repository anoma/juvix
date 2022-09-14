module Commands.Doctor.Options where

import CommonOptions

newtype DoctorOptions = DoctorOptions
  { _doctorOffline :: Bool
  }
  deriving stock (Data)

parseDoctorOptions :: Parser DoctorOptions
parseDoctorOptions = do
  _doctorOffline <-
    switch
      ( long "offline"
          <> help "Run the doctor offline"
      )
  pure DoctorOptions {..}

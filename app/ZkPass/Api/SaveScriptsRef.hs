module ZkPass.Api.SaveScriptsRef where

import           Control.Exception    (throwIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe           (fromJust)
import           Data.String          (fromString)
import           GHC.Generics
import           Prelude
import           System.FilePath      ((</>))

import           ZkPass.Api.Context


-- | Setup input parameters.
data SaveRefInput = SaveRefInput { sriTxId :: !String }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | Setup response
data SaveRefResponse = SaveRefResponse { srSaved :: !Bool }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

-- | Handle to construct setup response.
handleSaveRef :: FilePath -> SaveRefInput -> IO SaveRefResponse
handleSaveRef path SaveRefInput{..} = do
  let setupFile = path </> "setup-params.json"

  SetupParams x tag mref <- fromJust . decode <$> BL.readFile setupFile

  case mref of
    Just _  -> throwIO $ userError "Unexpected: scripts' reference TxId already saved."

    Nothing -> do
      let completeSetupParams = SetupParams x tag (Just $ fromString sriTxId)

      BL.writeFile setupFile $ encode completeSetupParams

      return $ SaveRefResponse { srSaved = True }

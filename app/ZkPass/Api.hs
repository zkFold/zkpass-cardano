{-# LANGUAGE TypeOperators #-}

module ZkPass.Api where

import           Control.Exception           (try)
import           Control.Monad.Trans.Except  (ExceptT (..))
import qualified Network.HTTP.Types          as HttpTypes
import           Network.Wai.Middleware.Cors
import           Prelude
import           Servant

import           ZkPass.Api.Burn             (BurnInput, handleBurn)
import           ZkPass.Api.Context          (Ctx (..), OwnAddress, OwnAddresses, UnsignedTxResponse, handleOwnAddr)
import           ZkPass.Api.Mint             (MintInput, ZkPassResponse,
                                              handleMint)
import           ZkPass.Api.SaveScriptsRef   (SaveRefInput, SaveRefResponse,
                                              handleSaveRef)
import           ZkPass.Api.Setup            (SetupInput, SetupResponse,
                                              handleSetup)
import           ZkPass.Api.Transfer         (TransferInput, handleTransfer)
import           ZkPass.Api.Tx               (AddWitAndSubmitParams,
                                              SubmitTxResponse,
                                              handleAddWitAndSubmitTx)


-- | Type for our Servant API.
type API = "setup" :> ReqBody '[JSON] SetupInput
                   :> Post    '[JSON] SetupResponse
      :<|> "save-ref" :> ReqBody '[JSON] SaveRefInput
                      :> Post    '[JSON] SaveRefResponse
      :<|> "transfer" :> ReqBody '[JSON] TransferInput
                      :> Post    '[JSON] UnsignedTxResponse
      :<|> "mint" :> ReqBody '[JSON] MintInput
                  :> Post    '[JSON] ZkPassResponse
      :<|> "burn" :> ReqBody '[JSON] BurnInput
                  :> Post    '[JSON] UnsignedTxResponse
      :<|> "add-wit-and-submit" :> ReqBody '[JSON] AddWitAndSubmitParams
                                :> Post    '[JSON] SubmitTxResponse
      :<|> "own-addr" :> ReqBody '[JSON] OwnAddresses
                      :> Post    '[JSON] OwnAddress

-- | Server Handler
server :: Ctx -> FilePath -> ServerT API IO
server ctx path = handleSetup ctx path
             :<|> handleSaveRef path
             :<|> handleTransfer ctx path
             :<|> handleMint ctx path
             :<|> handleBurn ctx path
             :<|> handleAddWitAndSubmitTx ctx
             :<|> handleOwnAddr

appApi :: Proxy API
appApi = Proxy

app :: Ctx -> FilePath -> Application
app ctx path = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $
  serve appApi $ hoistServer appApi (Handler . ExceptT . try) $
  server ctx path

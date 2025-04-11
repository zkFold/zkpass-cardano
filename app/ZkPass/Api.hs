{-# LANGUAGE TypeOperators #-}

module ZkPass.Api where

import           Control.Exception           (try)
import           Control.Monad.Trans.Except  (ExceptT (..))
import qualified Network.HTTP.Types          as HttpTypes
import           Network.Wai.Middleware.Cors
import           Prelude
import           Servant

import           ZkPass.Api.Burn             (BurnInput, handleBurn)
import           ZkPass.Api.Context          (Ctx (..), OwnAddress,
                                              OwnAddresses, SetupParams,
                                              UnsignedTxResponse, handleOwnAddr)
import           ZkPass.Api.Mint             (MintInput, ZkPassResponse,
                                              handleMint)
import           ZkPass.Api.Setup            (SetupInput, SetupResponse,
                                              handleSetup)
import           ZkPass.Api.Transfer         (TransferInput, handleTransfer)
import           ZkPass.Api.Tx               (AddWitAndSubmitParams,
                                              SubmitTxResponse,
                                              handleAddWitAndSubmitTx)


-- | Type for our Servant API.
type API = "setup" :> ReqBody '[JSON] SetupInput
                   :> Post    '[JSON] SetupResponse
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
server :: Ctx -> SetupParams -> ServerT API IO
server ctx sp = handleSetup ctx sp
           :<|> handleTransfer ctx
           :<|> handleMint ctx sp
           :<|> handleBurn ctx sp
           :<|> handleAddWitAndSubmitTx ctx
           :<|> handleOwnAddr

appApi :: Proxy API
appApi = Proxy

app :: Ctx -> SetupParams -> Application
app ctx sp = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $
  serve appApi $ hoistServer appApi (Handler . ExceptT . try) $
  server ctx sp

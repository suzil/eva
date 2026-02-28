{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API type definitions and handlers for the Prompt Assistance feature
-- (P2-M7, EVA-100).
--
-- One URL group:
--   /api/templates — collection + per-item CRUD
module Eva.Prompt.Api
  ( TemplatesAPI
  , templatesHandlers
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Servant

import Eva.Api.Types (ApiError (..), CreateTemplateReq (..), PatchTemplateReq (..))
import Eva.App (AppEnv, AppM, runAppM)
import Eva.Prompt.Store
  ( deleteTemplate
  , getTemplate
  , insertTemplate
  , listTemplates
  , updateTemplate
  )
import Eva.Prompt.Types

-- ---------------------------------------------------------------------------
-- API types
-- ---------------------------------------------------------------------------

type TemplatesAPI =
  "api" :> "templates" :>
    (    Get '[JSON] [PromptTemplate]
    :<|> ReqBody '[JSON] CreateTemplateReq :> PostCreated '[JSON] PromptTemplate
    :<|> Capture "id" Text :> TemplateByIdAPI
    )

type TemplateByIdAPI =
       Get '[JSON] PromptTemplate
  :<|> ReqBody '[JSON] PatchTemplateReq :> Patch '[JSON] PromptTemplate
  :<|> DeleteNoContent

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

templatesHandlers :: AppEnv -> Server TemplatesAPI
templatesHandlers env =
       listH
  :<|> createH
  :<|> byIdHandlers
  where
    run :: AppM a -> Handler a
    run = liftIO . runAppM env

    -- GET /api/templates
    listH :: Handler [PromptTemplate]
    listH = run listTemplates

    -- POST /api/templates
    createH :: CreateTemplateReq -> Handler PromptTemplate
    createH req = do
      tid <- liftIO (TemplateId . UUID.toText <$> nextRandom)
      now <- liftIO getCurrentTime
      let tmpl = PromptTemplate
            { promptTemplateId          = tid
            , promptTemplateName        = ctrName req
            , promptTemplateDescription = ctrDescription req
            , promptTemplateCategory    = ctrCategory req
            , promptTemplateTags        = ctrTags req
            , promptTemplateBody        = ctrBody req
            , promptTemplateVariables   = ctrVariables req
            , promptTemplateBuiltIn     = False
            , promptTemplateCreatedAt   = now
            , promptTemplateUpdatedAt   = now
            }
      run (insertTemplate tmpl)
      pure tmpl

    -- All endpoints under /api/templates/:id
    byIdHandlers :: Text -> Server TemplateByIdAPI
    byIdHandlers rawId =
           getH
      :<|> patchH
      :<|> deleteH
      where
        tid :: TemplateId
        tid = TemplateId rawId

        requireTemplate :: Handler PromptTemplate
        requireTemplate = do
          mt <- run (getTemplate tid)
          case mt of
            Nothing -> throwError err404 { errBody = encode (ApiError "Template not found") }
            Just t  -> pure t

        -- GET /api/templates/:id
        getH :: Handler PromptTemplate
        getH = requireTemplate

        -- PATCH /api/templates/:id — user templates only
        patchH :: PatchTemplateReq -> Handler PromptTemplate
        patchH req = do
          t <- requireTemplate
          if promptTemplateBuiltIn t
            then throwError err403 { errBody = encode (ApiError "Built-in templates cannot be modified") }
            else do
              now <- liftIO getCurrentTime
              let t' = t
                    { promptTemplateName        = maybe (promptTemplateName t)        id (ptrName req)
                    , promptTemplateDescription = maybe (promptTemplateDescription t) id (ptrDescription req)
                    , promptTemplateCategory    = maybe (promptTemplateCategory t)    id (ptrCategory req)
                    , promptTemplateTags        = maybe (promptTemplateTags t)        id (ptrTags req)
                    , promptTemplateBody        = maybe (promptTemplateBody t)        id (ptrBody req)
                    , promptTemplateVariables   = maybe (promptTemplateVariables t)   id (ptrVariables req)
                    , promptTemplateUpdatedAt   = now
                    }
              run (updateTemplate t')
              pure t'

        -- DELETE /api/templates/:id
        deleteH :: Handler NoContent
        deleteH = do
          result <- run (deleteTemplate tid)
          case result of
            Left msg
              | "built-in" `T.isInfixOf` msg ->
                  throwError err403 { errBody = encode (ApiError msg) }
              | otherwise ->
                  throwError err404 { errBody = encode (ApiError msg) }
            Right () -> pure NoContent

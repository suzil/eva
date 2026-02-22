{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Eva.Core.TypesSpec (spec) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec
import Test.QuickCheck

import Eva.Core.Types

-- ---------------------------------------------------------------------------
-- Arbitrary helpers
-- ---------------------------------------------------------------------------

arbitraryText :: Gen Text
arbitraryText = T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '_']))

arbitraryUTCTime :: Gen UTCTime
arbitraryUTCTime = do
  secs <- choose (0 :: Int, 1_735_689_600)
  pure $ posixSecondsToUTCTime (fromIntegral secs)

-- Simple Value generator — deep nesting is not needed to prove roundtrip.
arbitraryValue :: Gen Value
arbitraryValue =
  oneof
    [ pure Null
    , Bool <$> arbitrary
    , Number . fromInteger <$> (arbitrary :: Gen Integer)
    , String <$> arbitraryText
    , object <$> listOf ((,) . fromText <$> arbitraryText <*> (String <$> arbitraryText))
    ]

-- Non-null Value generator: used for Maybe Value fields because Aeson decodes
-- JSON null as Nothing, making Just Null indistinguishable from Nothing.
arbitraryNonNullValue :: Gen Value
arbitraryNonNullValue =
  oneof
    [ Bool <$> arbitrary
    , Number . fromInteger <$> (arbitrary :: Gen Integer)
    , String <$> arbitraryText
    , object <$> listOf ((,) . fromText <$> arbitraryText <*> (String <$> arbitraryText))
    ]

-- ---------------------------------------------------------------------------
-- Arbitrary instances — ID newtypes
-- ---------------------------------------------------------------------------

instance Arbitrary ProgramId where
  arbitrary = ProgramId <$> arbitraryText

instance Arbitrary NodeId where
  arbitrary = NodeId <$> arbitraryText

instance Arbitrary EdgeId where
  arbitrary = EdgeId <$> arbitraryText

instance Arbitrary RunId where
  arbitrary = RunId <$> arbitraryText

instance Arbitrary StepId where
  arbitrary = StepId <$> arbitraryText

-- ---------------------------------------------------------------------------
-- Arbitrary instances — state enums
-- ---------------------------------------------------------------------------

instance Arbitrary ProgramState where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary RunState where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary StepState where
  arbitrary = elements [minBound .. maxBound]

-- ---------------------------------------------------------------------------
-- Arbitrary instances — port types
-- ---------------------------------------------------------------------------

instance Arbitrary PortCategory where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary PortName where
  arbitrary = PortName <$> arbitraryText

instance Arbitrary PortSpec where
  arbitrary = PortSpec <$> arbitrary <*> arbitrary <*> arbitrary

-- ---------------------------------------------------------------------------
-- Arbitrary instances — config sub-types
-- ---------------------------------------------------------------------------

instance Arbitrary ResponseFormat where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ContentSource where
  arbitrary =
    oneof
      [ InlineText <$> arbitraryText
      , FileRef <$> arbitraryText
      , UrlRef <$> arbitraryText
      , pure UpstreamPort
      ]

instance Arbitrary KnowledgeFormat where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary RefreshPolicy where
  arbitrary =
    oneof
      [ pure RefreshStatic
      , pure RefreshOnRun
      , RefreshPeriodic <$> choose (60, 86400)
      ]

instance Arbitrary SystemType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ActionOperation where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ErrorHandlingMode where
  arbitrary =
    oneof
      [ pure ErrFail
      , pure ErrContinue
      , ErrUseDefault <$> arbitraryText
      ]

instance Arbitrary TriggerType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary BackoffStrategy where
  arbitrary =
    oneof
      [ BackoffFixed <$> choose (100, 5000)
      , BackoffExponential <$> choose (100, 1000) <*> choose (2, 10)
      ]

-- ---------------------------------------------------------------------------
-- Arbitrary instances — node config types
-- ---------------------------------------------------------------------------

instance Arbitrary AgentConfig where
  arbitrary =
    AgentConfig
      <$> arbitraryText
      <*> arbitraryText
      <*> arbitrary
      <*> choose (0.0, 2.0)
      <*> oneof [pure Nothing, Just <$> choose (100, 4096)]
      <*> choose (1, 10)
      <*> oneof [pure Nothing, Just <$> choose (0.01, 10.0)]

instance Arbitrary KnowledgeConfig where
  arbitrary = KnowledgeConfig <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ConnectorConfig where
  arbitrary =
    ConnectorConfig
      <$> arbitrary
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> listOf arbitraryText

instance Arbitrary ActionConfig where
  arbitrary = ActionConfig <$> arbitrary <*> arbitraryValue <*> arbitrary

instance Arbitrary TriggerConfig where
  arbitrary =
    TriggerConfig
      <$> arbitrary
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> oneof [pure Nothing, Just <$> arbitraryNonNullValue]

-- ---------------------------------------------------------------------------
-- Arbitrary instances — graph primitives
-- ---------------------------------------------------------------------------

instance Arbitrary NodeType where
  arbitrary =
    oneof
      [ AgentNode <$> arbitrary
      , KnowledgeNode <$> arbitrary
      , ConnectorNode <$> arbitrary
      , ActionNode <$> arbitrary
      , TriggerNode <$> arbitrary
      ]

instance Arbitrary Node where
  arbitrary =
    Node
      <$> arbitrary
      <*> arbitraryText
      <*> arbitrary
      <*> choose (-1000.0, 1000.0)
      <*> choose (-1000.0, 1000.0)

instance Arbitrary Edge where
  arbitrary = Edge <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Graph where
  arbitrary = do
    nodes <- Map.fromList . map (\n -> (nodeId n, n)) <$> listOf arbitrary
    edges <- listOf arbitrary
    pure $ Graph nodes edges

-- ---------------------------------------------------------------------------
-- Arbitrary instances — messaging
-- ---------------------------------------------------------------------------

instance Arbitrary MessageMeta where
  arbitrary = MessageMeta <$> arbitraryText <*> arbitraryUTCTime <*> arbitrary <*> arbitrary

instance Arbitrary Message where
  arbitrary = Message <$> arbitraryText <*> arbitraryValue <*> arbitrary

-- ---------------------------------------------------------------------------
-- Arbitrary instances — execution
-- ---------------------------------------------------------------------------

instance Arbitrary RetryPolicy where
  arbitrary = RetryPolicy <$> choose (1, 5) <*> arbitrary <*> oneof [pure Nothing, Just <$> choose (500, 30000)]

instance Arbitrary Run where
  arbitrary =
    Run
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> oneof [pure Nothing, Just <$> arbitraryNonNullValue]
      <*> oneof [pure Nothing, Just <$> arbitraryUTCTime]
      <*> oneof [pure Nothing, Just <$> arbitraryUTCTime]

instance Arbitrary Step where
  arbitrary =
    Step
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> oneof [pure Nothing, Just <$> arbitraryNonNullValue]
      <*> oneof [pure Nothing, Just <$> arbitraryNonNullValue]
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> choose (0, 3)
      <*> oneof [pure Nothing, Just <$> arbitraryUTCTime]
      <*> oneof [pure Nothing, Just <$> arbitraryUTCTime]

-- ---------------------------------------------------------------------------
-- Arbitrary instances — Program
-- ---------------------------------------------------------------------------

instance Arbitrary Program where
  arbitrary =
    Program
      <$> arbitrary
      <*> arbitraryText
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryUTCTime
      <*> arbitraryUTCTime

-- ---------------------------------------------------------------------------
-- Roundtrip helper
-- ---------------------------------------------------------------------------

roundtrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Property
roundtrip x = eitherDecode (encode x) === Right x

-- ---------------------------------------------------------------------------
-- Sample program — 5 node types, hand-written
-- ---------------------------------------------------------------------------

sampleProgram :: Program
sampleProgram =
  Program
    { programId = "prog-001"
    , programName = "Weekly Project Summarizer"
    , programState = Draft
    , programGraph =
        Graph
          { graphNodes =
              Map.fromList
                [ ( "trigger-1"
                  , Node
                      { nodeId = "trigger-1"
                      , nodeLabel = "Weekly Trigger"
                      , nodeType =
                          TriggerNode
                            TriggerConfig
                              { triggerType = TriggerCron
                              , triggerSchedule = Just "0 9 * * 1"
                              , triggerEventFilter = Nothing
                              , triggerPayloadTemplate = Nothing
                              }
                      , nodePosX = 100
                      , nodePosY = 200
                      }
                  )
                , ( "connector-1"
                  , Node
                      { nodeId = "connector-1"
                      , nodeLabel = "Linear"
                      , nodeType =
                          ConnectorNode
                            ConnectorConfig
                              { connectorSystem = SystemLinear
                              , connectorCredentialId = Just "cred-linear"
                              , connectorEndpoint = Nothing
                              , connectorScope = Just "read:issues"
                              , connectorActionFilter = []
                              }
                      , nodePosX = 300
                      , nodePosY = 100
                      }
                  )
                , ( "knowledge-1"
                  , Node
                      { nodeId = "knowledge-1"
                      , nodeLabel = "Team Context"
                      , nodeType =
                          KnowledgeNode
                            KnowledgeConfig
                              { knowledgeSource = InlineText "Sprint goals: ship EVA-6 and EVA-7."
                              , knowledgeFormat = FormatText
                              , knowledgeRefreshPolicy = RefreshStatic
                              }
                      , nodePosX = 300
                      , nodePosY = 300
                      }
                  )
                , ( "agent-1"
                  , Node
                      { nodeId = "agent-1"
                      , nodeLabel = "Summarizer"
                      , nodeType =
                          AgentNode
                            AgentConfig
                              { agentModel = "gpt-4o"
                              , agentSystemPrompt = "Summarize the sprint progress."
                              , agentResponseFormat = ResponseText
                              , agentTemperature = 0.7
                              , agentMaxTokens = Just 1024
                              , agentMaxIterations = 5
                              , agentCostBudgetUsd = Just 0.5
                              }
                      , nodePosX = 550
                      , nodePosY = 200
                      }
                  )
                , ( "action-1"
                  , Node
                      { nodeId = "action-1"
                      , nodeLabel = "Format Report"
                      , nodeType =
                          ActionNode
                            ActionConfig
                              { actionOperation = OpTemplate
                              , actionParameters = object ["template" .= ("# Weekly Report\n{{input}}" :: Text)]
                              , actionErrorHandling = ErrFail
                              }
                      , nodePosX = 750
                      , nodePosY = 200
                      }
                  )
                ]
          , graphEdges = []
          }
    , programCreatedAt = posixSecondsToUTCTime 1_740_000_000
    , programUpdatedAt = posixSecondsToUTCTime 1_740_000_000
    }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Eva.Core.Types" $ do
  describe "JSON roundtrip — ID newtypes" $ do
    it "ProgramId" $ property $ roundtrip @ProgramId
    it "NodeId" $ property $ roundtrip @NodeId
    it "EdgeId" $ property $ roundtrip @EdgeId
    it "RunId" $ property $ roundtrip @RunId
    it "StepId" $ property $ roundtrip @StepId

  describe "JSON roundtrip — state enums" $ do
    it "ProgramState" $ property $ roundtrip @ProgramState
    it "RunState" $ property $ roundtrip @RunState
    it "StepState" $ property $ roundtrip @StepState

  describe "JSON roundtrip — port types" $ do
    it "PortCategory" $ property $ roundtrip @PortCategory
    it "PortName" $ property $ roundtrip @PortName
    it "PortSpec" $ property $ roundtrip @PortSpec

  describe "JSON roundtrip — config sub-types" $ do
    it "ResponseFormat" $ property $ roundtrip @ResponseFormat
    it "ContentSource" $ property $ roundtrip @ContentSource
    it "KnowledgeFormat" $ property $ roundtrip @KnowledgeFormat
    it "RefreshPolicy" $ property $ roundtrip @RefreshPolicy
    it "SystemType" $ property $ roundtrip @SystemType
    it "ActionOperation" $ property $ roundtrip @ActionOperation
    it "ErrorHandlingMode" $ property $ roundtrip @ErrorHandlingMode
    it "TriggerType" $ property $ roundtrip @TriggerType
    it "BackoffStrategy" $ property $ roundtrip @BackoffStrategy

  describe "JSON roundtrip — node config types" $ do
    it "AgentConfig" $ property $ roundtrip @AgentConfig
    it "KnowledgeConfig" $ property $ roundtrip @KnowledgeConfig
    it "ConnectorConfig" $ property $ roundtrip @ConnectorConfig
    it "ActionConfig" $ property $ roundtrip @ActionConfig
    it "TriggerConfig" $ property $ roundtrip @TriggerConfig

  describe "JSON roundtrip — graph primitives" $ do
    it "NodeType" $ property $ roundtrip @NodeType
    it "Node" $ property $ roundtrip @Node
    it "Edge" $ property $ roundtrip @Edge
    it "Graph" $ property $ roundtrip @Graph

  describe "JSON roundtrip — messaging" $ do
    it "MessageMeta" $ property $ roundtrip @MessageMeta
    it "Message" $ property $ roundtrip @Message

  describe "JSON roundtrip — execution" $ do
    it "RetryPolicy" $ property $ roundtrip @RetryPolicy
    it "Run" $ property $ roundtrip @Run
    it "Step" $ property $ roundtrip @Step

  describe "JSON roundtrip — Program" $ do
    it "Program" $ property $ roundtrip @Program

  describe "sample program" $ do
    it "program with all 5 node types serializes and deserializes correctly" $ do
      eitherDecode (encode sampleProgram) `shouldBe` Right sampleProgram

    it "all 5 MLP node types are present in sampleProgram" $ do
      let nodes = Map.elems . graphNodes . programGraph $ sampleProgram
          types = map (\n -> case nodeType n of
                        AgentNode{} -> "agent" :: Text
                        KnowledgeNode{} -> "knowledge"
                        ConnectorNode{} -> "connector"
                        ActionNode{} -> "action"
                        TriggerNode{} -> "trigger") nodes
      length types `shouldBe` 5

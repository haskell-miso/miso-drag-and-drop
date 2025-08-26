-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Prelude hiding (unlines, rem)
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html.Element as H
import           Miso.Html.Event as E
import           Miso.Html.Property as P
import           Miso.String
import           Miso.Lens
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
-----------------------------------------------------------------------------
data Action
  = DragStart Section Task
  | DragEnd Section Task
  | DragOver
  | DragEnter
  | DragLeave
  | Drop Section
  deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run (startApp app)
-----------------------------------------------------------------------------
data Task
  = Task
  { taskId :: MisoString
  , taskTitle :: MisoString
  , taskDescription :: MisoString
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Model
  = Model
  { _currentTask :: Maybe Task
  , _currentSection :: Maybe Section
  , _sections :: Map Section [Task]
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
currentTask :: Lens Model (Maybe Task)
currentTask = lens _currentTask $ \record x -> record { _currentTask = x }
-----------------------------------------------------------------------------
sections :: Lens Model (Map Section [Task])
sections = lens _sections $ \record x -> record { _sections = x }
-----------------------------------------------------------------------------
currentSection :: Lens Model (Maybe Section)
currentSection = lens _currentSection $ \record x -> record { _currentSection = x }
-----------------------------------------------------------------------------
initialModel :: Model
initialModel = flip Model Nothing Nothing $ M.fromList
  [ Todo =:
     [ Task "1" "Design Homepage" "Create wire frames"
     , Task "2" "Write blogpost" "Draft article"
     ]
  , InProgress =:
     [ Task "3" "Implement API" "Connect user to backend authentication"
     ]
  , Review =:
     [ Task "4" "Fix mobile layout" "Address responsiveness issues"
     ]
  , Done =:
     [ Task "5" "User testing" "Complete first round of user testing"
     ]
  ]
-----------------------------------------------------------------------------
app :: App Model Action
app = (component initialModel update_ viewModel)
  { events = dragEvents
  , styles = [ Href "assets/style.css" ]
  } where
      update_ = \case
        DragStart section task -> do
          currentTask ?= task
          currentSection ?= section
        DragEnd _ _ -> do
          currentTask .= Nothing
          currentSection .= Nothing
        Drop droppedSection -> do
          use currentSection >>= \case
            Just section
              | section /= droppedSection -> do
                  use currentTask >>= \case
                    Nothing ->
                      pure ()
                    Just task ->
                      sections %= swap task section droppedSection 
            _ ->
              pure ()
          currentSection .= Nothing
          currentTask .= Nothing
        DragOver ->
          pure ()
        DragEnter ->
          pure ()
        DragLeave ->
          pure ()
-----------------------------------------------------------------------------
swap :: Task -> Section -> Section -> Map Section [Task] -> Map Section [Task] 
swap droppedTask oldSection newSection sections_ =
  case M.insertWith (++) newSection [droppedTask] sections_ of
    newMap ->
      case M.lookup oldSection newMap of
        Nothing -> newMap
        Just oldTasks ->
          let
            updatedTasks = Prelude.filter (\t -> taskId t /= taskId droppedTask) oldTasks
          in
            M.insert oldSection updatedTasks newMap
-----------------------------------------------------------------------------
data Section
  = Todo
  | InProgress
  | Review
  | Done
  deriving (Ord, Show, Eq)
-----------------------------------------------------------------------------
instance ToMisoString Section where
  toMisoString Todo        = "To Do"
  toMisoString InProgress  = "In Progress"
  toMisoString Review      = "Review"
  toMisoString Done        = "Done"
-----------------------------------------------------------------------------
showSection :: (Maybe Section, Maybe Task) -> (Section, [Task]) -> View Model Action
showSection (maybeSection, maybeTask) (section, tasks) =
  H.div_
  [ P.class_ "column" ]
  [ H.h2_
    [ P.class_ "column-title" ]
    [ text (ms section)
    ]
  , H.div_
    [ P.id_ "todo"
    , P.classList_
      [ "task-list" =: True
      , "drag-enter" =: (Just section == maybeSection)
      ] 
    , Main.onDragOver DragOver
    , Main.onDragEnter DragEnter
    , Main.onDragLeave DragLeave
    , Main.onDrop (Drop section)
    ]
    [ H.div_
        [ P.data_ "id" taskId
        , draggable_ True
        , P.classList_
          [ "task" =: True
          , "dragging" =: (Just task == maybeTask)
          ]
        , E.onDragStart (DragStart section task)
        , E.onDragEnd (DragEnd section task)
        ]
        [ H.div_ [P.class_ "task-title"] [ text taskTitle ]
        , H.div_
            [ P.class_ "task-desc"
            ]
            [ text taskDescription
            ]
        ]
    | task@Task {..} <- tasks
    ]
  ]
-----------------------------------------------------------------------------
onDrop :: action -> Attribute action
onDrop action = flip onWithOptions "drop" defaultOptions {
    preventDefault = True
  } emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onDragOver :: action -> Attribute action
onDragOver action = flip onWithOptions "dragover" defaultOptions {
    preventDefault = True
  } emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onDragEnter :: action -> Attribute action
onDragEnter action = flip onWithOptions "dragenter" defaultOptions {
    preventDefault = True
  } emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onDragLeave :: action -> Attribute action
onDragLeave action = flip onWithOptions "dragleave" defaultOptions {
    preventDefault = True
  } emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel model =
  H.div_
    []
    [ H.header_
        []
        [ H.h1_ [] ["Miso drag & drop 🍜 " ]
        , H.p_
            []
            [ "Drag tasks between columns to organize your workflow"
            ]
        ]
    , H.div_
      [ P.class_ "container"
      ] (showSection (model ^. currentSection, model ^. currentTask) <$> M.toList (model ^. sections))
    , H.div_
        [ P.id_ "successMessage"
        , P.class_ "success-message"
        ]
        [ "Task moved successfully!"
        ]
    ]
-----------------------------------------------------------------------------
draggable_ :: Bool -> Attribute action
draggable_ = boolProp "draggable"
-----------------------------------------------------------------------------

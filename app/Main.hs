-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad (when)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html.Element as H
import           Miso.Html.Event as E
import           Miso.Html.Property as P
import           Miso.String
import           Miso.Lens
-----------------------------------------------------------------------------
data Action
  = DragStart Section Task
  | DragEnd
  | Drop Section
  | DragOver
  | DragEnter
  | DragLeave
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
initialModel = Model Nothing Nothing $ M.fromList
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
#ifndef WASM
  , styles = [ Href "assets/style.css" ]
#endif
  } where
      update_ = \case
        DragStart section task -> do
          currentTask ?= task
          currentSection ?= section
        DragEnd -> do
          currentTask .= Nothing
          currentSection .= Nothing
        Drop dropped -> do
          section <- use currentSection
          when (section /= Just dropped) $ do
            task <- use currentTask
            sections %= swap task section dropped 
          currentSection .= Nothing
          currentTask .= Nothing
        DragOver ->
          pure ()
        DragEnter ->
          pure ()
        DragLeave ->
          pure ()
-----------------------------------------------------------------------------
swap :: Maybe Task -> Maybe Section -> Section -> Map Section [Task] -> Map Section [Task]
swap _ Nothing _ sections_ = sections_ 
swap Nothing _ _ sections_ = sections_
swap (Just droppedTask) (Just oldSection) newSection sections_ =
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
        , P.draggable_ True
        , P.classList_
          [ "task" =: True
          , "dragging" =: (Just task == maybeTask)
          ]
        , E.onDragStart (DragStart section task)
        , E.onDragEnd DragEnd
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
onDrop action = onWithOptions preventDefault "drop" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onDragOver :: action -> Attribute action
onDragOver action = onWithOptions preventDefault "dragover" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onDragEnter :: action -> Attribute action
onDragEnter action = onWithOptions preventDefault "dragenter" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onDragLeave :: action -> Attribute action
onDragLeave action = onWithOptions preventDefault "dragleave" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel model =
  H.div_
    []
    [ githubStar
    , H.header_ 
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
githubStar :: View model action
githubStar = H.iframe_
    [ P.title_ "GitHub"
    , P.height_ "30"
    , P.width_ "170"
    , textProp "scrolling" "0"
    , textProp "frameborder" "0"
    , P.src_
      "https://ghbtns.com/github-btn.html?user=haskell-miso&repo=miso-drag-and-drop&type=star&count=true&size=large"
    ]
    []
-----------------------------------------------------------------------------

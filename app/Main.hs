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
  | DragEnter Section
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
          io_ (consoleLog "drag start")
        DragEnd _ _ -> do
          currentTask .= Nothing
          currentSection .= Nothing
          io_ (consoleLog "drag end")
        DragOver -> do
          io_ (consoleLog "drag over")
        DragEnter section -> do
          currentSection ?= section
          io_ (consoleLog "drag enter")
        DragLeave -> do
          currentSection .= Nothing
          io_ (consoleLog "drag leave")
        Drop droppedSection -> do
          io_ (consoleLog "in drop!")
          use currentSection >>= \case
            Just section
              | section /= droppedSection -> do
                  io_ (consoleLog "in section!")
                  use currentTask >>= \case
                    Nothing -> do
                      io_ (consoleLog "no current task!")
                      pure ()
                    Just task -> do
                      io_ (consoleLog "found current task, swapping!")
                      sections %= swap task section droppedSection 
            _ ->
              pure ()
              
          currentSection .= Nothing
          io_ (consoleLog "drop")
-----------------------------------------------------------------------------
swap :: Task -> Section -> Section -> Map Section [Task] -> Map Section [Task] 
swap droppedTask oldSection newSection sections_ = do
  case M.filterWithKey (\s ts -> notElem (taskId droppedTask) (taskId <$> ts) && oldSection == s) sections_ of
    new -> M.insertWith (++) newSection [droppedTask] new
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
    , Main.onDragEnter (DragEnter section)
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
 -- document.addEventListener('DOMContentLoaded', () => {
 --            const tasks = document.querySelectorAll('.task');
 --            const taskLists = document.querySelectorAll('.task-list');
 --            const successMessage = document.getElementById('successMessage');
            
 --            let draggedItem = null;

 --            // Add event listeners to all tasks
 --            tasks.forEach(task => {
 --                task.addEventListener('dragstart', handleDragStart);
 --                task.addEventListener('dragend', handleDragEnd);
 --            });

 --            // Add event listeners to all task lists
 --            taskLists.forEach(list => {
 --                list.addEventListener('dragover', handleDragOver);
 --                list.addEventListener('dragenter', handleDragEnter);
 --                list.addEventListener('dragleave', handleDragLeave);
 --                list.addEventListener('drop', handleDrop);
 --            });

 --            function handleDragStart(e) {
 --                draggedItem = this;
 --                setTimeout(() => {
 --                    this.classList.add('dragging');
 --                }, 0);
 --            }

 --            function handleDragEnd() {
 --                this.classList.remove('dragging');
 --                draggedItem = null;
                
 --                // Show success message
 --                successMessage.classList.add('show');
 --                setTimeout(() => {
 --                    successMessage.classList.remove('show');
 --                }, 2000);
 --            }

 --            function handleDragOver(e) {
 --                e.preventDefault();
 --            }

 --            function handleDragEnter(e) {
 --                e.preventDefault();
 --                this.classList.add('drag-over');
 --            }

 --            function handleDragLeave() {
 --                this.classList.remove('drag-over');
 --            }

 --            function handleDrop(e) {
 --                e.preventDefault();
 --                this.classList.remove('drag-over');
                
 --                // Only move the item if it's being dropped in a different container
 --                if (this !== draggedItem.parentNode) {
 --                    this.appendChild(draggedItem);
 --                }
 --            }
 --        });

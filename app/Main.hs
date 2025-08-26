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
  | Drop
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
  { sections :: Map Section [Task]
  , _currentTask :: Maybe (Section, Task)
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
currentTask :: Lens Model (Maybe (Section, Task))
currentTask = lens _currentTask $ \record x -> record { _currentTask = x }
-----------------------------------------------------------------------------
initialModel :: Model
initialModel = flip Model Nothing $ M.fromList
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
          currentTask ?= (section, task)
        DragEnd _ _ ->
          currentTask .= Nothing
        DragOver -> pure ()
        DragEnter -> pure ()
        DragLeave -> pure ()
        Drop -> pure ()
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
showSection :: Maybe (Section, Task) -> (Section, [Task]) -> View Model Action
showSection maybeTask (section, tasks) =
  H.div_
  [ P.class_ "column" ]
  [ H.h2_ [P.class_ "column-title"] [ text (ms section) ]
  , H.div_
    [ P.id_ "todo"
    , P.class_ "task-list"
    , E.onDragOver DragOver
    , E.onDragEnter DragEnter
    , E.onDragLeave DragLeave
    , E.onDrop (AllowDrop True) Drop
    ]
    [ H.div_
        [ P.data_ "id" taskId
        , draggable_ True
        , P.classList_
          [ "task" =: True
          , "dragging" =: (Just (section, task) == maybeTask)
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
---
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
    , H.div_ [ P.class_ "container" ]
        (showSection (model ^. currentTask) <$> M.toList (sections model))
    , H.div_
        [ P.id_ "successMessage"
        , P.class_ "success-message"
        ]
        ["Task moved successfully!"]
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

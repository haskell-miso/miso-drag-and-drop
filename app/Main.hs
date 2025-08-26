-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Prelude hiding (unlines, rem)
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
import           Miso.Html.Element as H
import           Miso.Html.Event as E
import           Miso.Html.Property as P
import           Miso.String
-----------------------------------------------------------------------------
newtype Model = Model { _value :: Int }
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToMisoString Model where
  toMisoString (Model v) = toMisoString v
-----------------------------------------------------------------------------
value :: Lens Model Int
value = lens _value $ \m v -> m { _value = v }
-----------------------------------------------------------------------------
data Action
  = DragStart
  | DragEnd
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
app :: App Model Action
app = (component (Model 0) update_ viewModel)
  { events = dragEvents
  , styles = [ Href "assets/style.css" ]
  } where
      update_ = \case
        DragStart -> pure ()
        DragEnd -> pure ()
        DragOver -> pure ()
        DragEnter -> pure ()
        DragLeave -> pure ()
        Drop -> pure ()
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel _ =
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
        [ P.class_ "container" ]
        [ H.div_
            [P.class_ "column"]
            [ H.h2_ [P.class_ "column-title"] ["To Do"]
            , H.div_
                [ P.id_ "todo"
                , P.class_ "task-list"
                , E.onDragOver DragOver
                , E.onDragEnter DragEnter
                , E.onDragLeave DragLeave
                , E.onDrop (AllowDrop True) Drop
                ]
                [ H.div_
                    [ P.data_ "id" "1"
                    , draggable_ True
                    , P.class_ "task"
                    , E.onDragStart DragStart
                    , E.onDragEnd DragEnd
                    ]
                    [ H.div_ [P.class_ "task-title"] ["Design Homepage"]
                    , H.div_
                        [P.class_ "task-desc"]
                        ["Create wireframes for the new homepage design"]
                    ]
                , H.div_
                    [ P.data_ "id" "2"
                    , draggable_ True
                    , P.class_ "task"
                    ]
                    [ H.div_ [P.class_ "task-title"] ["Write Blog Post"]
                    , H.div_
                        [P.class_ "task-desc"]
                        ["Draft article about modern CSS techniques"]
                    ]
                ]
            ]
        , H.div_
            [P.class_ "column"]
            [ H.h2_ [P.class_ "column-title"] ["In Progress"]
            , H.div_
                [ P.id_ "progress"
                , P.class_ "task-list"
                , E.onDragOver DragOver
                , E.onDragEnter DragEnter
                , E.onDragLeave DragLeave
                , E.onDrop (AllowDrop True) Drop
                ]
                [ H.div_
                    [ P.data_ "id" "3"
                    , draggable_ True
                    , P.class_ "task"
                    ]
                    [ H.div_ [P.class_ "task-title"] ["Implement API"]
                    , H.div_
                        [P.class_ "task-desc"]
                        ["Connect user authentication to backend"]
                    ]
                ]
            ]
        , H.div_
            [P.class_ "column"]
            [ H.h2_ [P.class_ "column-title"] ["Review"]
            , H.div_
                [P.id_ "review", P.class_ "task-list"]
                [ H.div_
                    [ P.data_ "id" "4"
                    , draggable_ True
                    , P.class_ "task"
                    ]
                    [ H.div_ [P.class_ "task-title"] ["Fix Mobile Layout"]
                    , H.div_
                        [P.class_ "task-desc"]
                        ["Address responsive issues on mobile devices"]
                    ]
                ]
            ]
        , H.div_
            [P.class_ "column"]
            [ H.h2_ [P.class_ "column-title"] ["Done"]
            , H.div_
                [ P.id_ "done"
                , P.class_ "task-list"
                , E.onDragOver DragOver
                , E.onDragEnter DragEnter
                , E.onDragLeave DragLeave
                , E.onDrop (AllowDrop True) Drop
                ]
                [ H.div_
                    [ P.data_ "id" "5"
                    , draggable_ True
                    , P.class_ "task"
                    ]
                    [ H.div_ [P.class_ "task-title"] ["User Testing"]
                    , H.div_
                        [P.class_ "task-desc"]
                        ["Complete first round of user testing"]
                    ]
                ]
            ]
        ]
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

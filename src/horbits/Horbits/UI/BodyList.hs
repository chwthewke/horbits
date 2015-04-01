module Horbits.UI.BodyList
    (BodyList(BodyList), bodyListNew, bodyListView, bodyListModel, bodyListOnSelectionChange)
  where

import           Control.Lens
import           Control.Monad
import           Data.Tree
import           Graphics.UI.Gtk
import           Horbits.Body


data BodyList = BodyList { bodyListView              :: TreeView
                         , bodyListModel             :: TreeStore Body
                         , bodyListOnSelectionChange :: (Body -> IO ()) -> IO (ConnectId TreeView)
                         }


bodyListNew :: Forest Body -> IO BodyList
bodyListNew bs = do
    model <- treeStoreNew bs
    tree <- treeViewNewWithModel model
    treeViewGetSelection tree >>= flip treeSelectionSetMode SelectionSingle
    treeViewSetHeadersVisible tree False
    treeViewExpandAll tree
    (column, _) <- textColumn model (view bodyName)
    _ <- treeViewAppendColumn tree column
    return $ BodyList tree model (onBodyListSelectionChange tree model)

textColumn :: (TypedTreeModelClass m, TreeModelClass (m b)) =>
    m b -> (b -> String) -> IO (TreeViewColumn, CellRendererText)
textColumn model f = do
    renderer <- cellRendererTextNew
    column <- treeViewColumnNew
    treeViewColumnPackStart column renderer True
    cellLayoutSetAttributes column renderer model $ \r -> [ cellText := f r ]
    return (column, renderer)

getSelectionT :: TreeStore a -> TreeView -> IO [a]
getSelectionT m = treeViewGetSelection >=> treeSelectionGetSelectedRows >=> mapM (treeStoreGetValue m)

actSelection :: TreeView -> TreeStore Body -> (Body -> IO ()) -> IO ()
actSelection v m f = getSelectionT m v >>= mapM_ f

onBodyListSelectionChange :: TreeView -> TreeStore Body -> (Body -> IO ()) -> IO (ConnectId TreeView)
onBodyListSelectionChange v m f = v `on` cursorChanged $ actSelection v m f

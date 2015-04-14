module Horbits.UI.BodyList
    (BodyList(BodyList), BodyListSelectionChange,
     bodyListNew, bodyListView, bodyListModel, bodyListOnSelectionChange)
  where

import           Control.Lens
import           Control.Monad
import           Data.Tree
import           Graphics.UI.Gtk

import           Horbits.Body

type BodyListSelectionChange = (Maybe Body -> IO ()) -> IO (ConnectId TreeView)

data BodyList = BodyList { bodyListView              :: ScrolledWindow
                         , bodyListModel             :: TreeStore Body
                         , bodyListOnSelectionChange :: BodyListSelectionChange
                         }


bodyListNew :: Forest Body -> (PolicyType, PolicyType) -> IO BodyList
bodyListNew bs (hp, vp) = do
    model <- treeStoreNew bs
    tree <- treeViewNewWithModel model
    treeViewGetSelection tree >>= flip treeSelectionSetMode SelectionSingle
    treeViewSetHeadersVisible tree False
    treeViewExpandAll tree
    (column, _) <- textColumn model (view bodyName)
    _ <- treeViewAppendColumn tree column
    bodyListScroll <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy bodyListScroll hp vp
    containerAdd bodyListScroll tree
    return $ BodyList bodyListScroll model (onBodyListSelectionChange tree model)

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

actSelection :: TreeView -> TreeStore Body -> (Maybe Body -> IO ()) -> IO ()
actSelection v m f = getSelectionT m v >>= f . preview traverse

onBodyListSelectionChange :: TreeView -> TreeStore Body -> BodyListSelectionChange
onBodyListSelectionChange v m f = v `on` cursorChanged $ actSelection v m f

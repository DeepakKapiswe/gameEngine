module GE.UndoRedo where


data UndoRedo a = UndoRedo {
    undoLimit :: Int       -- Limit Of Undo and Redo Operations
  , undoStack :: [a]       -- The Stack containg Undo Data
  , currUndos :: Int       -- Current Length of Undo Data Stack
  , redoStack :: [a]       -- Redo Data Stack
} deriving (Show, Eq)

undo :: UndoRedo a -> Either String (a, UndoRedo a)
undo ur@(UndoRedo ul us 0 _ )  = Left "Oops !! Can't Undo Anymore! "
undo ur@(UndoRedo ul us cu rs) = Right ( val, newUR)
  where
    val   = head us
    newUR = ur {undoStack = tail us, redoStack = val : rs ,currUndos = cu -1 }

addToUR :: a -> UndoRedo a -> UndoRedo a
addToUR val ur@(UndoRedo ul us cu rs) | cu < ul =
  ur { undoStack = val : us, currUndos = cu + 1} 
                                      | otherwise =
  ur { undoStack = take ul ( val : us)}
  
redo :: UndoRedo a -> Either String (a, UndoRedo a)
redo ur@(UndoRedo ul us _ [] )  = Left "Oops !! Can't Redo Anymore! "
redo ur@(UndoRedo ul us cu rs) = Right ( val, newUR)
  where
    val   = head rs
    newUR = ur {undoStack = val : us, redoStack = tail rs, currUndos = cu + 1}


initUndoRedo :: Int -> UndoRedo a
initUndoRedo limit = UndoRedo limit [] 0 []

resetUndoRedo :: UndoRedo a -> UndoRedo a
resetUndoRedo ur@(UndoRedo ul us cu rs) = UndoRedo ul [] 0 []

undoWithDef :: UndoRedo a -> a -> (a, UndoRedo a)
undoWithDef ur defValue = case undo ur of
  Left _ -> (defValue, ur)
  Right x -> x

redoWithDef :: UndoRedo a -> a -> (a, UndoRedo a)
redoWithDef ur defVal = case redo ur of
  Left _ -> (defVal, ur)
  Right x -> x
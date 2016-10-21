module ObjectModel where

data ObjectModel = ObjectModel {
				 size	    :: {-# UNPACK #-} !Int
			       } deriving (Eq,Show)


data ChunkID =   Nursery
               | Large
	       | Adult
               | ID {-#UNPACK #-} !Int
	       deriving (Eq,Show)

--updateChunk::Int->ObjectModel->ObjectModel
--updateChunk i o = o{chunk=ID i}

module Queue where

data Queue a =
  Queue [a]
        [a]

pop :: Queue a -> (Maybe a, Queue a)
pop (Queue [] []) = (Nothing, Queue [] [])
pop (Queue (q:queue) fill) = (Just q, Queue queue fill)
pop (Queue [] fill) = pop $ Queue (reverse fill) []

push :: a -> Queue a -> Queue a
push p (Queue queue fill) = Queue queue (p : fill)

emptyQueue :: Queue a 
emptyQueue = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

# Monads

```haskell
fmap :: a -> b -> f a -> f b       -- Functor
pure :: a -> f a                   -- Applicative
apply :: f (a -> b) -> f a -> f b  -- Applicative

```

- Where clause in Data API
```haskell
-- Unit () Type

data () = () -- check for errors


typeCheckWC :: SymbolTable -> WC -> Either String ()

-- SymbolTable 
-- Map from table names to table information

type SymT = Map (TableName) (TableInfo)
type TableData = Map (ColName) (ColInfo)

typeCheckWC :: TableName -> SymbolTable -> WC -> Either String ()

data WC = WCS ColName Op Value

-- e.g. {"c": 1} becomes  WCS "c" EQ (ValueInt 1)

-- See whether the column exists

M.lookup :: Map k v -> k -> Maybe v

-- implements 

typeCheck tn st wc = case wc of
  wcs cn op v = case M.lookup st tn of
    Just ti -> case M.lookup cn ti of
      Just ci -> ...
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- cant do this

-- applicative? cant do 

-- custom lookup function and fmap
lookupCI :: ColName -> m -> MaybeCI

fmap (lookup cn) (mTi) :: Maybe (Maybe CI)

-- we need to collapse the maybe

join :: m (m a) -> m a

wcs cn op v -> case join (M.lookup cn (M.lookup tn st)) of
  Just ci -> ...

-- Maybe implements this join function

join :: Maybe (Maybe a) -> Maybe a
join (Just x)  = x
join Nothing  = Nothing

-- more simplified

f :: ma -> (a -> m b) -> m b -- >>=


-- then,

wcs cn op v -> case (M.lookup tn st) >>= lookupCI cn tf

-- kind of piping values
```

- Typeclass Monad is mathematically defined by join, but in Haskell, it is implemented using bind (>>=)

```haskell
bind c f = join (fmap f c)

return :: a -> m a
```

- Monad as a typeclass which provides bind
- Monad is applicative

- define bind for a type to be member of Monad typeclass
- `return == pure`
- return wraps the value in a container

```haskell
 (a ->  b) C a  -- functor, go inside and do something
C(a ->  b) C a  -- applicative, 
 (a -> Cb) Ca   -- monad

```

- join of list

```haskell
join :: [[a]] -> [a]
```

- Fundamental concept since it is the only context in which IO can be done. IO means interacting to the external world.

```haskell
IO :: * -> * -- type, there are no constructors for IO

putStrLn :: String -> IO ()

```

- echo program
```haskell

putStrLn :: String -> IO ()
readString :: IO String

main :: IO ()
main readString >>= putStrLn


```

- syntactic sugar for monads
```haskell
f1 :: a -> Maybe b
f2 :: b -> Maybe c
f3 :: c -> Maybe d

fn :: a -> Maybe d
fn v = f1 v >>= f2 >>= f3

fn v = f1v >>= (\x -> f2 x) -- expanded form
           >>= (\y -> f3 y)

fn v =            -- monad syntactic sugar 
   do x <- f1 v
      y <- f2 x
      f3 y

-- echo function
main :: do
  s <- readSring
  putStrLn s
```

- bind function for Either a
```haskell
bindEither ::  m (m a) -> m a
bindEither :: Either a b -> (b -> Either a c) -> Either a c
bindEither (

instance Monad (Either a) where
  >>= e f = case e of
      Left se -> Left se
      Right s -> f s

```

## Reader Monad

```haskell
f  :: ST -> d        -- R d
f1 :: ST -> b        -- R b
f2 :: ST -> c        -- R c
f3 :: b -> c -> d    -- R d

f = f3 (f1 ST) (f2 ST)

-- several functions needs ST, a value
-- instead of returning d, b, c, d return m d, m b, m c 

-- askST gives you R ST
askST :: R ST

peelR :: ST -> R a -> a

-- R is Monadic

f :: R d
f = do
  b <- f1
  c <- f2
  return (f3 b c)

-- old way
main = do
  st <- readST
  show (f a)

-- new way
main = do
  st <- readST
  show (peelR ST f)
```

- implementation

```haskell
data Reader r a 

peelReader :: r -> Reader r a -> a
ask :: Reader r r
```

- actual implementation

```haskell
data Reader r a = 
  Reader (r -> a)  

instance Monad (Reader r) where
  return :: a -> Reader r a
  return v = Reader (\_ -> v)

  bind :: Reader r a -> (a -> Reader r b) -> Reader r b
  bind (Reader f) ft = Reader (\r -> 
                            let a = f r
                            in ft a 
                          )
  ask = Reader (\r -> r)
  runReader r (Reader f) = f r
```

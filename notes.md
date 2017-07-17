haskell
=======

```haskell
data Maybe a = Just a
             | Nothing
```

Type signature of lookup function:
`lookup :: Map -> Int -> Maybe Int`


```haskell
data Maybe a = Just a
             | Nothing

data Either a b = Left a
                | Right b
```
type constructor: Maybe (1 arguments), Either (2 arguments)
type parameters: a, b

like Values have Types, Types have Kinds

Types: property of a value
Kind: property of a type

Value -> Type -> Kind

Int :: *
Maybe :: * -> *
Either :: * -> * -> *

Maybe Int :: *
Either Int :: * -> *

* : concrete type x parameterized type


## Type Classes

```haskell
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
  	-- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```

```haskell
Prelude> :i Functor
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
  	-- Defined in ‘GHC.Base’
instance Functor (Either a) -- Defined in ‘Data.Either’
instance Functor [] -- Defined in ‘GHC.Base’
instance Functor Maybe -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Functor ((->) r) -- Defined in ‘GHC.Base’
instance Functor ((,) a) -- Defined in ‘GHC.Base’
```

`f :: * -> *`
f cannot be implemented for types of kind other than `* -> *`

## Lists

define own lists

```haskell
data IntList = Empty
						 | IntCons Int IntList  -- recursive datatypes

a = Empty
b = IntCons 1 (IntCons 2 Empty) -- b = [1 2]

-- generic list which can have any types
data List a = Empty
					  | Cons a (List a)

printList :: (List a) -> String
printList l = case l of
						Empty -> ""
						(Cons x xs) -> (Show x) ++ "," ++ printList xs
```

[1 2 3 4] :  1 and 2 and 3 and 4 and empty list

### Haskell List

```haskell
a = []
b = 1:[]

data [] a = []
				  | : a [a]

-- length
length :: [a] -> Int
length l = case l of
			[] -> 0
	  x xs -> 1 + length xs

length [] = 0
length x:xs = 1 + length xs
```

### Multifanout tree
tree with arbitrary number of branches

```haskell
data MFTree = Leaf a
					  | Node [MFTree a] 
```
a tree can have either a node or a leaf.

[1..100] -- List on 1 to 100 

increment 1 to all elemets and return new list

```haskell
inc1 :: [a] -> [a]
inc1 [] = []
inc1 (x:xs) = (x+1):(inc1 xs)

opList :: (a->b) -> [a] -> [b]
opList f l = case l of
        [] -> []  
			x:xs -> (f s):(opList xs) --incomplete

incOpList l = opList (\x->x+1) l
-- \ anonymous function
```

- apply f over all elements in the tree
```haskell
mapTree :: (a->b) -> Tree a -> Tree b
mapTree f t = case t of
	Leaf a -> Leaf (f a)
	Node trees -> Node (map (mapTree f) trees)

-- look at the type that needs to be returned
  
```


```haskell
map :: (a->b)
		-> List a -> List b

mapTree :: (a->b)
				-> Tree a -> Tree b

fMap :: (a->b)
		 -> c a -> c b
```

functor type class implements fmap
c - container

fmap :: (a -> b) -> c a -> c b

c has to be of kind * -> *

instance Functor Tree where
	fmap = 


-- behaviour of fmap on Maybe

```haskell
-- functor for Maybe

fmap f m = case m of
	Just v -> Just (f v)
	Nothing -> Nothing

-- functor for Either

instance Functor (Either a) where
fmap f e = case e of
	Left v -> Left v
	Right v -> Right (f v)
```

BiFunctor implements functor for * -> * -> *
take two functions

## Folding together

```haskell
-- sum of list

sum :: [Num] -> Num
sum l = case l of
	[] -> 0
	x:xs -> x + sum xs
```

foldr
```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

Prelude> foldr (+) 0 [1..100]
5050
```

Foldable type class defines the property of accumulating things

Monoid type class defines the property of appending


## Parsing

```haskell
data Point = Point Int Int


parsePoint :: String -> Maybe Point
parsePoint = ...... -- implemented


data ShapeLoc = TriangleLoc Point Point Point
							| SquareLoc Point Point

parseShapeLoc :: String -> Maybe ShapeLoc
parseShapeLoc s = case sl of
	[] -> Nothing
	["T":xs] -> parseTriangleLoc xs
	["S":xs] -> parseSquareLoc xs
	where
		sl = split (:) s

parseTriangleLoc :: String -> Maybe ShapeLoc
parseTriangleLoc [p1, p2, p3] = let 											-- p1 p2 p3 are String
		mp1 = parsePoint p1
		mp2 = parsePoint p2
		mp2 = parsePoint p3
	in 
    apply (apply (apply (Just TriangleLoc) (mp1)) (mp2)) (mp3)    -- TriangleLoc :: p -> p -> p -> s

-- if apply was an inflix operator <*>

	in 
		(Just TriangleLoc) <*> mp1 <*> mp2 <*> mp3
		

apply :: Maybe (a->b)
	-> Maybe a
	-> Maybe b
apply (Just f) (Just v) = Just (f v)
apply _ _ = Nothing

{#
	<*> applicative
	if something is an applicative, its automatically a functor
#}

<*> :: c (a->b)
		-> c a
		-> c b

pure :: a -> c a

fmap :: a -> b
		 -> c a
		 -> c b

fmap f c = pure f <*> c		
```

#### Example

```haskell

parsePerson :: Value -> Parser Person 
parseAddress :: Value -> Parser Address 

data Value = VA [Value]
					 | VN Double
					 | VS String
					 | VO (Map String Value)
					 | Null

runParser :: String -> Parser a -> Either String a

data Bill = Bill Person Address

{# 
	bill: {
		person: <person>
		address: <address>
	}
#}
```

# 17 Jul 2017

## Applicative and Functor

```haskell
(+) <$> [1..4] <*> [100..104]
```

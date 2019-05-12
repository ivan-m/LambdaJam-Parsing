---
title: Just Parsing Through
author: Ivan Lazar Miljenovic
date: 14 May, 2017
...

About this workshop
===================

## Introductory presentation

. . .

### Feel free to jump straight to the exercises if you prefer.

## Getting the exercises

```{.bash style="font-size:70%"}
git clone https://github.com/ivan-m/LambdaJam-Parsing-exercises.git
```

Why Parser Combinators?
=======================

## Consider a common alternative...

## {#regex-cthulhu data-background-image="images/stack-overflow-regex-zalgo.png" data-background-size="contain" data-background="white"}

. . .

### This is what using regexes brings you to! Madness!

---
# https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454
...

## Why not regexes?

> * Stringly-typed
> * Not re-usable
> * Difficult to read
> * Re-interpreted by compiler

## Compare the pair

Regular Expressions
:   ```{.haskell style="font-size:60%"}
    "\.\([[:alpha:]][[:alnum:]-_:\.]*\)"
    ```

Parser Combinators
:   ```{.haskell style="font-size:60%"}
    identifierAttr = do
      char '.'
      first <- letter
      rest <- many $ alphaNum
                     <|> oneOf "-_:."
      return (first:rest)
    ```

::: notes
* Sample taken from Pandoc, identifier in attribute
* regex shorter
* Have I matched all the parens properly?
* combinator version could be shorter
* Which is more readable? combinable?
* regexes more convenient for custom munging
* I forgot the `char '.'`; which is it easier to spot in?
* Only realised long after writing: regex is invalid!

:::

## OK, Regexes have their uses...

> * Command-line
> * Inline searches
> * Quick validation

::: notes

* What is common with all these?
* They're small!

:::

## What about Parser Generators?

::: notes

* *Requires* a grammar
* Combinators easier to extend
* Probably faster
* Can emulate in a parser combinator
* Not embeddable in code
* Usually needs an external tool

:::

## Availability of parser combinators

* Most (all?) FP languages
* Javascript
* Java
* C#
* C++
* etc.

. . .

> **Not just for text!**

::: notes

* Multiple implementations (parsec, attoparsec, polyparse,
  trifecta, etc.)
* Rite of passage!
* If Java has it, *of course* C# has to have it to prove they're
  better...

:::

What are we going to parse today?
=================================

## Motivating example?

. . .

### Regexes!

::: notes

* After all, I've made fun of them enough!

:::

## Regex Representation

```haskell
-- Top-level: abc|(de)+f|ge?|[x-z]
type Pattern = [ConcatenatedAtoms]

-- Sequence that must be matched in turn
type ConcatenatedAtoms = [QuantifiedAtom]
```

## Regex Representation (2)

```haskell
-- An individual item to match with a quantifier.
data QuantifiedAtom = PlainAtom    Atom
                    | OptionalAtom Atom -- ?
                    | AtLeastOne   Atom -- +
                    | Multiple     Atom -- *
```

::: notes

* Not necessarily the best implementation
* Leads well to our parsing

:::

## Regex Representation (3)

```haskell
-- Something to be matched
data Atom = AnyChar              -- .
          | SpecificChar Char
          | BE BracketExpression
          | SubPattern Pattern
```

## Regex Representation (4)

```haskell
-- @[1-9]@, @[^a-z]@, etc.
data BracketExpression = BracketExpression
  { -- Starts with ^
    inverseExpression :: Bool

    -- Non-empty list.
  , bracketPatterns   :: [BracketPattern]
  }
```

## Regex Representation (5)

```haskell
-- An element in a bracket expression
data BracketPattern = BracketRange Char Char
                    | BracketChar  Char

-- The characters that must typically be escaped.
metaChars :: [Char]
metaChars = [ '.', '|', '\\', '(', ')'
            , '[', ']', '*', '+', '?'
            ]
```

::: notes

* For simplicity, only consider alphanumeric for `BracketPattern`.

:::

Defining our Parser
===================

## What is a parser?

```haskell
type Parser a = String -> a
```

::: notes

* Sounds like what we ultimately want
* Does it give us what we need?

:::

## Consider parsing an `Atom`

> * Let's parse `.`
> * Imagine a function `next :: Parser Char`{.haskell}

::: notes

* We get the `.`
* Then what?
* Where's the rest of the input?

:::

## Take 2

```haskell
type Parser a = String -> (a, String)
```

::: notes

* Returns unconsumed input
* What happens if it _isn't_ a `.`?
* This type says it always returns a value! What if it's wrong?

:::

## Take 3


```haskell
-- The result of a parser: either an error
-- or the expected result.
data Result a = Err String
              | OK a
              deriving (Show)

type Parser a = String -> (Result a, String)
```

::: notes

* We can use this now!
* But it lets the users of our parser manipulate internals.

:::

## Final definition

```{.haskell style="font-size:70%"}
newtype Parser a = P { runP :: String -> (Result a, String) }
```

. . .

```{.haskell style="font-size:70%"}
-- Shhhhhh!!!!!
newtype State s a = St { runState :: s -> (a, s) }
```

::: notes

* Don't export constructor
* `runP` runs the parser
* `newtype` is run-time isomorphic to original
* Implemented in `Simple.hs`
* Specialised version of State Monad
* Actual implementation adds the unconsumed input into `Result`

:::
Let's start parsing!
====================

## Create basic parsers

```haskell
-- Lift a value into a parser.
liftParser :: a -> Parser a
liftParser a = P $ \str -> (OK a, str)

-- Throw a parser error.
failParser :: String -> Parser a
failParser err = P $ \str -> (Err err, str)
```

## Get some data

```haskell
-- Obtain the next character in the input string.
next :: Parser Char
next = P $ \str -> case str of
                     c:str' -> (OK c, str')
                     _      -> (Err "empty", str)
```

## Matching a predicate

```{.haskell style="font-size:90%"}
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \str ->
  case str of
    c:str' | p c       -> (OK c, str')
           | otherwise -> (Err "not satisfied", str)
    _                  -> (Err "empty", str)

-- For example: parse the specified character.
char :: Char -> Parser Char
char c = satisfy (c==)
```

::: notes

* So we can use `char '.'`
* This definition looks a lot like `next`...
* Idea: call next, get the result, then test it!

:::

## Matching again

```{.haskell style="font-size:70%"}
-- Take the result from one parser, and pass it as a
-- parameter to a function that returns a parser.
withResult :: Parser a -> (a -> Parser b) -> Parser b
withResult pa f = P $ \str -> case runP pa str of
  (OK a,  str') -> runP (fpb a) str'
  (Err e, str') -> (Err e, str')

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = next `withResult` checkNext
  where
    checkNext c
      | p c       = toParser c
      | otherwise = failParser "not satisfied"
```

::: notes

* Better!
* `withResult` can be very handy

:::

## Let's get an `AnyChar`

. . .

```haskell
onSuccess :: Parser a -> Parser b -> Parser b
onSuccess pa pb = pa `withResult` (\_ -> pb)

parseAnyChar :: Parser Atom
parseAnyChar = char '.'
               `onSuccess`
               liftParser AnyChar
```

::: notes

* If we have a `.` we need to produce an `AnyChar`
* We can use `withResult` and ignore the first argument!

:::

## What happens if it _isn't_ `'.'`?

. . .

### Backtracking options

> 1. Return every single possible parse
> 2. `try`-based semantics
> 3. `commit`-based semantics
> 4. "Just do it already"-based semantics

::: notes

1. This is implemented by `ReadS`; no-one uses it
2. Overlapping patterns and forget the `try`? No backtracking!
3. Forget the `commit`? Still works, just worse error messages and
   possibly worse performance.
4. As epitomised by `attoparsec`.

## Taking Option 4

```haskell
-- If the first one fails, try the second one
onFail :: Parser a -> Parser a -> Parser a
onFail p1 p2 = P $ \str -> case runP p1 str of
  (Err _, _) -> runP p2 str
  ok         -> ok

-- Try the parsers in turn until one succeeds
oneOf :: [Parser a] -> Parser a
oneOf = foldr onFail (failparser "all failed")
```

::: notes

* So we can now try parsing the next one.

:::

## Parsing `SpecificChar`

```haskell
parseCharacter :: Parser Char
parseCharacter = satisfy (`notElem`metaChars)
                 `onFail`
                 (char '\\'
                  `onSuccess`
                  satisfy (`elem`metachars)
                 )
```

::: notes

* We need to somehow wrap the `Char` with `SpecificChar`

:::

## Apply a function to the result

```haskell
mapResult :: (a -> b) -> Result a -> Result b
mapResult f (OK a)  = OK (f a)
mapResult _ (Err e) = Err e

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p = P $ \str ->
  let (res, str') = runP p str
  in (mapResult f res, str')
```

## That reminds me of something

```{.haskell style="font-size:80%"}
mapResult :: (a -> b) -> Result a  -> Result b

mapParser :: (a -> b) -> Parser a  -> Parser b

map       :: (a -> b) ->       [a] ->       [b]

-- Generalising

mapF      :: (a -> b) -> f      a  -> f      b
```

## I dub thee Functor!

```{.haskell style="font-size:90%"}
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Result where
  fmap = mapResult

instance Functor Parser where
  fmap = mapParser

-- (<$>) is an infix alias for fmap
```

::: notes

* `<$>` vs `$`
* Functor laws

:::

## What we have so far

```haskell
parseAtom :: Parser Atom
parseAtom = parseAnyChar
            `onFail`
            fmap SpecificChar parseCharacter
```

::: notes

* Now need to look at parsing bracket expressions
* Let's start with the bracket patterns

:::

## Bracket Patterns

```haskell
parseBracketPattern :: Parser BracketPattern
parseBracketPattern =
  fmap BracketChar pChar
  `onFail`
  (pChar `withResult` \s ->
     char '-' `onSuccess`
       fmap (BracketRange s) pChar
  where
    pChar = satisfy isAlphaNum
```

::: notes

* Remember, we're simplifying by only considering alphanumeric bracket
  expression patterns.
* Can you see what's wrong with this?
* We're not really _using_ the result from `withResult`; can we make
  it nicer?

:::

## Applying lifted functions

```{.haskell style="font-size:80%"}
apply :: Parser (a -> b) -> Parser a -> Parser b
apply pf pa = P $ \str -> case runP pf str of
  (OK f, str')  -> runP (f <$> pa) str'
  (Err e, str') -> (Err e, str')

parseBracketPattern :: Parser BracketPattern
parseBracketPattern = (fmap BracketRange pChar `apply`
                         (char '-' `onSuccess` pChar))
                      `onFail`
                      fmap BracketChar pChar
  where
    pChar = satisfy isAlphaNum
```

::: notes

* Could always have done `f <$>` after `runP`

:::

## Re-visiting `onSuccess`

```haskell
onSuccess :: Parser a -> Parser b -> Parser b
onSuccess pa pb = (flip const <$> pa) `apply` pb

discard :: Parser a -> Parser b -> Parser b
discard pa pb = (const <$> pa) `apply` pb

-- const a b = a
-- flip f a b = f b a
-- flip const a b = const b a = b
```

::: notes

* `flip`, `const` already exists
* `apply` is a very common pattern

:::

## Applying all this

```{.haskell style="font-size:70%"}
class (Functor f) => Applicative f where
    -- Lift a value.
    pure :: a -> f a

    -- Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Parser where
  pure = liftParser
  (<*>) = apply

onSuccess = (*>)
discard   = (<*)
```

::: notes

* `*>` and `<*` are pre-defined (though can be overriden in newer
  versions of the typeclass)

:::

## Alternatively...

```{.haskell style="font-size:70%"}
class (Applicative f) => Alternative f where
    -- The identity of '<|>'
    empty :: f a

    -- An associative binary operation
    (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  empty = failParser "empty value"
  (<|>) = onFail

-- With Alternative, we also get for free:
many, some :: Parser a -> Parser [a]
```

::: notes

* `some` is non-empty; `many` can be empty
* `oneOf` stays as-is

:::

## Using Applicative

```haskell
parseBracketPattern :: Parser BracketExpression
parseBracketPattern =
  (BracketRange <$> pChar <*> (char '-' *> pChar)
  <|>
  (BracketChar <$> pChar)
  where
    pChar = satisfy isAlphaNum
```

::: notes

* We're parsing the first character both times
* We can solve this with `withResult`, but the syntax is a bit clunky.
* Is there a typeclass that captures `withResult`?

:::


## Warm Fuzzy Things

> * aka _Workflows_
> * ~~aka _Burritos_~~
> * aka "a monoid in the category of endofunctors"
> * aka [_Monads_]{.smallcaps}

::: notes

* Workflows are from F#
* SPJ: Our biggest mistake: Using the scary term "monad" rather than
  "warm fuzzy thing" (Wearing the hair shirt: a retrospective on
  Haskell (2003))
* "monoid" from "A Brief, Incomplete, and Mostly Wrong History of
  Programming Languages" by James Iry (supposedly Philip Wadler)
* Burritos from: Abstraction, intuition, and the “monad tutorial
  fallacy” by Brent Yorgey

:::

## The "M" word

```haskell
class (Applicative m) => Monad m where
    -- Usually called "bind"
    (>>=) :: m a -> (a -> m b) -> m b

    -- This is being moved into 'MonadFail'
    fail :: String -> m a

instance Monad Parser where
  (>>=) = withResult
  fail = failParser
```

::: notes

* Technically also has `return` and `(>>)`, but these are nowadays
  just aliases of `pure` and `(*>)`.
* `fail` is considered a wart; used for pattern matching failures;
  MonadFail in GHC 8.6
* Parsers possibly only semi-valid use of explicit `fail` (but better
  to use specialised function)

:::

## What do Monads give us?

```haskell
ma >>= f === do a <- ma
                f a

ma *> mb === do ma
                mb
```

## Using Monads

```haskell
parseBracketPattern :: Parser BracketPattern
parseBracketPattern = do
  c <- pChar
  oneOf [ char '-' *> fmap (BracketRange c) pChar
        , pure (BracketChar c)
        ]
  where
    pChar = satisfy isAlphaNum
```

## Bracket Expressions

```{.haskell style="font-size:80%"}
parseBracketExpression :: Parser BracketExpression
parseBracketExpressoin = bracket (char '[') (char ']')
  $ BracketExpression <$> checkInverse
                      <*> some parseBracketPattern
  where
    checkInverse = isJust <$> optional (char '^')

bracket :: Parser bra -> Parser ket -> Parser a -> Parser a
bracket pb pk pa = pb *> pa <* pk

-- Pre-defined
optional :: (Alternative f) => f a -> f (Maybe a)
```

## Finishing `parseAtom`

```{.haskell style="font-size:80%"}
parseAtom :: Parser Atom
parseAtom = oneOf [ char '.' *> pure AnyChar
                  , SpecificChar <$> parseCharacter
                  , BE <$> parseBracketExpression
                  , SubPattern <$> parseSubPattern
                  ]
  where
    parseSubPattern = bracket (char '(') (char ')')
                              parsePattern

-- Assume defined
parsePattern :: Parser Pattern
```

::: notes

* Remember, `parsePattern` is our top-level parser
* How do you do recursive parser calls with regexes?

:::

## Quantify atoms

```{.haskell style="font-size:80%"}
-- (<**>) is the same as (<*>)
-- but in the opposite order.
parseQuantifiedAtom :: Parser QuantifiedAtom
parseQuantifiedAtom = parseAtom <**> parseQuantifier

parseQuantifier :: Parser (Atom -> QuantifiedAtom)
parseQuantifier = oneOf [ char '?' *> pure OptionalAtom
                        , char '+' *> pure AtLeastOne
                        , char '*' *> pure Multiple
                        ,             pure PlainAtom
                        ]
```

::: notes

* Can you see why PlainAtom has to be last?

:::

## Wrapping up

```{.haskell style="font-size:80%"}
parsePattern :: Parser Pattern
parsePattern = sepBy1 parseConcatenatedAtoms (char '|')

-- Parse a non-empty list of items
-- separated by discarded junk.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- We need to allow the empty case as that's valid!
parseConcatenatedAtoms :: Parser ConcatenatedAtoms
parseConcatenatedAtoms = many parseQuantifiedAtom
```

## Let's test it out!

```{.haskell style="font-size:70%"}
λ> runParser parsePattern "abc|(de)+f|ge?|[x-z]"
(OK [ [ PlainAtom (SpecificChar 'a'),PlainAtom (SpecificChar 'b')
      , PlainAtom (SpecificChar 'c')]
    , [ AtLeastOne (SubPattern [[PlainAtom (SpecificChar 'd')
                                ,PlainAtom (SpecificChar 'e')]])
      , PlainAtom (SpecificChar 'f')]
    , [ PlainAtom (SpecificChar 'g'),OptionalAtom (SpecificChar 'e')]
    , [ PlainAtom (BE (BracketExpression False
                                         [BracketRange 'x' 'z']))]
    ]
,""
)
```

Exercises
=========

## Exercises

```{.bash style="font-size:70%"}
git clone https://github.com/ivan-m/LambdaJam-Parsing-exercises.git
```

1. Take a `Pattern`{.haskell} and turn it into a `Parser`{.haskell}
2. Add in `commit`{.haskell} functionality to backtracking.


---
# reveal.js settings
theme: night
transition: concave
backgroundTransition: zoom
center: true
history: true
css: custom.css
...

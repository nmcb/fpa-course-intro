-- This exercise is expressed in the programming langauge Haskell,
-- the reason why this is the case is difficult to explain.  As we
-- say in the author's Dutch native language: "ter leering ende
-- vermaeck" is probably the best reason though difficult to trans-
-- late.  The reader is allowed to infer his or her own reasons.
--
-- ( Independent of this, any language expressing solutions is OK. )
--
-- Install the Haskell `ghci` interpreter to load this file.
--
-- ```ghci
-- ✔ ~/code/fpa-course-intro/src/main/haskell [master|✚ 7⚑ 1] λ ghci
-- GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
-- Loaded GHCi configuration from /Users/marco/.ghc/ghci.conf
-- Prelude =>> :load list-comprehension.hs
-- [1 of 1] Compiling Main    ( list-comprehension.hs, interpreted )
-- Ok, one module loaded.
-- *Main =>>
-- ```
--
-- Haskell values can be interpreted by calling them from the GHCi
-- prompt.  E.g. to evaluate `boys` as defined below, just call that
-- value.
--
-- ```ghci
-- *Main =>> boys
-- [Matthew,Peter,Jack,Arnold,Carl]
-- it :: [Boy]
-- *Main =>>
-- ```


--
-- *** ***        Exercise: Crime Scene Investigation         *** ***
--
-- A group of five school children is caught in a crime.  One of them
-- has stolen something from some kid they all dislike.  The head-
-- mistress has to find out who did it.  She questions the children,
-- and this is what they say:
--
-- Matthew: Carl didn't do it, and neither did I.
-- Peter: It was Matthew or it was Jack.
-- Jack: Matthew and Peter are both lying.
-- Arnold: Matthew or Peter is speaking the truth, but not both.
-- Carl: What Arnold says is not true.
--
-- Their class teacher now comes in.  She says: three of these boys
-- always tell the truth, and two always lie.  You can assume that
-- what the class teacher says is true.  Use Haskell or some other
-- functional programming language of your choice to write a function
-- that computes who was the thief, and a function that computes
-- which boys made honest declarations.
--
-- Here are some definitions to get you started.


-- We define the data type `Boy` as a list of values, deriving The
-- type class instances `Eq` and `Show` (which provide the equality
-- operators `==`, `/=` and `show`).  In Scala, e.g., this data
-- type is encoded using a `sealed trait Boy` with `case object`
-- instances `Matthew`, `Peter`, etc. extending that trait.  I.e.
-- in Haskell the `data` type `|` operator combines coproduct
-- values.
data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

-- Then we define the value `boys` to be of type list of data type
-- `Boy`s and to contain the values `Matthew`, `Peter`, etc.  In
-- Scala, e.g. this value is encoded instantiating a concrete list
-- of type `List[Boy]`.
boys :: [Boy]
boys =  [Matthew, Peter, Jack, Arnold, Carl]


-- This defines the function value `accuses` to encode whether given
-- first argument `Boy` accuses given second argument `Boy`.  Encode
-- this function according to the stated crime investigation case
-- depicted above.
--
-- Q1: Implement `accuses` by means of pattern matching.
accuses :: Boy -> Boy -> Bool
accuses undefined

-- This defines the function value `accusers` to encode which `[Boy]`s
-- accuse given argument `Boy`.  I.e. the function returns the list of
-- boys that accuse the boy this function value is applied to.
--
-- Q2: Implement `accusers` by means of a filtering list-comprehension.
accusers :: Boy -> [Boy]
accusers =  undefined

-- This defines the value `guilty` which encodes the list of `[Boy]`s
-- that is guilty of the crime.  I.e. the exercise is defined in terms
-- of accusations such that `guilty` can be expected to be a singleton
-- list that contains the culprit of the theft provided that three boys
-- accuse that boy.
--
-- Q3: Implement `quilty` by means of a filtering list-comprehension.
guilty :: [Boy]
guilty =  undefined

-- This defines the value `honest` which encodes the list of `[Boy]`s
-- that answer `accuse` truthfully.  I.e. the list of boys who made
-- honest (true) statements.  Hint, re-use `guilty` to calculate the
-- value.
--
-- Q4: Implement `honest` as a combination of `guilty` and `accusers`
honest :: [Boy]
honest =  undefined

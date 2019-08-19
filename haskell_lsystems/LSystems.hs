module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (ang, base, rules)
  = ang

-- |Returns the base string for the given system.
base :: System -> String
base (ang, base, rules)
  = base

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (ang, base, rules)
  = rules

-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar ch rules
  = concat [out | (x, out) <- rules, x == ch]

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne rules [] = []
expandOne rules base
  = lookupChar (head base) rules ++ expandOne rules (tail base)

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand _ base 0     = base
expand rules base times
  = expand rules (expandOne rules base) (times-1)

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move command state deg
  | command == 'L' = ((x, y), m)
  | command == 'R' = ((x, y), n)
  | command == 'F' = (((x + cos rad), (y + sin rad)), point)
  | otherwise      = error "Command is not recognized by the turtle."
    where
      ((x, y), point) = state
      m               = (point + deg)
      n               = (point - deg)
      rad             = (point*pi/180)

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 1
trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 (rule:rules) ang clr
  = list
  where (list, _) = trace1' (rule:rules) ((0.0, 0.0), 90) ang clr 

trace1' :: String -> TurtleState -> Float -> Colour -> ([ColouredLine], String)
trace1' [] _ _ _ 
  = ([], [])
trace1' (rule:rules) ((x, y), point) ang clr
  | rule == 'F' = (((x, y), (a, b), clr):lines,str)
  | rule == '[' = (lines' ++ lines'', rem)
  | rule == ']' = ([], rules)
  | otherwise   = (lines, str)
  where
    ((a, b), c)    = move rule ((x, y), point) ang
    (lines, str)   = trace1' rules ((a, b), c) ang clr
    (lines', strR) = trace1' rules ((x, y), point) ang clr
    (lines'', rem) = trace1' strR ((x, y), point) ang clr

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 (rule:rules) ang clr
  = trace2' (rule:rules) ang clr ((0.0, 0.0), 90) []
  where
    trace2' [] _ _ _ _
      = []
    trace2' (rule:rules) ang clr state stack
      | rule == 'F' = ((x, y), (a, b), clr) : trace2' rules ang clr ((a, b), c) stack
      | rule == '[' = trace2' rules ang clr state (state : stack)
      | rule == ']' = trace2' rules ang clr old new
      | otherwise   = trace2' rules ang clr ((a, b), c) stack
      where 
        ((a, b), c)     = move rule ((x, y), point) ang
        (old:new)       = stack
        ((x, y), point) = state

-- Extension which changes the colour of the line recursively
trace2Clr :: String -> Float -> Colour -> [ColouredLine]
trace2Clr (rule:rules) ang clr
  = trace2' (rule:rules) ang clr ((0.0, 0.0), 90) []
  where
    trace2' [] _ _ _ _
      = []
    trace2' (rule:rules) ang clr state stack
      | rule == 'F' = ((x, y), (a, b'), clr) : trace2' rules ang ((r+0.0005), (g+0.0005), (b-0.0005)) ((a, b'), c) stack
      | rule == '[' = trace2' rules ang ((r+0.0005), (g+0.0005), (b-0.0005)) state (state : stack)
      | rule == ']' = trace2' rules ang ((r+0.0005), (g+0.0005), (b-0.0005)) old new
      | otherwise   = trace2' rules ang ((r+0.0005), (g+0.0005), (b-0.0005)) ((a, b'), c) stack
      where 
        ((a, b'), c)    = move rule ((x, y), point) ang
        (old:new)       = stack
        ((x, y), point) = state
        (r, g, b)       = clr
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2Clr (lSystem system n) (angle system) colour)

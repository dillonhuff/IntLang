def mfunc as recurAdd 5 3

def recurAdd x y as if x == 0
  then y
  else recurAdd (x - 1) (y + 1)

def someIfs as if 34 == (34 - 1)
  then applyTwice div3 81
  else apply4AltMaybe (3 <= (2 * 5 + 4)) div4 div3 144

def isLte x y as (x < y) || (x == y)

def applyOnce f x as f x

def compose f g x as f (g x)

def applyTwice f x as f (f x)

def apply4Alt f g x as f (g (f (g x)))

def apply4AltMaybe flag f g x as if flag
  then f (g (f (g x)))
  else f (g x)

def div4 x as x / 4

def div3 x as x / 3

def andOr x y z as x && (y || z)
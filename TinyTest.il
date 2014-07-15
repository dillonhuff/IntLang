def mfunc as (andOr True False True)

def applyOnce f x as f x

def compose f g x as f (g x)

def applyTwice f x as f (f x)

def apply4Alt f g x as f (g (f (g x)))

def div4 x as x / 4

def div3 x as x / 3

def andOr x y z as x && (y || z)
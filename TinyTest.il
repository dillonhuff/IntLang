def div4 x as x / 4

def mfunc as someUnaryOp div16 64

def someUnaryOp f x as f x

def div16 y as div4 (div4 y)

def sumTimes10ByAdds x y as (times10ByAdds x) + (times10ByAdds y)

def times10ByAdds n as n + n + n + n + n + n + n + n + n + n
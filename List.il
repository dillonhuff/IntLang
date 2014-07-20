record list { value, next }

def mfunc as head (tail (map plusOne (numList 10)))

def numList n as if n == 0
  then cons 0 nil
  else cons n (numList (n - 1))

def map f l as if isNil l
  then nil
  else cons (f (head l)) (map f (tail l))

def plusOne x as x + 1

def constrTest as head (tail (tail (cons 1 (cons 2 (cons 3 nil)))))

def cons newValue l as list newValue l

def head l as value l

def tail l as next l
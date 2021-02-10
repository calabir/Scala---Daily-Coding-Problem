def getRow(n: Int, x: Int, acc: Int): Int = {
  if(x-n >= 0) getRow(n, x-n, acc+1) else acc
}


def getCol(n: Int, x: Int): Int = {
  x % n
}


// 0 1 2
// 3 4 5
// 6 7 8

assert(getRow(3, 4, 0) == 1)
assert(getRow(3, 8, 0) == 2)
assert(getRow(3, 0, 0) == 0)

assert(getCol(3, 4) == 1)
assert(getCol(3, 0) == 0)
assert(getCol(3, 8) == 2)
module ProgExercises.FS_2019_ProgExer02Prob_V01 where

-- Develop some functions to work with matrices, using list comprehensions.
-- Higher-order functions are not required yet.

-- Helpful functions from the Prelude:
--   and, (!!), zip

-- the type for values
type Value = Double
-- the type for matrices
type Matrix = [[Value]]

-- We represent matrices as lists of lists,
-- where each inner list represents a row
-- (rather than a column) of the matrix.
-- Example:
-- [[a11, a12, a13],
--  [a21, a22, a23]]
-- Indexing, as often done in matrix computations,
-- starts with 1 rather than 0 (beg Dijkstra's forgiveness).

-- We assume as precondition for most of the
-- following functions that all inner lists
-- have the same length (if there are any).
-- Here is a function that might be used to
-- check this condition.
exaIsMat =
  isMat [[1, 2], [3, 4]] &&
  isMat [[1, 2]] &&
  isMat [] &&
  isMat [[], []] &&
  not (isMat [[1, 2], []])

isMat :: Matrix -> Bool
isMat [] = True
isMat (row1 : rows) =
    and [length row == lengthRow1 | row <- rows]
   where lengthRow1 = length row1

-- Sometimes we use square matrices.
-- Here is a function that might be used to
-- check whether a list of lists represents a square matrix.
exaIsSquareMat =
  isSquareMat [[1, 2], [3, 4]] &&
  isSquareMat [[1]] &&
  isSquareMat [] &&
  not (isSquareMat [[]]) &&
  not (isSquareMat [[1, 2, 3], [4, 5, 6]])

isSquareMat = isSquareMatV2

isSquareMatV1 :: Matrix -> Bool
isSquareMatV1 [] = True
isSquareMatV1 matrix = isMat matrix && length (head matrix) == length matrix

isSquareMatV2 :: Matrix -> Bool
isSquareMatV2 [] = True
isSquareMatV2 mat@(row1:_) = length row1 == length mat

-- zeroMat generates an m * n zero matrix, that is,
-- a matrix with all entries 0.
exaZeroMat =
  zeroMat 2 3 == [[0, 0, 0], [0, 0, 0]]

zeroMat :: Int -> Int -> Matrix
zeroMat rows columns = [zeroRow | _ <- [1..rows]]
  where
    zeroRow = [0 | _ <- [1..columns]]

-- Check whether a list of lists represents a zero matrix or not.
isZeroMatV1 :: Matrix -> Bool
isZeroMatV1 mat = isMat mat && and [a == 0 | row <- mat, a <-row]

isZeroMatV2 :: Matrix -> Bool
isZeroMatV2 mat = isMat mat && and [a == 0 | a <- concat mat]

-- unitMat generates an n * n unit matrix, that is,
-- a square matrix in which all entries are zero,
-- except of the elements on the main diagonal, which are 1.
exaUnitMat =
  unitMatV1 3 == [[1, 0, 0], [0, 1, 0], [0, 0, 1]] &&
  unitMatV2 3 == [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

unitMat = unitMatV1

unitMatV1 :: Int -> Matrix
unitMatV1 n = [row i | i <- [1..n]]
  where
    row i = [if i == j then 1 else 0 | j <- [1..n]]

unitMatV2 :: Int -> Matrix
unitMatV2 n = [row i | i <- [1..n]]
  where
    row i = [0 | _ <- [1..i-1]] ++ 1 : [0 | _ <- [i+1..n]]

-- Check whether two matrices are equal or not.
equalMatV1 :: Matrix -> Matrix -> Bool
equalMatV1 mat1 mat2 = isMat mat1 && isMat mat2 && and [x == x' | (x,x') <- zip (concat mat1) (concat mat2)]

equalMatV2 :: Matrix -> Matrix -> Bool
equalMatV2 = (==)

-- Check whether a list of lists represents a unit matrix or not.
isUnitMat :: Matrix -> Bool
isUnitMat mat = mat == unitMat (length mat)

-- neg negates a matrix, that is,
-- negates all corresponding components of it.
exaNeg =
  neg [[1, -2], [0, 4]] == [[-1, 2], [0, -4]]

neg :: Matrix -> Matrix
neg rows = [[-elem | elem <- row] | row <- rows]

-- plusMat adds two matrices, that is,
-- adds all corresponding components of them.
-- precondition: mat1 and mat2 have the same size
exaPlusMat =
  unit3 `plusMat` (neg unit3) == zeroMat 3 3
    where unit3 = unitMat 3

plusMat = plusMatV1

plusMatV1 :: Matrix -> Matrix -> Matrix
mat1 `plusMatV1` mat2 = [[a+b | (a, b) <- (zip l1 l2)] | (l1, l2) <- (zip mat1 mat2)]

plusMatV2 :: Matrix -> Matrix -> Matrix
mat1 `plusMatV2` mat2 = [zipWith (+) l1 l2 | (l1, l2) <- (zip mat1 mat2)]

-- Reads the element in row i and column j of matrix mat.
-- precondition: i and j are in range of mat
readMat :: Matrix -> Int -> Int -> Value
readMat mat i j = (mat !! i) !! j

-- updateMat 'updates' matrix mat.
-- The updated matrix is the same as mat, except of
-- position row i column j, where the new value is val.
-- Note: there is no imperative update; the function returns a new matrix.
-- precondition: i and j are in range of mat
exaUpdateMat =
  updateMat (unitMat 3) 3 1 5 == [[1,0,0],[0,1,0],[5,0,1]]

updateMat :: Matrix -> Int -> Int -> Value -> Matrix
updateMat mat i j val = [if i' == i then updateRow row j else row | (i', row) <- zip [1 ..] mat]
  where
    updateRow row j = [if j' == j then val else val' | (j', val') <- zip [1 ..] row]



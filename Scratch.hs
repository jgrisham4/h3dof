
main = do
  putStrLn "Enter a number."
  numstr <- getLine
  let num = read numstr
  print (num  +5.0)

main = print (fib 30)
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)
-- main :: IO ()
-- main = do
--   let message = "Hello"
--   putStrLn message

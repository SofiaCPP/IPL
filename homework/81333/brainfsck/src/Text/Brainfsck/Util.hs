module Text.Brainfsck.Util where

inc :: [Int] -> Int -> [Int]
inc b h = take h b <> [b !! h + 1] <> drop (h + 1) b

dec :: [Int] -> Int -> [Int]
dec b h = take h b <> [b !! h - 1] <> drop (h + 1) b

load :: [Int] -> Int -> Int -> [Int]
load b h v = take h b <> [v] <> drop (h + 1) b

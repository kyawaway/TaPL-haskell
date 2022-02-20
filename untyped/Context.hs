module Context where

type Context = [String]

newContext :: Context
newContext = []

bindName :: String -> Context -> Context
bindName = (:)

getIndexName :: Int -> Context -> String
getIndexName n ctx
        | ctxLength ctx > n = ctx !! n
        | otherwise = error "error "("Requested index " ++ show n
                              ++ " of Context of length " ++ show (ctxLength ctx))

ctxLength :: Context -> Int
ctxLength = length


pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x =
        let x' = getName ctx x
        in (bindName x' ctx, x')
            where getName [] x = x
                  getName ctx@(n:ns) x
                      | n == x = getName ctx (x ++ "'")
                      | otherwise = getName ns x

import Controller (withClog)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withClog $ run 3000

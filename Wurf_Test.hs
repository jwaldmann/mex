import Wurf
import Test.SmallCheck

main = test $ transitive ( (<) :: Wurf -> Wurf -> Bool )

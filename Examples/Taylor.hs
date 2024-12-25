{-# LANGUAGE OverloadedLabels #-}
import Tex

taylor a cn x = Fun #f [x] := sigma_ (cn := 0) #"\\infty" (\cn -> (x + n a) ** cn * Fun (#f ↑ Brace cn) [a] * r (fac cn)) cn
t = taylor #a #n #x

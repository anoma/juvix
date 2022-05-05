module MiniJuvix.Translation.MonoJuvixToMiniC.Strings where

import MiniJuvix.Prelude

stdlib :: IsString s => s
stdlib = "stdlib.h"

stdbool :: IsString s => s
stdbool = "stdbool.h"

stdio :: IsString s => s
stdio = "stdio.h"

main :: IsString s => s
main = "main"

fprintf :: IsString s => s
fprintf = "fprintf"

stderr_ :: IsString s => s
stderr_ = "stderr"

exit :: IsString s => s
exit = "exit"

exitFailure_ :: IsString s => s
exitFailure_ = "EXIT_FAILURE"

malloc :: IsString s => s
malloc = "malloc"

sizeof :: IsString s => s
sizeof = "sizeof"

true_ :: IsString s => s
true_ = "true"

tag :: IsString s => s
tag = "tag"

data_ :: IsString s => s
data_ = "data"

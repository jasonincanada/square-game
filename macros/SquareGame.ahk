; AutoHotKey macros for faster file access

::''m::app/Main.hs
::''a::src/SquareGame/Actions.hs
::''r::src/SquareGame/Render.hs
::''u::src/SquareGame/UI.hs
::''w::src/SquareGame/World.hs
::''t::test/Spec.hs

; Leave insert mode and save all modified files
#w::
  Send {Escape}:wa{Enter}
Return

; Swap to previous screen window
#n::
  Send ^an
Return

TermBox.init()

local str1 = "He两llo World"
local str2 = "Hello World"

TermBox.changeCells(0, 0, str1, TermBox.Red, TermBox.Default)
TermBox.changeCells(0, 1, str2, TermBox.Red, TermBox.Default)

TermBox.changeCells(str1:width(), 0, "!", TermBox.Red, TermBox.Default)
TermBox.changeCells(str2:width(), 1, "!", TermBox.Red, TermBox.Default)

TermBox.present()

sleep(5)
TermBox.shutdown()

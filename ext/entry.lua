require("ext/util")

local Rectangle = class()

function Rectangle:__init(x, y, width, height)
	self.x = x
	self.y = y
	self.width = width
	self.height = height
end

function Rectangle:bounds()
	return self.x, self.y, self.width, self.height
end

local Clip = class(Rectangle)

function Clip:__init(rect)
	Rectangle.__init(self, rect:bounds())
end

function Clip:changeCells(x, y, contents, fg, bg)
	-- Abort if the starting position is not inside the clip
	if x < 0 or y < 0 or y >= self.height or x >= self.width then
		return
	end

	local width = Util.stringWidth(contents)

	-- Cut off overlapping contents
	if x + width > self.width then
		local maxWidth = self.width - x
		local curWidth = 0
		local codepoints = {}

		-- Iterate over all codepoints
		for _, ch in utf8.codes(contents) do
			local charWidth = Util.wcharWidth(ch)

			-- Exit loop if adding the current code point would make the result string wider than
			-- the given maximum width
			if curWidth + charWidth > maxWidth then
				break
			end

			table.insert(codepoints, ch)
			curWidth = curWidth + charWidth
		end

		-- Combine extracted codepoints into a string
		contents = utf8.char(table.unpack(codepoints))
	end

	-- Do the actual changing
	TermBox.changeCells(self.x + x, self.y + y, contents, fg, bg)
end

function Clip:fill(fg, bg, ch)
	fg = fg or TermBox.Default
	bg = bg or TermBox.Default
	ch = ch or 32

	for x = 0, self.width - 1, 1 do
		for y = 0, self.height - 1, 1 do
			TermBox.changeCell(self.x + x, self.y + y, ch, fg, bg)
		end
	end
end

function Clip:clear(fg, bg)
	self:fill(TermBox.Default, TermBox.Default, 32)
end

local function splitVertical(rect, ratio)
	local left = Rectangle(rect.x, rect.y, rect.width * ratio, rect.height)
	local right = Rectangle(rect.x + left.width, rect.y, rect.width - left.width, rect.height)

	return left, right
end

TermBox.init()

local termBoxRect = Rectangle(0, 0, TermBox.width(), TermBox.height())

local left, right = splitVertical(termBoxRect, 0.5)

Clip(left):fill(TermBox.Default, TermBox.Red)
Clip(right):fill(TermBox.Default, TermBox.Blue)

TermBox.present()
TermBox.shutdown()

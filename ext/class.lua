function class(name)
	local tpl = {
		__init = function () end,
		__name = name,
		__tostring = function (self)
			local ret = (self.__name or name or "UnnamedClass") .. " {"
			local sep = ""

			for k, v in pairs(self) do
				ret = ret .. sep .. tostring(k) .. " = " .. tostring(v)
				sep = ", "
			end

			return ret .. "}"
		end
	}

	tpl.__index = tpl

	return setmetatable(tpl, {
		__call = function (self, ...)
			local instance = setmetatable({}, tpl)
			instance:__init(...)
			return instance
		end
	})
end

function class(base)
	base = base or {}

	local tpl = {
		__init = function () end
	}

	tpl.__index = tpl

	return setmetatable(tpl, {
		__call = function (self, ...)
			local instance = setmetatable({}, self)
			instance:__init(...)
			return instance
		end,

		__newindex = rawset,

		__index = function (self, key)
			return rawget(self, key) or base[key]
		end
	})
end

function Util.informf(fmt, ...)
	Util.inform(fmt:format(...))
end

function Util.warnf(fmt, ...)
	Util.warn(fmt:format(...))
end

function Util.debugf(fmt, ...)
	Util.debug(fmt:format(...))
end

function Util.errorf(fmt, ...)
	Util.error(fmt:format(...))
end

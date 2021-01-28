--[[
	Example for Lua IRC Engine (https://github.com/mirrexagon/lua-irc-engine)
	Uses LuaSocket for network communication.
]]


--- Require ---
local IRCe = require("irce")
print(IRCe._VERSION .. " running on " .. _VERSION)

local socket = require("socket")
--- ==== ---


--- Constants ---
local SERVER = arg[1] or "irc.example.com"

local NICK = "IRCe"
local USERNAME = "ircengine"
local REALNAME = "IRC Engine"

local CHANNEL = "#example"
--- ==== ---


--- Globals ---
local running = true
--- ==== ---


--- IRC object initialisation ---
local irc = IRCe.new()

-- Path may change depending on your directory structure.
-- These should work for LuaRocks installations.
assert(irc:load_module(require("irce.modules.base")))
assert(irc:load_module(require("irce.modules.message")))
assert(irc:load_module(require("irce.modules.channel")))
--- ==== ---


--- Raw send function ---
local client = socket.tcp()

irc:set_send_func(function(self, message)
    return client:send(message)
end)

client:settimeout(1)
--- ==== ---


--- Callbacks ---
irc:set_callback(IRCe.RAW, function(self, send, message)
	print(("%s %s"):format(send and ">>>" or "<<<", message))
end)

irc:set_callback("CTCP", function(self, sender, origin, command, params, pm)
	if command == "VERSION" then
		assert(self:CTCP_REPLY(origin, "VERSION", "Lua IRC Engine - Test"))
	end
end)

irc:set_callback("001", function(self, ...)
	assert(irc:JOIN(CHANNEL))
end)

irc:set_callback("PRIVMSG", function(self, sender, origin, message, pm)
	if message == "?quit" then
		assert(self:QUIT("And away we go!"))
		running = false
	end
end)


irc:set_callback("NAMES", function(self, sender, channel, list, kind, message)
	print("---")
	if not list then
		print("No channel called " .. channel)
	else
		print(("Channel %s (%s):"):format(channel, kind))
		print("-")
		for _, nick in ipairs(list) do
			print(nick)
		end
	end
	print("---")
end)


irc:set_callback("USERMODE", function(self, sender, operation, mode)
	print(("User mode: %s%s"):format(operation, mode))
end)

irc:set_callback("CHANNELMODE", function(self, sender, operation, mode, param)
	print(("Channel mode: %s%s %s"):format(operation, mode, param))
end)
--- ==== ---


--- Running ---
assert(client:connect(SERVER, 6667))

assert(irc:NICK(NICK))
assert(irc:USER(USERNAME, REALNAME))

while running do
    irc:process(client:receive())
end
--- ==== ---


--- Deinit ---
client:close()
--- ==== ---

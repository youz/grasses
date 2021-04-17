#!/usr/bin/env lua

--[[
*
* grass.lua - Grass interpreter
* http://www.blue.sky.or.jp/grass/
*
* Copyright (C) 2020 Yousuke Ushiki All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions
* are met:
* 1. Redistributions of source code must retain the above copyright
*    notice, this list of conditions and the following disclaimer.
* 2. Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in
*    the documentation and/or other materials provided with the
*    distribution.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
* THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
* PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS
* BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
* BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
* WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
* OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
* IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*
]]

unpack = unpack or table.unpack

function env_new(...)
  return {buf = {...}, parent = nil, anchor = nil}
end

function env_push(e, v)
  e.buf[#e.buf + 1] = v
  return e
end

function env_chain(e, parent, anchor)
  e.parent = parent
  e.anchor = anchor
  return e
end

function env_dup(e)
  return {buf = {unpack(e.buf)}, parent = e.parent, anchor = e.anchor}
end

function env_nth(e, i)
  local start = i
  local anchor = #e.buf
  while i > anchor do
    if e.parent == nil then
      error("stack underflow")
    else
      i = i - anchor
      anchor = e.anchor
      e = e.parent
    end
  end
  return e.buf[anchor - i + 1]
end

function abs_eval(abs, env)
  return {
    arity = abs.arity,
    code = abs.code,
    env = env_chain(env_new(), env, #env.buf)
  }
end

function fn_partial_apply(f, a)
  -- assert(f.arity > 1)
  local g = {arity = f.arity - 1, code = f.code, env = env_dup(f.env)}
  g.env.buf[#g.env.buf + 1] = a
  return g
end

function succ (c)
  return (c + 1) % 256
end

function makeout (out)
  return function (a)
    if type(a) ~= "number" then
      error("Out: not a char: " .. fmt(a))
    end
    out:write(string.char(a))
    return a
  end
end

function makein (inp)
  return function (eof)
    local buf = inp:read(1)
    if buf == nil then
      return eof
    else
      return buf:byte(1)
    end
  end
end

ctrue = function (x) return function (y) return x end end
cfalse = function (x) return function (y) return y end end
function eval_code(code, env)
  local dmp = {}
  local pc = 1
  while true do
    if pc > #code then
      local ret = env_nth(env, 1)
      if #dmp == 0 then
        return ret
      end
      code, pc, env = unpack(dmp[#dmp])
      dmp[#dmp] = nil
      env = env_push(env, ret)
    else
      local insn = code[pc]
      local v
      if insn.fun ~= nil then
        -- insn is App
        local f = env_nth(env, insn.fun)
        local a = env_nth(env, insn.arg)
        local t = type(f)
        if t == "table" then -- Fn
          if f.arity == 1 then
            if pc < #code then
              dmp[#dmp + 1] = {code, pc + 1, env}
            end
            code = f.code
            env = env_dup(f.env)
            v = a
            pc = 0
          else
            v = fn_partial_apply(f, a)
          end
        elseif t == "number" then -- Char
          if rawequal(f, a) then
            v = ctrue
          else
            v = cfalse
          end
        elseif t == "function" then -- Primitive
          v = f(a)
        else
          error("unknown value: type(f)=" .. t)
        end
      else
        -- insn is Abs
        v = abs_eval(insn, env)
      end
      env = env_push(env, v)
      pc = pc + 1
    end
  end
end


function lex(str)
  for rpl, pat in pairs{W = "\239\188\183", w = "\239\189\151", v = "\239\189\150"} do
    str = str:gsub(pat, rpl)
  end
  str = str:gsub("[^Wwv]", "")
  local start = str:find("w")
  if start == nil then
    error("syntax error")
  end
  local l = {}
  local cc = "w"
  local n = 1
  for c in str:sub(start + 1):gmatch(".") do
    if c == cc then
      n = n + 1
    else
      l[#l + 1] = {cc, n}
      cc = c
      n = 1
    end
  end
  l[#l + 1] = {cc, n}
  return l
end

function parse(str)
  local l = lex(str)
  local code = {}
  local i = 1
  while i <= #l do
    if l[i][1] == "w" then
      -- read Abs
      local arity = l[i][2]
      local body = {}
      i = i + 1
      while i < #l and l[i][1] ~= "v" do
        if l[i][1] ~= "W" or l[i + 1][1] ~= "w" then error("syntax error") end
        body[#body + 1] = {fun = l[i][2], arg = l[i + 1][2]}
        i = i + 2
      end
      code[#code + 1] = {arity = arity, code = body}
    elseif l[i][1] == "W" then
      -- read App
      if l[i + 1][1] ~= "w" then error("syntax error") end
      code[#code + 1] = {fun = l[i][2], arg = l[i + 1][2]}
      i = i + 2
    else
      i = i + 1
    end
  end
  return code
end

function initialenv(stdin, stdout)
  local out = makeout(stdout)
  local inp = makein(stdin)
  return env_new(inp, 119, succ, out)
end

function dump_code(code)
  local app2str = function (a)
    return string.format("App(%d, %d)", a.fun, a.arg)
  end
  local abs2str = function (a)
    local buf = string.format("Abs(len=%d, code=[", #a.code)
    for i, insn in ipairs(a.code) do
      buf = buf .. app2str(insn) .. ","
    end
    buf = buf .. "])"
    return buf
  end
  for i = 1, #code do
    local buf = string.format("%d : ", i)
    if code[i].fun ~= nil then
      buf = buf .. app2str(code[i])
    else
      buf = buf .. abs2str(code[i])
    end
    print(buf)
  end
end

function main()
  if arg[1] == nil then
    print(string.format("usage: lua %s <source.grass>", arg[0]))
    return
  end
  local f = io.open(arg[1])
  local code = parse(f:read("*a"))
  f:close()
  code[#code + 1] = {fun = 1, arg = 1}

  return eval_code(code, initialenv(io.stdin, io.stdout))
end

main()

local lpeg = require'lpeg'
local P,S,R,C,Ct,V = lpeg.P,lpeg.S,lpeg.R,lpeg.C,lpeg.Ct,lpeg.V
local match = lpeg.match

local Maybe = function(a)
	return P(a)^-1
end

local Space = S(' \n\t')^0

local Digit = R'09'

local Digits = Digit^1

local Sign = Maybe(S'+-') 

local Number = C(Sign * Digits * Maybe( C(P'.') * Digits ) * Maybe( S'eE' * Sign * Digits )) / function(v,dot) 
	return { type = 'number', value = tonumber(v), integer = dot == nil }
end

local Nil = C(P'nil') / function(v) 
	return { type = 'nil' } 
end

local Boolean = C(P'true' + P'false') / function(v) 
	return { type = 'boolean', value = 'v' == 'true' and true or false } 
end 

local String = C(P'"' * (P'\\"' + (P(1) - P'"'))^0 * P'"') / function(v) 
	return { type = 'string', value = tostring(v) } 
end

local FrontChar = P'_' + R'az' + R'AZ'

local Char = FrontChar + Digit

local Identifier = C(FrontChar^1 * Char^0) * C(Maybe(S'?')) / function(v,s) 
	return { 
		type = 'var', 
		value = tostring(v), 
		optional = s == '?' and true or nil, 
	}
end

local Ellipsis = P'...'

local Function = V'Function' 

local Table = V'Table'

local Value = V'Value'

local Single = V'Single'

local Union = V'Union'

local G = P{ Value,
	Single = Nil + Boolean + Number + String + Identifier + Function + Table,
	
	Union = Ct(P(Single * (Space * P'|' * Space * Single)^1)) / function(t) 
		return { type = 'union', values = t } 
	end,
	
	Value = Union + Single,
	
	Function = P'(' * Space * Ct((Value * Space)^0) * C(Maybe(Ellipsis)) * Space * P'->' * Space * Ct((Value * Space)^0) * C(Maybe(Ellipsis)) * Space * ')' / function(ins,morein,outs,moreout) 
		return {type = 'function', inputs = ins, outputs = outs, morein = morein == '...', moreout = moreout == '...'} 
	end,
	
	Table = P'{' * Space * Ct((Value * Space)^0) * C(Maybe( Ellipsis )) * Space * '}' / function(t,ellipsis) 
		return { type = 'table', values = t, more = ellipsis == '...' } 
	end
}
 
package.path=package.path..';../lsl/?.lua'

fn = require'sl'

--print( fn.tostring(match(G,'(1 ...->...)')))

---print( fn.tostring(match( G, '( number? -> (x->y) )' )))
--print( fn.tostring(match(G,'(number? | true | x ...  -> 0)')))

local function compile( p, var, depth )
	if p.type == 'number' or p.type == 'boolean' or p.type == 'number' then
		return ('(%s==%s)'):format( var, tostring(p.value))
	
	elseif p.type == 'string' then
		return ('(%s==%q)'):format( var, p.value )
	
	elseif p.type == 'union' then
		local t = {}
		for i, v in ipairs( p.values ) do
			t[i] = compile( v, var )
		end
		return '(' .. table.concat( t, ' or ' ) .. ')'
	
	elseif p.type == 'var' then 
		if p.value == 'any' then
			return p.optional and '' or ('%s~=nil'):format(var)
		else 
			return ('%s(type(%s)==%q)'):format( p.optional and ('(%s==nil) or '):format( var ) or '', var, p.value )
		end
	
	elseif p.type == 'table' then
		local outs = {}
		
		for i, v in ipairs(p.values) do
			local w = compile(v, var .. '[' .. i .. ']', (depth or 0)+1)
			if w and w ~= '' and w ~= 'true' then
				outs[#outs+1] = w
			end
		end

		return ('(type(%s)=="table"%s%s%s)'):format( var, p.more and '' or (' and (#%s==%d)'):format(var,#outs), #outs>0 and ' and ', table.concat(outs,' and '))

	elseif p.type == 'function' then
		local ins, outs = {}, {}
		
		for i, v in ipairs(p.inputs) do
			ins[i] = ('x%s%s%d'):format(depth or '', depth and '_' or '', i)
		end

		for i, v in ipairs(p.outputs) do
			outs[i] = ('y%s%s%d'):format(depth or '', depth and '_' or '', i)
		end

		local inargs = table.concat( ins, ',' )
		local outargs = table.concat( outs, ',' )

		local tmatchin = {}
		for i, v in ipairs(p.inputs) do
			local w = compile(v, ins[i], (depth or 0) + 1)
			if w and w ~= '' and w ~= 'true' then
				tmatchin[#tmatchin+1] = w
			end
		end
		
		local matchin = table.concat(tmatchin, ' and ')

		local tmatchout = {}
		for i, v in ipairs(p.outputs) do
			local w = compile(v, outs[i], (depth or 0) + 1)
			if w and w ~= '' and w ~= 'true' then
				tmatchout[#tmatchout+1] = w
			end
		end

		local matchout = table.concat(tmatchout, ' and ')

		if matchout == '' then
			matchout = 'true'
		end

		local inmore = ''
		if not p.morein then
			local n = 0
			for i, v in ipairs(p.inputs) do
				if v.optional then
				else
					n = n + 1
				end
			end
			inmore = ('(select("#",...) == %d) and '):format(n)
		end

		return ([[@return function(__wf__) assert(type(__wf__) == 'function'); return function(...)
@  local %s = ...
@  if (%s%s) then 
@    local %s = __wf__(%s)
@    if (%s) then
@      return %s
@    else
@      error("Function output mismatch")
@    end
@  else
@    error("Function input mismatch")
@  end
@end end]]):gsub('@',(' '):rep(depth or 0)):format( inargs, inmore, matchin, outargs, inargs, matchout, outargs )
end
end


--print( compile( match( G, '(1 {2 {3}} function -> 4)' )))
--print( compile( match(G,'12.3 | 2 | number?'),'x'))

print( compile( match( G, '(table any|number any? -> nil)')))

local wr = loadstring( compile( match( G, '(table any|number any? -> nil)')))()

table.insert = wr(table.insert)

table.insert({}, 1)


x = 1
print(x);
print(nil)
print("hello");
print([[hello
world]]);

x = {'a',
'b',
'c',
x = 'X',
z = {1,2},
["a b c"] = 1000
}
x.w = 3
x.z[1] = 100
if x.x then
  print("x exists")
end
if x.a == nil then
  print("a does not exist")
end
print(x[0])
print(x.x)
print "x contents"
for i,v in pairs(x) do
  print(i, v, x[i])
end
print "x.z contents"
for i,v in ipairs(x.z) do
  print(i, v)
end

x = {10, 20, 30}
for i,v in ipairs(x) do
  print(i, v)
end


function test(x)
  print(x)
end
test(10);

function printDebugInfo(info)
  print(info.source)
  print(info.what)
  print(info.name)
  print(info.linedefined)
  print(info.currentline)
  print()
end

function testDebug()
  print "debug.getinfo"
  info = debug.getinfo(test)
  printDebugInfo(debug.getinfo(test))
  printDebugInfo(debug.getinfo(0))
  printDebugInfo(debug.getinfo(1))
  printDebugInfo(debug.getinfo(2))
end
testDebug()
print(debug.traceback())

function testClass()
  print "==testClass=="
  MyObj = { value= 10 }
  function MyObj:setvalue(x)
    self.value = x
  end
  function MyObj:doSomething()
    print(self.value)
  end

  function MyObj:new(o)
    o = o or { value=0 }
    setmetatable(o, { __index=self })
    return o
  end

  MyObj:doSomething()
  MyObj:setvalue(20)
  MyObj:doSomething()

  x = MyObj:new()
  y = MyObj:new()
  x:doSomething()
  x:setvalue(30)
  y:setvalue(40)
  x:doSomething()
  y:doSomething()
end
testClass()

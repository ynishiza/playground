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


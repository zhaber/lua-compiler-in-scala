print"testing sort"

sprint(1.1)

function check (a, f)
  f = f or function (x,y) return x<y end;
  for n=table.getn(a),2,-1 do
    assert(not f(a[n], a[n-1]))
  end
end

a = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
     "Oct", "Nov", "Dec"}

f = 1<2

table.sort(a)
check(a)

limit = 30000
if rawget(_G, "_soft") then limit = 5000 end

a = {}

for i=1,limit do
  a[i] = math.random()
end

local x = os.clock()
table.sort(a)
print(string.format("Sorting %d elements in %.2f sec.", limit, os.clock()-x))
check(a)

for n=t.getn(a),2,-1 do
  f = 3
end

while i>0 do
  print(i)
  a = {"a", "b"}
end

repeat
  print(i)
  i = i-1+1
until i==0

if number < 1 then
   value = "smaller"
elseif number==1 then
   value = "one"
elseif number==2 then
   value = "two"
else
   value = "biggee"
end

x = os.clock(); i=0
table.sort(a, function(x,y) i=i+1; return y<x end)
print(string.format("Invert-sorting other %d elements in %.2f sec., with %i comparisons",
      limit, os.clock()-x, i))
check(a, function(x,y) return y<x end)

table.sort{}  -- empty array

for i=1,limit do a[i] = false end
x = os.clock();
table.sort(a, function(x,y) return nil end)
print(string.format("Sorting %d equal elements in %.2f sec.", limit, os.clock()-x))
check(a, function(x,y) return nil end)

table.sort(a)
check(a)

table.sort(a, function (x, y)
          loadstring(string.format("a[%q] = ''", x))()
          collectgarbage()
          return x<y
        end)

tt = {__lt = function (a,b) return a.val < b.val end}
a = {}
for i=1,10 do  a[i] = {val=math.random(100)}; setmetatable(a[i], tt); end
table.sort(a)
check(a, tt.__lt)
check(a)

print"OK"

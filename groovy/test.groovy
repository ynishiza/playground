#!/usr/bin/env groovy
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
println("hello")

def a = "a"
String b = "b"
println(a + b)

String[] l = ["a", "b"]
l = ["a", "b"] as String[]
l = new String[] {"d", "d"}
l[0] = "c"
String t = l.collect{ "-e" + it }.join(" ")
println(t)

List<String> ls = ["a"]
ls[0] = 2
ls[10] = 10
ls << "a" << "b"
println(ls)
println(ls + [1, 2])
println(ls)

Integer[] l2 = [1,2,3]
println(l2.sort { -it })

Map m = [ a:1 ]
println(m)
// println(JsonBuilder.toJSON([a:1]))
println(new JsonBuilder([a:1]))

println(m.collect { k, v -> "${k}:${v}" }.join(','))

func testEnum() {
    print("doing something")
    iterPrint("Alpha", Alpha.allCases)
    iterPrint("Alpha2", Alpha2.allCases)
    print(Alpha2.a)
    print(Alpha2.b)

    Alpha.printV(.a)
    Alpha.printV(.b)
    Alpha2.printV(.a)
    Alpha2.printV(.b)
    Alpha3.printV(.a)
    Alpha3.printV(.b)

    var id : Id
    id = .uid("ABC")
    id = .oid(1,2)

    var a1 : Alpha
    a1 = .a
    a1 = Alpha(rawValue: "a")!

    var alopt : Alpha?
    alopt = Alpha?(.a)
    alopt = Alpha(rawValue:"a")
    alopt = Alpha(rawValue:"ab")

    var expr : Expr
    expr = .num(1)
    expr = .add(.num(1), .num(2))
}

fileprivate func iterPrint(_ name: String, _ x: any Collection) {
    print(name)
    for v in x {
        print(v)
    }
}

fileprivate enum Alpha0 : CaseIterable {
    case a
    case b
}

fileprivate enum Alpha : String, CaseIterable {
    case a
    case b
    static func printV(_ x: Alpha) {
        print("Alpha \(x)=\(x.rawValue)")
    }
}

fileprivate enum Alpha2 : String, CaseIterable {
    case a = "A"
    case b = "B"
    static func printV(_ x: Alpha2) {
        print("Alpha2 \(x)=\(x.rawValue)")
    }
}
fileprivate enum Alpha3 : Int, CaseIterable {
    case a
    case b

    static func printV(_ x: Alpha3) {
        print("Alpha3 \(x)=\(x.rawValue)")
    }
}

fileprivate enum Id {
    case oid(Int, Int)
    case uid(String)
}

fileprivate enum Expr {
    case num(Int)
    indirect case add(Expr, Expr)
}

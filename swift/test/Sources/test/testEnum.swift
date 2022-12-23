func testEnum() {
    print("doing something")
    testEnumMain()
    testEnumCommon()
}

fileprivate func testEnumCommon() {
    let s = SomeEnum.InnerClass()
    let t = SomeEnum.InnerEnum.a
    print(s, t)
    print(s === s)
    SomeEnum.a.fn()
    SomeEnum.fn()
    print(SomeEnum.a.v)
    print(SomeEnum.v)
}

fileprivate func testEnumMain() {
    iterPrint("Alpha", Alpha.allCases)
    iterPrint("Alpha2", Alpha2.allCases)
    print(Alpha2.a)
    print(Alpha2.b)

    Alpha.a.printV()
    Alpha.b.printV()
    Alpha2.a.printV()
    Alpha2.b.printV()
    Alpha3.a.printV()
    Alpha3.b.printV()

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

fileprivate func iterPrint(_ name: String, _ x: [Any]) {
    print(name)
    for v in x {
        print(v)
    }
}

fileprivate enum SomeEnum {
    case a
    case b

    enum InnerEnum {
        case a
    }
    class InnerClass {
    }

    var v : String { "value prop" }
    static var v : String { "static prop" }
    func fn() { print("value hello") }
    static func fn() { print("static hello") }
}

fileprivate enum Alpha0 : CaseIterable {
    case a
    case b
}

fileprivate enum Alpha : String, CaseIterable {
    case a
    case b
    var hello : String { "hello" }
    func printV() {
        print("Alpha \(self)=\(self.rawValue)")
    }
}

fileprivate enum Alpha2 : String, CaseIterable {
    case a = "A"
    case b = "B"
    func printV() {
        print("Alpha2 \(self)=\(self.rawValue)")
    }
}
fileprivate enum Alpha3 : Int, CaseIterable {
    case a
    case b

    func printV() {
        print("Alpha3 \(self)=\(self.rawValue)")
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

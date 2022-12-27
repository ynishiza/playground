import Foundation

func testProtocol () {
    print("testProtocol")
    testProtocolBasic()
}

fileprivate func testProtocolBasic() {
    let obj1 : Any = MyStruct()
    var x = obj1 as! SomeProtocol

    print(obj1 is SomeProtocol)
    print(obj1 as? SomeProtocol)
    print(obj1 as! SomeProtocol)
    if var x = obj1 as? SomeProtocol {
        x.doSomething()
    }
    // if var x = obj1 as? any Identifiable {
    //     print("Identifiable id=\(x.id)")
    // }
    useProtocol(&x)
    printSomeProtocol(x)
    printSomeProtocol(obj1 as! SomeProtocol)
    // doStuff(obj1 as! MyStruct) 

    let cls1 = MyClass()
    print(cls1.isClass)
}

fileprivate func useProtocol(_ x : inout SomeProtocol) {
    printSomeProtocol(x)
    x.doSomething()
    x.change(1)
    printSomeProtocol(x)
}

// fileprivate func doStuff(_ x: any Identifiable & Equatable) {
//     print("Identifiable and Equatable", x)
// }

fileprivate func printSomeProtocol(_ x: SomeProtocol) {
    print("x0_10:\(x.x0_10) x0:\(x.x0) x1:\(x.x1)", x)
}

fileprivate protocol A { }
fileprivate protocol B  { }
fileprivate protocol C : A, B { }

fileprivate protocol SomeProtocol  {
    init()
    init(x: Int)
    init?(y: Int)

    var x0 : Int { get }
    var x1 : Int { get set }
    func doSomething() 
    func add(x: Int, y: Int) -> Int
    mutating func change(_ x: Int) -> Void

    static var x0 : Int { get }
    static var x1 : Int { get set }
    static func doSomething()
}

extension SomeProtocol {
    var x0_10 : Int { 
        get { x0 * 10 }
    }
    mutating func change(_ x: Int) {
        self.x1 = x
    }
    func doSomething() {
        printSomeProtocol(self)
    }
}

extension SomeProtocol where Self : Equatable, Self: Identifiable {
    func printID() { print(self.id) }
    func checkEqual(x : Self) -> Bool { x == self }
}

extension SomeProtocol where Self : AnyObject {
    var isClass : Bool { get { true } }
}

fileprivate struct MyStruct : SomeProtocol {
    // var someValue : any Identifiable & Comparable
    let x0 : Int = 1
    var x1 : Int = 2
    var _id : UUID = UUID()
    init() {}
    init(x: Int) { self.x1 = x }
    init?(y: Int) { self.x1 = y }
    func add(x: Int, y: Int) -> Int { x + y }
    static let x0 : Int = 0
    static var x1 : Int = 0
    static func doSomething() { print("MyStruct.doSomething") }
}

fileprivate class MyClass : SomeProtocol {
    let x0 : Int = 1
    var x1 : Int = 2
    required init() {}
    required init(x: Int) { self.x1 = x }
    required init?(y: Int) { self.x1 = y }
    func add(x: Int, y: Int) -> Int { x + y }
    static let x0 : Int = 0
    static var x1 : Int = 0
    static func doSomething() { print("MyStruct.doSomething") }
}

extension MyStruct : Identifiable {
    var id : UUID { get { _id } }
}
extension MyStruct : Equatable {
    // static func ==(_ x: Self, _ y: Self) -> Bool { x.id == y.id }
}

// comment
/*
 * A block comment
 */

func testStructures() -> () {
    print("Test structures")
    // testProperties()
    // testPropertyWrapper()
    testMutation()
    testSubscript()
}

fileprivate func testSubscript() {
    var arr = MyArray(array: [1,2,3,4,5])
    print(arr[])
    print(arr[1])
    print(arr[2])
    print(arr[100, "default"])
    arr[1] = 10
    print(arr[1])
    arr[1, 20] = 10
    print(arr[1])

    print(arr["a"])
    print(arr["b"])
}

fileprivate func testInitializer() {
    let o1 = DefaultInitTest(a : 1, b: 1)
    let o2 = DefaultInitTest(a : 1, b: 1, c: 1)
    let o3 = DefaultInitTest(a : 1, b: 1, c: 1, d: 1)
}

fileprivate func testPropertyWrapper() {
    var p1 = MyPoint()
    p1.log()
    p1.x = 10
    p1.log()
    p1.x = 100
    p1.log()
    p1.y = 100
    p1.log()
    p1.y = 1000
    p1.log()
    p1.z = -1
    p1.log()
    p1.z = -100
    p1.log()
    p1.$z = 1
    p1.log()
}

fileprivate func testProperties() {
    var obj = MyStruct()
    _printObj(obj)
    obj.x = 10
    obj.y = 20
    obj.computedV1 = 100
    obj.computedV2 = 1000
    // obj.vReadOnly = 1
    // obj._computedV1 = 1 
    _printObj(obj)

    // Static
    _printStatic()
    MyStruct.x = 100
    // MyStruct.vReadonly = 10
    MyStruct.vcomp = 1000
    _printStatic()

    func _printObj(_ v: MyStruct) {
        printLabel("obj", v);
        printLabel("obj.computedV1", v.computedV1);
        printLabel("obj.computedV2", v.computedV2);
    }

    func _printStatic() {
        printLabel("MyStruct.x", MyStruct.x);
        printLabel("MyStruct.vReadonly", MyStruct.vReadonly);
        printLabel("MyStruct.vcomp", MyStruct.vcomp);
    }
}

fileprivate func testMutation() {
    let o1 = MutatingObj(x: "a", y: "b")
    var o2 = o1
    // o1.replaceX("hello")
    print(o1, o2)
    o2.replaceSelf(MutatingObj())
    print(o1, o2)
    o2.replaceX("ABC")
    print(o1, o2)
}

fileprivate struct MyPoint {
    @LogWrapper var x : Int = 1 
    @LogWrapper(wrappedValue: 2) var y : Int

    @TrueOnly var t : Bool
    @NonNegative var z : Int = 10
    func log() -> Void {
        print("x=\(self.x) $x=\(self.$x) _x=\(self._x)")
        print("y=\(self.y) $y=\(self.$y) _y=\(self._y)")
        print("t=\(self.t) $t=\(self.$t) _t=\(self._t)")
        print("z=\(self.z) $z=\(self.$z) _z=\(self._z)")
    }
}

@propertyWrapper
fileprivate struct LogWrapper<T> {
    private var count = 0
    private var value : T
    // private(set) var projectedValue : Bool = false
    private func _log(_ s: String) {
        print("\(s):\(value)")
    }
    var wrappedValue : T {
        set (x) { _log("set"); count += 1; value = x }
        get { _log("get"); return value }
    }
    var projectedValue : Bool { count > 1 }
    init(wrappedValue: T) { 
        value = wrappedValue
    }
}

fileprivate struct MyStruct {
    var x : Int = 1
    var y = 1

    // case: readonly property
    var vReadOnly : String {
        return self.y == 1 ? "one" : "unknown"
    }

    var vReadonly2 : String { self.y == 1 ? "one" : "not one" }

    // case: setter/getter
    private var _computedV1: Int = 0
    var computedV1 : Int {
        set (v) {
            _computedV1 = v
        }
        get {
            return _computedV1
        }
    }

    // case: setter/getter shorthand
    private var _computedV2 = 0
    var computedV2 : Int {
        set { _computedV2 = newValue }
        get { _computedV2 }
    }

    // case: static
    static var x = 1
    static var y : Int = 1
    private static var _vcomp = 0
    static var vcomp : Int { 
        set { _vcomp = newValue }
        get { _vcomp }
    }
    static var vReadonly : Int { 1 }
}


@propertyWrapper
fileprivate struct NonNegative {
    private var v : Int = 0
    var wrappedValue : Int {
        set { v = newValue }
        get { max(v, 0) }
    }
    var projectedValue : Int { 
        set { v = newValue }
        get { v }
    }
    init(wrappedValue: Int) {
        v = wrappedValue
    }
}

@propertyWrapper
fileprivate struct TrueOnly {
    var wrappedValue : Bool { true }
    var projectedValue : Bool { false }
}

fileprivate struct MutatingObj {
    var x : String = "x"
    var y : String = "y"

    mutating func replaceX(_ x: String) {
        self.x = x
    }
    mutating func replaceSelf(_ x: MutatingObj) {
        self = x
    }
}

fileprivate struct MyArray {
    var array : [Any] = []
    subscript(index: Int = 0, v : Any? = nil) -> Any {
        set (newValue) {
            array[index] = v ?? newValue
        } get {
            if index < array.count {
                return array[index]
            } else if let d = v {
                return d
            } 
            return -1
        }
    }

    subscript(key: String) -> String{
        get { "key=\(key)" }
    }

    let y : Int
    init(array: [Any]) {
        self.array = array
        self.y = 1
    }
}

fileprivate struct InitTest {
    let a0 : Int
    let a1 : Int = 0
    var a2 : Int
    var a3 : Int = 1
    var a4 : Int?
    var a5 : Int? = 1

    init() {
        self.init(a0: 1, a2: -1)
        // self.a1 = 10
        self.a3 = 10
        self.a4 = nil
        self.a4 = 1
        self.a5 = nil
    }

    init(a0: Int, a2: Int) {
        self.a0 = a0
        self.a2 = a2
    }

    init(x: Any...) {
        self.a0 = 10
        self.a2 = 10
    }
}

fileprivate struct DefaultInitTest {
    let a : Int
    var b  : Int
    var c  : Int = 1
    var d : Int?
}

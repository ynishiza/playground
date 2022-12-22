// comment
/*
 * A block comment
 */

func testStructures() -> () {
    print("Test structures")
    testProperties()
    testPropertyWrapper()
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

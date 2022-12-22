
func testFunctions() {
    print("Test functions")
    testClosure()
    testTrailingClosure()
    testEscaping()
    testAutoClosure()
    testFunctionBasic()
}

fileprivate func testFunctionBasic() {
    print(f1(x: 1, 10))
    var y = 1
    f3(x: &y)
    print(y)

    func f1(x: Int, _ y: Int) -> Int {
        return x + y
    }
    func f2(_ x: Int, _ y: Int) -> Int {
        return x + y
    }
    func f3(x: inout Int) {
        x += 1
    }
}

fileprivate func testClosure() {
    let emp : () -> () = { }
    let emp2 : () -> Bool = { return true }
    let f1 : (Int, Int) -> Bool =  {(x, y) in
        return x == y
    }
    let f1a =  {(x: Int, y: Int) -> Bool in
        return x == y
    }
    let f2 : (Int, Int) -> Bool = {(x : Int, y: Int) -> Bool in x == y}
    let f3 : (Int, Int) -> Bool = { $0 == $1 }
    let f4 : () -> Int = { 100 }
    let f5 : () -> Void = { print("void") }

    [
        ("empty1", emp()),
        ("empty2", emp2()),
        ("f1", f1(1,1)),
        ("f1a", f1a(1,1)),
        ("f2", f2(1,1)),
        ("f3", f3(1,1)),
        ("f4", f4()),
        ("f5", f5())
    ].forEach() { x in printLabel(nil, x) }
}

fileprivate func testTrailingClosure() {
    // 
    let l = 1...10
    l.forEach({ printLabel("forEach1", $0) })
    l.forEach() { printLabel("forEach2", $0) }

    var ctx = 1
    let cap = { print("\(ctx)") }
    cap()
    ctx = 2
    cap()

    func doSomething(onStart : () -> Void, onEnd : () -> Void) -> Void {
        onStart()
        onEnd()
    }
    doSomething() {
        print("Start")
    } onEnd: {
        print("End")
    }
}

fileprivate func testEscaping() {
    var handlers : [() -> Void] = []
    func doSomething(_ h: @escaping () -> Void) {
        handlers.append(h)
    }
    doSomething({ print("a") })
    doSomething({ print("b") })
    handlers.forEach() { $0() }
}

fileprivate func testAutoClosure() {
    func callAuto(_ h: @autoclosure () -> Void) {
        h()
    }
    func call(_ h: () -> Void) {
        h()
    }
    callAuto({ print("a") }())
    callAuto(print("b"))
    call({ print("a") })
}

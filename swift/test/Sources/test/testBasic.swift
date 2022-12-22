// comment
/*
 * A block comment
 */

func testBasic() -> () {
    print("Basic test")
    // testType()
    // testsConversion()
    // testAssertion() 
    // testString()
    testSwitch()
}

fileprivate func testType() {
    let u1 : UInt = UInt.max
    let u2 : UInt8 = UInt8.max
    let u3 : UInt16 = UInt16.max
    let i1 = Int.max
    let v : [Any] = [ 1, "a", u1, i1]

    let c1 = ""
    let f1 : Double = 1.1
    let f3 : Float = 1.1
    let f4 : Float64 = 1.1
    let f5 : Float32 = 1.1

    let l : [Any] = [
        u1,
        u2, 
        u3,
        c1,
        v,
        f1,
        f3,
        f4,
        f5,
        1.2e2,
        1.2e-2,
        0b1011,
        0o17,
        0x1a,
        1_000_000,
        1_000_000.000_000_000_1
    ]
    printLabel("list", l)
}

fileprivate func testsConversion() {
    let i1 : Int = 1
    let u1 : UInt8 = 1
    let d1 : Double = 1.1
    printLabel("Uint -> Int", i1 + Int(u1))
    printLabel("Double -> Int", i1 + Int(d1))
    printLabel("UInt -> Double", d1 + Double(u1))
    printLabel("Float -> Double", d1 + Double(i1))
}

fileprivate func testAssertion() {
    assert(1 == 1, "1==1")
    /* assert(1 != 1, "1!=1") */
    /* assert(1 != 1, "1!=1", file: "testBasic module", line: 100000) */
    assertionFailure()
    assert(false)
    /* assertionFailure("FATAL") */
    /* preconditionFailure() */
    /* preconditionFailure("OOPS") */
    precondition(1 == 1, "OOPS")
}

fileprivate func testString() {
    let msg = """
    Hello
    This 
    is 
    a test

    null=\0
    tab=\ta
    a=\u{3045}
    1=\(1)
    """
    print(msg)
    print(#" abc\n def"#)

    for c in "abc" {
        print("c=\(c)")
    }
}

fileprivate func testSwitch() {
    _f(0)
    _f(1)
    _f(11)
    _f(12)
    _f(100)


    func _f (_ x:Int) -> Void {
        switch x {
            case 0:
                print("Zero")
            case 1...10:
                print("1~10")
            default:
                print(x)
        }

        switch x {
            case let y where y < 10:
                print("<10")
                break
            case 11, 12, 13:
                print("11,12,13")
                fallthrough
            default:
                print("default")
                break
        }
    }
}

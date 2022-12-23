func testClasses() {
    testClassInitialization()
}

fileprivate func testClassInitialization() {
    MyBaseClass()
    MyChild1()
    MyChild2()

    let o1 : MyBaseClass2? = MyBaseClass2(y: 1)
    print(MyBaseClass2(y: 1)!)
    print(MyBaseClass2(y: -1))
}

fileprivate class MyBaseClass {
    let x : Int
    init () {
        x = 1
        print("init MyBaseClass", self)
    }
}

fileprivate class MyChild1 : MyBaseClass {
    let y : Int
    override init() {
        print("init MyChild1")
        y = 1
    }
}

fileprivate class MyChild2 : MyBaseClass {
    let y : Int
    override init() {
        print("init MyChild2")
        y = 1
        super.init()
    }
}

fileprivate class MyChildConvenience : MyBaseClass {
    let a : Int
    let b : Int

    convenience init(x: Int) {
        self.init(a: x)
    }

    convenience init(a: Int) {
        self.init(a: a, b: 1)
    }

    init(a: Int, b: Int) {
        print("init MyChild3")
        self.a = a
        self.b = b
    }
}

fileprivate class MyBaseClass2 {
    var x : Int
    init(x: Int) {
        self.x = x
    }
    init?(y: Int) {
        guard y > 0 else { return nil }
        self.x = y
    }
    required init() {
        self.x = 1
    }
}
fileprivate class MyChild4 : MyBaseClass2 {
    required init() {
        super.init()
        self.x = 1
    }
    convenience override init(x: Int) {
        self.init()
        // super.init(x: x)
    }
}

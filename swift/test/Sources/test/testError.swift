func testErrorHandling() {
    testThrowAndHandler()
}

private func testThrowAndHandler() {
    let handlers = [
        { throw MyError.BadWeather },
        { throw MyError.BadMood },
        { throw MyError.Bad("OOPS") },
        { throw MyErrorStr(message: "OOOOPS") },
        { throw MyError.BadSleep(10) },
        {}
    ]
    handlers.forEach() { handleMyError($0) }

    do {
        try rethroMyError( { throw MyError.BadMood })
    } catch { 
        print("ucaught", error)
    }
    
    if let res = try? rethroMyError( { throw MyError.BadMood }) {
        print("try? success", res)
    } else {
        print("try? fail")

    }
    handlers.forEach() { deferTestHandler($0) }

    handleOptional({ throw MyError.BadMood })
    handleOptional({ 1 })
}

fileprivate func handleOptional(_ fn: () throws -> Int) {
    let res = try? fn()
    if case let x? = res {
        print("Success", x)
    } else {
        print("Fail")
    }
}

fileprivate func rethroMyError(_ fn: () throws -> Void) throws {
    do {
        try fn()
    } catch {
        print(error)
        throw error
    }
}

fileprivate func deferTestHandler(_ fn: () throws -> Void) {
    do {
        defer { print("defer1") }
        defer { print("defer2") }
        try fn()
        defer { print("defer3") }
        print("Success")
    } catch {
        print("failed", error)
    }
}

fileprivate func handleMyError(_ fn: () throws -> Void) {
    do {
        try fn()
    } catch MyError.BadWeather { 
        print(MyError.BadWeather)
    } catch MyError.BadMood { 
        print(MyError.BadMood)
    } catch MyError.Bad(let msg) { 
        print(msg, MyError.Bad)
    } catch let e as MyErrorStr {
        print(e, e.message)
    } catch {
        print(error)
    }
}

fileprivate enum MyError : Error {
    case BadWeather
    case BadMood
    case BadSleep(Int)
    case BadVegetable(String)
    case Bad(String)
}

fileprivate struct MyErrorStr : Error {
    var message : String
}

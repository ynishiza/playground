@main
public struct test {
    public private(set) var text = "Hello, World!"

    public static func main() {
        print(test().text)
        testEnum()
        // testBasic()
        // testStructures()
        // testFunctions()
    }
}

fileprivate enum A : String {
    case a 
    case b

    var t : String {
        switch self {
        case .a: return "a"
        default: return "d"
        }
    }
}

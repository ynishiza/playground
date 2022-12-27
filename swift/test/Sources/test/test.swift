@main
public struct Test {
    public private(set) var text = "Hello, World!"

    public static func main() {
        print(Test().text)
        // testBasic()
        // testStructures()
        // testFunctions()
        // runScratchSpace()
        // testStructures()
        // testClasses()
        // testProtocol()
        testErrorHandling()
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

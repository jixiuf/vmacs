import Foundation
import AppKit
import ApplicationServices

struct Options {
    var titlePattern: String?
    var bundleId: String?
    var appName: String?
    var execPath: String?
    var isToggle: Bool = false
    var envVars: [String] = []
    var fallbackCmd: [String] = []  // Command and its arguments
}

func parseArgs() -> Options? {
    let args = ProcessInfo.processInfo.arguments
    var options = Options()
    
    var i = 1
    while i < args.count {
        let arg = args[i]
        switch arg {
        case "--title":
            guard i + 1 < args.count else { return nil }
            options.titlePattern = args[i + 1]
            i += 2
        case "--id":
            guard i + 1 < args.count else { return nil }
            options.bundleId = args[i + 1]
            i += 2
        case "--app":
            guard i + 1 < args.count else { return nil }
            options.appName = args[i + 1]
            i += 2
        case "--exec":
            guard i + 1 < args.count else { return nil }
            options.execPath = args[i + 1]
            i += 2
        case "--env":
            guard i + 1 < args.count else { return nil }
            options.envVars.append(args[i + 1])
            i += 2
        case "--toggle":
            options.isToggle = true
            i += 1
        case "--cmd":
            // Take all remaining arguments as the command
            guard i + 1 < args.count else { return nil }
            options.fallbackCmd = Array(args[(i + 1)...])
            i = args.count  // Exit loop
        default:
            return nil
        }
    }
    
    return options
}

func checkAccessibilityPermission() -> Bool {
    let options = [kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String: false] as CFDictionary
    return AXIsProcessTrustedWithOptions(options)
}

struct WindowInfo {
    let app: NSRunningApplication
    let element: AXUIElement
    let title: String
    let pid: pid_t
    let isMinimized: Bool
    let zOrder: Int  // Lower = more front
}

// Get windows ordered by z-order (front to back)
func getWindowsInZOrder() -> [WindowInfo] {
    var results: [WindowInfo] = []
    let workspace = NSWorkspace.shared
    
    // Get window list in z-order from CGWindowList
    let windowListOptions = CGWindowListOption(arrayLiteral: .optionAll)
    guard let windowList = CGWindowListCopyWindowInfo(windowListOptions, kCGNullWindowID) as? [[String: Any]] else {
        return results
    }
    
    // Build a map of pid -> app
    var appMap: [pid_t: NSRunningApplication] = [:]
    for app in workspace.runningApplications {
        appMap[app.processIdentifier] = app
    }
    
    // Build window info from CGWindowList (in z-order) and match with AX windows
    var zOrder = 0
    var seenPidTitle = Set<String>()
    
    for cgWindow in windowList {
        guard let layer = cgWindow[kCGWindowLayer as String] as? Int, layer == 0,
              let ownerPid = cgWindow[kCGWindowOwnerPID as String] as? Int else { continue }
        
        let pid = pid_t(ownerPid)
        guard let app = appMap[pid] else { continue }
        
        let cgTitle = cgWindow[kCGWindowName as String] as? String ?? ""
        let isOnScreen = cgWindow[kCGWindowIsOnscreen as String] as? Bool ?? false
        
        // Skip empty titles (usually non-main windows)
        if cgTitle.isEmpty { continue }
        
        let key = "\(pid):\(cgTitle)"
        if seenPidTitle.contains(key) { continue }
        seenPidTitle.insert(key)
        
        // Find matching AX window
        let axApp = AXUIElementCreateApplication(pid)
        var windowsRef: CFTypeRef?
        guard AXUIElementCopyAttributeValue(axApp, kAXWindowsAttribute as CFString, &windowsRef) == .success,
              let windows = windowsRef as? [AXUIElement] else { continue }
        
        for axWindow in windows {
            var titleRef: CFTypeRef?
            AXUIElementCopyAttributeValue(axWindow, kAXTitleAttribute as CFString, &titleRef)
            let axTitle = (titleRef as? String) ?? ""
            
            // Match by title
            if axTitle == cgTitle || (cgTitle.isEmpty && axTitle.isEmpty) {
                var minimizedRef: CFTypeRef?
                let minResult = AXUIElementCopyAttributeValue(axWindow, kAXMinimizedAttribute as CFString, &minimizedRef)
                var isMinimized = !isOnScreen
                if minResult == .success, let boolRef = minimizedRef {
                    isMinimized = (boolRef as! CFBoolean) == kCFBooleanTrue
                }
                
                results.append(WindowInfo(app: app, element: axWindow, title: axTitle, pid: pid, isMinimized: isMinimized, zOrder: zOrder))
                zOrder += 1
                break
            }
        }
    }
    
    // Sort by z-order
    results.sort { $0.zOrder < $1.zOrder }
    return results
}

func regexMatch(_ string: String, _ pattern: String) -> Bool {
    guard let regex = try? NSRegularExpression(pattern: pattern, options: [.caseInsensitive]) else {
        return string.lowercased().contains(pattern.lowercased())
    }
    let range = NSRange(string.startIndex..., in: string)
    return regex.firstMatch(in: string, options: [], range: range) != nil
}

func getExecInfo(_ path: String) -> (bundleId: String?, appName: String?, execPath: String)? {
    let fm = FileManager.default
    var execPath = path
    
    if !path.hasPrefix("/") {
        if let foundPath = which(path) {
            execPath = foundPath
        } else {
            return nil
        }
    }
    
    guard fm.fileExists(atPath: execPath) else { return nil }
    
    var resolvedPath = execPath
    if let target = try? fm.destinationOfSymbolicLink(atPath: execPath) {
        resolvedPath = target
    }
    
    if execPath.hasSuffix(".app") {
        let appUrl = URL(fileURLWithPath: execPath)
        if let bundle = Bundle(url: appUrl), let bundleId = bundle.bundleIdentifier {
            return (bundleId, appUrl.deletingPathExtension().lastPathComponent, execPath)
        }
        return nil
    }
    
    let workspace = NSWorkspace.shared
    for app in workspace.runningApplications {
        guard let exec = app.executableURL?.path else { continue }
        var appExec = exec
        if let target = try? fm.destinationOfSymbolicLink(atPath: exec) {
            appExec = target
        }
        if appExec == resolvedPath || execPath == appExec {
            return (app.bundleIdentifier, app.localizedName, resolvedPath)
        }
    }
    
    return (nil, nil, resolvedPath)
}

func which(_ command: String) -> String? {
    let paths = ProcessInfo.processInfo.environment["PATH"]?.split(separator: ":") ?? []
    for dir in paths {
        let fullPath = "\(dir)/\(command)"
        if FileManager.default.fileExists(atPath: fullPath) {
            return fullPath
        }
    }
    return nil
}

func matchesOptions(_ info: WindowInfo, _ options: Options) -> Bool {
    let titleMatch = options.titlePattern.map { regexMatch(info.title, $0) } ?? true
    let bundleMatch = options.bundleId.map { info.app.bundleIdentifier == $0 } ?? true
    let appMatch = options.appName.map { regexMatch(info.app.localizedName ?? "", $0) } ?? true
    
    var execMatch = true
    if let execPath = options.execPath {
        if let execInfo = getExecInfo(execPath) {
            let matchesBundle = execInfo.bundleId.map { $0 == info.app.bundleIdentifier } ?? false
            let matchesApp = execInfo.appName.map { regexMatch(info.app.localizedName ?? "", $0) } ?? false
            let matchesExec = info.app.executableURL.map { appExec in
                var appPath = appExec.path
                if let target = try? FileManager.default.destinationOfSymbolicLink(atPath: appPath) {
                    appPath = target
                }
                return appPath == execInfo.execPath || execPath == appExec.path
            } ?? false
            execMatch = matchesBundle || matchesApp || matchesExec
        } else {
            execMatch = false
        }
    }
    
    return titleMatch && bundleMatch && appMatch && execMatch
}

func focusWindow(_ info: WindowInfo) {
    if info.isMinimized {
        let result = AXUIElementSetAttributeValue(info.element, kAXMinimizedAttribute as CFString, kCFBooleanFalse)
        if result != .success {
            print("Warning: Failed to unminimize window, error: \(result.rawValue)")
        }
        usleep(100000)  // 100ms
    }
    
    if let execPath = info.app.executableURL?.path {
        let task = Process()
        task.executableURL = URL(fileURLWithPath: "/usr/bin/open")
        task.arguments = ["-a", execPath]
        try? task.run()
        task.waitUntilExit()
    } else if let appName = info.app.localizedName {
        let task = Process()
        task.executableURL = URL(fileURLWithPath: "/usr/bin/open")
        task.arguments = ["-a", appName]
        try? task.run()
        task.waitUntilExit()
    }
    usleep(100000)
    
    let raiseResult = AXUIElementPerformAction(info.element, kAXRaiseAction as CFString)
    if raiseResult != .success {
        print("Warning: Failed to raise window, error: \(raiseResult.rawValue)")
    }
    
    AXUIElementSetAttributeValue(info.element, kAXFocusedAttribute as CFString, kCFBooleanTrue)
    AXUIElementSetAttributeValue(info.element, kAXMainAttribute as CFString, kCFBooleanTrue)
}

func hideWindow(_ info: WindowInfo) {
    AXUIElementSetAttributeValue(info.element, kAXMinimizedAttribute as CFString, kCFBooleanTrue)
}

func isFocusedWindow(_ info: WindowInfo) -> Bool {
    let workspace = NSWorkspace.shared
    guard let frontApp = workspace.frontmostApplication else { return false }
    
    if info.pid != frontApp.processIdentifier { return false }
    
    let axApp = AXUIElementCreateApplication(info.pid)
    var focusedWindowRef: CFTypeRef?
    guard AXUIElementCopyAttributeValue(axApp, kAXFocusedWindowAttribute as CFString, &focusedWindowRef) == .success,
          let focusedWindow = focusedWindowRef else { return false }
    
    // Compare by title since we can't reliably get window IDs
    let axFocused = focusedWindow as! AXUIElement
    var titleRef: CFTypeRef?
    AXUIElementCopyAttributeValue(axFocused, kAXTitleAttribute as CFString, &titleRef)
    let focusedTitle = (titleRef as? String) ?? ""
    
    return info.title == focusedTitle
}

// MARK: - Main

guard checkAccessibilityPermission() else {
    print("ERROR: Accessibility permission required")
    print("Please grant permission in System Settings > Privacy & Security > Accessibility")
    exit(2)
}

guard let options = parseArgs() else {
    print("Usage: run-or-raise [--title \"regex\"] [--id bundle_id] [--app \"app_name\"] [--exec \"command\"] [--env KEY=VALUE] [--toggle] [--cmd command args...]")
    print("  --title:  Window title regex pattern")
    print("  --id:     App bundle identifier (e.g., com.apple.Safari)")
    print("  --app:    App name regex pattern (e.g., Emacs, Firefox)")
    print("  --exec:   Executable path or command name (e.g., /usr/bin/emacs, emacs)")
    print("  --env:    Environment variable to set (can be used multiple times  --env key=value)")
    print("  --toggle: Hide if at last matched window, else cycle through matched windows")
    print("  --cmd:    Command to run if window not found (must be last option)")
    print("")
    print("Examples:")
    print("  run-or-raise --app Emacs --cmd /usr/local/bin/emacsclient -c")
    print("  run-or-raise --id org.mozilla.firefox --toggle")
    print("  run-or-raise --exec emacs --env VISUAL=emacs")
    print("")
    print("When multiple windows match, repeated calls cycle through them.")
    print("With --toggle: if current window is the last match, switch to most recent non-matching window.")
    exit(1)
}

guard options.titlePattern != nil || options.bundleId != nil || options.appName != nil || options.execPath != nil else {
    print("ERROR: Must specify --title, --id, --app, or --exec")
    exit(1)
}

let allWindows = getWindowsInZOrder()
let matchedWindows = allWindows.filter { matchesOptions($0, options) }
let unmatchedWindows = allWindows.filter { !matchesOptions($0, options) }

if matchedWindows.isEmpty {
    if !options.fallbackCmd.isEmpty {
        print("NOT_FOUND: Running command: \(options.fallbackCmd.joined(separator: " "))")
        var env = ProcessInfo.processInfo.environment
        for varItem in options.envVars {
            let parts = varItem.split(separator: "=", maxSplits: 1)
            if parts.count == 2 {
                env[String(parts[0])] = String(parts[1])
            }
        }
        let task = Process()
        task.executableURL = URL(fileURLWithPath: options.fallbackCmd[0])
        task.environment = env
        if options.fallbackCmd.count > 1 {
            task.arguments = Array(options.fallbackCmd[1...])
        }
        try? task.run()
        exit(0)
    } else if let bundleId = options.bundleId {
        print("NOT_FOUND: Opening bundle: \(bundleId)")
        let task = Process()
        task.executableURL = URL(fileURLWithPath: "/usr/bin/open")
        task.arguments = ["-b", bundleId]
        try? task.run()
        exit(0)
    } else if let execPath = options.execPath {
        if let execInfo = getExecInfo(execPath), execInfo.bundleId != nil {
            print("NOT_FOUND: Opening: \(execInfo.appName ?? execPath)")
            let task = Process()
            task.executableURL = URL(fileURLWithPath: "/usr/bin/open")
            task.arguments = ["-a", execInfo.execPath]
            try? task.run()
            exit(0)
        } else {
            print("NOT_FOUND: No matching window")
            exit(1)
        }
    } else {
        print("NOT_FOUND: No matching window")
        exit(1)
    }
}

// matchedWindows is already sorted by z-order (front to back)
// allWindows[0] is the absolute frontmost window

// Use a state file to track cycling across invocations
let stateFile = "/tmp/window_tool_cycle_state"

func readCycleState() -> (pattern: String, index: Int, timestamp: TimeInterval)? {
    guard let data = FileManager.default.contents(atPath: stateFile),
          let content = String(data: data, encoding: .utf8) else { return nil }
    let parts = content.split(separator: "\n")
    guard parts.count >= 3,
          let index = Int(parts[1]),
          let timestamp = TimeInterval(parts[2]) else { return nil }
    return (String(parts[0]), index, timestamp)
}

func writeCycleState(pattern: String, index: Int) {
    let content = "\(pattern)\n\(index)\n\(Date().timeIntervalSince1970)"
    try? content.write(toFile: stateFile, atomically: true, encoding: .utf8)
}

// Build a pattern key for state tracking
let patternKey = "\(options.appName ?? "")-\(options.bundleId ?? "")-\(options.titlePattern ?? "")-\(options.execPath ?? "")"

if matchedWindows.count == 1 {
    // Only one matched window
    let theMatch = matchedWindows[0]
    let matchIsAtTop = allWindows.first.map { $0.title == theMatch.title && $0.pid == theMatch.pid } ?? false
    
    if options.isToggle && matchIsAtTop {
        // Match is at top, toggle to unmatched
        if let firstUnmatched = unmatchedWindows.first {
            focusWindow(firstUnmatched)
            writeCycleState(pattern: patternKey, index: -1)
            print("TOGGLE: Switched to '\(firstUnmatched.title)'")
            exit(0)
        } else {
            hideWindow(theMatch)
            print("HIDDEN: No other window")
            exit(0)
        }
    } else {
        focusWindow(theMatch)
        writeCycleState(pattern: patternKey, index: 0)
        print("FOCUS: '\(theMatch.title)'")
        exit(0)
    }
}

// Multiple matched windows
// Check state to determine next index
var nextIndex = 0
let now = Date().timeIntervalSince1970

if let state = readCycleState(),
   state.pattern == patternKey,
   now - state.timestamp < 5.0 {  // State is fresh (within 5 seconds)
    // Continue cycling from last position
    nextIndex = (state.index + 1) % matchedWindows.count
} else {
    // Fresh start or different pattern, start from 0
    nextIndex = 0
}

// Check if we should toggle (at the end of cycle)
if options.isToggle && nextIndex == 0 {
    // We've cycled through all, check if we should switch to unmatched
    if let state = readCycleState(),
       state.pattern == patternKey,
       state.index == matchedWindows.count - 1,
       now - state.timestamp < 5.0 {
        // Just finished a complete cycle, switch to unmatched
        if let firstUnmatched = unmatchedWindows.first {
            focusWindow(firstUnmatched)
            writeCycleState(pattern: patternKey, index: -1)
            print("TOGGLE: Switched to '\(firstUnmatched.title)'")
            exit(0)
        }
    }
}

focusWindow(matchedWindows[nextIndex])
writeCycleState(pattern: patternKey, index: nextIndex)
print("CYCLE: \(nextIndex + 1)/\(matchedWindows.count) '\(matchedWindows[nextIndex].title)'")
exit(0)
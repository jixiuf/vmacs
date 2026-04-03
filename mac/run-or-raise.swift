import AppKit
import ApplicationServices
import Foundation

struct Options {
    var titlePattern: String?
    var skipTitlePatterns: [String] = []
    var bundleId: String?
    var appName: String?
    var execPath: String?
    var isToggle: Bool = false
    var matchAll: Bool = true  // true=AND, false=OR
    var envVars: [String] = []
    var fallbackCmd: [String] = []  // Command and its arguments
    var postCmd: [String] = []  // Command to run after focusing a window
    var preCmd: [String] = []   // Command to run before focusing a window
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
        case "--skip-title":
            guard i + 1 < args.count else { return nil }
            options.skipTitlePatterns.append(args[i + 1])
            i += 2
        case "--match":
            guard i + 1 < args.count else { return nil }
            switch args[i + 1].lowercased() {
            case "or":  options.matchAll = false
            case "and": options.matchAll = true
            default: return nil
            }
            i += 2
        case "--toggle":
            options.isToggle = true
            i += 1
        case "--post-cmd":
            // Take arguments until next known option or end
            var j = i + 1
            let nextOptions = Set(["--title", "--id", "--app", "--exec", "--env", "--skip-title", "--match", "--toggle", "--cmd", "--post-cmd", "--pre-cmd"])
            while j < args.count && !nextOptions.contains(args[j]) {
                j += 1
            }
            guard j > i + 1 else { return nil }
            options.postCmd = Array(args[(i + 1)..<j])
            i = j
        case "--pre-cmd":
            // Take arguments until next known option or end
            var j = i + 1
            let nextOptions = Set(["--title", "--id", "--app", "--exec", "--env", "--skip-title", "--match", "--toggle", "--cmd", "--post-cmd", "--pre-cmd"])
            while j < args.count && !nextOptions.contains(args[j]) {
                j += 1
            }
            guard j > i + 1 else { return nil }
            options.preCmd = Array(args[(i + 1)..<j])
            i = j
        case "--cmd":
            // Take arguments until next known option or end
            var j = i + 1
            let nextOptions = Set(["--title", "--id", "--app", "--exec", "--env", "--skip-title", "--match", "--toggle", "--cmd", "--post-cmd", "--pre-cmd"])
            while j < args.count && !nextOptions.contains(args[j]) {
                j += 1
            }
            guard j > i + 1 else { return nil }
            options.fallbackCmd = Array(args[(i + 1)..<j])
            i = j
        default:
            return nil
        }
    }

    return options
}

func checkAccessibilityPermission() -> Bool {
    let options =
        [kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String: false]
        as CFDictionary
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

// Fast path: get windows directly from AX API for a single target app (no z-order needed for simple focus)
func getWindowsForPid(_ pid: pid_t, _ app: NSRunningApplication) -> [WindowInfo] {
    var results: [WindowInfo] = []
    let axApp = AXUIElementCreateApplication(pid)
    var windowsRef: CFTypeRef?
    guard AXUIElementCopyAttributeValue(axApp, kAXWindowsAttribute as CFString, &windowsRef) == .success,
          let windows = windowsRef as? [AXUIElement] else {
        return results
    }
    
    for (index, axWindow) in windows.enumerated() {
        var titleRef: CFTypeRef?
        AXUIElementCopyAttributeValue(axWindow, kAXTitleAttribute as CFString, &titleRef)
        let title = (titleRef as? String) ?? ""
        
        var minimizedRef: CFTypeRef?
        let minResult = AXUIElementCopyAttributeValue(axWindow, kAXMinimizedAttribute as CFString, &minimizedRef)
        var isMinimized = false
        if minResult == .success, let boolRef = minimizedRef {
            isMinimized = (boolRef as! CFBoolean) == kCFBooleanTrue
        }
        
        results.append(WindowInfo(app: app, element: axWindow, title: title, pid: pid, isMinimized: isMinimized, zOrder: index))
    }
    return results
}

// Get windows ordered by z-order (front to back)
// If targetPids is provided, only get windows for those pids (optimization)
func getWindowsInZOrder(targetPids: Set<pid_t>? = nil) -> [WindowInfo] {
    var results: [WindowInfo] = []
    let workspace = NSWorkspace.shared

    // Use .optionAll to include hidden/off-screen windows (e.g. after app.hide())
    let windowListOptions = CGWindowListOption(arrayLiteral: .optionAll, .excludeDesktopElements)
    guard
        let windowList = CGWindowListCopyWindowInfo(
            windowListOptions,
            kCGNullWindowID
        ) as? [[String: Any]]
    else {
        return results
    }

    // Build a map of pid -> app (only for target pids if specified)
    var appMap: [pid_t: NSRunningApplication] = [:]
    for app in workspace.runningApplications {
        let pid = app.processIdentifier
        if targetPids == nil || targetPids!.contains(pid) {
            appMap[pid] = app
        }
    }

    // Cache AX windows per pid to avoid repeated calls
    var axWindowsCache: [pid_t: [AXUIElement]] = [:]

    // Build window info from CGWindowList (in z-order) and match with AX windows
    var zOrder = 0
    var seenPidTitle = Set<String>()

    for cgWindow in windowList {
        guard let layer = cgWindow[kCGWindowLayer as String] as? Int,
            layer == 0,
            let ownerPid = cgWindow[kCGWindowOwnerPID as String] as? Int
        else { continue }

        let pid = pid_t(ownerPid)
        guard let app = appMap[pid] else { continue }

        let cgTitle = cgWindow[kCGWindowName as String] as? String ?? ""

        // Skip empty titles (usually non-main windows)
        if cgTitle.isEmpty { continue }

        let key = "\(pid):\(cgTitle)"
        if seenPidTitle.contains(key) { continue }
        seenPidTitle.insert(key)

        // Get AX windows from cache or fetch
        let axWindows: [AXUIElement]
        if let cached = axWindowsCache[pid] {
            axWindows = cached
        } else {
            let axApp = AXUIElementCreateApplication(pid)
            var windowsRef: CFTypeRef?
            guard
                AXUIElementCopyAttributeValue(
                    axApp,
                    kAXWindowsAttribute as CFString,
                    &windowsRef
                ) == .success,
                let windows = windowsRef as? [AXUIElement]
            else { continue }
            axWindows = windows
            axWindowsCache[pid] = windows
        }

        for axWindow in axWindows {
            var titleRef: CFTypeRef?
            AXUIElementCopyAttributeValue(
                axWindow,
                kAXTitleAttribute as CFString,
                &titleRef
            )
            let axTitle = (titleRef as? String) ?? ""

            // Match by title
            if axTitle == cgTitle || (cgTitle.isEmpty && axTitle.isEmpty) {
                var minimizedRef: CFTypeRef?
                let minResult = AXUIElementCopyAttributeValue(
                    axWindow,
                    kAXMinimizedAttribute as CFString,
                    &minimizedRef
                )
                var isMinimized = false
                if minResult == .success, let boolRef = minimizedRef {
                    isMinimized = (boolRef as! CFBoolean) == kCFBooleanTrue
                }

                results.append(
                    WindowInfo(
                        app: app,
                        element: axWindow,
                        title: axTitle,
                        pid: pid,
                        isMinimized: isMinimized,
                        zOrder: zOrder
                    )
                )
                zOrder += 1
                break
            }
        }
    }

    // Already in z-order from CGWindowList
    return results
}

func regexMatch(_ string: String, _ pattern: String) -> Bool {
    guard
        let regex = try? NSRegularExpression(
            pattern: pattern,
            options: [.caseInsensitive]
        )
    else {
        return string.lowercased().contains(pattern.lowercased())
    }
    let range = NSRange(string.startIndex..., in: string)
    return regex.firstMatch(in: string, options: [], range: range) != nil
}

func getExecInfo(_ path: String) -> (
    bundleId: String?, appName: String?, execPath: String
)? {
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
        if let bundle = Bundle(url: appUrl),
            let bundleId = bundle.bundleIdentifier
        {
            return (
                bundleId, appUrl.deletingPathExtension().lastPathComponent,
                execPath
            )
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
    let paths =
        ProcessInfo.processInfo.environment["PATH"]?.split(separator: ":") ?? []
    for dir in paths {
        let fullPath = "\(dir)/\(command)"
        if FileManager.default.fileExists(atPath: fullPath) {
            return fullPath
        }
    }
    let commonPaths = [
        "/usr/local/bin", "/usr/bin", "/bin", "/opt/local/bin",
        "/Applications/MacPorts/Alacritty.app/Contents/MacOS",
    ]
    for dir in commonPaths {
        let fullPath = "\(dir)/\(command)"
        if FileManager.default.fileExists(atPath: fullPath) {
            return fullPath
        }
    }
    return nil
}

// Cached exec info to avoid repeated lookups
var cachedExecInfo: (bundleId: String?, appName: String?, execPath: String)? = nil
var execInfoResolved = false

func getOrCacheExecInfo(_ execPath: String) -> (bundleId: String?, appName: String?, execPath: String)? {
    if execInfoResolved {
        return cachedExecInfo
    }
    execInfoResolved = true
    cachedExecInfo = getExecInfo(execPath)
    return cachedExecInfo
}

func matchesOptions(_ info: WindowInfo, _ options: Options, targetPid: pid_t? = nil) -> Bool {
    // Check skip-title first (always applies regardless of match mode)
    if !options.skipTitlePatterns.isEmpty {
        for pattern in options.skipTitlePatterns {
            if regexMatch(info.title, pattern) { return false }
        }
    }

    let titleMatch =
        options.titlePattern.map { regexMatch(info.title, $0) } ?? true
    let bundleMatch =
        options.bundleId.map { info.app.bundleIdentifier == $0 } ?? true
    let appMatch =
        options.appName.map { regexMatch(info.app.localizedName ?? "", $0) }
        ?? true

    var execMatch = true
    if let execPath = options.execPath {
        if let execInfo = getOrCacheExecInfo(execPath) {
            let matchesBundle =
                execInfo.bundleId.map { $0 == info.app.bundleIdentifier }
                ?? false
            let matchesApp =
                execInfo.appName.map {
                    regexMatch(info.app.localizedName ?? "", $0)
                } ?? false
            let matchesExec =
                info.app.executableURL.map { appExec in
                    var appPath = appExec.path
                    if let target = try? FileManager.default
                        .destinationOfSymbolicLink(atPath: appPath)
                    {
                        appPath = target
                    }
                    return appPath == execInfo.execPath
                        || execPath == appExec.path
                } ?? false
            execMatch = matchesBundle || matchesApp || matchesExec
        } else {
            // Fast path: already know the pid, only need to check title
            if let pid = targetPid {
                if info.pid != pid { return false }
                return titleMatch
            }
            execMatch = false
        }
    }

    // Fast path: pid already resolved, only verify title and skip filters
    if let pid = targetPid {
        if info.pid != pid { return false }
        return titleMatch
    }

    if options.matchAll {
        return titleMatch && bundleMatch && appMatch && execMatch
    } else {
        // OR mode: at least one specified criterion must match
        var specified = false
        var anyMatch = false
        if options.titlePattern != nil {
            specified = true
            if titleMatch { anyMatch = true }
        }
        if options.bundleId != nil {
            specified = true
            if bundleMatch { anyMatch = true }
        }
        if options.appName != nil {
            specified = true
            if appMatch { anyMatch = true }
        }
        if options.execPath != nil {
            specified = true
            if execMatch { anyMatch = true }
        }
        return specified && anyMatch
    }
}

func focusWindow(_ info: WindowInfo) {
    // Unhide the app if it was hidden via app.hide()
    if info.app.isHidden {
        info.app.unhide()
    }

    if info.isMinimized {
        AXUIElementSetAttributeValue(
            info.element,
            kAXMinimizedAttribute as CFString,
            kCFBooleanFalse
        )
    }

    let axApp = AXUIElementCreateApplication(info.pid)

    AXUIElementPerformAction(info.element, kAXRaiseAction as CFString)
    AXUIElementSetAttributeValue(info.element, kAXMainAttribute as CFString, kCFBooleanTrue)
    AXUIElementSetAttributeValue(info.element, kAXFocusedAttribute as CFString, kCFBooleanTrue)

    // Retry activation until the OS confirms frontmost has changed,
    // to handle race conditions when invoked from hotkey managers (e.g. karabiner).
    let deadline = Date(timeIntervalSinceNow: 0.3)
    repeat {
        AXUIElementSetAttributeValue(axApp, kAXFrontmostAttribute as CFString, kCFBooleanTrue)
        info.app.activate()
        // Thread.sleep(forTimeInterval: 0.001)
    } while NSWorkspace.shared.frontmostApplication?.processIdentifier != info.pid
           && Date() < deadline
}

func executePreCmd(_ cmd: [String], sourceWindow: WindowInfo?) {
    guard !cmd.isEmpty else { return }

    var env = ProcessInfo.processInfo.environment
    if env["PATH"] == nil {
        env["PATH"] = "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/bin"
    }

    // Inject source window info as environment variables
    if let src = sourceWindow {
        env["SOURCE_TITLE"] = src.title
        env["SOURCE_APP"] = src.app.localizedName ?? ""
        env["SOURCE_PID"] = String(src.pid)
    }

    let task = Process()
    var cmdPath = cmd[0]

    // Resolve command path
    if !cmdPath.hasPrefix("/") {
        if let found = which(cmdPath) {
            cmdPath = found
        }
    }

    task.executableURL = URL(fileURLWithPath: cmdPath)
    task.environment = env
    if cmd.count > 1 {
        task.arguments = Array(cmd[1...])
    }

    do {
        try task.run()
    } catch {
        print("ERROR: Failed to execute pre command: \(cmd.joined(separator: " "))")
        print("       \(error)")
    }
}

func executePostCmd(_ cmd: [String], sourceWindow: WindowInfo?, targetWindow: WindowInfo?) {
    guard !cmd.isEmpty else { return }
    
    var env = ProcessInfo.processInfo.environment
    if env["PATH"] == nil {
        env["PATH"] = "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/bin"
    }
    
    // Inject source window info
    if let src = sourceWindow {
        env["SOURCE_TITLE"] = src.title
        env["SOURCE_APP"] = src.app.localizedName ?? ""
        env["SOURCE_PID"] = String(src.pid)
    }
    
    // Inject target window info
    if let tgt = targetWindow {
        env["TARGET_TITLE"] = tgt.title
        env["TARGET_APP"] = tgt.app.localizedName ?? ""
        env["TARGET_PID"] = String(tgt.pid)
    }

    let task = Process()
    var cmdPath = cmd[0]
    
    // Resolve command path
    if !cmdPath.hasPrefix("/") {
        if let found = which(cmdPath) {
            cmdPath = found
        }
    }
    
    task.executableURL = URL(fileURLWithPath: cmdPath)
    task.environment = env
    if cmd.count > 1 {
        task.arguments = Array(cmd[1...])
    }
    
    do {
        try task.run()
    } catch {
        print("ERROR: Failed to execute post command: \(cmd.joined(separator: " "))")
        print("       \(error)")
    }
}

func hideWindow(_ info: WindowInfo) {
    AXUIElementSetAttributeValue(
        info.element,
        kAXMinimizedAttribute as CFString,
        kCFBooleanTrue
    )
}

func isFocusedWindow(_ info: WindowInfo) -> Bool {
    let workspace = NSWorkspace.shared
    guard let frontApp = workspace.frontmostApplication else { return false }

    if info.pid != frontApp.processIdentifier { return false }

    let axApp = AXUIElementCreateApplication(info.pid)
    var focusedWindowRef: CFTypeRef?
    guard
        AXUIElementCopyAttributeValue(
            axApp,
            kAXFocusedWindowAttribute as CFString,
            &focusedWindowRef
        ) == .success,
        let focusedWindow = focusedWindowRef
    else { return false }

    // Compare by title since we can't reliably get window IDs
    let axFocused = focusedWindow as! AXUIElement
    var titleRef: CFTypeRef?
    AXUIElementCopyAttributeValue(
        axFocused,
        kAXTitleAttribute as CFString,
        &titleRef
    )
    let focusedTitle = (titleRef as? String) ?? ""

    return info.title == focusedTitle
}

// MARK: - Main

guard checkAccessibilityPermission() else {
    print("ERROR: Accessibility permission required")
    print(
        "Please grant permission in System Settings > Privacy & Security > Accessibility"
    )
    exit(2)
}

guard let options = parseArgs() else {
    print("Usage: run-or-raise [options] [--cmd command [args...]]")
    print("")
    print("Window matching options (can be combined, default: AND logic):")
    print("  --title  <regex>   Match window title (case-insensitive regex)")
    print("                     e.g. --title ssh   --title 'Gmail|Calendar'")
    print("  --id     <bundle>  Match app bundle identifier (most reliable)")
    print("                     e.g. --id com.apple.Safari")
    print("                     e.g. --id org.mozilla.firefox")
    print("  --app    <regex>   Match app name (case-insensitive regex)")
    print("                     e.g. --app Emacs  --app 'Firefox|Chrome'")
    print("  --exec   <path>    Match by executable name or path.")
    print("                     Works for apps without a bundle ID (e.g. CLI emacs).")
    print("                     e.g. --exec emacs  --exec /usr/local/bin/emacs")
    print("  --skip-title <regex>  Exclude windows whose title matches this regex.")
    print("                     Can be repeated for multiple patterns.")
    print("                     e.g. --skip-title WatermarkWidget")
    print("  --match  and|or    How to combine the above criteria (default: and).")
    print("                     'and': all specified criteria must match (e.g. app=alacritty AND title=ssh)")
    print("                     'or':  any criterion matches (e.g. app=emacs OR title=ssh)")
    print("")
    print("Behavior options:")
    print("  --toggle           If the matched window is already frontmost, hide the app")
    print("                     (macOS auto-restores the previous app).")
    print("                     Repeated presses cycle through all matched windows.")
    print("  --env KEY=VALUE    Set environment variable when launching (--cmd). Repeatable.")
    print("  --pre-cmd <cmd> [args]")
    print("                     Command to run BEFORE focusing a window.")
    print("                     Receives env vars: SOURCE_TITLE, SOURCE_APP, SOURCE_PID")
    print("  --post-cmd <cmd> [args]")
    print("                     Command to run AFTER focusing a window.")
    print("                     Receives env vars: SOURCE_*, TARGET_*")
    print("  --cmd <cmd> [args] Command to run when no matching window is found.")
    print("                     Both --pre-cmd, --post-cmd and --cmd can be used together:")
    print("                     --pre-cmd cwd --post-cmd cd.sh --cmd emacs")
    print("                     Order is flexible: --cmd emacs --pre-cmd cwd also works.")
    print("")
    print("Environment variables for --pre-cmd:")
    print("  SOURCE_TITLE       Title of the window being switched away from")
    print("  SOURCE_APP         App name of the source window")
    print("  SOURCE_PID         PID of the source window")
    print("")
    print("Environment variables for --post-cmd:")
    print("  SOURCE_TITLE       Title of the window being switched away from")
    print("  SOURCE_APP         App name of the source window")
    print("  SOURCE_PID         PID of the source window")
    print("  TARGET_TITLE       Title of the window being focused")
    print("  TARGET_APP         App name of the target window")
    print("  TARGET_PID         PID of the target window")
    print("")
    print("How to find a bundle ID:")
    print("  # From a running app (replace 'Safari' with the app name):")
    print("  osascript -e 'id of app \"Safari\"'")
    print("  # List all running bundle IDs:")
    print("  swift - <<<'import AppKit; NSWorkspace.shared.runningApplications.forEach { print($0.bundleIdentifier ?? \"nil\", $0.localizedName ?? \"\") }'")
    print("  # From an .app bundle:")
    print("  defaults read /Applications/Firefox.app/Contents/Info CFBundleIdentifier")
    print("")
    print("Examples:")
    print("  # Focus/launch emacs (CLI, no bundle ID):")
    print("  run-or-raise --exec emacs --toggle --cmd emacs")
    print("  # Focus alacritty window with ssh in title:")
    print("  run-or-raise --exec alacritty --title ssh --toggle")
    print("  # Focus Firefox, launch if not running:")
    print("  run-or-raise --id org.mozilla.firefox --toggle --cmd open -a Firefox")
    print("  # Cycle emacs windows, skip *scratch* buffers:")
    print("  run-or-raise --exec emacs --skip-title '^\\*' --toggle --cmd emacs")
    print("  # OR: focus emacs OR any window titled 'vim':")
    print("  run-or-raise --match or --app emacs --title vim")
    print("")
    print("When multiple windows match, repeated calls cycle through them.")
    exit(1)
}

guard
    options.titlePattern != nil || options.bundleId != nil
        || options.appName != nil || options.execPath != nil
else {
    print("ERROR: Must specify --title, --id, --app, or --exec")
    exit(1)
}

// Pre-resolve exec info once before filtering
if let execPath = options.execPath {
    _ = getOrCacheExecInfo(execPath)
}

// Find target app(s) early.
// Collect by ALL applicable methods and merge (handles cases like two processes
// with the same name: one bundled, one not).
var targetApps: [NSRunningApplication] = []
let workspace = NSWorkspace.shared

// OR mode: we can't narrow by app upfront (title may match any app), use slow path
let canUseFastPath = options.matchAll

if canUseFastPath {
    var targetPidsSeen = Set<pid_t>()
    func addApp(_ app: NSRunningApplication) {
        if targetPidsSeen.insert(app.processIdentifier).inserted {
            targetApps.append(app)
        }
    }

    // 1. By explicit --id or resolved bundleId from exec
    if let bundleId = options.bundleId ?? cachedExecInfo?.bundleId {
        for app in workspace.runningApplications where app.bundleIdentifier == bundleId {
            addApp(app)
        }
    }

    // 2. By --app name or resolved app name from exec
    if let appName = options.appName ?? cachedExecInfo?.appName {
        let lower = appName.lowercased()
        for app in workspace.runningApplications {
            guard let localName = app.localizedName else { continue }
            if localName.lowercased() == lower || regexMatch(localName, appName) {
                addApp(app)
            }
        }
    }

    // 3. By --exec process name (catches non-bundle processes with same name)
    if let execPath = options.execPath {
        let processName = URL(fileURLWithPath: execPath).lastPathComponent.lowercased()
        for app in workspace.runningApplications {
            if let localName = app.localizedName, localName.lowercased() == processName {
                addApp(app)
            }
        }
        // Also try by exec path (symlink-resolved)
        if let foundPath = which(execPath) {
            let fm = FileManager.default
            let resolved = (try? fm.destinationOfSymbolicLink(atPath: foundPath)) ?? foundPath
            for app in workspace.runningApplications {
                guard let exec = app.executableURL?.path else { continue }
                let appExec = (try? fm.destinationOfSymbolicLink(atPath: exec)) ?? exec
                if appExec == resolved || exec == execPath { addApp(app) }
            }
        }
    }
}

// Get target PIDs for fast matching
let targetPidSet: Set<pid_t> = Set(targetApps.map { $0.processIdentifier })

// Check if current frontmost app is one of our targets
let frontmostApp = workspace.frontmostApplication

var matchedWindows: [WindowInfo] = []
var unmatchedWindows: [WindowInfo] = []
var allWindows: [WindowInfo] = []

if !targetApps.isEmpty {
    // Always get target windows directly (fast)
    for app in targetApps {
        let windows = getWindowsForPid(app.processIdentifier, app)
        if options.titlePattern == nil && options.skipTitlePatterns.isEmpty {
            matchedWindows.append(contentsOf: windows)
        } else {
            for window in windows {
                if matchesOptions(window, options, targetPid: app.processIdentifier) {
                    matchedWindows.append(window)
                }
            }
        }
    }
    allWindows = matchedWindows

    // For toggle: if frontmost IS target, lazily find first unmatched window
    // Don't enumerate all windows upfront - defer until actually needed
} else {
    // No known targets (e.g. --title only), search all windows
    let targetWindows = getWindowsInZOrder(targetPids: nil)
    for window in targetWindows {
        if matchesOptions(window, options) {
            matchedWindows.append(window)
        } else {
            unmatchedWindows.append(window)
        }
    }
    allWindows = targetWindows
}

// frontmostIsTarget: either we know the app by PID, or the frontmost window title matches
let frontmostIsTarget: Bool
if !targetPidSet.isEmpty {
    frontmostIsTarget = frontmostApp.map { targetPidSet.contains($0.processIdentifier) } ?? false
} else {
    // title-only or no-app match: check if the current frontmost window is in matchedWindows
    let frontPid = frontmostApp?.processIdentifier
    frontmostIsTarget = matchedWindows.contains { $0.pid == frontPid }
}

if matchedWindows.isEmpty {
    if !options.fallbackCmd.isEmpty {
        print(
            "NOT_FOUND: Running command: \(options.fallbackCmd.joined(separator: " "))"
        )
        var env = ProcessInfo.processInfo.environment
        if env["PATH"] == nil {
            env["PATH"] =
                "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/bin"
        }
        for varItem in options.envVars {
            let parts = varItem.split(separator: "=", maxSplits: 1)
            if parts.count == 2 {
                env[String(parts[0])] = String(parts[1])
            }
        }
        let task = Process()
        var cmdPath = options.fallbackCmd[0]
        if !cmdPath.hasPrefix("/") {
            if let found = which(cmdPath) {
                cmdPath = found
            }
        }
        task.executableURL = URL(fileURLWithPath: cmdPath)
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
        if let execInfo = getExecInfo(execPath) {
            if let bundleId = execInfo.bundleId {
                print("NOT_FOUND: Opening: \(execInfo.appName ?? execPath)")
                let task = Process()
                task.executableURL = URL(fileURLWithPath: "/usr/bin/open")
                task.arguments = ["-b", bundleId]
                try? task.run()
                exit(0)
            } else if execInfo.appName != nil {
                print("NOT_FOUND: Opening: \(execInfo.appName ?? execPath)")
                let task = Process()
                task.executableURL = URL(fileURLWithPath: "/usr/bin/open")
                task.arguments = ["-a", execInfo.execPath]
                try? task.run()
                exit(0)
            } else {
                var runPath = execInfo.execPath
                if !runPath.hasPrefix("/"), let found = which(runPath) {
                    runPath = found
                }
                print("NOT_FOUND: Running: \(runPath)")
                var env = ProcessInfo.processInfo.environment
                if env["PATH"] == nil {
                    env["PATH"] =
                        "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/bin"
                }
                for varItem in options.envVars {
                    let parts = varItem.split(separator: "=", maxSplits: 1)
                    if parts.count == 2 {
                        env[String(parts[0])] = String(parts[1])
                    }
                }
                let task = Process()
                task.executableURL = URL(fileURLWithPath: runPath)
                task.environment = env
                try? task.run()
                exit(0)
            }
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

func readCycleState() -> (pattern: String, index: Int, timestamp: TimeInterval)?
{
    guard let data = FileManager.default.contents(atPath: stateFile),
        let content = String(data: data, encoding: .utf8)
    else { return nil }
    let parts = content.split(separator: "\n")
    guard parts.count >= 3,
        let index = Int(parts[1]),
        let timestamp = TimeInterval(parts[2])
    else { return nil }
    return (String(parts[0]), index, timestamp)
}

func writeCycleState(pattern: String, index: Int) {
    let content = "\(pattern)\n\(index)\n\(Date().timeIntervalSince1970)"
    try? content.write(toFile: stateFile, atomically: true, encoding: .utf8)
}

// Build a pattern key for state tracking
let patternKey =
    "\(options.appName ?? "")-\(options.bundleId ?? "")-\(options.titlePattern ?? "")-\(options.execPath ?? "")"

// Get the current frontmost window info before any focus changes (source window)
func getFrontmostWindowInfo() -> WindowInfo? {
    guard let frontApp = NSWorkspace.shared.frontmostApplication else { return nil }
    let pid = frontApp.processIdentifier
    let windows = getWindowsForPid(pid, frontApp)
    // Return the first non-minimized window (usually the focused one)
    return windows.first { !$0.isMinimized }
}

if matchedWindows.count == 1 {
    // Only one matched window
    let theMatch = matchedWindows[0]

    // Check if this specific window is already focused (not just the app)
    let isWindowFocused = isFocusedWindow(theMatch)

    if options.isToggle && isWindowFocused {
        // Hide the target app - macOS auto-activates the previous app
        theMatch.app.hide()
        writeCycleState(pattern: patternKey, index: -1)
        print("HIDDEN: '\(theMatch.title)'")
        exit(0)
    } else {
        // Get source window before focus change
        let sourceWindow = getFrontmostWindowInfo()
        
        // Execute pre command if specified
        if !options.preCmd.isEmpty {
            executePreCmd(options.preCmd, sourceWindow: sourceWindow)
        }
        
        focusWindow(theMatch)
        writeCycleState(pattern: patternKey, index: 0)
        print("FOCUS: '\(theMatch.title)'")

        // Execute post command if specified
        if !options.postCmd.isEmpty {
            executePostCmd(options.postCmd, sourceWindow: sourceWindow, targetWindow: theMatch)
        }

        exit(0)
    }
}

// Multiple matched windows
// Check state to determine next index
var nextIndex = 0
let now = Date().timeIntervalSince1970

if let state = readCycleState(),
    state.pattern == patternKey,
    now - state.timestamp < 5.0
{  // State is fresh (within 5 seconds)
    // Continue cycling from last position
    nextIndex = (state.index + 1) % matchedWindows.count
} else {
    // Fresh start or different pattern, start from 0
    nextIndex = 0
}

// Check if we should toggle (at the end of cycle)
// Only hide if the currently focused window is the one we would focus (matchedWindows[nextIndex])
if options.isToggle && nextIndex == 0 {
    let currentWindow = matchedWindows[0]
    let isCurrentWindowFocused = isFocusedWindow(currentWindow)
    
    if isCurrentWindowFocused,
        let state = readCycleState(),
        state.pattern == patternKey,
        state.index == matchedWindows.count - 1,
        now - state.timestamp < 5.0
    {
        // Just finished a complete cycle, hide the app
        matchedWindows[0].app.hide()
        writeCycleState(pattern: patternKey, index: -1)
        print("HIDDEN: '\(matchedWindows[0].title)'")
        exit(0)
    }
}

// Get source window before any focus change
let sourceWindow = getFrontmostWindowInfo()

// Execute pre command if specified (before focus)
if !options.preCmd.isEmpty {
    executePreCmd(options.preCmd, sourceWindow: sourceWindow)
}

focusWindow(matchedWindows[nextIndex])
writeCycleState(pattern: patternKey, index: nextIndex)
print(
    "CYCLE: \(nextIndex + 1)/\(matchedWindows.count) '\(matchedWindows[nextIndex].title)'"
)

// Execute post command if specified (after focus)
if !options.postCmd.isEmpty {
    executePostCmd(options.postCmd, sourceWindow: sourceWindow, targetWindow: matchedWindows[nextIndex])
}

exit(0)

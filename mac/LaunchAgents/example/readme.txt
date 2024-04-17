 Launch Daemon: 在開機時載入 (load) 。
Launch Agent: 在使用者登入時載入。

Launch Daemon 是 system-wide 的 service ，稱為 daemon，Launch Agent 是 per-user
的 service ，稱為 agent，前者在開機時會載入 （load） ，後者在使用者登入時（才）
會載入

launchctl load test.plist
launchctl unload test.plist
launchctl start test
launchctl stop test

launchctl list|grep test
# man launchd.plist

# copy passwd to clipboard
```
pass show -c github.com
pass show -c www.jianguoyun.com/jixiuf@qq.com
```
# get passwd
```
pass show github.com |head -n 1
```
# get username to clipboard
```
pass show github.com |grep login| awk -F': ' '{print $2}'
```
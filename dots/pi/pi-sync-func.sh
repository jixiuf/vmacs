# pi-sync-func.sh — pi 跨机器 session 同步
# source 此文件即可，不要直接执行

# 确保统一路径软链接存在
__pi_ensure_link() {
    local link="/var/tmp/pi-sync/jixiuf"
    if [ ! -L "$link" ]; then
        mkdir -p "$(dirname "$link")" 2>/dev/null
        ln -sfn "$HOME" "$link" 2>/dev/null
    fi
}

# pi 包装函数: 自动把 ~/xxx 路径翻译为 /var/tmp/pi-sync/jixiuf/xxx
function pi {
    __pi_ensure_link

    local cwd="$PWD"
    local norm=""

    # 把本机 home 路径翻译成统一路径
    case "$cwd" in
        "$HOME"|"$HOME"/*)
            norm="/var/tmp/pi-sync/jixiuf${cwd#$HOME}"
            ;;
        *)
            # 不在 home 下，保持原路径（session 不会跨机器同步也无所谓）
            command pi "$@"
            return
            ;;
    esac

    # 确保归一化路径存在（实际上软链接已保证）
    if [ -d "$norm" ] || [ -f "$norm" ]; then
        cd "$norm"
        command pi "$@"
        cd "$cwd"
    else
        echo "⚠️  无法访问 $norm ，使用原始路径"
        command pi "$@"
    fi
}

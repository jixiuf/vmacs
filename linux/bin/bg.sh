# Download the latest pic of the day from NASA and delete or archive any
# previous ones in a specified directory. Optionally create a copy of the most
# current picture to allow OSX to pick up pictures as wallpapers
#
# Steve Challis 2011
# http://schallis.com/2011/mar/20/nasa-astronomy-picture-of-the-day-background/
skip_download="$1"
DEST='/home/jixiuf/Documents/jianguo/jianguo/wallpaper'
NAME_COPY='imgcopy.jpg'
BASE='http://apod.nasa.gov'
DATE=`date "+%Y%m%d"`
NAME="$DATE-img.jpg"

# KEEP_ARCHIVE=true

function ensure_dir() {
    # Check if passed directory exists
    if [ ! -d $1 ]; then
        # create it if not
        echo "Creating archive directory ..."
        mkdir $1
    fi
}

function download_new_pic() {
    local TEMP_FILE=$(mktemp)  # 创建临时文件
    # 下载新图片到临时文件
    echo "Downloading new picture to temp file ..."
    wget -qO- http://apod.nasa.gov/apod/ |
        grep "href=\"image" | head -n 1 |
        sed "s;.*\"\(.*\)\".*; wget -O $TEMP_FILE $BASE/\1;" |
        bash -

    # 检查下载是否成功
    if [ ! -s "$TEMP_FILE" ]; then
        echo "Download failed or empty file."
        rm "$TEMP_FILE"
        return 1
    fi

    # 如果目标文件存在，比较MD5哈希
    if [ -f "$DEST/$NAME" ]; then
        local old_md5=$(md5sum "$DEST/$NAME" | awk '{print $1}')
        local new_md5=$(md5sum "$TEMP_FILE" | awk '{print $1}')
        if [ "$old_md5" = "$new_md5" ]; then
            echo "Image unchanged, skipping download."
            rm "$TEMP_FILE"
            return 0
        fi
    fi

    # 图片不同，移动旧图片到归档目录
    # move_old_pic

    # 移动临时文件到目标位置
    mv "$TEMP_FILE" "$DEST/$NAME"
}

export NEW_IMG="$(find $DEST/ -type f | shuf -n1)"
echo "Using new backgruond image $NEW_IMG"
pidof swww-daemon || swww-daemon
swww img $NEW_IMG&
ln -sf $NEW_IMG /tmp/screenlock.jpg
if [ -z "$skip_download" ]; then
    # 触发下载过程
    download_new_pic
fi

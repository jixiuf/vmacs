# Download the latest pic of the day from NASA and delete or archive any
# previous ones in a specified directory. Optionally create a copy of the most
# current picture to allow OSX to pick up pictures as wallpapers
#
# Steve Challis 2011
# http://schallis.com/2011/mar/20/nasa-astronomy-picture-of-the-day-background/

DEST='/home/jixiuf/Pictures/bg'
NAME='img.jpg'
NAME_COPY='imgcopy.jpg'
BASE='http://apod.nasa.gov'
ARCHIVE_DIR='/home/jixiuf/Pictures/bg/old'
DATE=`date "+%Y%m%d"`

KEEP_ARCHIVE=false
MAKE_COPY=false

function ensure_dir() {
    # Check if passed directory exists
    if [ ! -d $1 ]; then
        # create it if not
        echo "Creating archive directory ..."
        mkdir $1
    fi
}

function move_old_pic() {
    # If the user has chosen to archive old pictures then move them to an
    # archive directory. Otherwise they will be overwritten
    ensure_dir $ARCHIVE_DIR
    if [ -f "$DEST/$NAME" ]; then
        echo "Archiving old picture ..."
        $KEEP_ARCHIVE && mv "$DEST/$NAME" "$DEST/$ARCHIVE_DIR/$DATE-$NAME"
    fi
}

function download_new_pic() {
    # Get a fresch picture from NASA
    echo "Downloading new picture ..."
    wget -qO- http://apod.nasa.gov/apod/ |
        grep "href=\"image" | head -n 1 |
        sed "s;.*\"\(.*\)\".*; wget -O $DEST/$NAME $BASE/\1;" |
        bash -

    # Copy the image if needed
    $MAKE_COPY &&
        echo "Copying picture ..." &&
        cp "$DEST/$NAME" "$DEST/$NAME_COPY"

    # Remove the copy the user decides they do not want one anymore
    if [ -f "$DEST/$NAME_COPY" ]; then
        $MAKE_COPY ||
            (echo "Removing copy ..." &&
            rm "$DEST/$NAME_COPY")
    fi
}

# Kick off the process
move_old_pic
download_new_pic

#!/usr/bin/env python3

import json

stop_icon=""

data = {
        "text": f"{stop_icon}",
        "tooltip": f"Click {stop_icon} to stop screen recording"
}

print(json.dumps(data));


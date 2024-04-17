git clone https://github.com/doy/rbw.git
cargo build --release
rbw register # 第1次

emerge dev-python/pip
  pip install --user rofi-rbw  --break-system-packages
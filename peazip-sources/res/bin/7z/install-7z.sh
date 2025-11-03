# URL = "add here the URL to download 7z"
# SHA256 = "add here the SHA256 checksum of the 7z archive"
#!/bin/sh
set -e
ARCHIVE="7z.tar.xz"
INSTALL_DIR="/usr/local/bin"/7z
DOWNLOAD_URL="https://www.7-zip.org/a/7z2201-linux-x64.tar.xz"
wget -O "$ARCHIVE" "$DOWNLOAD_URL"
tar -xf "$ARCHIVE"
mkdir -p "$INSTALL_DIR"
mv 7zz "$INSTALL_DIR"/7zz
ln -sf "$INSTALL_DIR"/7zz /usr/local/bin/7zz
rm "$ARCHIVE"
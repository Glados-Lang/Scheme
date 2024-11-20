ghcup --version

if [ $? -eq 0 ]; then
  echo "\033[92mghcup is already installed\033[0m"
  exit 0
fi

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
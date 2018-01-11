# Run tests
if [[ "$(uname)" = "Darwin" ]]; then
    brew update && brew install coreutils
    timeout_cmd="gtimeout"
else
    timeout_cmd="timeout"
fi
$timeout_cmd 20m stack --no-terminal --jobs=2 --install-ghc --system-ghc build --only-dependencies
ret=$?
case "$ret" in
    0)
    # continue
    ;;
    124)
        echo "Timed out while installing dependencies."
        echo "Try building again by pushing a new commit."
        exit 1
        ;;
    *)
        echo "Failed to install dependencies; stack exited with $ret"
        exit "$ret"
        ;;
esac
